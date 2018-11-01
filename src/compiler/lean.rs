
use compiler::{ColorMode, Compiler, CompilerArguments, CompilerHasher, CompilerKind, HashResult};
use std::path::{PathBuf,Path};
use std::ffi::{OsString};
#[cfg(feature = "dist-client")]
use dist::pkg;
use futures::{Future};
use futures_cpupool::CpuPool;
use std::os::unix::ffi::OsStringExt;
use std::borrow::Cow;
use mock_command::{CommandCreatorSync, RunCommand};
use util::*;
// use compiler::{NoopOutputsRewriter, OutputsRewriter};

use std::sync::{Arc,Mutex};
use std::collections::{HashMap,BTreeSet};

use errors::*;

lazy_static! {
    static ref dep_cache: Mutex<HashMap<PathBuf,BTreeSet<PathBuf>>> =
        Mutex::new(HashMap::new());
}

#[derive(Clone, Debug)]
pub struct Lean {
    executable: PathBuf,
    deps: Vec<OsString>
}

#[derive(Debug, Clone)]
pub struct LeanHasher
{
    parsed_args: ParsedArguments,
    executable: PathBuf,
    // compiler_shlibs_digests: Vec<String>,
    // executable_digest: String,
    deps: Vec<OsString>
}

fn lean_deps<T>(creator: &T, args: &ParsedArguments) -> SFuture<Vec<OsString>>
    where T: CommandCreatorSync,
{ // println!("- exe: {:?}", args.executable.canonicalize());
  let mut cmd = creator.clone().new_command_sync(&args.executable);
  cmd.arg("--deps")
     .arg(&args.input)
     .current_dir(&args.cwd);
  let deps = run_input_output(cmd, None);
  Box::new(
      deps.and_then(move |deps| -> SFuture<_> {
          println!("- stdout: {:?}", OsString::from_vec(deps.stdout.clone()));
          println!("- stderr: {:?}", OsString::from_vec(deps.stderr.clone()));
          match String::from_utf8(deps.stdout) {
              Ok(outstr) =>
              { let mut deps = Vec::new();
                println!(": {}", outstr);
                for ln in outstr.lines()
                { deps.push (OsString::from(ln)) };
                f_ok(deps) }
              Err(_e) => f_err("Failed to parse output") } } ) )
}

// impl LeanHasher
// {
//     pub fn new<T>(creator: &T, args: &[OsString]) -> SFuture<LeanHasher>
//     where T: CommandCreatorSync,
//     {
//         let args = ParsedArguments::new(args);
//         let deps = lean_deps(creator, &args);
//         Box::new(
//             deps.and_then(move |deps| -> SFuture<_> {
//                 f_ok ( LeanHasher {
//                     deps: deps,
//                     executable: PathBuf::from("lean"),
//                     parsed_args: args.clone() } ) }
//         ) )
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedArguments
{
    pub input: PathBuf,
    pub args: Vec<OsString>,
    pub executable: PathBuf,
    cwd: PathBuf
    // pub args : HashMap<OsString,Option<OsString>>
}

pub enum LeanOptArg
{ NoArgument, RequiredArgument, OptionalArgument }

use self::LeanOptArg::*;

pub fn lean_cmd_opts_fn () -> Vec<(OsString,LeanOptArg,char)>
{ vec![(OsString::from("version"),   NoArgument, 'v'),
       (OsString::from("help"),      NoArgument, 'h'),
       (OsString::from("run"),       RequiredArgument, 'a'),
       (OsString::from("githash"),   NoArgument, 'g'),
       (OsString::from("make"),      NoArgument, 'm'),
       (OsString::from("recursive"), NoArgument, 'R'),
       (OsString::from("export"),    RequiredArgument, 'E'),
       (OsString::from("only-export"),  RequiredArgument, 'o'),
       (OsString::from("memory"),       RequiredArgument, 'M'),
       (OsString::from("trust"),        RequiredArgument, 't'),
       (OsString::from("profile"),      NoArgument,       'P'),
       (OsString::from("threads"),      RequiredArgument, 'j'),
       (OsString::from("quiet"),        NoArgument,       'q'),
       (OsString::from("deps"),         NoArgument,       'd'),
       (OsString::from("test-suite"),   NoArgument,       'e'),
       (OsString::from("timeout"),      OptionalArgument, 'T'),
       (OsString::from("json"),         NoArgument,       'J'),
       (OsString::from("path"),         NoArgument,       'p'),
       (OsString::from("server"),       OptionalArgument, 'S'),
       (OsString::from("doc"),          RequiredArgument, 'r'),
       (OsString::from("tstack"),       RequiredArgument, 's'),
       (OsString::from("debug"),        RequiredArgument, 'B') ] }

impl ParsedArguments {
    pub fn new(args : &[OsString], cwd: &PathBuf) -> ParsedArguments {
        assert!(args.len() == 2);
        assert!(args[0] == "--make");
        ParsedArguments {
            input: PathBuf::from(&args[1]),
            // executable: PathBuf::from(env!("HOME").to_owned() + "/lean/lean-master/bin/lean"),
            executable: PathBuf::from("echo"),
            args: args.to_vec(),
            cwd: cwd.clone() } }
    fn output_pretty(&self) -> Cow<str> {
        self.input.to_string_lossy()
    }
}

/// Version number for cache key.
const CACHE_VERSION: &[u8] = b"0";

impl<T> CompilerHasher<T> for LeanHasher
    where T: CommandCreatorSync,
{
    fn generate_hash_key(self: Box<Self>,
                         creator: &T,
                         cwd: PathBuf,
                         env_vars: Vec<(OsString, OsString)>,
                         may_dist: bool,
                         pool: &CpuPool)
                         -> SFuture<HashResult> {
        let source_hashes_pool = pool.clone();
        let mut m = Digest::new();
        // 1. A version
        m.update(CACHE_VERSION);
        // 2. compiler_shlibs_digests
        // 3. The full commandline (self.arguments)
        // TODO: there will be full paths here, it would be nice to
        // normalize them so we can get cross-machine cache hits.
        // A few argument types are not passed in a deterministic order
        // by cargo: --extern, -L, --cfg. We'll filter those out, sort them,
        // and append them to the rest of the arguments.
        // 4. The digest of all source files (this includes src file from cmdline).
        // 5. The digest of all files listed on the commandline (self.externs).
        // 6. The digest of all static libraries listed on the commandline (self.staticlibs).
        // 7. Environment variables. Ideally we'd use anything referenced
        // via env! in the program, but we don't have a way to determine that
        // currently, and hashing all environment variables is too much, so
        // we'll just hash the CARGO_ env vars and hope that's sufficient.
        // Upstream Rust issue tracking getting information about env! usage:
        // https://github.com/rust-lang/rust/issues/40364
        f_ok(HashResult {
            key: panic!("key"),
            weak_toolchain_key: panic!("toolchain"),
            compilation: panic!("compilation") }) }

    fn color_mode(&self) -> ColorMode {
        ColorMode::Auto
    }

    fn output_pretty(&self) -> Cow<str> {
        self.parsed_args.output_pretty()
    }

    fn box_clone(&self) -> Box<CompilerHasher<T>> {
        Box::new((*self).clone())
    }
}

impl Lean {
    pub fn new<T> (creator: &T, args: &[OsString], cwd: &PathBuf) -> SFuture<Lean>
        where T : CommandCreatorSync,
    { let args = ParsedArguments::new(args,cwd);
      let deps = lean_deps(creator, &args);
      let exe = args.executable.clone();
      Box::new(
          deps.and_then(move |deps|
                        f_ok( Lean { executable: exe,
                                     deps: deps } ) ) ) }
}

impl<T> Compiler<T> for Lean
    where T: CommandCreatorSync,
{
    /// Return the kind of compiler.
    fn kind(&self) -> CompilerKind { CompilerKind::Lean }
    /// Retrieve a packager
    #[cfg(feature = "dist-client")]
    fn get_toolchain_packager(&self) -> Box<pkg::ToolchainPackager> { f_error }
    /// Determine whether `arguments` are supported by this compiler.
    fn parse_arguments(&self,
                       arguments: &[OsString],
                       cwd: &Path) -> CompilerArguments<Box<CompilerHasher<T> + 'static>> {
        let cwd = cwd.to_path_buf();
        CompilerArguments::Ok(Box::new(
            LeanHasher { parsed_args: ParsedArguments::new(arguments,&cwd),
                         executable: self.executable.clone(),
                         deps: self.deps.clone() } )) }
    fn box_clone(&self) -> Box<Compiler<T>> {
        Box::new((*self).clone())
    }
}

#[cfg(feature = "dist-client")]
impl pkg::ToolchainPackager for Lean {
    fn write_pkg(self: Box<Self>, f: fs::File) -> Result<()> {
        Ok(()) } }
