
use compiler::{ColorMode, Compiler, CompilerArguments, CompilerHasher, CompilerKind, Compilation, HashResult, CompileCommand, Cacheable};
use std::path::{PathBuf,Path};
use std::ffi::{OsString};
use dist;
#[cfg(feature = "dist-client")]
use dist::pkg;
use futures::{Future};
use futures_cpupool::CpuPool;
use std::os::unix::ffi::OsStringExt;
use std::borrow::Cow;
use mock_command::{CommandCreatorSync, RunCommand};
use util::*;
// use compiler::{NoopOutputsRewriter, OutputsRewriter};

use std::sync::{Mutex};
use std::collections::{HashMap,BTreeSet,BTreeMap};

use errors::*;

lazy_static! {
    pub static ref DEP_CACHE: Mutex<HashMap<Key,BTreeSet<PathBuf>>> =
        Mutex::new(HashMap::new());
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedArguments
{
    pub input: PathBuf,
    pub args: Vec<OsString>,
    pub executable: PathBuf,
    cwd: PathBuf
}

impl ParsedArguments {
    pub fn new(args : &[OsString], cwd: &PathBuf) -> ParsedArguments {
        assert!(args.len() == 2);
        assert!(args[0] == "--make");
        ParsedArguments {
            input: PathBuf::from(&args[1]),
            executable: PathBuf::from(env!("HOME").to_owned() + "/lean/lean-master/bin/lean"),
            // executable: PathBuf::from("echo"),
            args: args.to_vec(),
            cwd: cwd.clone() } }
    fn output_pretty(&self) -> Cow<str> {
        self.input.to_string_lossy()
    }
}

/// Version number for cache key.
const CACHE_VERSION: &[u8] = b"0";

#[derive(Debug, Clone)]
pub struct LeanHasher
{
    parsed_args: ParsedArguments,
    executable: PathBuf,
}

impl LeanHasher
{
    pub fn new(exe: PathBuf,
               arguments: &[OsString],
               cwd: &Path) -> LeanHasher
    {
        let cwd = cwd.to_path_buf();
        let args = ParsedArguments::new(arguments,&cwd);
        LeanHasher { parsed_args: args,
                     executable: exe }
    }
}

fn lean_deps<T>(creator: &T, input: &PathBuf, args: &ParsedArguments)
                -> SFuture<Vec<PathBuf>>
    where T: CommandCreatorSync,
{ let mut cmd = creator.clone().new_command_sync(&args.executable);
  cmd.arg("--deps")
     .arg(&input)
     .current_dir(&args.cwd);
  let deps = run_input_output(cmd, None);
  Box::new(
      deps.and_then(move |deps| -> SFuture<_> {
          match String::from_utf8(deps.stdout) {
              Ok(outstr) =>
                  f_ok(outstr.lines()
                       .map(|ln| PathBuf::from(ln).with_extension("lean"))
                       .collect()),
              Err(_e) => f_err("Failed to parse output") } } ) ) }

fn lean_version<T>(creator: &T, args: &ParsedArguments)
                -> SFuture<String>
where T: CommandCreatorSync,
{ let mut cmd = creator.clone().new_command_sync(&args.executable);
  cmd.arg("--githash")
     .current_dir(&args.cwd);
  Box::new(run_input_output(cmd, None)
           .and_then(|r|
                     if let Ok(outstr) = String::from_utf8(r.stdout) {
                         f_ok(String::from(outstr.lines().next().unwrap()))
                     } else {
                         f_err("Failed to parse output") }))
}

type Key = (PathBuf,String);

fn lean_cached_deps<T>
    (creator: &T, input: &Key,
     args: &ParsedArguments,
     cache: &mut HashMap<Key,BTreeSet<PathBuf>>)
     -> BTreeSet<PathBuf>
where T: CommandCreatorSync,
{ cache.entry(input.clone()).or_insert_with (
    || { let res = lean_deps(creator, &input.0, &args)
         .and_then (|v|
                    f_ok(v.into_iter().collect()) );
        res.wait().unwrap()
    } ).clone() }

fn lean_hash_deps<T>
    (creator: &T, input: &PathBuf,
     args: &ParsedArguments,
     pool: &CpuPool,
     cache: &mut HashMap<Key,BTreeSet<PathBuf>>,
     visit: &mut BTreeMap<PathBuf,String>)
     -> ()
where T: CommandCreatorSync, {
    if !visit.contains_key (input) {
        let h: String = Digest::file(args.cwd.join(input),&pool).wait().unwrap();
        visit.insert(input.clone(),h.clone());
        let deps = lean_cached_deps(
            creator,
            &(input.clone(),h.clone()),
            args,&mut *cache);
        for m in deps
        { lean_hash_deps(creator,&m,args,pool,cache,visit) }
    } }

fn transitive_hash<T>
    (creator: &T, input: &PathBuf,
     args: &ParsedArguments,
     pool: &CpuPool) ->
    BTreeMap<PathBuf,String>
where T: CommandCreatorSync, {
    let mut m = BTreeMap::new();
    let mut cache = DEP_CACHE.lock().unwrap();
    lean_hash_deps(creator,input,args,pool,&mut *cache,&mut m);
    m
}

impl<T> CompilerHasher<T> for LeanHasher
    where T: CommandCreatorSync,
{
    fn generate_hash_key(self: Box<Self>,
                         creator: &T,
                         _cwd: PathBuf,
                         _env_vars: Vec<(OsString, OsString)>,
                         _may_dist: bool,
                         pool: &CpuPool)
                         -> SFuture<HashResult> {
        // let source_hashes_pool = pool.clone();
        let mut m = Digest::new();
        // 1. A version
        m.update(CACHE_VERSION);
        // 2. compiler version
        m.update(lean_version(creator, &self.parsed_args).wait().unwrap().as_bytes());
        let toolchain = m.clone().finish();
        // 3. all sources directly or indirectly imported by self.parsed_args.input
        for src in transitive_hash(creator, &self.parsed_args.input, &self.parsed_args, pool)
        { m.update(&src.0.into_os_string().into_vec()); m.update(src.1.as_bytes());  }
        let input = self.parsed_args.input.clone();
        let output = self.parsed_args.input.with_extension("olean");
        let mut descr = HashMap::new();
        descr.insert(String::from("compiled Lean bytecode and proofs"),output.clone());
        f_ok(HashResult {
            key: m.finish(),
            weak_toolchain_key: toolchain,
            compilation: Box::new(LeanCompilation
                                  { executable: self.executable.clone(),
                                    input: input,
                                    output: output,
                                    cwd: self.parsed_args.cwd,
                                    descrs: descr }) }) }

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

#[derive(Debug, PartialEq, Clone)]
pub struct LeanCompilation
{ executable: PathBuf,
  input: PathBuf,
  output: PathBuf,
  cwd: PathBuf,
  descrs: HashMap<String, PathBuf> }

impl Compilation for LeanCompilation
{
    fn generate_compile_commands(&self, _path_transformer: &mut dist::PathTransformer)
                                 -> Result<(CompileCommand,
                                            Option<dist::CompileCommand>,
                                            Cacheable)>
    { let cmd = (CompileCommand
          { executable: self.executable.clone(),
            arguments: vec![OsString::from("--make"),OsString::from(self.input.clone())],
            // executable: PathBuf::from("echo"),
            // arguments: vec![self.executable.clone().into_os_string(),OsString::from("--json"),OsString::from("--make"),OsString::from(self.input.clone())],
            env_vars: vec![],
            cwd: self.cwd.clone() },
                 None,Cacheable::Yes);
      Ok(cmd)
    }

    fn outputs<'a>(&'a self) -> Box<Iterator<Item=(&'a str, &'a Path)> + 'a>
    { Box::new( self.descrs.iter().map( |(k,v)| (&*k.as_str(),v.as_path()) ) ) }
}

#[derive(Clone, Debug)]
pub struct Lean {
    executable: PathBuf
}

impl Lean
{
    pub fn new (executable: PathBuf) -> Lean
    { Lean { executable: executable } }
}

impl<T> Compiler<T> for Lean
where T: CommandCreatorSync,
{
    /// Return the kind of compiler.
    fn kind(&self) -> CompilerKind { CompilerKind::Lean }
    /// Retrieve a packager
    #[cfg(feature = "dist-client")]
    fn get_toolchain_packager(&self) -> Box<pkg::ToolchainPackager> { Box::new(self.clone()) }
    /// Determine whether `arguments` are supported by this compiler.
    fn parse_arguments(&self,
                       arguments: &[OsString],
                       cwd: &Path) -> CompilerArguments<Box<CompilerHasher<T> + 'static>> {
        CompilerArguments::Ok(Box::new(
            LeanHasher::new(self.executable.clone(), arguments, cwd) )) }
    fn box_clone(&self) -> Box<Compiler<T>> {
        Box::new((*self).clone())
    }
}

#[cfg(feature = "dist-client")]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
impl pkg::ToolchainPackager for Lean {
    fn write_pkg(self: Box<Self>, f: fs::File) -> Result<()> {
        Ok(()) } }
