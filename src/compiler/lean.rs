
use compiler::{Cacheable, ColorMode, Compiler, CompilerArguments, CompileCommand, CompilerHasher, CompilerKind,
               Compilation, HashResult};
use std::path::{PathBuf,Path};
use std::ffi::{OsStr, OsString};
// use compiler::{NoopOutputsRewriter, OutputsRewriter};
// use dist;
use mock_command::CommandCreatorSync;

use errors::*;

pub struct Lean {
    executable: PathBuf
}

impl Lean {
    pub fn new (executable: PathBuf) -> SFuture<Lean>
    { return Box::new( Lean { executable: executable } ) }
}

impl<T> Compiler<T> for Lean
    where T: CommandCreatorSync,
{
    /// Return the kind of compiler.
    fn kind(&self) -> CompilerKind { }
    /// Retrieve a packager
    #[cfg(feature = "dist-client")]
    fn get_toolchain_packager(&self) -> Box<pkg::ToolchainPackager> { }
    /// Determine whether `arguments` are supported by this compiler.
    fn parse_arguments(&self,
                       arguments: &[OsString],
                       cwd: &Path) -> CompilerArguments<Box<CompilerHasher<T> + 'static>> { }
    fn box_clone(&self) -> Box<Compiler<T>> { }
}
