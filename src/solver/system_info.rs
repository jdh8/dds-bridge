//! DDS library version, build, and threading metadata

use dds_bridge_sys as sys;
use semver::Version;

use core::ffi::{CStr, c_char};
use core::fmt;

/// OS platform reported by the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    /// Microsoft Windows
    Windows,
    /// Cygwin (Windows POSIX layer)
    Cygwin,
    /// Linux
    Linux,
    /// Apple (macOS / iOS)
    Apple,
    /// Unknown or unrecognized platform
    Unknown(i32),
}

/// C++ compiler used to build the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Compiler {
    /// Microsoft Visual C++
    MSVC,
    /// MinGW
    MinGW,
    /// GNU g++
    GCC,
    /// Clang
    Clang,
    /// Unknown or unrecognized compiler
    Unknown(i32),
}

/// Threading model used by the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Threading {
    /// No threading
    None,
    /// Windows native threads
    Windows,
    /// OpenMP
    OpenMP,
    /// Grand Central Dispatch (Apple)
    GCD,
    /// Boost.Thread
    Boost,
    /// C++ standard library threads (`std::thread`)
    STL,
    /// Intel Threading Building Blocks
    TBB,
    /// Unknown or experimental threading model
    Unknown(i32),
}

/// Information about the DDS library and how it was built
///
/// Returned by [`Solver::system_info`](super::Solver::system_info).  Exposes
/// the version, hardware configuration (cores, threads, pointer width), and
/// compile-time choices (OS, compiler, threading model) that DDS was built
/// with.
#[derive(Debug, Clone, Copy)]
pub struct SystemInfo(pub(super) sys::DDSInfo);

impl SystemInfo {
    /// DDS version
    #[must_use]
    pub const fn version(&self) -> Version {
        #[allow(clippy::cast_sign_loss)]
        Version::new(
            self.0.major as u64,
            self.0.minor as u64,
            self.0.patch as u64,
        )
    }

    /// OS platform DDS was built for
    #[must_use]
    pub const fn platform(&self) -> Platform {
        match self.0.system {
            1 => Platform::Windows,
            2 => Platform::Cygwin,
            3 => Platform::Linux,
            4 => Platform::Apple,
            n => Platform::Unknown(n),
        }
    }

    /// Pointer size in bits (32 or 64)
    #[must_use]
    pub const fn num_bits(&self) -> u32 {
        #[allow(clippy::cast_sign_loss)]
        return self.0.numBits as u32;
    }

    /// C++ compiler DDS was built with
    #[must_use]
    pub const fn compiler(&self) -> Compiler {
        match self.0.compiler {
            1 => Compiler::MSVC,
            2 => Compiler::MinGW,
            3 => Compiler::GCC,
            4 => Compiler::Clang,
            n => Compiler::Unknown(n),
        }
    }

    /// Threading model DDS was built with
    ///
    /// Currently, [`dds_bridge_sys`] only supports [`Threading::STL`] for
    /// maximum compatibility, minimum code size, and competitive performance.
    /// Other variants may be activated in the future for specialized use cases
    /// or platforms.
    #[must_use]
    pub const fn threading(&self) -> Threading {
        match self.0.threading {
            0 => Threading::None,
            1 => Threading::Windows,
            2 => Threading::OpenMP,
            3 => Threading::GCD,
            4 => Threading::Boost,
            5 => Threading::STL,
            6 => Threading::TBB,
            n => Threading::Unknown(n),
        }
    }

    /// Number of CPU cores detected by DDS
    #[must_use]
    pub const fn num_cores(&self) -> usize {
        #[allow(clippy::cast_sign_loss)]
        return self.0.numCores as usize;
    }

    /// Number of threads configured in the DDS thread pool
    #[must_use]
    pub const fn num_threads(&self) -> usize {
        #[allow(clippy::cast_sign_loss)]
        return self.0.noOfThreads as usize;
    }

    /// Memory-size description for each thread slot
    ///
    /// A string such as `"0 S, 16 L"` where `L` denotes a large transposition
    /// table and `S` a small one.
    #[must_use]
    pub const fn thread_sizes(&self) -> &str {
        // SAFETY: [DDS fills `threadSizes` with a null-terminated ASCII string.](https://github.com/dds-bridge/dds/blob/d2bc4c2c703941664fc1d73e69caa5233cdeac18/src/System.cpp#L756)
        unsafe { c_chars_to_str(&self.0.threadSizes) }
    }

    /// Human-readable summary of the full DDS system configuration
    #[must_use]
    pub const fn system_string(&self) -> &str {
        // SAFETY: [DDS fills `systemString` with a null-terminated ASCII string.](https://github.com/dds-bridge/dds/blob/d2bc4c2c703941664fc1d73e69caa5233cdeac18/src/System.cpp#L773)
        unsafe { c_chars_to_str(&self.0.systemString) }
    }
}

const unsafe fn c_chars_to_str(bytes: &[c_char]) -> &str {
    unsafe { core::str::from_utf8_unchecked(CStr::from_ptr(bytes.as_ptr()).to_bytes()) }
}

impl fmt::Display for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.system_string())
    }
}
