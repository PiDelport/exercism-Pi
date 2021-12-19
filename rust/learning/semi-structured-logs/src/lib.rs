//! <https://exercism.org/tracks/rust/exercises/semi-structured-logs>

/// Various log levels.
///
/// The `add-a-variant` feature flag enables the `Debug` level.
#[derive(Clone, PartialEq, Debug)]
pub enum LogLevel {
    #[cfg(feature = "add-a-variant")]
    Debug,
    Info,
    Warning,
    Error,
}

/// Return a log message formatted as: `"[<LEVEL>]: <MESSAGE>"`
pub fn log(level: LogLevel, message: &str) -> String {
    let label = format!("{:?}", level).to_uppercase();
    format!("[{}]: {}", label, message)
}

pub fn info(message: &str) -> String {
    log(LogLevel::Info, message)
}

pub fn warn(message: &str) -> String {
    log(LogLevel::Warning, message)
}

pub fn error(message: &str) -> String {
    log(LogLevel::Error, message)
}
