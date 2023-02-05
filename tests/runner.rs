use std::{
    collections::HashMap,
    io::Write,
    panic::UnwindSafe,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Arc,
    time::Instant,
};

use clap::Parser;
use mica::{Engine, Error, Value};
use owo_colors::OwoColorize;
use rayon::prelude::*;

/// The specification for a test.
#[derive(Default)]
struct TestSpec {
    /// The name of the file that contains this test.
    file_name: String,

    /// The expected outcome of this test.
    ///
    /// This can be set to `Outcome::Failure` by using `@error` annotations, like so:
    /// ```
    /// # @error error message goes here
    /// # @error another error line
    /// ```
    ///
    /// Individual lines are going to be joined into one string, separated by `'\n'`.
    /// See [`Outcome::Failure`] for docs on how error patterns work.
    expected_outcome: Outcome,

    /// Whether the test is skipped.
    ///
    /// This can be set to `true` by using a `@skip` annotation, like so:
    /// ```
    /// # @skip
    /// ```
    ///
    /// Skipped tests do not run by default. This can be overridden by using the `--ignore-skipped`
    /// CLI flag. This is useful for omitting tests that are slow to run. Note that tests are
    /// compiled regardless of whether they're skipped.
    ignore: bool,

    /// Named markers for line numbers.
    ///
    /// Markers can be set using the `@line` annotation, like so:
    /// ```
    /// a = 1  # @line NAME
    /// ```
    /// and can be referred to using `{:NAME}` in error patterns.
    line_markers: HashMap<String, usize>,
}

impl TestSpec {
    fn parse(file_name: &str, code: &str) -> Self {
        let mut parser = SpecParser {
            spec: TestSpec { file_name: file_name.to_string(), ..Default::default() },
            error_lines: vec![],
            current_line: 1,
        };

        for (line_number, line) in code.lines().enumerate() {
            parser.current_line = line_number + 1;

            if let Some((_, comment)) = line.split_once('#') {
                let comment = comment.trim_start();
                if !comment.starts_with('@') {
                    continue;
                }
                let annotation = &comment[1..];
                if let Some((directive, argument)) = annotation.split_once(' ') {
                    parser.parse_directive_with_argument(directive, argument);
                } else {
                    parser.parse_directive(annotation);
                }
            }
        }

        let mut spec = parser.spec;
        if !parser.error_lines.is_empty() {
            let joined_lines = parser.error_lines.join("\n");
            spec.expected_outcome = Outcome::Failure(joined_lines);
        }

        spec
    }
}

struct SpecParser {
    spec: TestSpec,
    error_lines: Vec<String>,

    current_line: usize,
}

impl SpecParser {
    /// Parses the provided directive. Returns `true` if it's a valid directive, or `false` if it's
    /// not.
    fn parse_directive(&mut self, directive: &str) -> bool {
        match directive {
            "ignore" => self.spec.ignore = true,
            _ => return false,
        }
        true
    }

    /// Parses the provided directive with an argument. Returns `true` if it's a valid directive, or
    /// `false` if it's not.
    fn parse_directive_with_argument(&mut self, directive: &str, argument: &str) -> bool {
        match directive {
            "error" => self.error_lines.push(argument.to_string()),
            "line" => {
                self.spec.line_markers.insert(argument.to_string(), self.current_line);
            }
            _ => return false,
        }
        true
    }
}

/// The outcome of a test.
#[derive(PartialEq, Eq, Default)]
enum Outcome {
    /// Successful outcome.
    #[default]
    Success,

    /// Failing outcome. The field specifies the expected error message.
    ///
    /// When comparing two outcomes together, one of them is used as a pattern to match. This
    /// pattern can contain the following matchers:
    ///
    /// - `{file}` - matches the filename.
    /// - `{:NAME}` - matches the line number saved with a `@line NAME` annotation.
    Failure(String),

    /// The test was ignored.
    Ignored,
}

struct ErrorMatcher<'s> {
    spec: &'s TestSpec,
    error: &'s [u8],
    pattern: &'s [u8],
    position_in_error: usize,
    position_in_pattern: usize,
}

impl<'s> ErrorMatcher<'s> {
    fn error_char(&self) -> u8 {
        self.error.get(self.position_in_error).copied().unwrap_or(b'\0')
    }

    fn pattern_char(&self) -> u8 {
        self.pattern.get(self.position_in_pattern).copied().unwrap_or(b'\0')
    }

    fn match_string(&mut self, s: &str) -> bool {
        if !self.error[self.position_in_error..].starts_with(s.as_bytes()) {
            return false;
        }
        self.position_in_error += s.len();
        true
    }

    fn check_matcher(&mut self, instruction: &[u8]) -> bool {
        if instruction == b"file" && !self.match_string(&self.spec.file_name) {
            return false;
        }

        if let Some(line_marker) = instruction.strip_prefix(b":") {
            let line_marker = std::str::from_utf8(line_marker).unwrap();
            if let Some(&line_number) = self.spec.line_markers.get(line_marker) {
                let line_number = line_number.to_string();
                if !self.match_string(&line_number) {
                    return false;
                }
            }
        }

        true
    }

    fn check(&mut self) -> bool {
        while self.position_in_error < self.error.len()
            && self.position_in_pattern < self.pattern.len()
        {
            match self.pattern_char() {
                b'{' => {
                    self.position_in_pattern += 1;

                    // Support escaping literal '{'s by doubling them.
                    if self.pattern_char() == b'{' {
                        if self.error_char() != self.pattern_char() {
                            return false;
                        }
                        self.position_in_error += 1;
                        self.position_in_pattern += 1;
                        continue;
                    }

                    let start = self.position_in_pattern;
                    while !matches!(self.pattern_char(), b'}' | b'\0') {
                        self.position_in_pattern += 1;
                    }
                    let end = self.position_in_pattern;
                    self.position_in_pattern += 1; // skip over '}'

                    let inside = &self.pattern[start..end];
                    if !self.check_matcher(inside) {
                        return false;
                    }
                }
                b'}' => {
                    self.position_in_pattern += 1;
                    if self.pattern_char() == b'}' {
                        if self.error_char() != self.pattern_char() {
                            return false;
                        }
                        self.position_in_error += 1;
                        self.position_in_pattern += 1;
                    } else {
                        // This could really use a better error message...
                        // If you use only a single }, the match will always fail.
                        return false;
                    }
                }
                pattern_char => {
                    if self.error_char() != pattern_char {
                        return false;
                    }
                    self.position_in_error += 1;
                    self.position_in_pattern += 1;
                }
            }
        }

        self.position_in_error >= self.error.len()
    }
}

impl Outcome {
    /// Returns `true` if the outcome is [`Success`].
    ///
    /// [`Success`]: Outcome::Success
    #[must_use]
    fn is_success(&self) -> bool {
        matches!(self, Self::Success)
    }

    /// Returns `true` if the outcome is [`Failure`].
    ///
    /// [`Failure`]: Outcome::Failure
    #[must_use]
    fn is_failure(&self) -> bool {
        matches!(self, Self::Failure(..))
    }

    /// Returns `true` if the outcome is [`Ignored`].
    ///
    /// [`Ignored`]: Outcome::Ignored
    #[must_use]
    fn is_ignored(&self) -> bool {
        matches!(self, Self::Ignored)
    }

    fn match_error_against_pattern(error: &str, pattern: &str, spec: &TestSpec) -> bool {
        let (error, pattern) = (error.as_bytes(), pattern.as_bytes());
        let mut matcher =
            ErrorMatcher { spec, error, pattern, position_in_error: 0, position_in_pattern: 0 };
        matcher.check()
    }

    /// Returns whether this outcome matches the provided pattern outcome.
    fn matches(&self, pattern: &Outcome, spec: &TestSpec) -> bool {
        if let (Self::Failure(error), Self::Failure(pattern)) = (self, pattern) {
            Self::match_error_against_pattern(error, pattern, spec)
        } else {
            self.eq(pattern)
        }
    }

    /// Attempts to run `f`, returning a [`Failure`][Outcome::Failure] outcome if it panics.
    fn wrap_panic<F, R>(f: F) -> Self
    where
        F: FnOnce() -> R + UnwindSafe,
        Self: From<R>,
    {
        match std::panic::catch_unwind(f) {
            Ok(result) => Self::from(result),
            Err(payload) => {
                let message = if let Some(s) = payload.downcast_ref::<&str>() {
                    s
                } else if let Some(s) = payload.downcast_ref::<String>() {
                    s
                } else {
                    "<panic payload of unknown type>"
                };
                Self::Failure(format!("(!) test panicked: {message}"))
            }
        }
    }
}

impl From<Result<(), mica::Error>> for Outcome {
    fn from(result: Result<(), mica::Error>) -> Self {
        match result {
            Ok(_) => Self::Success,
            Err(error) => Self::Failure(error.to_string()),
        }
    }
}

/// Starts a new fiber and returns an iterator over its yielded values.
fn interpret<'e>(
    engine: &'e mut Engine,
    filename: &str,
    input: &str,
) -> Result<impl Iterator<Item = Result<Value, Error>> + 'e, Error> {
    let mut fiber = engine.start(filename, input)?;

    Ok(Some(std::iter::from_fn(move || match fiber.resume() {
        Ok(Some(value)) => Some(Ok(value)),
        Ok(None) => None,
        Err(error) => Some(Err(error)),
    }))
    .into_iter()
    .flatten())
}

/// Evaluates a script, discarding its result and reporting any errors along the way.
fn evaluate(test_name: &str, code: &str) -> Result<(), mica::Error> {
    let mut engine = Engine::new();
    for result in interpret(&mut engine, test_name, code)? {
        result?;
    }
    Ok(())
}

#[derive(Debug)]
struct Test {
    suite: Arc<str>,
    name: String,
    path: PathBuf,
}

impl Test {
    /// Runs the test, printing the outcome into stdout.
    fn run(&self, include_ignored: bool) -> Outcome {
        let test_name = self
            .path
            .file_name()
            .unwrap()
            .to_str()
            .expect("test filename has non-UTF-8 characters");
        let code = std::fs::read_to_string(&self.path).unwrap();

        let spec = TestSpec::parse(test_name, &code);
        let mut outcome = Outcome::Success;

        if !spec.ignore || include_ignored {
            let got = Outcome::wrap_panic(|| evaluate(test_name, &code));
            if !got.matches(&spec.expected_outcome, &spec) {
                if got.is_success() {
                    if let Outcome::Failure(error) = &spec.expected_outcome {
                        outcome = Outcome::Failure(format!(
                            "the test should have failed with a matching error:\n{error}"
                        ));
                    }
                }
                if spec.expected_outcome.is_success() {
                    if let Outcome::Failure(error) = &got {
                        outcome = Outcome::Failure(format!(
                            "the test should have succeeded but failed with error:\n{error}"
                        ));
                    }
                }
                if let Outcome::Failure(got_error) = &got {
                    if let Outcome::Failure(expected_error) = &spec.expected_outcome {
                        outcome = Outcome::Failure(format!("the test failed as expected, but with the wrong error:\n{got_error}\n\nthe above error does not match this pattern:\n{expected_error}"));
                    }
                }
            }
        } else {
            outcome = Outcome::Ignored;
        }

        let mut stdout = std::io::stdout().lock();
        write!(
            stdout,
            "{} {}{}{} ",
            "test".cyan(),
            self.suite.bright_black(),
            ".".bright_black(),
            self.name
        )
        .unwrap();
        match outcome {
            Outcome::Success => write!(stdout, "{}", "OK".green()).unwrap(),
            Outcome::Failure(_) => write!(stdout, "{}", "FAIL".white().on_red()).unwrap(),
            Outcome::Ignored => write!(stdout, "{}", "IGNORED".yellow()).unwrap(),
        }
        writeln!(stdout).unwrap();

        outcome
    }
}

/// Collects all tests into a set of suites.
fn collect_all_tests(base_path: impl AsRef<Path>) -> Vec<Test> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(base_path);
    assert!(path.is_dir());

    fn visit(tests: &mut Vec<Test>, suite: Arc<str>, dir: &Path) {
        for entry in std::fs::read_dir(dir).expect("cannot read tests from directory") {
            let entry = entry.expect("cannot read directory entry");
            let file_type = entry.file_type().expect("cannot stat directory entry");
            let path = entry.path();
            let file_name =
                path.file_name().unwrap().to_str().expect("invalid UTF-8 found in path");

            if file_type.is_dir() {
                let suite = if suite.is_empty() {
                    file_name.to_owned()
                } else {
                    format!("{suite}.{file_name}")
                };
                let suite = Arc::from(suite);
                visit(tests, suite, &entry.path());
            } else if let Some(test_name) = file_name.strip_suffix(".test.mi") {
                tests.push(Test { suite: Arc::clone(&suite), name: test_name.to_owned(), path });
            }
        }
    }

    let mut tests = Vec::new();
    visit(&mut tests, Arc::from(""), &path);
    tests
}

#[derive(Parser)]
struct Arguments {
    /// Include tests marked with `@ignore`.
    #[clap(long)]
    include_ignored: bool,
}

fn main() -> ExitCode {
    let args = Arguments::parse();

    println!();

    let tests = collect_all_tests("tests");
    let start_time = Instant::now();
    let outcomes: Vec<_> =
        tests.par_iter().map(|test| (test.run(args.include_ignored), test)).collect();
    let end_time = Instant::now();

    let total = outcomes.len();
    let successful = outcomes.iter().filter(|(outcome, _)| outcome.is_success()).count();
    let failures: Vec<_> = outcomes.iter().filter(|(outcome, _)| outcome.is_failure()).collect();
    let ignored = outcomes.iter().filter(|(outcome, _)| outcome.is_ignored()).count();

    if !failures.is_empty() {
        for (outcome, test) in &failures {
            println!();
            println!(
                "{}{}{} {}",
                test.suite.bright_black(),
                ".".bright_black(),
                test.name,
                "did not pass!".bright_red()
            );
            if let Outcome::Failure(error) = outcome {
                println!("{error}");
            }
        }
    }

    println!();
    println!("{}", "summary:".cyan().bold());

    println!("    {} tests in total", total.cyan());
    println!(
        "    {} passed ({:.1}%)",
        successful.bright_green(),
        successful as f32 / total as f32 * 100.0
    );
    if !failures.is_empty() {
        println!(
            "    {} failed ({:.1}%)",
            failures.len().bright_red(),
            failures.len() as f32 / total as f32 * 100.0
        );
    } else {
        println!("    no tests failed! {}", "✓".bright_green());
    }
    if ignored > 0 {
        println!("    {} tests were ignored", ignored.yellow());
    }
    println!("    took {:.2?}", end_time - start_time);

    println!();

    if !failures.is_empty() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
