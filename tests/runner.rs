use std::path::PathBuf;

use libtest_mimic::{Arguments, Outcome, Test};
use mica::{Engine, Error, Value};

fn engine() -> Result<Engine, Error> {
   let mut engine = Engine::new(mica_std::lib());
   mica_std::load(&mut engine)?;
   Ok(engine)
}

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

/// Returns None if the test should succeed
///
/// Returns Some(Assertion) if the test should fail.
/// The assertion here is what the error message should contain.
/// (This is inspired by SerenityOS/jakt tests.)
fn test_should_fail(code: &str) -> Option<String> {
   for line in code.lines() {
      if !line.starts_with('#') {
         return None;
      }
      if line.contains("# Exception: ") {
         return Some(line.replace("# Exception: ", ""));
      }
   }
   None
}

fn test_mica_file(test: &Test<PathBuf>) -> Outcome {
   let code = std::fs::read_to_string(&test.data).unwrap();
   let outcome = test_mica_file_impl(&test.name, &code);

   let should_fail = test_should_fail(&code);
   if let Some(assertion) = should_fail {
      if outcome.is_ok() {
         return Outcome::Failed {
            msg: Some("Should have failed, but did not!".to_string()),
         };
      }

      let error = outcome.unwrap_err();
      let message = error.to_string();
      if !message.contains(&assertion) {
         Outcome::Failed {
            msg: Some(format!(
               "Should have failed with '{}', but failed with '{}'",
               assertion, message
            )),
         }
      } else {
         Outcome::Passed
      }
   } else if let Err(err) = outcome {
      Outcome::Failed {
         msg: Some(format!("{}", err)),
      }
   } else {
      Outcome::Passed
   }
}

fn test_mica_file_impl(testname: &str, code: &str) -> Result<(), Error> {
   let mut engine = engine()?;

   let res = interpret(&mut engine, testname, code)?;

   for result in res {
      result?;
   }

   Ok(())
}

/// Collects all files ending with `.mi` in a directory.
fn collect_mi_files(base_path: &str) -> Vec<String> {
   let mut tests = Vec::new();

   let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
   path.push(base_path);

   assert!(path.is_dir());

   for entry in std::fs::read_dir(path).unwrap() {
      let entry = entry.unwrap();
      let path = entry.path();
      if path.is_dir() {
         tests.extend(collect_mi_files(path.to_str().unwrap()));
      } else if path.extension() == Some(std::ffi::OsStr::new("mi")) {
         tests.push(path.to_str().unwrap().to_string());
      }
   }

   tests
}

fn generate_tests(mi_files: &[String]) -> Vec<Test<PathBuf>> {
   mi_files
      .iter()
      .map(|file| {
         let path = PathBuf::from(file);
         Test {
            name: path.file_stem().unwrap().to_str().unwrap().to_string(),
            kind: "Sample Test".to_string(),
            is_bench: false,
            data: path,
            is_ignored: file.ends_with(".skip.mi"),
         }
      })
      .collect()
}

fn main() {
   let args = Arguments::from_args();

   let files = collect_mi_files("tests");
   let tests = generate_tests(&files);

   libtest_mimic::run_tests(&args, tests, test_mica_file).exit();
}
