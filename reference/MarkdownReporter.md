# Markdown Reporter for Test Results

This class defines a Markdown reporter for test results, inheriting from
the `R6::Reporter` class. It logs test results in Markdown format and
saves them to a file named `test_results.md`.

## Fields

- `startTime`:

  The start time of the test run.

- `output`:

  A character vector to store the log output.

- `failures`:

  The number of test failures.

- `fileName`:

  The name of the current test file being processed.

## Methods

- `initialize(...)`:

  Initializes the reporter, setting up the output and failures fields.

- `log(...)`:

  Logs messages to the output field.

- `start_reporter()`:

  Starts the reporter, logging the introduction and test results header.

- `start_file(file)`:

  Sets the current file name being processed.

- `getContext()`:

  Gets the context from the current file name.

- `add_result(context, test, result)`:

  Adds a test result to the log, marking it as passed or failed.

- `end_reporter()`:

  Ends the reporter, logging the summary and saving the output to a
  file.

- `finalize()`:

  Finalizes the reporter, displaying a message that the test results
  were saved.
