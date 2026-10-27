# Instructions for Coding Agents

## Required Windows preflight check

On Windows, at the beginning of the first task, before making changes or running
development, build, or test workflows, compare the current environment with the
last successful preflight check stored in `_dev/windows-preflight.json` relative
to the repository root. This is a local cache: do not commit it or include it in
the built package (`_dev` is already excluded from both).

- Always obtain the active R installation's `R.version.string` and `R.home()`
  from the R installation actually used by the current session or task.
  Do not infer them from another installed R executable.
- Obtain a system ID consisting of the computer name and the Windows version,
  including its build and update revision. For example, use `$env:COMPUTERNAME`
  and `DisplayVersion`, `CurrentBuildNumber`, and `UBR` from
  `HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion` in PowerShell.
- Read the cache and compare `rVersion`, `rHome`, and `systemId` with these
  current values. If all values match and `status` is `passed`, reuse the
  successful result and skip the full Rtools compatibility and compilation
  checks. Do not repeat those checks merely because a new task or session starts.
- Run the full check below only if the cache is missing, unreadable, incomplete,
  or not marked `passed`, or if the R version, R installation path, or system ID
  has changed. A changed R version always requires a new full check.
- If the current environment cannot be identified, stop and explain what could
  not be verified; do not reuse an unverifiable cached result.

For a required full check:

- Identify the R installation actually used by the current session or task.
  Obtain its full version from `R.version.string` and its installation path
  from `R.home()` using that same R installation. Do not infer the active
  version from another installed R executable.
- Determine the Rtools release required for that R version using the official
  Rtools compatibility guidance. Do not assume that the newest installed
  Rtools release is compatible.
- Verify that the matching Rtools installation is usable by that R session,
  including its build tools and C/C++ compilers. Use a toolchain diagnostic
  that verifies compilation with the active R installation; finding an
  installation directory alone is not sufficient.
- If matching Rtools is missing, incompatible, or unusable, stop the task and
  notify the user. The message must state the active R version and installation
  path, the expected Rtools release, and the reason the check failed. Ask the
  user to install or configure the matching Rtools and make it accessible to
  the active R installation before work resumes.
- If compatibility or usability cannot be established, stop and explain what
  could not be verified rather than proceeding with an unverified toolchain.

After the full check succeeds, create `_dev` if needed and write the JSON cache
with `status = "passed"`, `rVersion`, `rHome`, `systemId`, the verified
`rtoolsRelease`, `rtoolsHome`, and `checkedAt` (an ISO 8601 UTC timestamp).
Use the values actually verified, and replace the cache only after all required
checks succeed. On failure, invalidate any existing successful cache by setting
`status = "failed"`; never record an unsuccessful or unverified check as passed.
If saving the cache fails, report that limitation; the next task must perform
the full check again. If a later build reveals a toolchain failure, invalidate
the cache so that the next task checks the toolchain again.

## Task-specific guidance

Read and follow the applicable instruction files before starting work. If a task
spans multiple categories, apply all relevant files:

- **Bug fixing or refactoring:** Read
  [.github/agents/rpact_bugfix_refactor.agent.md](.github/agents/rpact_bugfix_refactor.agent.md).
- **General development:** Read
  [.github/agents/rpact_general.agent.md](.github/agents/rpact_general.agent.md).
- **Creating unit tests or improving test coverage:** Read
  [.github/agents/rpact_testcoverage.agent.md](.github/agents/rpact_testcoverage.agent.md)
  and follow the test location requirements below.
- **Tasks involving GitHub:** Read
  [.github/agents/rpact_github.agent.md](.github/agents/rpact_github.agent.md).

## Required error handling in R code

All errors raised explicitly by rpact code under `R/` must use the appropriate
error helper from [R/f_error_handling.R](R/f_error_handling.R). Direct calls to
`stop()` (including `base::stop()`, rethrowing caught conditions, and calls
hidden behind aliases or new wrappers) are prohibited. The only permitted
direct call is the existing `stop(condition)` in `.stopWithError()` in
`R/f_error_handling.R`; keep this as the single central error-signalling point.
Existing violations elsewhere are not examples to follow.

Before adding or changing an error path, read `R/f_error_handling.R` and inspect
comparable calls in the package, especially in `R/f_core_assertions.R` and
`R/class_design_optimal.R`. Reuse a suitable existing assertion when it already
performs the required validation and supplies the appropriate error metadata.
Otherwise, call the most specific helper below rather than calling
`.stopWithError()` or `.createCondition()` directly.

| Helper | Use for |
| --- | --- |
| `stopMissingArgument()` | A required argument is absent. |
| `stopArgumentUnknown()` | An unknown or unsupported argument name. |
| `stopIllegalArgument()` | An invalid argument type, value, or option not covered by a more specific helper. |
| `stopArgumentOutOfRange()` | An argument value outside its allowed bounds. |
| `stopArgumentLengthOutOfBounds()` | An argument with an invalid length. |
| `stopIndexOutOfBounds()` | An invalid index. |
| `stopConflictingArguments()` | Mutually incompatible arguments or parameterizations. |
| `stopIncompleteArguments()` | A partially supplied set of arguments requiring companion parameters. |
| `stopIllegalDataInput()` | Invalid input data or inconsistent dataset contents. |
| `stopRuntimeIssue()` | An internal, execution, or computational failure rather than invalid user input. |

### Supply structured diagnostic information

A message alone is insufficient for new or revised error calls. These helpers
feed the configurable condition factories (`rpact.error.factory.*`) and provide
consistent exception prefixes, codes, categories, and condition classes. Supply
the diagnostic information as **named helper arguments**, not just as text in
the message:

- Always specify `functionName`: the function or method whose validation or
  operation failed. In a reusable assertion, propagate the caller's function
  name when its interface supports this; otherwise use the assertion's name,
  following comparable existing calls.
- For argument or data errors, specify `parameter` (the relevant name or names)
  and `value` (the actual offending value). Preserve the value's R type rather
  than converting it to display text. Do not evaluate a missing argument just
  to populate `value`; omit it or use `NULL` when no value exists.
- Specify `constraint` to describe the violated requirement. For dependent or
  conflicting inputs, also supply `relatedParameter` and `relatedValue`.
- Use the helper's specific fields when applicable: `lowerBound` and
  `upperBound` for ranges or indices; `expectedLength` and `actualLength` for
  lengths; `index` for invalid indexing; `missingParameters` and
  `definedParameters` for incomplete argument sets. Describe open or closed
  bounds in `constraint` as well.
- Supply a stable, meaningful `diagnosticId` for the failure rule. Reuse an
  existing ID for the same rule; follow the existing dotted naming convention
  (for example, `validation.values_outside_interval`). Do not put runtime
  values, translated messages, or line numbers in an ID, or reuse an unrelated
  ID merely because the helper is the same.
- Add `context = list(...)` with named entries for useful additional facts not
  covered by the dedicated fields, such as the affected stage, object position,
  allowed choices, or computational operation. For runtime failures without an
  offending argument, supply the relevant operational context. Include only
  information available at the failure site; do not invent values or fill
  irrelevant fields with placeholders.
- Use only formal metadata arguments supported by the chosen helper. Its `...`
  arguments are concatenated into the message by `.getErrorMessage()`; arbitrary
  named entries in `...` do **not** become structured metadata. Put additional
  diagnostic fields in the named `context` list.

Keep the message clear and actionable, using the existing English message
style. The helpers add the exception prefix themselves. When replacing an
existing error, preserve its substantive explanation and validation behavior
unless the task requires a change. When handling a caught error, include its
useful explanation and context in the appropriate helper call rather than
rethrowing it with `stop(e)`.

For example, a length check following the pattern in
`R/class_design_optimal.R` should include both the actual inputs and their
lengths (the call belongs inside the failing validation branch):

```r
stopArgumentLengthOutOfBounds(
    "Must provide exactly one weight in weightsLR per entry of thetaLR.",
    functionName = "getDesignOptimalConditionalError",
    parameter = "weightsLR",
    value = weightsLR,
    constraint = "one weight per thetaLR entry",
    relatedParameter = "thetaLR",
    relatedValue = thetaLR,
    expectedLength = length(thetaLR),
    actualLength = length(weightsLR),
    diagnosticId = "validation.weights_length_matches_effects"
)
```

The example ID illustrates the naming convention; check existing IDs before
assigning one. A choice validation can additionally use
`context = list(allowedValues = allowedValues)`, with the actual choices from
the validation branch, rather than embedding those choices only in the message.

### Review before completing R code changes

- Inspect the diff for every new or modified error path: verify the helper's
  semantics, message, and applicable structured fields.
- Search for direct calls, for example with
  `rg -n '\b(base::)?stop\s*\(' R -g '*.R'`, and review matches as code rather
  than counting documentation or comments. Confirm that the changes introduce
  no direct `stop()` calls and that any such calls in the error paths being
  changed are replaced with the appropriate enriched helpers. The target for
  the package is exactly one direct call, in `.stopWithError()`.
- For behavior changes, run relevant tests and verify the error category and
  metadata where applicable, following the test location rules below. Do not
  assume metadata will be attached by the default `simpleError` fallback;
  inspect metadata through an appropriate configured condition factory when
  testing it.

## Braces in R conditionals

Always use curly braces for every `if`, `else if`, and `else` branch in rpact
R code, even when a branch contains only one statement or R would allow the
braces to be omitted. This also applies to early returns and conditional
expressions used in assignments. Put the branch body on separate lines and
indent it with four spaces; write `} else {` and `} else if (...) {` on the
same line.

For example, do not write:

```r
if (!is.call(call)) return(FALSE)
```

Write:

```r
if (!is.call(call)) {
    return(FALSE)
}
```

Apply the same rule to both branches of an `if (...) { ... } else { ... }`
statement. When adding or modifying conditional logic, ensure every branch
uses braces; existing unbraced conditionals are not examples to follow.

## Roxygen documentation formatting

When creating or updating Roxygen documentation, use the following source-code
format consistently, even when Roxygen could infer sections from their order:

- Begin and end every documentation block with an empty Roxygen comment line
  (`#'`). Keep these separator lines free of trailing whitespace.
- Separate logical sections with an empty Roxygen comment line, including
  before `@return`, `@details`, `@examples`, and `@export`. Keep consecutive
  `@param` entries together as one parameter section.
- Always write an explicit `@title` tag on its own line, followed by the title
  on the next Roxygen line. Do not use an untagged first line as an implicit
  title.
- Always introduce a description with an explicit `@description` tag on its
  own line, followed by the description text on subsequent Roxygen lines.
  Do not rely on an untagged paragraph being interpreted as the description.
- Likewise, put section tags such as `@details` and `@examples` on their own
  lines above their content. Keep named tags and short entries such as
  `@param x ...`, `@rdname ...`, and `@return ...` inline as shown below;
  `@export` occupies its own line.
- Use actual Roxygen syntax in R source: do not add Markdown bold markers to
  tags or literal HTML entities such as `&#x20;` to represent blank lines.

For example:

```r
#'
#' @title
#' Access Objects in a Trial Comparison
#'
#' @rdname extract.TrialComparison
#'
#' @description
#' Extracts a contained object by name or position.
#'
#' @param x A `TrialComparison`.
#' @param i A single object name or a positive integer position.
#' @param name An object name, supplied literally after `$`.
#' @param ... Further indices are not supported.
#' @param exact Whether character indices must match exactly, as for list `[[`.
#'   `$` always matches exactly.
#'
#' @return The contained object by reference, or NULL for an unknown name.
#'
#' @details
#' Duplicate names select the first occurrence. Numeric positions refer to the
#' stored object order. R6 members take precedence for `$`: `x$add` is the method
#' even when an object is named "add"; use `x[["add"]]` to select that object.
#' `[[` selects contained objects first and falls back to existing R6 members for
#' other character names. This also preserves existing internal field access.
#' These methods provide extraction; replacing collection entries is not supported.
#' Use [getComparison()] or `$add()` to build or extend the collection.
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2))
#' identical(comparison$fixed, comparison[["fixed"]])
#' identical(comparison[[1]], comparison$fixed)
#'
#' @export
#'
```

## Unit tests and test templates

The rpact-specific rules in this section, including the optional
`rpact.validator` template workflow, apply only to direct development of the
`rpact` package and its tests. When working on another package, such as
`rpact.code.api` or `rpact.cloud`, create and maintain that package's tests in
its own `tests/testthat` directory, following its applicable instructions and
existing test conventions. Do not create or modify tests for another package
in `rpact.tests` or apply the rpact-specific template rules to those tests.
Using `rpact` as a dependency does not make a package's tests rpact tests.
For tasks spanning multiple packages, apply these rules only to tests of
`rpact`; keep tests of each other package in that package.

All rpact unit tests are hosted and developed in the separate `rpact.tests`
package, except for the small example test `test-pkgname.R` in the `rpact`
package.

- Create all new rpact unit tests in `rpact.tests/tests/testthat`. Do not create
  new unit tests in the `rpact` repository.
- When improving existing tests, check whether they have a corresponding
  template in `rpact.tests/tests-raw/testthat`. Update that template as needed
  to keep the tests and their source templates consistent.
- Locate the `rpact.tests` package using the coding agent's configured projects
  and workspace folders. It may also be available as a sibling directory of
  `rpact`; do not assume a machine-specific absolute path.
- If `rpact.tests` cannot be located or accessed, ask the user to provide its
  local path and make it accessible to the coding agent, for example by adding
  it to the agent's workspace. Do not create tests in the `rpact` repository as
  a fallback.

### Optional template-based regression tests with rpact.validator

When creating general regression tests that freeze the current state of rpact
result objects, consider the template-based workflow provided by
`rpact.validator`. Prefer this option for capturing object parameters and
calculated results without manually writing repetitive assertions. Handwritten
tests remain an option for targeted behavior, edge cases, or checks that are
better expressed explicitly.

For template-based tests, create or update the source template in
`rpact.tests/tests-raw/testthat`. Declare the test title with `@test_that`, create
the result object, and pass it to `rpact.validator::getUnitTestObject()` together
with its variable name as a string. For example:

```r
#' @test_that 'getDesignInverseNormal' with default parameters: parameters and results are as expected

design1 <- getDesignInverseNormal()
rpact.validator::getUnitTestObject(design1, "design1")
```

Rendering the templates generates executable `test_that()` blocks with
`expect_equal()` assertions for the captured object fields, including expected
values, tolerances, and `getTestLabel()` labels. For this example, these include
checks of `design1$alphaSpent`, `design1$criticalValues`, and
`design1$stageLevels`. The generated test files belong in
`rpact.tests/tests/testthat`, as required above.

Before rendering, locate and verify both package directories and ensure that
`rpact.validator` is available. Set `rpactProjectPath` and
`rpactTestsProjectPath` to the resolved local paths of `rpact` and `rpact.tests`,
respectively; do not hard-code machine-specific paths in shared instructions.

Regeneration must be controlled through the following exact template comment
(preserve its spelling, capitalization, and spacing):

```r
#' @exit Do not create the unit tests again
```

When `enforceCreationOfAllUnitTests = FALSE`, a template containing this comment
is skipped and its unit test file is not regenerated. Always keep
`enforceCreationOfAllUnitTests = FALSE`; never set it to `TRUE`.

Before every call to `rpact.validator::createRpactUnitTests()`:

- Identify the specific template whose unit test file is to be generated or
  regenerated.
- Check all templates in `rpact.tests/tests-raw/testthat`. Every template except
  the selected template must contain the exact `@exit` comment above. If it is
  missing, insert it at the beginning of that template before proceeding.
- Ensure the selected template does not contain the `@exit` comment; remove it
  from that template if necessary to enable generation.
- Run the generator only after these checks and edits are complete, keeping
  `enforceCreationOfAllUnitTests = FALSE`.

Render using:

```r
rpact.validator::createRpactUnitTests(
    packageProjectBaseDirectory = rpactProjectPath,
    qualityAssuranceBaseDirectory = rpactTestsProjectPath,
    testFilesProjectBaseDirectory = rpactTestsProjectPath,
    copyUnitTestFilesToTestthatDir = TRUE,
    enforceCreationOfAllUnitTests = FALSE,
    ignoreFilesRegex = NA_character_,
    qaSubDirs = list(
        templates = file.path("tests-raw", "testthat"),
        output = "tests-raw"
    )
)
```

Treat the templates as the source for generated tests: make changes in the
corresponding template and regenerate the test files rather than maintaining
changes only in generated output. Review the generated assertions and run the
relevant tests. Capturing the current state establishes a regression baseline;
it does not independently establish statistical correctness. Do not regenerate
expected values merely to make an unexpected regression pass.

## New vignettes

- Create new rpact vignettes as Quarto (`.qmd`) files in the separate website
  project `rpact-org_website`. Do not create new `.Rmd` vignettes in the
  `rpact` package's `vignettes` directory.
- Before creating a vignette, locate `rpact-org_website` using the coding
  agent's configured projects and workspace folders, and verify that its
  `vignettes` directory exists and is accessible. Do not assume a
  machine-specific absolute path.
- If the website project or its `vignettes` directory cannot be located or
  accessed, ask the user to provide the correct path and make it accessible to
  the coding agent. Do not create the vignette in the `rpact` repository as a
  fallback.
- Choose the appropriate topic directory under `rpact-org_website/vignettes`:
  `analysis`, `getting-started`, `planning`, or `utilities`.
- Always create a new directory named after the vignette in `snake_case`
  within the selected topic directory, and save the vignette as `index.qmd`
  inside it. Use this structure:
  `rpact-org_website/vignettes/<topic>/<vignette_name>/index.qmd`.
- Before drafting, read the applicable instructions in the website project
  and inspect existing vignettes, especially those in the selected topic
  directory. Follow their layout, overall structure, and writing style,
  including established conventions for metadata, headings, and code examples.
