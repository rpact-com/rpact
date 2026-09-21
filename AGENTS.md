# Instructions for Coding Agents

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

## Unit tests and test templates

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
