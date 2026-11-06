# Copilot instructions for rpact

- `rpact` is a scientific R package for confirmatory adaptive clinical
  trial design, simulation, and analysis.
- Prioritize statistical correctness, numerical robustness,
  reproducibility, backward compatibility, and clear user-facing
  documentation.
- Treat all changes to statistical methodology as high risk.
- Do not change formulas, boundary logic, stage-wise calculations,
  p-value handling, confidence interval logic, conditional power logic,
  survival logic, or simulation behavior unless explicitly requested.
- Do not invent, simplify, or reinterpret statistical methodology. Use
  precise statistical terminology and stay consistent with the existing
  implementation and documentation.
- If the intended statistical behavior is unclear, inspect surrounding
  code, tests, and documentation before making changes.

## R code style

- Follow the style of the surrounding file and existing package
  conventions.
- Prefer explicit, readable, maintainable R code over clever or overly
  compact code.
- Use `camelCase` for object names, function names, and internal
  helpers. Do not introduce new `snake_case` names unless required by an
  external interface.
- Reuse existing helpers and repository patterns instead of duplicating
  logic.
- Avoid unnecessary new dependencies.

## Public API and backward compatibility

- This is a mature package with an existing user base. Avoid unnecessary
  breaking changes.
- Do not rename exported functions, arguments, return values, classes,
  S3 methods, or printed output unless explicitly requested.
- Preserve argument defaults and existing return structures whenever
  possible.
- Do not silently change numerical behavior or output semantics.

## Tests

- Any non-trivial code change should be considered incomplete without
  checking whether tests need to be added or updated.
- For bug fixes, prefer adding or updating a regression test first, then
  implementing the fix.
- Prefer focused, deterministic tests with clear intent.
- Do not weaken tests just to make them pass.
- If a change affects numerical or statistical behavior, review edge
  cases, tolerances, missing values, vector lengths, and boundary
  conditions.

## Documentation and Roxygen

- Keep roxygen documentation synchronized with the implementation.
- When changing exported functions or user-visible behavior, review and
  update:
  - roxygen comments
  - parameter descriptions
  - return value documentation
  - examples
  - vignettes
  - NEWS if the change is user-visible
- Use precise technical language in documentation. Do not oversell
  features or methodological implications.
- Prefer documentation that explains statistical meaning, not only
  software behavior.

## CRAN and package discipline

- Keep changes compatible with normal R package and CRAN expectations.
- Avoid fragile, slow, or unnecessary examples.
- Prefer stable, portable code.
- Do not introduce changes that are likely to create avoidable check,
  documentation, or example issues.

## Preferred Copilot behavior

- Prefer small, targeted patches over broad refactors.
- Before editing, inspect related functions and existing patterns in the
  repository.
- When proposing a change, consider whether tests, roxygen
  documentation, vignettes, and NEWS must also be updated.
- If uncertainty remains, say so explicitly instead of guessing.
