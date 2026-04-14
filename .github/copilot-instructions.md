# Copilot instructions for rpact

## Repository purpose

This repository contains the R package `rpact`, which provides methodology and implementation for confirmatory adaptive clinical trial design, simulation, and analysis.

Copilot must prioritize:
- statistical correctness
- numerical robustness
- reproducibility
- backward compatibility
- clear user-facing documentation

Do not optimize for brevity if this reduces correctness, readability, or traceability.

## Domain-specific expectations

`rpact` is a scientific/statistical package. Changes to formulas, testing logic, decision boundaries, stage-wise calculations, p-value handling, confidence intervals, conditional power, futility/efficacy boundaries, survival calculations, or simulation logic must be treated as high-risk.

When editing statistical code:
- preserve the existing methodology unless a change is explicitly requested
- do not invent or simplify statistical logic
- keep terminology precise and technically correct
- if a method is unclear, inspect surrounding code and documentation before changing behavior
- prefer consistency with existing `rpact` methodology and naming over generic R idioms

## Backward compatibility

This is a mature package with an existing user base.

Therefore:
- avoid unnecessary API changes
- do not rename exported functions, arguments, list elements, S3 methods, classes, or printed output unless explicitly requested
- preserve argument defaults unless a change is explicitly requested
- preserve existing return structure wherever possible
- avoid changing numerical behavior unless justified and covered by tests

If a breaking change seems necessary, clearly flag it in comments or in the proposed summary.

## Coding style

When generating or editing R code:
- follow the existing style of the surrounding file, e.g., use "camelCase" and not "snake_case"
- prefer explicit, readable code over clever one-liners
- keep function interfaces stable and self-explanatory
- avoid introducing new dependencies unless clearly justified
- prefer reuse of existing internal helpers over duplicating logic
- keep validation and error messages informative and precise
- avoid hidden side effects

## Numerical and scientific quality

For numerical code:
- be careful with rounding, tolerance, boundary cases, missing values, and vector lengths
- preserve deterministic behavior where expected
- avoid changes that silently alter statistical results
- do not replace established calculations with approximate shortcuts unless explicitly requested

If code affects statistical results, simulations, or reportable quantities:
- check whether tests, examples, vignettes, or documentation also need updates

## Tests

Whenever code is changed, Copilot should consider tests part of the task.

Expectations:
- add or update focused tests for any changed behavior
- prefer minimal, deterministic tests
- preserve existing test intent
- cover edge cases and regression scenarios where relevant
- do not weaken tests just to make them pass

For bug fixes:
- first add or update a regression test that captures the issue
- then implement the fix

## Documentation

When changing exported functions or user-visible behavior:
- update roxygen documentation if needed
- keep parameter names, defaults, and details consistent with the implementation
- update examples only if necessary and keep them robust
- avoid examples that are unnecessarily slow or fragile
- update NEWS or release notes if the change is user-visible and meaningful

Use precise scientific language. Do not oversell changes.

## Vignettes and user guidance

`rpact` users rely on trustworthy explanations.

Therefore:
- prefer clear, technically accurate explanations
- align examples with realistic clinical trial methodology
- use consistent terminology across function docs, vignettes, and messages
- do not introduce methodological claims without support from the package context

## Safety checks before proposing changes

Before proposing a non-trivial change, check:
1. Does this alter statistical methodology or numerical output?
2. Does this change a public interface?
3. Does this require test updates?
4. Does this require documentation or NEWS updates?
5. Is there an existing helper or pattern in the repository that should be reused?

## Preferred Copilot behavior

When assisting in this repository, Copilot should:
- inspect related functions before editing
- prefer small, targeted patches
- explain statistical or technical consequences of proposed changes
- highlight uncertainty instead of guessing
- avoid broad refactors unless explicitly requested
