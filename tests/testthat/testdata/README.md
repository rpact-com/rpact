# Optimal conditional error reference data

`testdataOptimalConditionalError.RData` was supplied with the original contribution
(commit 98c898cb, PR #93). It contains the original numerical comparison grid and
reference conditional error functions. The values are retained unchanged.

The original contribution does not include a generator or provenance sufficient
to treat this fixture as independent statistical validation. The test suite also
checks the level condition, operating characteristics, likelihood ratios against
numerical integration of the prior, and optimisation against an independently
evaluated objective. Do not regenerate the fixture from the implementation under test.

For the constrained maximum-likelihood-ratio design, tighter calibration changes
conditional error by up to 0.000125711 on this grid. That historical comparison
uses an explicit absolute bound of 0.00013; a separate, more accurate integration
checks the target type I error. The fixture itself is not updated.
