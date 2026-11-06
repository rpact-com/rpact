# Parameter Description: Correlation Computation

Parameter Description: Correlation Computation

## Arguments

- correlationComputation:

  If `correlationComputation = "alternative"`, for simulating log-rank
  statistics in the many-to-one design, a correlation matrix according
  to Deng et al. (Biometrics, 2019) accounting for the respective
  alternative is used; if `correlationComputation = "null"`, a constant
  correlation matrix valid under the null, i.e., not accounting for the
  alternative is used, default is `"alternative"`.
