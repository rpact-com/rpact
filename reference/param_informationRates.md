# Parameter Description: Information Rates

Parameter Description: Information Rates

## Arguments

- informationRates:

  The information rates t_1, ..., t_kMax (that must be fixed prior to
  the trial), default is `(1:kMax) / kMax`. For the weighted inverse
  normal design, the weights are derived through w_1 = sqrt(t_1), and
  w_k = sqrt(t_k - t\_(k-1)). For the weighted Fisher's combination
  test, the weights (scales) are w_k = sqrt((t_k - t\_(k-1)) / t_1) (see
  the documentation).
