# Parameter Description: Information Epsilon

Parameter Description: Information Epsilon

## Arguments

- informationEpsilon:

  Positive integer value specifying the absolute information epsilon,
  which defines the maximum distance from the observed information to
  the maximum information that causes the final analysis. Updates at the
  final analysis in case the observed information at the final analysis
  is smaller ("under-running") than the planned maximum information
  `maxInformation`, default is 0. Alternatively, a floating-point number
  \> 0 and \< 1 can be specified to define a relative information
  epsilon.
