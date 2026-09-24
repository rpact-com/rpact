# Parameter Description: Use Interim Estimate (optimal conditional error design)

Parameter Description: Use Interim Estimate (optimal conditional error
design)

## Arguments

- useInterimEstimate:

  Logical. Defines whether or not an interim estimate should be used for
  conditional power. If `TRUE`, a lower cut-off for the interim estimate
  must be specified by `minThetaH1` or `minNonCentralityParameterH1`. An
  upper cut-off may also be specified by `maxThetaH1` or
  `maxNonCentralityParameterH1`. If `FALSE`, the fixed effect size must
  be specified by `thetaH1` or `nonCentralityParameterH1`.
