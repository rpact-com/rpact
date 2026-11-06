# Parameter Description: Type Of Shape

Parameter Description: Type Of Shape

## Arguments

- typeOfShape:

  The shape of the dose-response relationship over the treatment groups.
  This can be either `"linear"`, `"sigmoidEmax"`, or `"userDefined"`,
  default is `"linear"`.  
  For `"linear"`, `muMaxVector` specifies the range of effect sizes for
  the treatment group with highest response. If `"sigmoidEmax"` is
  selected, `gED50` and `slope` has to be entered to specify the ED50
  and the slope of the sigmoid Emax model. For `"sigmoidEmax"`,
  `muMaxVector` specifies the range of effect sizes for the treatment
  group with response according to infinite dose. If `"userDefined"` is
  selected, `effectMatrix` has to be entered.
