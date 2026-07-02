# Parameter Description: Simulation Type

Parameter Description: Simulation Type

## Arguments

- simulationType:

  Character value specifying the simulation approach: `"auto"`,
  `"patientWise"`, `"testStatisticBased"`, or `"patientWiseBasic"`. If
  `"auto"` is specified, the simulation type is selected automatically
  based on the explicitly specified arguments. If patient-wise-specific
  arguments such as `eventTime`, `accrualTime`, `accrualIntensity`,
  `dropoutRate1`, `dropoutRate2`, `dropoutTime`, `maxNumberOfSubjects`,
  or `kappa` are provided, patient-wise simulation is used; otherwise
  the test-statistic-based simulation is used for backward
  compatibility.
