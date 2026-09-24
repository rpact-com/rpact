# Optimal Conditional Error Design

R6 parameter set for an adaptive two-stage design based on an optimal
conditional error function.

## Details

Create objects with
[`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md).
This class inherits from `ParameterSet`, not `TrialDesign`: its
second-stage information depends on the interim result. Functions
accepting conventional group sequential or combination-test designs
cannot use this object.

## See also

[`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md)
