# Characteristics of an Optimal Conditional Error Design

Operating characteristics returned by
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md)
for an optimal conditional error design. Inherits from `ParameterSet`.

## Details

`theta` contains the evaluated effects and `overallReject` the overall
rejection probabilities. `rejectPerStage` has two rows (stages one and
two), and `futilityPerStage` one row (interim futility). Columns
correspond to `theta`. Final non-rejection is not classified as early
futility.
