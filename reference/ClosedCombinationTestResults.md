# Analysis Results Closed Combination Test

Class for multi-arm analysis results based on a closed combination test.

## Details

This object cannot be created directly; use
[`getAnalysisResults`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md)
with suitable arguments to create the multi-arm analysis results of a
closed combination test design.

## Fields

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

- `indices`:

  Indicates which stages are available for analysis.

- `adjustedStageWisePValues`:

  The multiplicity adjusted p-values from the separate stages. Is a
  numeric matrix.

- `overallAdjustedTestStatistics`:

  The overall adjusted test statistics.

- `separatePValues`:

  The p-values from the separate stages. Is a numeric matrix.

- `conditionalErrorRate`:

  The calculated conditional error rate.

- `secondStagePValues`:

  For conditional Dunnett test, the conditional or unconditional p-value
  calculated for the second stage.

- `rejected`:

  Indicates whether a hypothesis is rejected or not.

- `rejectedIntersections`:

  The simulated number of rejected arms in the closed testing
  procedure.. Is a logical matrix.
