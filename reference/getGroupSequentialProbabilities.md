# Get Group Sequential Probabilities

Calculates probabilities in the group sequential setting.

## Usage

``` r
getGroupSequentialProbabilities(decisionMatrix, informationRates)
```

## Arguments

- decisionMatrix:

  A matrix with either 2 or 4 rows and kMax = length(informationRates)
  columns, see details.

- informationRates:

  The information rates t_1, ..., t_kMax (that must be fixed prior to
  the trial), default is `(1:kMax) / kMax`. For the weighted inverse
  normal design, the weights are derived through w_1 = sqrt(t_1), and
  w_k = sqrt(t_k - t\_(k-1)). For the weighted Fisher's combination
  test, the weights (scales) are w_k = sqrt((t_k - t\_(k-1)) / t_1) (see
  the documentation).

## Value

Returns a numeric matrix containing the probabilities described in the
details section.

## Details

Given a sequence of information rates (fixing the correlation
structure), and decisionMatrix with either 2 or 4 rows and kMax =
length(informationRates) columns, this function calculates a probability
matrix containing, for two rows, the probabilities:  
P(Z_1 \< l_1), P(l_1 \< Z_1 \< u_1, Z_2 \< l_2),..., P(l_kMax-1 \<
Z_kMax-1 \< u_kMax-1, Z_kMax \< l_l_kMax)  
P(Z_1 \< u_1), P(l_1 \< Z_1 \< u_1, Z_2 \< u_2),..., P(l_kMax-1 \<
Z_kMax-1 \< u_kMax-1, Z_kMax \< u_l_kMax)  
P(Z_1 \< Inf), P(l_1 \< Z_1 \< u_1, Z_2 \< Inf),..., P(l_kMax-1 \<
Z_kMax-1 \< u_kMax-1, Z_kMax \< Inf)  
with continuation matrix  
l_1,...,l_kMax  
u_1,...,u_kMax  
That is, the output matrix of the function provides per stage (column)
the cumulative probabilities for values specified in decisionMatrix and
Inf, and reaching the stage, i.e., the test statistics is in the
continuation region for the preceding stages. For 4 rows, the
continuation region contains of two regions and the probability matrix
is obtained analogously (cf., Wassmer and Brannath, 2016).

## See also

Other design functions:
[`getDesignCharacteristics()`](https://rpact-com.github.io/rpact/reference/getDesignCharacteristics.md),
[`getDesignConditionalDunnett()`](https://rpact-com.github.io/rpact/reference/getDesignConditionalDunnett.md),
[`getDesignFisher()`](https://rpact-com.github.io/rpact/reference/getDesignFisher.md),
[`getDesignGroupSequential()`](https://rpact-com.github.io/rpact/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://rpact-com.github.io/rpact/reference/getDesignInverseNormal.md),
[`getPowerAndAverageSampleNumber()`](https://rpact-com.github.io/rpact/reference/getPowerAndAverageSampleNumber.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate Type I error rates in the two-sided group sequential setting when
# performing kMax stages with constant critical boundaries at level alpha:
alpha <- 0.05
kMax <- 10
decisionMatrix <- matrix(c(
    rep(-qnorm(1 - alpha / 2), kMax),
    rep(qnorm(1 - alpha / 2), kMax)
), nrow = 2, byrow = TRUE)
informationRates <- (1:kMax) / kMax
probs <- getGroupSequentialProbabilities(decisionMatrix, informationRates)
cumsum(probs[3, ] - probs[2, ] + probs[1, ])

# Do the same for a one-sided design without futility boundaries:
decisionMatrix <- matrix(c(
    rep(-Inf, kMax),
    rep(qnorm(1 - alpha), kMax)
), nrow = 2, byrow = TRUE)
informationRates <- (1:kMax) / kMax
probs <- getGroupSequentialProbabilities(decisionMatrix, informationRates)
cumsum(probs[3, ] - probs[2, ])

# Check that two-sided Pampallona and Tsiatis boundaries with binding 
# futility bounds obtain Type I error probabilities equal to alpha:
x <- getDesignGroupSequential(
    alpha = 0.05, beta = 0.1, kMax = 3, typeOfDesign = "PT",
    deltaPT0 = 0, deltaPT1 = 0.4, sided = 2, bindingFutility = TRUE
)
dm <- matrix(c(
    -x$criticalValues, -x$futilityBounds, 0,
    x$futilityBounds, 0, x$criticalValues
), nrow = 4, byrow = TRUE)
dm[is.na(dm)] <- 0
probs <- getGroupSequentialProbabilities(
    decisionMatrix = dm, informationRates = (1:3) / 3
)
sum(probs[5, ] - probs[4, ] + probs[1, ])

# Check the Type I error rate decrease when using non-binding futility bounds:
x <- getDesignGroupSequential(
    alpha = 0.05, beta = 0.1, kMax = 3, typeOfDesign = "PT",
    deltaPT0 = 0, deltaPT1 = 0.4, sided = 2, bindingFutility = FALSE
)
dm <- matrix(c(
    -x$criticalValues, -x$futilityBounds, 0,
    x$futilityBounds, 0, x$criticalValues
), nrow = 4, byrow = TRUE)
dm[is.na(dm)] <- 0
probs <- getGroupSequentialProbabilities(
    decisionMatrix = dm, informationRates = (1:3) / 3
)
sum(probs[5, ] - probs[4, ] + probs[1, ])
} # }
```
