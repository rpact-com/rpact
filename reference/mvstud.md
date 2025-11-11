# Original Algorithm AS 251: Student T Distribution

Calculates the Multivariate Normal Distribution with Product Correlation
Structure published by Charles Dunnett, Algorithm AS 251.1 Appl.Statist.
(1989), Vol.38, No.3,
[doi:10.2307/2347754](https://doi.org/10.2307/2347754) .

## Usage

``` r
mvstud(..., NDF, A, B, BPD, D, EPS = 1e-06, INF, IERC = 1, HINC = 0)
```

## Arguments

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- NDF:

  Degrees of Freedom. Use 0 for infinite D.F.

- A:

  Upper limits of integration. Array of N dimensions

- B:

  Lower limits of integration. Array of N dimensions

- BPD:

  Values defining correlation structure. Array of N dimensions

- D:

  Non-Centrality Vector

- EPS:

  desired accuracy. Defaults to 1e-06

- INF:

  Determines where integration is done to infinity. Array of N
  dimensions. Valid values for INF(I): 0 = c(B(I), Inf), 1 = c(-Inf,
  A(I)), 2 = c(B(I), A(I))

- IERC:

  error control. If set to 1, strict error control based on fourth
  derivative is used. If set to zero, error control based on halving
  intervals is used

- HINC:

  Interval width for Simpson's rule. Value of zero caused a default .24
  to be used

## Details

This is a wrapper function for the original Fortran 77 code. For a
multivariate normal vector with correlation structure defined by
RHO(I,J) = BPD(I) \* BPD(J), computes the probability that the vector
falls in a rectangle in n-space with error less than eps.

## Examples

``` r
if (FALSE) { # \dontrun{
N <- 3
RHO <- 0.5
B <- rep(-5.0, length = N)
A <- rep(5.0, length = N)
INF <- rep(2, length = N)
BPD <- rep(sqrt(RHO), length = N)
D <- rep(0.0, length = N)
result <- mvstud(NDF = 0, A = A, B = B, BPD = BPD, INF = INF, D = D)
result
} # }
```
