# Algorithm AS 251: Normal Distribution

Calculates the Multivariate Normal Distribution with Product Correlation
Structure published by Charles Dunnett, Algorithm AS 251.1 Appl.Statist.
(1989), Vol.38, No.3,
[doi:10.2307/2347754](https://doi.org/10.2307/2347754) .

## Usage

``` r
as251Normal(
  lower,
  upper,
  sigma,
  ...,
  eps = 1e-06,
  errorControl = c("strict", "halvingIntervals"),
  intervalSimpsonsRule = 0
)
```

## Arguments

- lower:

  Lower limits of integration. Array of N dimensions

- upper:

  Upper limits of integration. Array of N dimensions

- sigma:

  Values defining correlation structure. Array of N dimensions

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- eps:

  desired accuracy. Defaults to 1e-06

- errorControl:

  error control. If set to 1, strict error control based on fourth
  derivative is used. If set to zero, error control based on halving
  intervals is used

- intervalSimpsonsRule:

  Interval width for Simpson's rule. Value of zero caused a default .24
  to be used

## Details

For a multivariate normal vector with correlation structure defined by
rho(i,j) = bpd(i) \* bpd(j), computes the probability that the vector
falls in a rectangle in n-space with error less than eps.

This function calculates the `bdp` value from `sigma`, determines the
right `inf` value and calls
[`mvnprd`](https://rpact-com.github.io/rpact/reference/mvnprd.md).
