# Fisher Information

An R6 result class containing Fisher information calculated by
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
together with its analysis stages, information type, and planning
situations.

## Usage

``` r
# S3 method for class 'FisherInformation'
as.matrix(x, ...)

# S3 method for class 'FisherInformation'
as.double(x, ...)

# S3 method for class 'FisherInformation'
is.finite(x)

# S3 method for class 'FisherInformation'
is.infinite(x)

# S3 method for class 'FisherInformation'
is.nan(x)

# S3 method for class 'FisherInformation'
is.na(x)

# S3 method for class 'FisherInformation'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'FisherInformation'
length(x)

# S3 method for class 'FisherInformation'
dim(x)

rownames.FisherInformation(x)

# S3 method for class 'FisherInformation'
print(x, ...)

# S3 method for class 'FisherInformation'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  A `FisherInformation` object.

- ...:

  Additional arguments passed to the respective method.

- row.names:

  Ignored.

- optional:

  Ignored.

## Details

Objects of this class cannot be created directly. Use
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
instead.

The public fields `information`, `type`, `stage`, and `situations`
describe the calculated values. The originating design plan is retained
privately so that the complete object can be piped into
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
without exposing or duplicating that potentially large object in the
printed result.

Use [`as.numeric()`](https://rdrr.io/r/base/numeric.html) to obtain the
information values as a plain numeric vector (column by column, i.e.,
all stages within each situation),
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) to obtain a
stage-by-situation matrix, and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to obtain
a long-format data frame;
[`is.finite()`](https://rdrr.io/r/base/is.finite.html),
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html), and
[`is.na()`](https://rdrr.io/r/base/NA.html) to check the information
values for finiteness, infiniteness, NaN, and NA, respectively, and
[`length()`](https://rdrr.io/r/base/length.html) to obtain the number of
information values.

## Fields

- `information`:

  A numeric value, vector, or matrix with the calculated Fisher
  information.

- `type`:

  Either `"cumulative"` or `"stageWise"`.

- `stage`:

  Integer vector identifying the represented analysis stages.

- `situations`:

  Optional character vector identifying the represented planning
  situations.
