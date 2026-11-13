# Render a Trial Comparison Table in knitr

Renders data frames prepared by
[`as.data.frame.TrialComparison()`](https://docs.rpact.org/qtd/reference/as.data.frame.TrialComparison.md)
with `output = "auto"`, `"html"`, or `"pdf"`. HTML tables scroll
horizontally. LaTeX tables use longtable and split wide results into
column blocks, repeating the identifier columns in each block.

## Usage

``` r
# S3 method for class 'TrialComparisonTable'
knit_print(x, ...)
```

## Arguments

- x:

  A `TrialComparisonTable` returned by
  [`as.data.frame.TrialComparison()`](https://docs.rpact.org/qtd/reference/as.data.frame.TrialComparison.md).

- ...:

  Further arguments passed to the ordinary knitr data-frame renderer
  when the target is neither HTML nor LaTeX.

## Value

A [`knitr::asis_output`](https://rdrr.io/pkg/knitr/man/asis_output.html)
object for HTML or LaTeX. Other targets use the ordinary knitr
data-frame representation. Empty tables emit no output.

## Details

PDF mode emits a LaTeX fragment for the surrounding document. It
requires the suggested rmarkdown package and declares the longtable
dependency via knitr metadata. No PDF file is created directly by this
method.
