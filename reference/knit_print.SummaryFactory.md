# Print Summary Factory in Markdown Code Chunks

The function `knit_print.SummaryFactory` is the default printing
function for rpact summary objects in knitr. The chunk option `render`
uses this function by default. To fall back to the normal printing
behavior set the chunk option `render = normal_print`. For more
information see
[`knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html).

## Usage

``` r
# S3 method for class 'SummaryFactory'
knit_print(x, ...)
```

## Arguments

- x:

  A `SummaryFactory`.

- ...:

  Other arguments (see
  [`knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html)).

## Details

Generic function to print a summary object in Markdown.

## Markdown options

Use `options("rpact.print.heading.base.number" = NUMBER)` (where
`NUMBER` is an integer value \>= -2) to specify the heading level.

NUMBER = 1 results in the heading prefix `#`, NUMBER = 2 results in
`##`, ...

The default is `options("rpact.print.heading.base.number" = -2)`, i.e.,
the top headings will be written italic but are not explicit defined as
header. `options("rpact.print.heading.base.number" = -1)` means that all
headings will be written bold but are not explicit defined as header.

Furthermore the following options can be set globally:

- `rpact.auto.markdown.all`: if `TRUE`, all output types will be
  rendered in Markdown format automatically.

- `rpact.auto.markdown.print`: if `TRUE`, all print outputs will be
  rendered in Markdown format automatically.

- `rpact.auto.markdown.summary`: if `TRUE`, all summary outputs will be
  rendered in Markdown format automatically.

- `rpact.auto.markdown.plot`: if `TRUE`, all plot outputs will be
  rendered in Markdown format automatically.

Example: `options("rpact.auto.markdown.plot" = FALSE)` disables the
automatic knitting of plots inside Markdown documents.
