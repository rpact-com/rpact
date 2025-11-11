# Print Dataset Values

`print` prints its
[`Dataset`](https://rpact-com.github.io/rpact/reference/Dataset.md)
argument and returns it invisibly (via `invisible(x)`).

## Usage

``` r
# S3 method for class 'Dataset'
print(
  x,
  ...,
  markdown = NA,
  output = c("list", "long", "wide", "r", "rComplete")
)
```

## Arguments

- x:

  A [`Dataset`](https://rpact-com.github.io/rpact/reference/Dataset.md)
  object.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- markdown:

  If `TRUE`, the output will be created in Markdown.

- output:

  A character defining the output type, default is "list".

## Details

Prints the dataset.
