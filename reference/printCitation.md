# Print Citation

How to cite `rpact` and `R` in publications.

## Usage

``` r
printCitation(inclusiveR = TRUE, language = "en", markdown = NA)
```

## Arguments

- inclusiveR:

  If `TRUE` (default) the information on how to cite the base R system
  in publications will be added.

- language:

  Language code to use for the output, default is "en".

- markdown:

  If `TRUE`, the output will be created in Markdown.

## Details

This function shows how to cite `rpact` and `R` (`inclusiveR = TRUE`) in
publications.

## Examples

``` r
printCitation()
#> 
#> To cite R in publications use:
#> 
#>   R Core Team (2025). _R: A Language and Environment for Statistical
#>   Computing_. R Foundation for Statistical Computing, Vienna, Austria.
#>   <https://www.R-project.org/>.
#> 
#> To cite package ‘rpact’ in publications use:
#> 
#>   Wassmer G, Pahlke F (2025). _rpact: Confirmatory Adaptive Clinical
#>   Trial Design and Analysis_. doi:10.32614/CRAN.package.rpact
#>   <https://doi.org/10.32614/CRAN.package.rpact>, R package version
#>   4.2.0, <https://cran.r-project.org/package=rpact>.
```
