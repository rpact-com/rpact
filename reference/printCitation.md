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
#> To cite package 'rpact' in publications use:
#> 
#>   Wassmer G, Pahlke F (2026). rpact: Confirmatory Adaptive Clinical
#>   Trial Design and Analysis. R package version 4.3.1.9302.
#>   doi:10.32614/CRAN.package.rpact
#> 
#>   Wassmer G, Brannath W (2025). _Group Sequential and Confirmatory
#>   Adaptive Designs in Clinical Trials_, 2nd edition. Springer, Cham,
#>   Switzerland. ISBN 978-3-031-89668-2, doi:10.1007/978-3-031-89669-9
#>   <https://doi.org/10.1007/978-3-031-89669-9>.
```
