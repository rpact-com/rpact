# Parameter Description: Type of Selection

Parameter Description: Type of Selection

## Arguments

- typeOfSelection:

  The way the treatment arms or populations are selected at interim.
  Five options are available: `"best"`, `"rbest"`, `"epsilon"`, `"all"`,
  and `"userDefined"`, default is `"best"`.  
  For `"rbest"` (select the `rValue` best treatment arms/populations),
  the parameter `rValue` has to be specified, for `"epsilon"` (select
  treatment arm/population not worse than epsilon compared to the best),
  the parameter `epsilonValue` has to be specified. If `"userDefined"`
  is selected, `"selectArmsFunction"` or `"selectPopulationsFunction"`
  has to be specified.
