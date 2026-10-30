# Parameter Description: Design with Default for Enrichment Designs

Parameter Description: Design with Default for Enrichment Designs

## Arguments

- design:

  The trial design. If no trial design is specified, a fixed sample size
  design is used. In this case, Type I error rate `alpha`, Type II error
  rate `beta`, `twoSidedPower`, and `sided` can be directly entered as
  argument where necessary. Must be created with
  [`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
  [`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
  or
  [`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md);
  designs created with
  [`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md)
  or
  [`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md)
  are not supported.
