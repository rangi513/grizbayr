# grizbayr

## A Bayesian Inference Package for A|B and Bandit Marketing Tests

### Description:

Uses simple Bayesian conjugate prior update rules to calculate the following metrics for various marketing objectives:

  1. Win Probability of each option
  2. Value Remaining in the Test
  3. Percent Lift Over the Baseline

This allows a user to implement Bayesian Inference methods when analyzing the results of a split test or Bandit experiment.

### Marketing objectives supported:

 - Conversion Rate
 - Response Rate
 - Click Through Rate (CTR)
 - Revenue Per Session
 - Multi Revenue Per Session
 - Cost Per Activation (CPA)
 - Total Contribution Margin (CM)
 - CM Per Click
 - Cost Per Click (CPC)


## Contributing
To add a new posterior distribution you must complete the following:

1. Create a new function called `sample_...(input_df, priors, n_samples)`. Use the internal helper functions update_gamma, update_beta, etc. included in this package or you can create a new one.
1. This function (and the name) must be added to the switch statement in `sample_from_posterior()`
1. A new row must be added to the internal data object `distribution_column_mapping`. 
    - Select this object from the package
    - Add a new row with a 1 for every column that is required for this distribution (this is for data validation and clear alerting for the end user)
    - Save the updated tibble object using `use_data(new_tibble, internal = TRUE, overwrite = TRUE)` and it will be saved as `sysdata.rda` in the package for internal use.
    - Update the start.Rmd markdown table to include which columns are required for your function.
  
1. Create a PR for review.  


#### Package Name

The name is a play on Bayes with an added r (bayesr). The added griz (or Grizzly Bear) creates a unique name that is searchable due to too many similarly named packages.
