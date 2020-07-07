# grizbayr 1.2.3

# News

## 1.2.3

- Remove `add = FALSE` argument to a group_by since default is already FALSE and dplyr 1.0.0 throws deprecation warning.
- Add hard requirement for tidyr >= 1.0.0 to use pivot_wider and pivot_longer functions.

## 1.2.2

- Bugfix - Fixed Win Probability Vs. Baseline to divide by samples per option, not total samples.
- Fixed documentation example on `estimate_all_values()` to make sure option_name column returned a string not a factor.
- Changed `estimate_lift()` metric argument to default to `"lift"` and provided clearer error messaging when there is an invalid argument.

## 1.2.1 

- Initial release.
