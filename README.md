# mccr

An r package to account for misclassification in nonparametric cumulative incidence functions using external validation data.

mccr can be installed by running

```r
library(devtools)
install_github("edwardsjk/mccr")
```
## Functions 

The package contains the following functions. 
1. `cif_naive`: A function to estimate a nonparametric cumulative incidence function ignoring misclassification of event type
2. `cif_adj_nondiff`: A function to account for nondifferential misclassification of event type in cumulative incidence functions
3. `cif_adj`: A function to account for differential misclassification of event type in cumulative incidence functions

## Inputs

All functions require inputting the main study data and specifying the time and event type variables. `cif_adj_nondiff` and `cif_adj` additioanlly require an external validation dataset that contains the gold standard event type indicator in addition to the possibly misclassified indicator. All functions require the user to specify the maximum follow-up time tau. Details on function calls can be found using `?cif_adj`, `?cif_adj_nondiff`, and `?cif_naive`. 

## Outputs

All functions return a dataframe with a vector of unique event times and the corresponding cumulative incidence estimate. If only the cumulative incidence estimate at the final timepoint is desired, use 

```r
ci <- tail(cif_naive(data = mydata, t = "t", type = "c", level = 1, tau = 1), n = 1)
```
