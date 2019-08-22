<!-- README.md is generated from README.Rmd. Please edit that file -->

scallopr
========

<!-- badges: start -->
<!-- badges: end -->

The goal of scallopr is to provide a suite of functions for analyzing
ADF&G weathervane scallop survey data.

Installation
------------

You can install the released version of scallopr from the commercial
fisheries github site with:

``` r
# install.packages("devtools")
devtools::install_github("commfish/scallopr")
```

Example
-------

This is a basic example which shows how to setup and run each function
independently. Note that this is based around using an R-project format
that has the following folder structure:

-   `data`
-   `figs`
-   `output`
-   `analysis`
-   `text`

``` r
library(scallopr)
theme_set(FNGr::theme_sleek())

# Constants for 2019 survey ----
YEAR <- 2019
Q <- 0.83

# data ----
events_data <- read_csv(paste0('data/', YEAR, '/HaulTableData_Cruise1901.csv')) 
catch_data <- read_csv(paste0('data/', YEAR, '/CatchTableData_Cruise1901.csv'))
awl_data <- read_csv(paste0('data/', YEAR, '/ScalMeatTableData_Cruise1901.csv'))
shell_data <- read_csv(paste0('data/', YEAR, '/ScalShellTableData_Cruise1901.csv'))
set.seed(8456)

# clean data ----
tows <- clean_tow(events_data)
beds <- clean_bed(tows)
scal_catch <- clean_catch(catch_data, tows, YEAR)
scal_awl <- clean_awl(awl_data, tows)

# table of catch results
tbl_catch <- tbl_catch(scal_catch, beds)
abund_est <- scallop_est(scal_catch, beds, Q = Q, abundance = TRUE, boot = TRUE)
biom_est <- scallop_est(scal_catch, beds, Q = Q, abundance = TRUE, boot = TRUE)
meat_wt_est(scal_awl, scal_catch, Q = Q, beds, boot = TRUE)
tbl_sex(scal_awl, tows)
tbl_worm(scal_awl, tows)
tbl_gonad(scal_awl, tows)


# figures ----
plot_catch_rate(scal_catch, tows, YEAR)

# Function for plotting abundance (abundance = TRUE) or biomass estimates
plot_scal_est(abund_est, abundance = TRUE, YEAR)
plot_scal_est(biom_est, abundance = FALSE, YEAR)

plot_size_dist(scal_awl, scal_catch, tows, YEAR)
plot_mw_rw(scal_awl, tows, Districts, YEAR)
plot_mw_sh(scal_awl, tows, Districts, YEAR)
```
