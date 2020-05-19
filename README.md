# 2020-ncov

Analysis of the COVID-19 outbreak during 2019/20. _Note: this is working repository, so code and data are likely to change over time_

This code was adopted from https://github.com/adamkucharski/2020-ncov/

All the relevant codes are in the `stoch_model_V2` directory 
### Guide to files for `stoch_model_V2`

This is a stochastic SIR model implemented using Euler-Maruyama, with likelihood estimated using SMC by jointly fitting to cases in different  and exported cases over time in countries with high connectivity to Wuhan.

Analysis for all groups is done in `R/driver.r`. Other R files:

> `scripts/main_model.R` - Load data and run analysis for a single group

> `R/load_timeseries.r` - Load and format timeseries data

> `R/model_functions.r` - Load process model and SMC

> `R/plotting_functions.r` - Plotting functions and saving R_0 values

> `R/outputs_main.R` - Run main model outputs

> `R/province_clustering.R` - Clustering prefectueres with deferent features

> `R/plot_visuals.R` - Plot visualization with PCA

> `R/case_rmse.R` - calculate case RMSE

> `plot_map.R` - plot maps
