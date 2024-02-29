# Summary
This repository contains the updated code used in simulations comparing serologically-triggered versus fixed-time interval long-term COVID-19 vaccination strategies in Mozambique. Our findings are described in this preprint entitled ["Can long-term COVID-19 vaccination be improved by serological surveillance?: a modeling study for Mozambique"](https://www.medrxiv.org/content/10.1101/2023.08.29.23294793v1)

# Code description
## Main forward model simulation and analysis
Code for the main modeling work and analysis can be found in [0_model](https://github.com/lopmanlab/COVID_serovax_Mozambique_v2/tree/main/0_model). The 10-year forward model simulations were run using a High Performance Cluster. Relevant pieces of the main code are detailed in the table below. For the serologically-triggered vaccination scenarios, vaccination was implemented using an event function in the model setup through the DeSolve package framework for solving ODEs. For fixed-time interval vaccination, the event function was removed and vaccination was implemented directly in the model code to avoid accidental triggering of vaccination. While the codes are separated, all other code structure, initial conditions and parameters were the same between the two vaccination scenarios. Model outputs of deaths, cases, seroprevalence over time, vaccine doses from each model run from the randomly sampled annual Rts were then summarized and interim results are made available in [0_res](1_main/0_res). These results can be further summarized for each vaccine scenario (seroprevalence triggeres of 50%-80% and biennial and annual fixed-time vaccinations) into medians and ranges which are used to produce tables and figures estimating vaccine impact. 

### Description of main model code and processing of raw model outputs
The model code and processing pipeline is used for all of the scenario analysis detailed in the subsequent sections. 

| File                   | Description |Category|
| ---------------------- | ------------- |------------- |
| [0_model/0_model_code_sero](0_model/0_model_code_sero.R)           |Model code function using serology to trigger vaccinations| Serology-triggered models|
| [0_model/0_model_setup](0_model/0_model_setup.R)        | Setup model with seroprevalence vax trigger | Sero-trigger models|
| [0_model/0_model_setup_hiescape](0_model/0_model_setup_hiescape.R) |Setup model with seroprevalence vax trigger & high immune escape| Sero-trigger models|
| [0_model/0_model_setup_randtime](0_model/0_model_setup_randtime.R) |Setup model with seroprevalence vax trigger & randomly-timed epidemics| Sero-trigger models|
| [0_model/1_model_code_int](0_model/1_model_code_int.R)           |Model code function with fixed-time vaccinations| Fixed-time models|
| [0_model/1_model_setup](0_model/1_model_setup.R)        | Setup model with fixed-time trigger | Fixed-time models|
| [0_model/1_model_setup_hiescape](0_model/1_model_setup_hiescape.R) |Setup model with fixed-time vax trigger& high immune escape|Fixed-time models|
| [0_model/0_model_setup_randtime](0_model/0_model_setup_randtime.R) |Setup model with fixed-time vax trigger & randomly-timed epidemics|Fixed-time models|
| [9_last_Rrand](0_model/9_last_Rrand.RDS)      | Distribution of compartments at end of calibration|Model input|
| [9_mixing_matrix_gmix](0_model/9_mixing_matrix_gmix.R)      | Social mixing matrix input|Model input|
| [9_spec_humid](0_model/9_spec_humid.csv)      | Specific humidity over calendar year|Model input||
[0_postprocess/0_case_sero_death_timeseries](0_postprocess/0_case_sero_death_timeseries.R)      | Takes raw outputs from models and summarize into time-series|Compile results &summarise| 
| [0_postprocess/0_imm_timeseries](0_postprocess/0_imm_timeseries.R)      | Takes raw outputs from simulations and summarize into time-series (immune landscape) |Compile results &summarise|
| [0_postprocess/0_nnt_sero](0_postprocess/0_nnt_sero.R)      | Takes raw outputs from simulations and summarize results for NNV |Compile results &summarise|

### Sample folder structure for model input/output and result generation for a single scenario
| File                   | Description |
| ---------------------- | ------------- |
| [0_plot](1_main/0_plot)|Plots and figures from scenario|
| [0_res](1_main/0_res)|Summarized results from scripts in [0_post_process](0_postprocess)|
| [0_sweep_sero](1_main/0_sweep_imm4_foisp0.7.RDS)|Data frame of model parameter inputs for serology-triggered vax scenarios|
| [0_sweep_int](1_main/0_sweep_imm4_foisp0.7_int.RDS)|Data frame of model parameter inputs for fixed-time vax scenarios|
| [2_combine_res_vax](1_main/2_combine_res_vax.R)|Combine model runs and summarise with scripts in 0_postprocess[0_postprocess]|
| [3_plots](1_main/3_plots)|Time series plots and tables|
| [3_plot_nnt](1_main/3_plot_nnt)|Plots and tables for NNV|
| [3_plots_corr](1_main/3_plots_corr)|Plots and tables for correlation|

## Sensitivity analysis
The structure of code for the sensitivity analysis repicate the same structure as the main analysis. The following sensitiivty analysis were conducted

* [Varying rate of waning anitbody](2_kappasweep)
* [Varying decreased susceptibility among seropositive](3_foisweep)
* [Randomly-timed annual epidemics as opposed to seasonal epidemics](4_randtime)
* [High immune escape where future waves are primarily driven by increasingly transmissible variants](5_hiescape)
* [Varying rate of waning immunity](6_waneimmune)
## Model calibration
The code used for model calibration implemented using Approximate Bayesian Approach can be found in [0_calibration](0_calibration)
