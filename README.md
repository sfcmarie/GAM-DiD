# GAM-DiD Model for Enhanced Rock Weathering (W-2, VT)

This repository contains the core R code for the Generalized Additive Model with Difference-in-Differences (GAM-DiD) analysis used in the W-2 ERW study at Sleepers River Research Watershed, Vermont. The model estimates the  treatment effect of basalt application on streamwater solute concentrations and fluxes. Demo data are included for reproducibility.

## Requirements
- R 4.3.3
- Packages: mgcv, gratia, dplyr, ggplot2, ggpmisc

## Usage
1. Run 00C_main_script.R to load the data and fit the GAM-DiD model.
2. Run 07_figure4_deltaC_percent.R to plot the treatment effect over time.

## Demo data (data_did_wrtds)
- Group: 0 = control watershed (W-9), 1 = treatment watershed (W-2)
- prepost: 0 = pre-treatment (2022-05-01 to 2023-06-30), 1 = post-treatment (2023-07-01 to 2025-04-30)
- c_target: solute concentration, e.g., alkalinity [mueqL-1]
- tmean..degrees.C.: daily mean air temperature [C]
- ppt..mm. : daily mean precipitation [mm/day]

## Model specification

The GAM-DiD model fits log-concentration as a function of smooth hydroclimatic covariates, a group-specific seasonal cycle, and a difference-in-differences structure on 3-month event-time bins:

logc ~ s(logq, k=4) + s(ppt, k=4) + s(tmean, k=4)
     + s(Month, by = Group)
     + Group + prepost.3monthlag + Group:prepost.3monthlag

- Smooth terms adjust for discharge, precipitation, and temperature.
- s(Month, by = Group) captures group-specific seasonality.
- prepost.3monthlag is a factor with levels -4 to 7 representing 3-month bins relative to treatment onset (July 2023).
- The Group:prepost.3monthlag interaction provides the dynamic treatment effect plotted in Figure 4.
- Fit with method = "REML", Gaussian family, thin-plate regression splines (default).
  
## Citation
Preprint: https://doi.org/10.21203/rs.3.rs-8224816/v1
