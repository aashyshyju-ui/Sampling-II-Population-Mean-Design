Sampling Techniques – Stratified Sampling Design for Population Mean

This repository presents an R-based implementation for determining the required sample size to estimate the population mean using stratified (subgroup) sampling, developed as part of Sampling–II coursework.

The design integrates sampling bias, within-subgroup variability, and operational constraints such as survey cost and time, reflecting practical survey conditions.

Scope

Sample size determination for population mean estimation

Subgroup-wise allocation using standard stratified sampling strategies

Incorporation of cost and time considerations

Visual representation of the sampling design

Allocation Methods

Proportional Allocation – based on subgroup population sizes

Neyman Allocation – variance-minimising allocation using subgroup variability

Optimised Allocation – cost and time adjusted optimal design

Inputs and Outputs

Inputs: subgroup sizes, subgroup variability, allowable error, cost and time per subgroup
Outputs: total sample size, subgroup-wise allocation and design plots

Implementation

Language: R

Visualisation: Base R plotting

Repository Contents

sampling2_population_mean_design.R – main script for computation and visualisation
