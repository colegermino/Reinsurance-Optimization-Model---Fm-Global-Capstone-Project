# FM Global Reinsurance Optimization Capstone Project
<img src="Graphic.png" alt="Simulation Overview" width = "100%" height = "300"/>

## Simulation_Engine.R

This R script performs a comprehensive simulation and evaluation of reinsurance structures applied to a hospital portfolio stratified by risk quality. It integrates statistical modeling, loss distribution fitting, and Monte Carlo simulation to assess the financial performance of various reinsurance strategies under a range of parameter sensitivities. Loss severity is modeled using a hybrid approach that fits a log-normal distribution for body losses and a generalized Pareto distribution (GPD) for tail losses, with dynamic truncation informed by each hospital’s Maximum Foreseeable Loss (MFL). A log-linear regression model is employed to estimate reinsurance premiums across a range of attachment points and quota share agreements.

The script supports the evaluation of layered reinsurance towers that incorporate both quota share and excess-of-loss arrangements. Sensitivity analysis is performed across multiple dimensions, including risk probabilities, shape and scale parameters, threshold levels, and premium multipliers ranging from 50% to 150% of their base estimates. Simulation outputs include key performance indicators such as average annual losses, recovery ratios, volatility metrics, and 1 in 100-year worst loss estimates. All results are compiled and exported to Excel for further analysis. This tool is intended for use by insurance analysts, underwriters, actuaries, and risk managers seeking to optimize reinsurance programs and quantify volatility mitigation under complex, multi-layer risk frameworks.

## Compute_Recovery.R

This R script performs a retrospective analytical evaluation of historical reinsurance performance by calculating realized recoveries on actual loss events across a hospital portfolio. It ingests and joins loss data, reinsurance tower structures, and policy identifiers to simulate recovery flows across layered reinsurance arrangements for each historical loss. By iterating through reinsurance layers, the script identifies the portion of each loss recovered under each layer, based on attachment points, layer limits, and quota shares.

The script aggregates these recoveries by reinsurance policy and year, and then joins them with estimated reinsurance premiums. Since only 2023 premium data is available, simulated premiums for earlier years are derived by applying de-trending factors to account for inflation or pricing shifts. The result is a policy-year-level dataset containing both recoveries and adjusted premiums, enabling the calculation of recovery ratios and performance diagnostics across the portfolio. The output is exported as an Excel file for further analysis and reporting.

## Results_and_Analysis.zip

A zip containing excel files of the simulation engine's final output, an analysis of the results, an analysis of the models sensitivty to key parameters, and a preliminary analysis of the hospital portfolio's reinsurance program.  

### Data_Analysis.xlsx

A file containing analysis on reinsurance recoveries and losses between 2014-2023. Key graphics and insights present in the Data Analysis section of the Final Report can be found in this file. 

### Results.xlsx

Contains the final output from the simulation engine. 

### Sensitivity_Summary.xlsx

Contains histograms of how sensitive a reinsurance tower's Volatilty Reduction, 1 in 100 Year Loss, Recovery Ratio, and Additional Volatilty Reduction with a 0-50 Million Dollar Burn Layer Policy is to changes in key model parameters. Also includes a summary of each towers performance under base case parameter assumptions. 

## Hospital_Data.xlsx

The master dataset that contains all reinsurance, loss, reinsurance pricing, and tower setting data required to run Compute_Recovery.R and Simulation_Engine.R.

