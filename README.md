# COVID-19_Prisons
This code is my code submitted as part of a group project for UCLA's undergraduate statistical consulting series: Stats 140 in Spring 2021. 

Goal: Predicting COVID-19 case frequencies among inmates in 33 CA state prisons and investigating the relationship between prison characteristics and COVID-19 infections. 

Predictors include:
* Total Population
* Staffed Capacity
* Design Capacity
* Percent Occupied
* Facility Sex (Male, Female, Mixed)
* White Prison Admission Rate
* Male Prison Admission Rate
* Female Prison Admission Rate

## Data Sources: 
* UCLA Law's COVID Behind Bars Project: https://github.com/uclalawcovid19behindbars
* California Department of Corrections and Rehabilitation population reports: https://www.cdcr.ca.gov/research/population-reports-2/
* Vera Institute of Justice's 2018 prison demographics data: https://github.com/vera-institute/incarceration-trends 

## Datasets:
* PrisonCovid.csv = merged prison-level time series data of COVID-19 case counts and death rates for inmates and staff as well as prison-level demographics and facility characteristics  

## Code:
* Modelling.Rmd = all frequentist and bayesian regression models for predicting case frequencies as well as comparisons and analysis of performance 
* Visualizations.RM = a collection of graphs and visualizations used in the reports and presentation

## Documents:
* Final_Presentation.pdf = slides from the final presentation delivered in June 2021
* Final_Report.docx
