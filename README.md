# ghana-recovr-rct

This repository hosts reproducible analysis of an experiment conducted as part of the Innovations for Poverty Action Core RECOVR survey implemented in Ghana in 2020; and a follow-on text experiment. 

Last updated: March 07, 2021. 


**Reproducing results, RECOVR survey:**
 - Do `analsis/ghana_convert-to-csv.do` in Stata
    + input `data/Ghana Core RECOVR Survey.dta` (not public)
    + produces `data/ghana_recovr.csv`
- Compile `ghana_recovr_analysis.Rmd` in R
    + input `data/ghana_recovr.csv`

The compiled analysis file is linked [here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/mollyow/ghana-recovr-rct/master/analysis/ghana_recovr_analysis.html)


**Reproducing results, text experiment:**
 - Run `analysis/Ghana_Analysis.Rmd` in R
     + inputs: 
         * `data/Week1PSA.xlsx` (not public)
         * `data/Week2PSA.xlsx` (not public)
         * `data/Week3PSA.xlsx` (not public)
         * `data/Week4PSA.xlsx` (not public)
         * `data/Week1QA.xlsx` (not public)
         * `data/Week2QA.xlsx` (not public)
         * `data/Week3QA.xlsx` (not public)
         * `data/Week4QA.xlsx` (not public)
         * `IVR-survey.xlsx` (not public)
         * `IPA-IVR survey-combined-tree-results.xlsx` (not public)
         * `IPA 321 List.csv` (not public)
