# ghana-recovr-rct

This repository hosts reproducible analysis of an experiment conducted as part of the Innovations for Poverty Action Core RECOVR survey implemented in Ghana in spring 2020. 

Last updated: May 22, 2020. 

**Current status** Preliminary results, data collection is still underway. 

**Reproducing results**
 - Do `analsis/ghana_convert-to-csv.do` in Stata
    + input `data/Ghana Core RECOVR Survey.dta` (not public)
    + produces `data/ghana_recovr.csv`
- Compile `ghana_recovr_analysis.Rmd` in R
    + input `data/ghana_recovr.csv`

The compiled analysis file is linked [here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/mollyow/ghana-recovr-rct/master/analysis/ghana_recovr_analysis.html)
