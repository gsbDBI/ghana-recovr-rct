/*******************************************************************************

Title	 : Ghana COVID 
Purposes : Convert Ghana COVID RECOVR data from .dta to .csv, version(12)
Author	 : Molly Offer-Westort
Inputs	 : "Ghana Core RECOVR Survey.dta"
Output	 : data: 
			- ghana_recovr_variables.csv, 
			- ghana_recovr.dta [version(12)]
		   variables: 
		    - ghana_recovr.csv

*******************************************************************************/

// For Stanford Sherlock, first:
// module load stata/15

// Read in data
use "Ghana Core RECOVR Survey.dta", clear

// save as .csv
outsheet  using ghana_recovr.csv , comma replace

// save variable name, type, isnumeric, format, labels in .csv file
preserve
    describe, replace
    list
    outsheet using ghana_recovr_variables.csv, comma replace
restore

// shorten variable labels for version 12
foreach i of varlist _all {
	local longlabel: var label `i'
	local shortlabel = substr(`"`longlabel'"',1,79)
	label var `i' `"`shortlabel'"'
}

// save to version 12 (Molly's personal version)
saveold ghana_recovr, version(12) replace
