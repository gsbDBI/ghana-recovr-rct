/*******************************************************************************

Title	 : Ghana COVID Randomization
Purposes : Randomizes RDD respondents in treatment groups
Author	 : Ishmail Azindoo Baako 
		   Based on "Molly Offer-Westort" randomization dofile
Inputs	 : List of Phone Numbers (required) - dta or csv format (add extension to filename)
Output	 : List of phone numbers with 2 random asigment variables

	Randomization Procedure
	-----------------------
	
	Additional variables generated:
		
		index 
			order (_n) by which numbers will be contacted. This will be based on 
			a random sort order
		
		identifiable_victim
			- 1 : identifiable victim condition (treatment)
			- 0 : ghana community condition (control)
	
		gamification
			- 1 : gamification condition (treatment)
			- 0 : Pure PSA (control)
			
		cons_vers
			- 1 : CONSENT VERSION 1 - researchers
			- 2 : CONSENT VERSION 2 - policymakers
			- 3 : CONSENT VERSION 3 - policymakers + personalized
			- 4 : CONSENT VERSION 4 - government
			
		cons_vers
			- researchers 					: CONSENT VERSION 1 - researchers
			- policymakers 					: CONSENT VERSION 2 - policymakers
			- policymakers + personalized 	: CONSENT VERSION 3 - policymakers + personalized
			- government					: CONSENT VERSION 4 - government
			
		end_type 
			- 1 : Treatment 1
			- 2 : Treatment 2
			
		end_type_lab
			- researchers  : Treatment 1
			- leaders 	   : Treatment 1
			
	NB: MAKE ALL CHANGES AT THE ADDITIONAL SETUP SECTION
	
*******************************************************************************/			 

* SETTING UP
	
	* create log file
//	cls
	cd ~/Documents/Atheylab/ipa_comms/randomization
	cap log close _all 
	log using "ghana_covid_randomization_`c(current_date)'_`=subinstr("`c(current_time)'", ":", "", .)'", replace smcl
	
	* clear and set stata version
//	version 15.1
	clear all
	set more off
	
	* set seeds
	set seed 		94305
	set sortseed	94305 
	set obs 20000


********************************************************************************
	
* ADDITIONAL SETUP: MAKE CHANGES HERE

	* phone_survey? Set to one if this is the phone survey sample, else set to 0
	
		loc phone_survey 0
	
	* ID prefix : "GA" - batch 1, "GB" - batch 2, "GC" for batch 3
	
		loc prefix "GA"
	
	* input path and filename.
		loc input_file 		"RDD_Sample_Ghana_Replicate_1.xlsx"
		
	* output path and filename
		loc output_file		"recovr_rdd_batch1.dta"	
	
********************************************************************************

* IMPORT RAW DATA FRM VENDOR
/*	
	import excel using "`input_file'", clear first case(lower)
	
		describe
		
		keep phone
		ren  phone phonenumber
		
	* remove leading 00233
	
		replace phonenumber = substr(phonenumber, 6, .)
	
	* check that phone numbers are unique
	
		isid phonenumber
	
	* confirm validity of phone numbers (contains 9 chars) and follows expected format
		assert regexm(phonenumber, "[245][023456789][0-9]+") & length(phonenumber) == 9
*/	
********************************************************************************

* SORT LIST - Using Random Order

	gen phonenumber = runiform()
	
	gen order1 = runiform()
	gen order2 = runiform()
	
	isid order1 order2
	sort order1 order2
	gen index =_n
	
	keep phonenumber index

********************************************************************************

* GENERATE UNIQUE IDS

	* generate six digit number for each respondent
/*	
		gen rand1 = runiformint(10, 99)
		gen rand2 = runiformint(10, 99) 
		gen rand3 = runiformint(10, 99) 
		
		* check that ids are unique. Re-generate rand 3 until all osb have uids
		
		cap isid rand1 rand2 rand3 
		loc i 0
		loc rc `=_rc'
		
		while `rc' == 459 {
			loc ++i
			
			di "`i'"
			
			duplicates tag rand1 rand2 rand3, gen (dup)
			replace rand3 = runiformint(10, 99) if dup
			
			drop dup
			
			cap isid rand1 rand2 rand3 
			loc rc `=_rc'
		}
		
		
		gen id = "`prefix'" + string(rand1) + string(rand2) + string(rand3)
		drop rand*
		
		isid id
*/		

********************************************************************************

* GENERATE PHONE SURVEY VARIABLE:
	
		gen phone_survey = `phone_survey'

* RANDOMIZE IDENTIFIABLE VICTIM
	
	gen identifiable_victim = rbinomial(1, 0.5) if phone_survey
	
		tab identifiable_victim
    
* RANDOMIZE SMS GAMIFICATION

	gen gamification = rbinomial(1, 0.5)
	
		tab gamification
	
* RANDOMIZE CONSENT & END TYPE

	* Randomize within gamification
		
		bys gamification: gen cons_vers = runiformint(1, 4)
		bys gamification: gen end_type = runiformint(1, 2)
		
	* label cons_vers and end_type
		gen cons_vers_lab = cond(cons_vers 	== 1, "researchers", 				 ///
							cond(cons_vers 	== 2, "policymakers", 				 ///
							cond(cons_vers 	== 3, "policymakers + personalized", ///
												  "government")))
											 
		gen end_type_lab  =	cond(end_type 	== 1, "researchers", ///
												  "leaders")
												  
		tab cons_vers 		gamification
		tab cons_vers_lab 	gamification
		tab end_type	 	gamification
		tab end_type_lab 	gamification
		
	sort index
	order id phonenumber phone_survey identifiable_victim gamification cons_vers* end_type* index
	
********************************************************************************

* SAVE DATA

	describe

	save "`output_file'", replace
	
********************************************************************************

	cap log close
