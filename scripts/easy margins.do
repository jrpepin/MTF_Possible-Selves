
log using "MH_stat_output_only self-concept.log", replace

fre gdwk gdsp gdpa

global CVs i.sex i.race i.momed i.region

cap drop year_c
cap drop year_sq

summ year
gen year_c = year - `r(mean)'
gen year_sq = year_c^2 // same as year_c*year_c


******** WORKER (gdwk)

ologit   gdwk $CVs                                           [pweight = svyweight], or
ologit   gdwk $CVs year_c year_sq                            [pweight = svyweight], or
ologit   gdwk $CVs year_c year_sq i.religion i.famstru_d     [pweight = svyweight], or
ologit   gdwk $CVs year_c year_sq i.religion i.famstru_d c.selfconcept  [pweight = svyweight], or

	
******** SPOUSE (gdsp)
ologit   gdsp $CVs                                           [pweight = svyweight]
ologit   gdsp $CVs year_c year_sq                            [pweight = svyweight]
ologit   gdsp $CVs year_c year_sq i.religion i.famstru_d     [pweight = svyweight]
ologit   gdsp $CVs year_c year_sq i.religion i.famstru_d c.selfconcept  [pweight = svyweight]
	
******** PARENT (gdpa)
ologit   gdpa $CVs                                           [pweight = svyweight]
ologit   gdpa $CVs year_c year_sq                            [pweight = svyweight]
ologit   gdpa $CVs year_c year_sq i.religion i.famstru_d     [pweight = svyweight]
ologit   gdpa $CVs year_c year_sq i.religion i.famstru_d c.selfconcept_std  [pweight = svyweight]

log close





marginsplot, title("Predicted self-concept (1 - 5)") ///
subtitle("Good spouse") ytitle("Scale") xtitle(" ")










* Good Spouse - self-concept
regress selfconcept ib5.gdsp##c.year [aweight = svyweight]
margins i.gdsp, at(year=(1984(1)2022))

marginsplot, title("Predicted self-concept (1 - 5)") ///
subtitle("Good spouse") ytitle("Scale") xtitle(" ")

* Good Spouse - happy
regress happy_N ib5.gdsp##c.year [aweight = svyweight]
margins i.gdsp, at(year=(1984(1)2022))

marginsplot, title("Predicted happiness (1 - 3)") ///
subtitle("Good spouse") ytitle("Scale") xtitle(" ")

* Good Parent
regress selfconcept ib5.gdpa##c.year [aweight = svyweight]
margins i.gdpa, at(year=(1984(1)2022))

marginsplot, title("Predicted self-concept (1 - 5)") ///
subtitle("Good parent") ytitle("Scale") xtitle(" ")

* Good Worker
regress selfconcept ib5.gdwk##c.year [aweight = svyweight]
margins i.gdwk, at(year=(1984(1)2022))

marginsplot, title("Predicted self-concept (1 - 5)") ///
subtitle("Good worker") ytitle("Scale") xtitle(" ")