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