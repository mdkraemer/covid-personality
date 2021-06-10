global soep "\\hume\rdc-prod\distribution\soep-core\soep.v36\Stata"
global soepis "\\hume\rdc-prod\distribution\soep-is\doi10.5684\soep.is.2019\additional_and_unzipped_files\soep-is.2019_stata_en"
global data "S:\MA\mkraemer\Paper_Covid"

use "$soep\pl.dta" ,clear

keep pid syear plh0212 plh0213 plh0214 plh0215 plh0216 plh0217 plh0218 plh0219 plh0220 plh0221 plh0222 plh0223 plh0224 plh0225 plh0226 plh0255

keep if inlist(syear, 2005, 2009, 2012, 2013, 2017, 2019) // all years with Big Five assessments

mvdecode _all, mv(-5 -3 -2 -1)

* Openness
*alpha plh0215 plh0220 plh0225 plh0255, d i g(open) // rather do this in R
*label variable open Openness
rename plh0215 open1
rename plh0220 open2
rename plh0225 open3
rename plh0255 open4

* Conscientiousness
*recode plh0218 (7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0212 plh0218 plh0222, d i g(cons)
*label variable cons Conscientiousness
rename plh0212 cons1
rename plh0218 cons2
rename plh0222 cons3

* Extraversion
*recode plh0223(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0213 plh0219 plh0223, d i g(extra)
*label variable extra Extraversion
rename plh0213 extra1
rename plh0219 extra2
rename plh0223 extra3

* Neuroticism
*recode plh0225(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0216 plh0221 plh0226, d i g(neuro)
*label variable neuro Neuroticism
rename plh0216 neuro1
rename plh0221 neuro2
rename plh0226 neuro3

* Agreeableness
*recode plh0214(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0214 plh0217 plh0224, d i g(agree)
*label variable agree Agreeableness
rename plh0214 agree1
rename plh0217 agree2
rename plh0224 agree3

keep pid syear open* cons* extra* neuro* agree*

keep if (open1!=. | open2!=. | open3!=. | open4!=. | /// anyone, who has ever answered at least one Big Five item
         cons1!=. | cons2!=. | cons3!=. | ///
		 extra1!=. | extra2!=. | extra3!=. | ///
		 neuro1!=. | neuro2!=. | neuro3!=. | ///
		 agree1!=. | agree2!=. | agree3!=.)

gen core=1

saveold "$data\bf5-soep.dta", replace


use "$soepis\p.dta" ,clear

keep pid syear plh0212 plh0213 plh0214 plh0215 plh0216 plh0217 plh0218 plh0219 plh0220 plh0221 plh0222 plh0223 plh0224 plh0225 plh0226 plh0255

keep if inlist(syear, 2005, 2009, 2013, 2015, 2017, 2019)

mvdecode _all, mv(-5 -3 -2 -1)

* Openness
*alpha plh0215 plh0220 plh0225 plh0255, d i g(open) // rather do this in R
*label variable open Openness
rename plh0215 open1
rename plh0220 open2
rename plh0225 open3
rename plh0255 open4

* Conscientiousness
*recode plh0218 (7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0212 plh0218 plh0222, d i g(cons)
*label variable cons Conscientiousness
rename plh0212 cons1
rename plh0218 cons2
rename plh0222 cons3

* Extraversion
*recode plh0223(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0213 plh0219 plh0223, d i g(extra)
*label variable extra Extraversion
rename plh0213 extra1
rename plh0219 extra2
rename plh0223 extra3

* Neuroticism
*recode plh0225(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0216 plh0221 plh0226, d i g(neuro)
*label variable neuro Neuroticism
rename plh0216 neuro1
rename plh0221 neuro2
rename plh0226 neuro3

* Agreeableness
*recode plh0214(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
*alpha plh0214 plh0217 plh0224, d i g(agree)
*label variable agree Agreeableness
rename plh0214 agree1
rename plh0217 agree2
rename plh0224 agree3

keep pid syear open* cons* extra* neuro* agree*

keep if (open1!=. | open2!=. | open3!=. | open4!=. | /// anyone, who has ever answered at least one Big Five item
         cons1!=. | cons2!=. | cons3!=. | ///
		 extra1!=. | extra2!=. | extra3!=. | ///
		 neuro1!=. | neuro2!=. | neuro3!=. | ///
		 agree1!=. | agree2!=. | agree3!=.)

gen core=0

append using "$data\bf5-soep.dta"

compress

saveold "$data\bf5-soep-core-is.dta", replace
