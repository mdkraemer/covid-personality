
use "H:\Data\soep.v36\pl.dta" ,clear

keep pid syear plh0212 plh0213 plh0214 plh0215 plh0216 plh0217 plh0218 plh0219 plh0220 plh0221 plh0222 plh0223 plh0224 plh0225 plh0226 plh0255

keep if inlist(syear,2017,2019)

mvdecode _all, mv(-5 -3 -2 -1)

* Openness
alpha plh0215 plh0220 plh0225 plh0255, d i g(open)
label variable open Openness
* Conscientiousness
recode plh0218 (7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0212 plh0218 plh0222, d i g(cons)
label variable cons Conscientiousness
* Extraversion
recode plh0223(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0213 plh0219 plh0223, d i g(extra)
label variable extra Extraversion
* Neuroticism
recode plh0225(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0216 plh0221 plh0226, d i g(neuro)
label variable neuro Neuroticism
* Agreeableness
recode plh0214(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0214 plh0217 plh0224, d i g(agree)
label variable agree Agreeableness

keep pid syear open cons extra neuro agree

saveold "H:\bf5-soep",replace


use "H:\Data\soep.is.2019\p.dta" ,clear

keep pid syear plh0212 plh0213 plh0214 plh0215 plh0216 plh0217 plh0218 plh0219 plh0220 plh0221 plh0222 plh0223 plh0224 plh0225 plh0226 plh0255

keep if inlist(syear,2017,2019)

mvdecode _all, mv(-5 -3 -2 -1)

* Openness
alpha plh0215 plh0220 plh0225 plh0255, d i g(open)
label variable open Openness
* Conscientiousness
recode plh0218 (7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0212 plh0218 plh0222, d i g(cons)
label variable cons Conscientiousness
* Extraversion
recode plh0223(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0213 plh0219 plh0223, d i g(extra)
label variable extra Extraversion
* Neuroticism
recode plh0225(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0216 plh0221 plh0226, d i g(neuro)
label variable neuro Neuroticism
* Agreeableness
recode plh0214(7=1) (6=2) (5=3) (4=4) (3=5) (2=6) (1=7)
alpha plh0214 plh0217 plh0224, d i g(agree)
label variable agree Agreeableness

keep pid syear open cons extra neuro agree

append using "H:\bf5-soep"

compress

saveold "H:\bf5-soep & soep-is",replace