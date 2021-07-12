set more off
clear
log close _all

use "C:\Users\dmwalla3\Desktop\KS disparities\prison release data for disparity analysis 042021.dta" , clear

*cleaning

bysort personID: gen index = _N

rename _all, lower

gen female = .
replace female = 0 if gender == 2
replace female = 1 if gender == 1

gen hisp = .
replace hisp = 0 if ethnicity == 2
replace hisp = 1 if ethnicity == 1

replace race_category = . if race_category == 40
tab race_category, gen(race)
lookfor race

rename race1 amindian
rename race2 asianpi
rename race3 black
rename race4 white

gen ageatadmin1 = .
gen age = .

sum ageadm
replace ageatadmin1 = ageadm-r(mean)
replace age = ageadm-r(min)

label var age "Age at Admission, 0=15"

gen sentlength = sentrange_newsent
label var sentlength "Sentence Length, in months"

keep if index == 1

egen miss = rowmiss(totscore female hisp age los race_category)

keep if miss == 0

**Interactions

*Example 2: Catagorical and Linear - Interaction between black and age

*continuous and catagorical

reg totscore black age 
estimates store m1

reg totscore black age i.black#c.age
estimates store m2

lrtest m2 m1

*to get that graphic again
predict hat
twoway (line hat age if black == 0, sort) (line hat age if black == 1, sort), legend(order(1 "Non-Black" 2 "Black") pos(3) col(1))

*continuous and continuous

gen los_int = los*age
browse los_int los age

reg totscore los age los_int
reg totscore los age c.los#c.age
reg totscore c.los##c.age

*Graphing the continuous-continuous interaction
*I'm going to use my prefered way of modeling interactions to do this

*because we have to graphic the interaction of levels of 1 of the continuous variables, we start with making some "above" and "below" macros

sum los
local sd=r(sd)
local mean=r(mean)

*Now do the regression
reg totscore los age c.los#c.age

*see the variables names for coding:
ereturn list 
matrix list e(b)

twoway (function _b[_cons]+_b[age]*x+_b[los]*`mean'+_b[c.los#c.age]*x*`mean', range(0 72)) /// pv of dv for age when z is at the mean (x is the x axis)
	   (function _b[_cons]+_b[age]*x+_b[los]*(`mean'+`sd')+_b[c.los#c.age]*x*(`mean'+`sd'), range(0 72)) /// the pv for when the interaction is 1 sd above the mean
	   (function _b[_cons]+_b[age]*x+_b[los]*(`mean'-`sd')+_b[c.los#c.age]*x*(`mean'-`sd'), range(0 72)), /// the pv for when the interaction is 1 sd below the mean
	   legend(order(1 "LOS at mean" 2 "LOS at 1 sd above" 3 "LOS at 1 sd below") pos(3) col(1)) // add a legend for the three lines and pos=position at 3 o'clock and 1 column legend
graph export "C:\Users\dmwalla3\Dropbox\CSG Consulting\los and age interaction session 4.tif", replace

*grabbing a pv for the interpretation example on slide 24
reg totscore los age c.los#c.age

sum los // generating variables representing the mean and sd of los because di commands don't play well with macros
gen sd=r(sd)
gen mean=r(mean)

*pv for young people: remember we want the constant, age, los and the interaction with the appropriate values
di _b[_cons]+ ///
	(_b[age]*0)+ /// age at 15 which is 0
	(_b[los]*mean)+ /// with los is at the mean
	(_b[c.los#c.age]*0*mean) // the interaction between age and los when the age is 0 and los is avg.
	
*pv for old people: remember we want the constant, age, los and the interaction with the appropriate values
di _b[_cons]+ ///
	(_b[age]*72)+ /// age at 15 which is 0
	(_b[los]*mean)+ /// with los is at the mean
	(_b[c.los#c.age]*72*mean) // the interaction between age and los when the age is 0 and los is avg.
	

**cleaning variables to play with:

gen newcrime = .
replace newcrime = 1 if admtype_output == 1
replace newcrime = 0 if admtype_output != 1
replace newcrime = . if admtype_output == 7
label var newcrime "Committed a new crime? No - Tech Vio."
label def yn 0 "No" 1 "Yes" 
label val newcrime yn

gen sped = .
replace sped = 1 if education_level == 21
replace sped = 0 if education_level != 21
replace sped = . if education_level == 99
label var sped "Received Special Education"
label val sped yn

gen hsdip = .
replace hsdip = 1 if education_level == 12 | education_level == 22
replace hsdip = 0 if education_level != 12 & education_level != 22
replace hsdip = . if education_level == 99
label var hsdip "Received HS Diploma/Graduated HS"
label val hsdip yn

gen lessthan9th = .
replace lessthan9th = 1 if education_level < 9
replace lessthan9th = 0 if education_level >= 9
replace lessthan9th = . if education_level == 99
label var lessthan9th "Less than a 9th grade education"
label val lessthan9th yn

*because we are short on linear variables...
*been diagnosed with a chronic, physical illness
gen pchronic = runiform() < 0.45
label var pchronic "Have a physcial chronic illness, like HBP, Arit., Asthma, Back Injury, etc.?"
label val pchronic yn

*been diagnosed with a chronic, mental illness
gen mchronic = runiform() < 0.15
label var mchronic "Been diagnosed significant mental health condition, like anx., dep, bipolar, OCD, etc?"
label val mchronic yn

set seed 888
drawnorm sf12 
label var sf12 "SF-12 Physical Health Scale; z-score units"

drawnorm msf12
label var msf12 "SF-12 Mental Health Scale; z-score units"

egen miss2 = rowmiss(totscore female hisp age los race_category newcrime sped hsdip lessthan9th *chronic *12)
keep if miss2 == 0




