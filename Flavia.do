/* Group number: 11 */
/* Group composition: Claudia Ambrosino, Flavia Grasso and Titouan Renault */

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "grasso") {
    global filepath "/Users/grasso/Documents/Bocconi ESS/2024-2025/Semester 2/20295 - Microeconometrics/Problem Set 2"
}

if ("`user'" == "user") {
    global filepath "C:/Users/user/Desktop/Microeconometrics/Problem set 2"
}

if ("`user'" == "C") {
    global filepath "/FILE/PATH/C/"
}


display "$filepath"

*Change working directory 
cd "$filepath"

global data "${filepath}/data"
display "$data"

global output "${filepath}/output"
display "$output"


/* Question X */
/* Comments... */


use "$data/pset_4.dta", clear

/* Exercise 1 */ 

/* Question a. 
Because we wish to estimate divorce rates, which are group averages (for each state), according to the weighting procedures summary, we should use analytic weights, which suggest to weight on stpop, the number of units behind those averages. We use analytic weights when working with group-mean observations to give more weight to the more precise observations (because the more measurements are used in the average the more precise it will be).
*/


/* Question b.
The article relies on the timing of the introduction of unilateral divorce laws to compare divorce rates in the two possible regimes. 

One of the assumptions of this analysis is that states with the previous divorce law and the ones that introduced unilateral divorce laws would both follow parallel trends in their divorce rates in the absence of the changes to the legislation. 

Create 2 different graphs to support this assumption: (i) the first graph should convey the same message as the one in Figure 1 of the original paper, comparing states that did not change their divorce laws during 1968 - 1988 (Friedberg's sample) and the ones that did; 
(ii) the second graph should perform the same description, but focusing on the simpler analysis we will perform in the next exercise: compare the states adopting the unilateral divorce law between 1969 and 1973 to the ones that introduced it in the year 2000, only reporting the time trend up to 1978 and including a vertical line between 1968 and 1969 (when the first reforms in our sample started). Do your results support the assumption of parallel trends?
*/

// First graph
use "$data/pset_4.dta", clear
gen reform_friedberg = (lfdivlaw >= 1968 & lfdivlaw <= 1988)

collapse (mean) div_rate [aweight=stpop], by(year reform_friedberg)

reshape wide div_rate, i(year) j(reform_friedberg)
gen diff = div_rate1 - div_rate0

twoway (line div_rate1 year,  lcolor(black) lpattern(solid)) ///
	(line div_rate0 year,  lcolor(gray) lpattern(solid)) ///
	(line diff year, lcolor(black) lpattern(dash)) ///
	, title("Divorce Rates in Reform and Control States") ///
	legend(label(1 "Reform states") label(2 "Control states (Friedberg)") label(3 "Difference in divorce rates: Reform - Control (Friedberg)") pos(1)) ///
	xtitle("Year") ///
	xlabel(1956(2)1998, angle(forty_five)) ///
	ytitle("Yearly Divorce Rates (per 1000 people)") ///
	xline(1968 1988, lcolor(gray) lpattern(dash)) ///
	xsize(12) ysize(8)

//Second graph
use "$data/pset_4.dta", clear

gen reform_early = .
replace reform_early = 1 if lfdivlaw >= 1969 & lfdivlaw <= 1973
replace reform_early = 0 if lfdivlaw == 2000
drop if missing(reform_early)

keep if year <= 1978 

collapse (mean) div_rate [aweight=stpop], by (year reform_early)

reshape wide div_rate, i(year) j(reform_early)
gen diff = div_rate1 - div_rate0

twoway (line div_rate1 year,  lcolor(black) lpattern(solid)) ///
	(line div_rate0 year,  lcolor(gray) lpattern(solid)) ///
	(line diff year, lcolor(black) lpattern(dash)) ///
	, title("Divorce Rates in Early Reform and Late Reform States") ///
	legend(label(1 "Early Reform states") label(2 "Late Reform states") label(3 "Difference in divorce rates: Early - Late") pos(1)) ///
	xtitle("Year") ///
	xlabel(1956(2)1979, angle(forty_five)) ///
	ytitle("Yearly Divorce Rates (per 1000 people)") ///
	xline(1969, lcolor(gray) lpattern(dash)) ///
	xsize(12) ysize(8)


/* Do your results support the assumption of parallel trends? 
seems ok - aussi dans le 2e dis que apres 1978 la diff a monte tabdis qu'avant ct assez stable
*/

/* Question c.
Let us now start an analysis of the effects of the introduction of unilateral divorce laws. As a first step, let us perform a 2-period difference-in-difference analysis using "long differences", focusing on the evolution of divorces between 1968 and 1978. Keeping only these 2 years in our sample, you should compare states adopting the unilateral divorce law between 1969 and 1973 to the ones that introduced it in the year 2000. On this restricted sample, you should create: (i) a variable UNILATERAL equal to 1 if a state introduced the unilateral divorce law during this period (as signaled by variable lfdivlaw); (ii) a variable POST equal to 1 if the year is 1978; and (iii) a variable POST UNILATERAL when both POST and UNILATERAL are equal to 1. 
*/

use "$data/pset_4.dta", clear

keep if year == 1968 | year == 1978
gen UNILATERAL = (lfdivlaw >= 1969 & lfdivlaw <= 1973)
gen POST = (year==1978)
gen POST_UNILATERAL = POST*UNILATERAL



/* Now estimate the following regressions: (i) a pooled OLS regression of the divorce rate per 1,000 people (div rate) on POST UNILATERAL and POST; 
(ii) a full Difference- in-Differences specification, including POST, UNILATERAL and POST UNILATERAL as regressors. Based on the graphs you created in section (a), could you say something about the difference in the coefficients from regressions (i) and (ii)? What is the effect of introducing unilateral divorce laws according to this analysis?
*/

*pooled OLS 
reg div_rate POST POST_UNILATERAL [aweight = stpop]

*diff-in-diff specification
reg div_rate POST UNILATERAL POST_UNILATERAL [aweight=stpop]


/* Question d. */

// POST = 1, UNILATERAL = 1
sum div_rate if UNILATERAL==1 & POST==1 [aweight=stpop]
scalar AVG_Y_1_1 = r(mean)
// POST = 0, UNILATERAL = 1
sum div_rate if UNILATERAL==1 & POST==0 [aweight=stpop]
scalar AVG_Y_1_0 = r(mean)
// POST = 1, UNILATERAL = 0
sum div_rate if UNILATERAL==0 & POST==1 [aweight=stpop]
scalar AVG_Y_0_1 = r(mean)
// POST = 0, UNILATERAL = 0
sum div_rate if UNILATERAL==0 & POST==0 [aweight=stpop]
scalar AVG_Y_0_0 = r(mean)



matrix TABLE_1 = J(3,3,.)
matrix colnames TABLE_1 = "UNILATERAL=1" "UNILATERAL=0" "Difference 2"
matrix rownames TABLE_1 = "POST=1" "POST=0" "Difference 1"

matrix TABLE_1[1,1] = AVG_Y_1_1   // POST = 1, UNILATERAL = 1
matrix TABLE_1[1,2] = AVG_Y_0_1   // POST = 1, UNILATERAL = 0
matrix TABLE_1[1,3] = AVG_Y_1_1 - AVG_Y_0_1

matrix TABLE_1[2,1] = AVG_Y_1_0   // POST = 0, UNILATERAL = 1
matrix TABLE_1[2,2] = AVG_Y_0_0   // POST = 0, UNILATERAL = 0
matrix TABLE_1[2,3] = AVG_Y_1_0 - AVG_Y_0_0

matrix TABLE_1[3,1] = AVG_Y_1_1 - AVG_Y_1_0
matrix TABLE_1[3,2] = AVG_Y_0_1 - AVG_Y_0_0
matrix TABLE_1[3,3] = (AVG_Y_1_1 - AVG_Y_1_0) - (AVG_Y_0_1 - AVG_Y_0_0)

matrix list TABLE_1

putexcel set "$output/TABLE_1.xlsx", sheet("DiD Matrix") replace
putexcel A2=matrix(TABLE_1), names nformat(number_d2) 





/* Question e. */

use "$data/pset_4.dta", clear

keep if year >= 1956 & year <= 1988
gen IMP_UNILATERAL = (lfdivlaw <= year)
encode st, gen(st_id)

* Regression (i)
reg div_rate IMP_UNILATERAL i.st_id i.year [aweight = stpop]


gen t = year - 1956
gen t2 = t^2

xtset st_id year

* Regression (ii) 
reg div_rate IMP_UNILATERAL i.year c.t##i.st_id [aweight=stpop]

* Regression (iii)
reg div_rate IMP_UNILATERAL i.year c.t##i.st_id c.t2##i.st_id [aweight=stpop]






