**************************
* The Role of Luck in Educational Achievement | Analysis
* Last updated: 2026-10-21
**************************

* PART 1: ESS — Descriptive Analysis

// Set the Working Directory to the Location of the .dta File
cd "C:\"
use "ESS8e02_3-ESS9e03_2-ESS10-ESS11.dta", clear

// Keep Respondents with Nonmissing Education Information
keep edulvlb edulvlfb edulvlmb yrbrn agea
keep if edulvlb < 1000 & edulvlfb < 1000 & edulvlmb < 1000

// Construct Educational Attainment Indicators
gen edu1 = (edulvlb >= 100)
gen edu1father = (edulvlfb >= 100)
gen edu1mother = (edulvlmb >= 100)
gen edu1parents = edu1father | edu1mother
gen edu2 = (edulvlb >= 300)
gen edu2father = (edulvlfb >= 300)
gen edu2mother = (edulvlmb >= 300)
gen edu2parents = edu2father | edu2mother
gen edu3 = (edulvlb >= 600)
gen edu3father = (edulvlfb >= 600)
gen edu3mother = (edulvlmb >= 600)
gen edu3parents = edu3father | edu3mother
gen phd = (edulvlb >= 800)
gen phdfather = (edulvlfb >= 800)
gen phdmother = (edulvlmb >= 800)
gen phdparents = phdfather | phdmother

gen edu = edu1 + edu2 + edu3 + phd
gen eduparents = edu1parents + edu2parents + edu3parents + phdmother + phdfather

// Calculate PhD Share (Main Text)
tab phd if yrbrn >= 1980 & yrbrn <= 1990 & agea >= 30

// Reproduce Table 1
tab phd eduparents if yrbrn >= 1980 & yrbrn <= 1990 & agea >= 30, col

// Calculate First-Generation Share (Main Text)
tab eduparents if edu3 & yrbrn >= 1980 & yrbrn <= 1990 & agea >= 30
tab eduparents if phd & yrbrn >= 1980 & yrbrn <= 1990 & agea >= 30


* PART 2: ESS — Prediction Analysis

* -------------------------------------------------------------
* STEP 1: Load ESS Data and Define the Analysis Sample and PhD Outcome
* -------------------------------------------------------------

clear all
// Set the Working Directory to the Location of the .dta File
cd "C:\"
use "ESS8e02_3-ESS9e03_2-ESS10-ESS11.dta", clear

// Restrict Sample to Birth Years 1980–1990 and Age 30 or Older
keep if yrbrn >= 1980 & yrbrn <= 1990 & agea >= 30

// Keep Respondents with Valid Education Codes
keep if edulvlb < 1000

// Define Binary PhD Outcome
gen phd = (edulvlb >= 800)

* -------------------------------------------------------------
* STEP 2: Create Stratified Training (80%) and Holdout (20%) Samples
* -------------------------------------------------------------

set seed 1234567
gen double u = runiform()  // Generate Random Values for Shuffling
bys phd: egen ru = rank(u)
by phd: replace ru = ru / _N
gen byte holdout = ru < 0.2  // 20% Holdout Sample
gen byte train = ru >= 0.2   // 80% Training Sample
drop ru u

/*
// Alternative Holdout: Select Two Birth Years (Expected to Reduce Predictive Performance)
gen byte holdout = (yrbrn == 1983 | yrbrn == 1987)  // 18% Holdout Sample
gen byte train = holdout == 0   // 82% Training Sample
*/

* -------------------------------------------------------------
* STEP 3: Prepare Predictors, Exclude Selected Variables, and Code Missing Values
* -------------------------------------------------------------

drop edulvlb eduyrs eisced edlvegr edlvdhu edlvdit edlvgpl edupl2 edlvdrs edlvdme edlvdmk edlvpdmk edctn edctnp edulvlpb edlveat edlvebe edlvebg edlvdch edlvgcy edlvdcz edlvdfi edlvdis edlvddk edlvdee edlvges edlvies edlvdfr edlvehr edlvdahu ///
edlvdie edubil1 edlveit edlvfit edlvdlt edlvdlv edlvenl edlvdno edlveno edlvepl edlvipl edlvdpt edlvept edlvdru edlvdse edlvesi edlvdsk edubde1 edudde1 eduade2 educde2 eduade3 eduagb2 edubgb2 eduail2 edubgb1 educgb1 ///
edagepgb edlvpeat edlvpfat edlvpebe edlvpdch edlvpgcy edlvpdcz edlvpebg edlvpdfi edlvpdis edlvpdit edlvpeit edlvpfit edlvpddk edlvpdee edlvpfes edlvphes edlvpdfr edlvpegr edlvpehr edlvpdhu edlvpdah edlvpdie edupbil1 edlvpdlt edlvpdlv ///
edlvpenl edlvpdno edlvpeno edlvpepl edlvpfpl edlvphpl edlvpdpt edlvpept edlvpdru edlvpdse edlvpesi edlvpdsk edupbde1 edupdde1 edupade2 edupcde2 edupade3 edupagb2 edupbgb2 edupail2 eduppl2 edupbgb1 edupcgb1 edlvpdrs edlvpdme ///
hincfel hinctnta hincsrca isco08 isco08p estsz eiscedp psu stratum dweight pspwght pweight anweight
ds, has(type numeric)
local allvars `r(varlist)'  // List Numeric Variables
local predictors ""

* Exclude Predictors That Are Entirely Missing in the Training Sample
foreach var of local allvars {
    if "`var'" != "phd" {
        quietly count if !missing(`var') & train == 1
        if r(N) > 0 {
            // quietly summarize `var' if train == 1, detail
            // local med = r(p50)
            quietly replace `var' = 999 if missing(`var')
			local predictors `predictors' `var'  // Add Variable to Predictor List
        }
    }
}

* Inspect the Predictor List
display "`predictors'"

* -------------------------------------------------------------
* STEP 4: Fit Baseline Logistic Regression Using Parental Education and Occupation
* -------------------------------------------------------------

gen edu1father = (edulvlfb >= 100)
gen edu1mother = (edulvlmb >= 100)
gen edu2father = (edulvlfb >= 300)
gen edu2mother = (edulvlmb >= 300)
gen edu3father = (edulvlfb >= 600)
gen edu3mother = (edulvlmb >= 600)
gen phdfather = (edulvlfb >= 800)
gen phdmother = (edulvlmb >= 800)

gen edufather = edu1father + edu2father + edu3father + phdfather
gen edumother = edu1mother + edu2mother + edu3mother + phdmother
replace edufather = -1 if edulvlfb >= 1000
replace edumother = -1 if edulvlmb >= 1000

xi: logit phd i.edufather i.edumother i.occm14b i.occf14b if train
predict dp, pr

roctab phd dp if holdout

egen d_TP = total((dp > .06) & phd & train)
egen d_FP = total((dp > .06) & (phd == 0) & train)
egen d_TN = total((dp <= .06) & (phd == 0) & train)
egen d_FN = total((dp <= .06) & phd & train)

gen d_prec = d_TP / (d_TP + d_FP)
gen d_recall = d_TP / (d_TP + d_FN)
gen d_f1 = 2 * (d_prec * d_recall) / (d_prec + d_recall)
sum dp d_TP d_FP d_TN d_FN d_prec d_recall d_f1

drop d_TP d_FP d_TN d_FN d_prec d_recall d_f1

egen d_TP = total((dp > .06) & phd & holdout)
egen d_FP = total((dp > .06) & (phd == 0) & holdout)
egen d_TN = total((dp <= .06) & (phd == 0) & holdout)
egen d_FN = total((dp <= .06) & phd & holdout)

gen d_prec = d_TP / (d_TP + d_FP)
gen d_recall = d_TP / (d_TP + d_FN)
gen d_f1 = 2 * (d_prec * d_recall) / (d_prec + d_recall)
sum dp d_TP d_FP d_TN d_FN d_prec d_recall d_f1

* -------------------------------------------------------------
* STEP 5: Fit Random Forest Using Parental Education and Occupation
* -------------------------------------------------------------

rforest phd edulvlmb edulvlfb occm14b occf14b if train, type(class) iter(500) numvars(1) lsize(100)
display "`e(OOB_Error)'"

predict rrp rrq, pr

roctab phd rrq if holdout

egen rrf_TP = total((rrq > .1) & phd & train)
egen rrf_FP = total((rrq > .1) & (phd == 0) & train)
egen rrf_TN = total((rrq <= .1) & (phd == 0) & train)
egen rrf_FN = total((rrq <= .1) & phd & train)

gen rrf_prec = rrf_TP / (rrf_TP + rrf_FP)
gen rrf_recall = rrf_TP / (rrf_TP + rrf_FN)
gen rrf_f1 = 2 * (rrf_prec * rrf_recall) / (rrf_prec + rrf_recall)
sum rrq rrf_TP rrf_FP rrf_TN rrf_FN rrf_prec rrf_recall rrf_f1

drop rrf_TP rrf_FP rrf_TN rrf_FN rrf_prec rrf_recall rrf_f1

egen rrf_TP = total((rrq > .1) & phd & holdout)
egen rrf_FP = total((rrq > .1) & (phd == 0) & holdout)
egen rrf_TN = total((rrq <= .1) & (phd == 0) & holdout)
egen rrf_FN = total((rrq <= .1) & phd & holdout)

gen rrf_prec = rrf_TP / (rrf_TP + rrf_FP)
gen rrf_recall = rrf_TP / (rrf_TP + rrf_FN)
gen rrf_f1 = 2 * (rrf_prec * rrf_recall) / (rrf_prec + rrf_recall)
sum rrq rrf_TP rrf_FP rrf_TN rrf_FN rrf_prec rrf_recall rrf_f1


* -------------------------------------------------------------
* STEP 6: Fit Random Forest Using the Full Predictor Set
* -------------------------------------------------------------

rforest phd `predictors' if train, type(class) iter(500) numvars(30) lsize(30)
display "`e(OOB_Error)'"

predict rp rq, pr

roctab phd rq if holdout

egen rf_TP = total((rq > .065) & phd & train)
egen rf_FP = total((rq > .065) & (phd == 0) & train)
egen rf_TN = total((rq <= .065) & (phd == 0) & train)
egen rf_FN = total((rq <= .065) & phd & train)

gen rf_prec = rf_TP / (rf_TP + rf_FP)
gen rf_recall = rf_TP / (rf_TP + rf_FN)
gen rf_f1 = 2 * (rf_prec * rf_recall) / (rf_prec + rf_recall)
sum rq rf_TP rf_FP rf_TN rf_FN rf_prec rf_recall rf_f1

drop rf_TP rf_FP rf_TN rf_FN rf_prec rf_recall rf_f1

egen rf_TP = total((rq > .065) & phd & holdout)
egen rf_FP = total((rq > .065) & (phd == 0) & holdout)
egen rf_TN = total((rq <= .065) & (phd == 0) & holdout)
egen rf_FN = total((rq <= .065) & phd & holdout)

gen rf_prec = rf_TP / (rf_TP + rf_FP)
gen rf_recall = rf_TP / (rf_TP + rf_FN)
gen rf_f1 = 2 * (rf_prec * rf_recall) / (rf_prec + rf_recall)
sum rq rf_TP rf_FP rf_TN rf_FN rf_prec rf_recall rf_f1


* PART 3: Interview Analysis

**************************
* STEP 1: Prepare Interview Data and Figures
* Prepare Interview Data and Recode First- and Second-Generation Status
* Output: Figure 2
**************************
clear
graph drop _all
use "luck_definitions_102326"

recode luck1 luck2 luck3 luck4 luck5 luck6 luck7 luck8 luck9 luck10 luck11 luck12 luck13 luck14 luck15 (. = 0)
tab1 luck1 luck2 luck3 luck4 luck5 luck6 luck7 luck8 luck9 luck10 luck11 luck12 luck13 luck14 luck15 if idluck==1
* Note: luck13 does not exist.

* Flag Professors for Exclusion
gen prof = 0
recode prof (0 =1 )  if id ==59 | id ==42 |  id == 33

* Define First- and Second-Generation Status, Including Category 1.5
recode firstgen (0 =3) (3=2)
label define firstgenl 1 "1st generation" 2 "1.5 Gen" 3 "2nd generation"
label values firstgen firstgenl
tab firstgen if idluck==1 & prof==0

*After inspecting again the interviews first coder recodes 8 cases 1.5 gen into 1st or 2nd generation
* 2A Uncle professor at uni, but maybe first generation given low education of parents and grandparents
*First generation
*5A second generation
*Second generation
*many uncles studied one Germany, one in UK, subject studied in the Swiss school in Madrid, very posh school, very affluent grandmothers
*6A probably second generation
*Second generation
*2T clearly second generation
*Second generation
*10J both started studying at the university but not completed
*First generation
*20A second generation, father attended higher education
*Second generation
*22A uncles professors of economics but parents no university education
*First generation (could be second)
*18J second generation
*Second generation

recode firstgen (2 = 1) if id==2 | id==43 | id==66
recode firstgen (2 = 3) if id==5 | id==6 | id==64 | id==68 | id==9
tab firstgen if idluck==1 & prof==0
gen firstgenre=firstgen
label define firstgenlre 1 "1st generation" 3 "2nd generation"
label values firstgenre firstgenlre
* reported in section 4.1
tab firstgenre
keep if idluck == 1 & prof == 0
drop idluck prof

*** Not reported in paper: More finegrained definitions
gen lucksequence = string(luck1) + string(luck2) + string(luck3) + string(luck4) + string(luck5) + string(luck6) + string(luck7) + string(luck8) + string(luck9) + string(luck10) + string(luck11) + string(luck12) + string(luck14) + string(luck15)
egen totalluck = rowtotal(luck1-luck12 luck14-luck15)
expand totalluck
bys id: gen idlucktype = _n
gen lucktype = strpos(subinstr(lucksequence, "1", "0", idlucktype-1), "1")
label define lucktypelabels 1 "Lack of control" 2 "Coincidence of events" 3 "Luck does not exist" 4 "Lottery of birth" ///
           5 "Randomness" 6 "Low probability" 7 "Right place, right time" 8 "DNK" ///
           9 "Other" 10 "Unexpected event" 11 "Unlikely event" 12 "Counterfactual path" ///
		   13 "Ignorance of causes" 14 "No merit/effort"
label values lucktype lucktypelabels
bys lucktype: gen sortcount = _N
bys lucktype: gen lucktypefirstcase = _n == 1
egen sortorder = rank(sortcount) if lucktypefirstcase, unique
replace sortorder = sortorder[_n-1] if sortorder == .
generate freq=1

graph hbar (percent) freq, ///
    over(lucktype, sort(sortorder) descending label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%") ///
	ytitle("") ///

graph hbar (percent) freq if firstgenre == 1, ///
    over(lucktype, sort(sortorder) descending label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%") ///
	yscale(range(0 25)) ///
	ytitle("") ///
	title("First generation", margin(b=5)) ///

graph hbar (percent) freq if firstgenre == 3, ///
    over(lucktype, sort(sortorder) descending label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%") ///
	yscale(range(0 25)) ///
	ytitle("") ///
	title("Second generation", margin(b=5)) ///

label drop lucktypelabels
keep if idlucktype == 1
drop sortcount sortorder freq lucktype lucktypefirstcase idlucktype totalluck lucksequence

* Reduce definitions of luck to 6 categories	
gen lottery=luck4
gen lackcontrol=0
recode lackcontrol (0=1) if luck1==1 | luck15==1 | luck5==1	
gen coincidence=0
recode coincidence (0=1) if luck2==1 
gen lowprob=0
recode lowprob (0=1) if luck6==1| luck10==1 | luck11==1
gen counterfact=0
recode counterfact (0=1) if luck12==1| luck7==1
gen other=0
recode other (0=1) if luck14==1 | luck9==1 | luck3==1 | luck8==1

gen lucksequence = string(lackcontrol) + string(lottery) + string(counterfact) + string(lowprob) + string(other) + string(coincidence)
egen totalluck = rowtotal(lackcontrol lottery counterfact lowprob other coincidence)
expand totalluck
bys id: gen idlucktype = _n
gen lucktype = strpos(subinstr(lucksequence, "1", "0", idlucktype-1), "1")
label define lucktypelabels 1 "Lack of control" 2 "Lottery of birth" 3 "Counterfactual outcomes" ///
           4 "Low probability" 5 "Other" 6 "Coincidence of events"
label values lucktype lucktypelabels

* Definitions, 1st and 2nd gen combined
graph hbar (percent), ///
    over(lucktype, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%") ///
	yscale(range(0 48)) ///
	ytitle("") ///

* Figure 3, left panel: 1st gen
graph hbar (percent) if firstgenre == 1, ///
    over(lucktype, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%") ///
	yscale(range(0 48)) ///
	ytitle("") ///
	title("First generation", margin(b=5)) ///
	name(Figure_3a)
graph export 3a.png, replace

* Figure 3, right panel: 2nd gen
graph hbar (percent) if firstgenre == 3, ///
    over(lucktype, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
	ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%") ///
	yscale(range(0 48)) ///
	ytitle("") ///
	title("Second generation", margin(b=5)) ///
	name(Figure_3b)
graph export 3b.png, replace



**************************
* STEP 2: Analyze Counterfactual Coding and Agreement
* Independent and agreement coding of counterfactual 
* Calculate Cohen's Kappa for Independent Counterfactual Coding
* Outputs: Figures 3 and A3
**************************

clear
graph drop _all
use "luck_events_102326"


********************
* Assign Labels to Chance-Event Types
***********************


label define typelab ///
0 "birth lottery" ///
1 "administrative threshold" ///
2 "serendipitous encounter" ///
3 "erratic luck" ///
4 "institutional lottery" ///
6 "near miss" ///
7 "health accident"


label values type typelab

tab type




**************************
* Create Recoded Counterfactual Outcomes While Preserving Original Coder Variables
**************************
tab firstcoder
gen firstcoderrec=firstcoder
tab secondcoder 
gen secondcoderrec=secondcoder
recode secondcoderrec (0=.)
tab secondcoderrec
replace firstcoderrec=. if firstcoder> 3 

**************************
* Flag Professors for Exclusion from the Analysis
**************************
gen prof = 0
recode prof (0 =1 )  if id ==59 | id ==42 |  id == 33
tab firstcoderrec if prof==0
tab2 secondcoderrec firstcoderrec if prof==0, col
* Show Coding Disagreement Before Correcting Clear Errors and Typos

* Recode First- and Second-Generation Status
* Define First- and Second-Generation Status, Including Category 1.5
recode firstgen (0 =3) (3=2)
label define firstgenl 1 "1st generation" 2 "1.5 gen" 3 "2nd generation"
label values firstgen firstgenl
recode firstgen (2 = 1) if id==2 | id ==43  | id== 66
recode firstgen (2 = 3) if id==5 | id ==6  | id==64 | id==68 | id==9
tab firstgen if idluck==1 & prof==0
gen firstgenre=firstgen
label define firstgenlre 1 "1st generation" 3 "2nd generation"
label values firstgenre firstgenlre
tab firstgenre

**************************
* Compare Counterfactual Coding by Generation and Coder
**************************

kap secondcoderrec firstcoderrec if prof==0 & firstcoderrec<4 & secondcoderrec>0
tab2 secondcoderrec firstcoderrec if prof==0, row
tab2 secondcoder firstcoder if prof==0, row

* Identify Coding Discrepancies
list id idluck if secondcoder==3 & firstcoder==1 | secondcoder==1 & firstcoder==3
list id idluck if secondcoder==1 & firstcoder==2 | secondcoder==2 & firstcoder==1
list id idluck if secondcoder==3 & firstcoder==2 | secondcoder==2 & firstcoder==3

**************************
* Correct clear case of mistakes and typos 
**************************

tab1 firstcoderrec secondcoderrec
gen firstcoderrecNew=firstcoderrec
gen secondcoderrecNew=secondcoderrec

label define counterfactural 1 "very different"  2 "different" 3 "similar"
label values firstcoderrecNew counterfactural
label values secondcoderrecNew counterfactural

recode firstcoderrecNew (3 =1) if id==43 & idluck==1 
recode firstcoderrecNew (2 =1) if id==1 & idluck==2 | id==54 & idluck==1
recode firstcoderrecNew (1 =2) if id==59 & idluck==3 
recode firstcoderrecNew (3 =2) if id==32 & idluck==1 | id==45 & idluck==1 | id==45 & idluck==2
recode secondcoderrecNew (2=1) if id==35 & idluck==2 | id==57 & idluck==2
recode secondcoderrecNew (2=3) if id==65 & idluck==3 | id==62 & idluck==1 | id==56 & idluck==2 | id==39 & idluck==4 | id==57 & idluck==1

kap secondcoderrecNew firstcoderrecNew if prof==0 
spearman secondcoderrecNew firstcoderrecNew if prof==0 

*******************************
* Apply Corrections Following Agreement Coding
*******************************

gen firstcoderrecNew2=firstcoderrecNew
gen secondcoderrecNew2=secondcoderrecNew
label values firstcoderrecNew2 counterfactural
label values secondcoderrecNew2 counterfactural

recode firstcoderrecNew2 (2 =3) if id==11 & idluck==2 | id==34 & idluck==2 | id==12 & idluck==2 | id==12 & idluck==3 | id==23 & idluck==1 
recode firstcoderrecNew2 (1 =2) if id==24 & idluck==1
recode secondcoderrecNew2 (2=3) if id==1 & idluck==3 | id==1 & idluck==4 | id==2 & idluck==2 | id==7 & idluck==3 | id==11 & idluck==2  
recode secondcoderrecNew2 (2=3) if id==15 & idluck==1 | id==16 & idluck==1 | id==22 & idluck==1 | id==23 & idluck==1 
recode secondcoderrecNew2 (2=3) if id==26 & idluck==2 | id==30 & idluck==3 | id==30 & idluck==4  | id==31 & idluck==1  |id==47 & idluck==1 
recode secondcoderrecNew2 (2=3) if id==51 & idluck==1  | id==63 & idluck==1 | id==65 & idluck==3 | id==69 & idluck==2 | id==69 & idluck==1
recode secondcoderrecNew2 (.=3) if id==29 & idluck==3 
recode secondcoderrecNew2 (.=2) if id==69 & idluck==1 | id==69 & idluck==2 
recode secondcoderrecNew2	 (2=1) if id==30 & idluck==1 | id==43 & idluck==3 | id==47 & idluck==4
recode secondcoderrecNew2 (2=1) if id==7 & idluck==2

kap secondcoderrecNew2 firstcoderrecNew2 if prof==0 
spearman secondcoderrecNew2 firstcoderrecNew2 if prof==0 

* Identify Two Remaining Discrepant Cases for Exclusion from Figures
gen sel = 1 if secondcoderrecNew2 != firstcoderrecNew2 & firstcoderrecNew2>0 & firstcoderrecNew2<4 & prof==0
tab2 secondcoderrecNew2 firstcoderrecNew2 if prof==0 , row



***********************************************
* Exclude non phd interviewees with prof==0, 
* missing on firstcoderrecNew2
* and the remaining discrepant coding (n=2) with sel!=1 
* Analyze Counterfactual Outcomes by Chance-Event Type
*************************************************	

* Figures not in paper: Counterfactual outcomes by type of event
graph hbar (percent) if prof==0 & sel!=1 & type == 1, ///
    over(firstcoderrecNew2, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("Administrative threshold", margin(b=5))

graph hbar (percent) if prof==0 & sel!=1 & type == 2, ///
    over(firstcoderrecNew2, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("Serendipitous encounter", margin(b=5)) ///


***********************************************
* Exclude non phd interviewees with prof==0, 
* missing on firstcoderrecNew2
* and the remaining discrepant coding (n=2) with sel!=1 
* Analyze Counterfactual Outcomes by Generation
*************************************************	

* Figure 1: Counterfactual outcomes by 1st vs. 2nd gen
graph hbar (percent) if prof==0 & sel!=1 & firstgenre == 1, ///
    over(firstcoderrecNew2, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("First generation", margin(b=5)) ///
	name(Figure_1a)
graph export 1a.png, replace

graph hbar (percent) if prof==0 & sel!=1 & firstgenre == 3, ///
    over(firstcoderrecNew2, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("Second generation", margin(b=5)) ///
	name(Figure_1b)
graph export 1b.png, replace
	
* Figure 2: Counterfactual outcomes by 1st vs. 2nd gen, by 1st and 2nd author coding
graph hbar (percent) if prof==0 & firstgenre == 1, ///
    over(firstcoderrecNew, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%") ///
	yscale(range(0 64)) ///
	ytitle("") ///
	title("First generation, first author", margin(b=5)) ///
	name(Figure_2a1)
graph export 2a1.png, replace

graph hbar (percent) if prof==0 & firstgenre == 3, ///
    over(firstcoderrecNew, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%") ///
	yscale(range(0 64)) ///
	ytitle("") ///
	title("Second generation, first author", margin(b=5)) ///
	name(Figure_2a2)
graph export 2a2.png, replace

graph hbar (percent) if prof==0 & firstcoderrec !=. & firstgenre == 1, ///
    over(secondcoderrecNew, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%") ///
	yscale(range(0 64)) ///
	ytitle("") ///
	title("First generation, second author", margin(b=5)) ///
	name(Figure_2b1)
graph export 2b1.png, replace

graph hbar (percent) if prof==0 & firstcoderrec !=. & firstgenre == 3, ///
    over(secondcoderrecNew, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%") ///
	yscale(range(0 64)) ///
	ytitle("") ///
	title("Second generation, second author", margin(b=5)) ///
	name(Figure_2b2)
graph export 2b2.png, replace

* Figure 4: Event type by 1st vs. 2nd gen
graph hbar (percent) if prof==0 & sel!=1 & firstcoderrecNew2 != . & (type == 1 | type == 2) & firstgenre == 1, ///
    over(type, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%" 70 "70%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("First generation", margin(b=5)) ///
	name(Figure_4a)
graph export 4a.png, replace
	
graph hbar (percent) if prof==0 & sel!=1 & firstcoderrecNew2 != . & (type == 1 | type == 2) & firstgenre == 3, ///
    over(type, label(angle(0) labsize(medium))) ///
    bar(1, color(gs12) lcolor(black) lwidth(medthin)) ///
    bargap(25) ///
    graphregion(color(white)) ///
    plotregion(margin(zero)) ///
    legend(off) ///
    ylabel(0 "0%" 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%" 70 "70%") ///
	yscale(range(0 51)) ///
	ytitle("") ///
	title("Second generation", margin(b=5)) ///
	name(Figure_4b)
graph export 4b.png, replace