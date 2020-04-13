clear all
use "M:\ECON4150\seminar_econometric.dta"
*use "S:\F2826\Grupper\Gruppe15\Victoria\seminar_econometric.dta"
log using "M:\ECON4150\kobling_12", replace //Logfil oppdateres hver gang//
*log using "S:\F2826\Grupper\Gruppe15\Victoria\kobling_9_ECON4150", replace //Logfil oppdateres hver gang//
describe
*sum if female==1
*estout
*sum if female==0
*estout
*esttab 

histogram weekpay
*translate mygraph.gph mygraph.eps
histogram wage4
*translate mygraph.gph mygraph.eps

tab state, gen(statex)
tab educ, gen(educx)
tab occ_m03, gen(occ_m03x)
tab wbho, gen(wbhox)
tab ownchild, gen(ownchildx)

*Race dummi
label var wbhox1 "White"
label var wbhox2 "Black"
label var wbhox3 "Hispanic"
label var wbhox4 "Other"
*Child dummi
label var ownchildx1 "0 own child"
label var ownchildx2 "1 own child"
label var ownchildx3 "2 own child"
label var ownchildx4 "3 own child"
label var ownchildx5 "4 own child"
label var ownchildx6 "5 own child"
label var ownchildx7 "6 own child"
label var ownchildx8 "7 own child"
label var ownchildx9 "8 own child"
label var ownchildx10 "9 own child"
label var ownchildx11 "10 own child"
*State dummi

label var statex1 "Maine"
label var statex2 "New Hampshire"
label var statex3 "Vermont"
label var statex4 "Massachusets"
label var statex5 "Rhode Island"
label var statex6 "Connecticut"
label var statex7 "New York"
label var statex8 "New Jersey"
label var statex9 "Pennsylvania"
label var statex10 "Ohio"
label var statex11 "Indiana"
label var statex12 "llinois"
label var statex13 "Michigan"
label var statex14 "Wisconsin"
label var statex15 "Minnesota"
label var statex16 "Iowa"
label var statex17 "Missouri"
label var statex18 "North Dakota"
label var statex19 "South Dakota"
label var statex20 "Nebraska"
label var statex21 "Kansas"
label var statex22 "Delaware"
label var statex23 "Maryland"
label var statex24 "District of Columbia"
label var statex25 "Virginia"
label var statex26 "West Virginia"
label var statex27 "North California"
label var statex28 "South California"
label var statex29 "Georgia"
label var statex30 "Florida"
label var statex31 "Kentucky"
label var statex32 "Tennessee"
label var statex33 "Alabama"
label var statex34 "Mississippi"
label var statex35 "Arkansas"
label var statex36 "Louisiana"
label var statex37 "Oklahoma"
label var statex38 "Texas"
label var statex39 "Montana"
label var statex40 "Idaho"
label var statex41 "Wyoming"
label var statex42 "Colorado"
label var statex43 "New Mexico"
label var statex44 "Arizona"
label var statex45 "Utah"
label var statex46 "Nevada"
label var statex47 "Washington"
label var statex48 "Oregon"
label var statex49 "California"
label var statex50 "Alaska"
label var statex51 "Hawaii"
*Education
label var educx1 "Lower then High School"
label var educx2 "High School"
label var educx3 "Some college"
label var educx4 "College"
label var educx5 "Advanced"
*Occupation
label var occ_m03x1 "Management, business and financial"
label var occ_m03x2 "Professional and related"
label var occ_m03x3 "Service occupation"
label var occ_m03x4 "Sales and related"
label var occ_m03x5 "Office and administration support"
label var occ_m03x6 "Farming, fishing and foresty"
label var occ_m03x7 "Constructon and extraction"
label var occ_m03x8 "Installation, maintenance and repair"
label var occ_m03x9 "Product and material moving"
label var occ_m03x10 "Transportation and material moving"


*describe weekpay wage4 age female i.wbho married i.ownchild empl pubsect union multjob i.state i.educ i.occ_m03 agric manuf servs
describe weekpay wage4 age female wbhox* married ownchildx* empl pubsect union multjob statex* educx* occ_m03x* agric manuf
*sum if female==1
*estout
*sum if female==0
*estout
*esttab

*eststo upper: quietly estpost summarize weekpay wage4 age wbhox* married ownchildx* empl pubsect union multjob agric manuf servs statex* educx* occ_m03x* if female==1
*eststo less: quietly estpost summarize weekpay wage4 age wbhox* married ownchildx* empl pubsect union multjob agric manuf servs statex* educx* occ_m03x*  if female==0

*esttab upper less, cell(mean(fmt(%9.3f))) varwidth(40) label nonumbers modelwidth(50 50) collabels(none) title ("Table 1: Sample statistics for gender wage gap analysis") mtitles ("Female" "Men")

*esttab using prosjekt1.rtf, append wide label modelwidth(8)

*gen agesq=age^2
*tabstat weekpay wage4 age agesq female wbho married ownchild empl pubsect union multjob, by(female) col(stat) stat(n mean sd var median) format(%12.2f)
tab female empl
tab female married
tab female ownchild
tab state educ 
tab educ wbho 
tab state wbho 
tab occ_m03 wbho if female==0
tab occ_m03 wbho if female==1
tab agric manuf 
*tab servs
//-regress- with interactions
*keep if (age >=15 & age <=74 & female==1) | (age >=15 & age <=74 & female==0) // Labour force population referes to people aged 16 to 64 years 

gen lwage=log(weekpay)
sum lwage, detail
*gen xstep1=(r(max)-r(min))/50
*kwage will be the wage at which the density is estimated;
*gen kwage12=r(min)+(_n-1)
*kdensity lwage if female==1, at(kwage12) gauss width(0.065) 
     
gen lwage4=log(wage4)
sum lwage4, detail

regress weekpay i.female##i.occ_m03
regress wage4 i.female##i.occ_m03

///Another way. Mean gender wage gape within occupation
by occ_m03, sort: egen wage1 = mean(weekpay / (female == 1))
by occ_m03, sort: egen wage0 = mean(weekpay / (female == 0))
gen gap = wage1 - wage0 
label var wage1 "Weekly wage female"
label var wage0 "Weekly wage male"
label var gap "Gap"
egen tag = tag(occ_m03) 
tabdisp occ_m03 if tag, c(wage1 wage0 gap)
eststo week_occ


//Mean gender wage gape within race per week
by wbho, sort: egen wage11 = mean(weekpay / (female == 1))
by wbho, sort: egen wage00 = mean(weekpay / (female == 0))
gen gap1 = wage11 - wage00 
label var wage11 "Weekly wage female"
label var wage00 "Weekly wage male"
label var gap1 "Gap"
egen tag1 = tag(wbho) 
tabdisp wbho if tag1, c(wage11 wage00 gap1)
eststo week_race

//Mean gender wage gape within education per week
by educ, sort: egen wage111 = mean(weekpay / (female == 1))
by educ, sort: egen wage000 = mean(weekpay / (female == 0))
gen gap11 = wage111 - wage000 
label var wage111 "Weekly wage female"
label var wage000 "Weekly wage male"
label var gap11 "Gap"
egen tag11 = tag(educ) 
tabdisp educ if tag11, c(wage111 wage000 gap11)
eststo week_educ

//Mean gender wage gape within stat per week

by state, sort: egen wage1111 = mean(weekpay / (female == 1))
by state, sort: egen wage0000 = mean(weekpay / (female == 0))
gen gap111 = wage1111 - wage0000 
label var wage1111 "Weekly wage female"
label var wage0000 "Weekly wage male"
label var gap111 "Gap"
egen tag111 = tag(state) 
tabdisp state if tag111, c(wage1111 wage0000 gap111)
eststo week_stat

//Mean gender wage gap own children per week

by ownchild, sort: egen wage11111 = mean(weekpay / (female == 1))
by ownchild, sort: egen wage00000 = mean(weekpay / (female == 0))
gen gap1111 = wage11111 - wage00000 
label var wage11111 "Weekly wage female"
label var wage00000 "Weekly wage male"
label var gap1111 "Gap"
egen tag1111 = tag(ownchild) 
tabdisp ownchild if tag1111, c(wage11111 wage00000 gap1111)
eststo week_children
esttab week_occ week_race week_educ week_stat week_children

//Mean gender wage gape within state per hour
//Occupation

by occ_m03, sort: egen wage14 = mean(wage4/ (female == 1))
by occ_m03, sort: egen wage04 = mean(wage4 / (female == 0))
gen gap4 = wage14 - wage04
egen tag4 = tag(occ_m03) 
tabdisp occ_m03 if tag4, c(wage14 wage04 gap4)
eststo hourly_occu

//Race
by wbho, sort: egen wage144 = mean(wage4/ (female == 1))
by wbho, sort: egen wage044 = mean(wage4 / (female == 0))
gen gap44 = wage144 - wage044
label var wage144 "Hourly wage female"
label var wage044 "Hourly wage male"
label var gap44 "Gap"
egen tag44 = tag(wbho) 
tabdisp wbho if tag44, c(wage144 wage044 gap44)
eststo hourly_race

//Education
by educ, sort: egen wage1444 = mean(wage4/ (female == 1))
by educ, sort: egen wage0444 = mean(wage4 / (female == 0))
gen gap444 = wage1444 - wage0444
label var wage1444 "Hourly wage female"
label var wage0444 "Hourly wage male"
label var gap444 "Gap"
egen tag444 = tag(educ) 
tabdisp educ if tag444, c(wage1444 wage0444 gap444)
eststo hourly_educ

//State
by state, sort: egen wage14444 = mean(wage4/ (female == 1))
by state, sort: egen wage04444 = mean(wage4 / (female == 0))
gen gap4444 = wage14444 - wage04444
label var wage14444 "Hourly wage female"
label var wage04444 "Hourly wage male"
label var gap4444 "Gap"
egen tag4444 = tag(state) 
tabdisp state if tag4444, c(wage14444 wage04444 gap4444)
eststo hourly_state

//Own child
by ownchild, sort: egen wage144444 = mean(wage4/ (female == 1))
by ownchild, sort: egen wage044444 = mean(wage4 / (female == 0))
gen gap44444 = wage144444 - wage044444
label var wage144444 "Hourly wage female"
label var wage044444 "Hourly wage male"
label var gap44444 "Gap"
egen tag44444 = tag(ownchild) 
tabdisp ownchild if tag44444, c(wage144444 wage044444 gap44444)
eststo hourly_children
esttab week_occ week_race week_educ week_stat week_children 

*qui reg lwage age agesq i.wbho married i.ownchild pubsect union multjob i.state i.educ i.occ_m03 agric manuf hourslw if female==1
*himod, ds

*qui reg lwage age agesq i.wbho married i.ownchild pubsect union multjob i.state i.educ i.occ_m03 agric manuf hourslw  if female==0
*lomod, ds
*decomp

// Regression for log pay per hour
tabstat lwage lwage4 weekpay wage4 age wbhox* married ownchildx* ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) col(stat) stat(n mean sd var median) format(%12.2f)

*reg lwage4 age agesq female i.wbho married i.ownchild pubsect union multjob i.state i.educ i.occ_m03 agric manuf hourslw
//Regression for log pay per week
*gen agesq=age^2

reg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, robust
*graph matrix lwage age agesq female, half maxis(ylabel(none) xlabel(none))
*graph matrix  lwage4 multjob statex* educx* occ_m03x*, half maxis(ylabel(none) xlabel(none))
matrix b_all= e(b) //Obtained estimated coefficients of regression in a vector form for all workers
reg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, robust 
matrix b_women= e(b) //Obtained estimated coefficients of regression in a vector form for women
reg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, robust
matrix b_men= e(b) //Obtained estimated coefficients of regression in a vector form for men

su lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw // Obtained vector of means of each individual and job characteristics
su lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1 //Obtained vector of means of each individual and job characteristics
su lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0 //Obtained vector of means of each individual and job characteristics

//Heteroskedastisity
predict res, res
kdensity res, normal
gen res2=res^2
predict lwage_h
avplot age
avplot female
avplot wbho 
avplot married 
avplot ownchild 
*avplot empl 
avplot pubsect 
avplot union 
avplot multjob

//Plot the residuals vs fittet (predicted) values
rvfplot, yline(0)
*estat imtest
*estat hettest
*Both tests could not reject the null hypothesis H0: Constant variance

//Model spesification
linktest
//Multicollinearity
estat ovtest
estat vif

*Categorical variables cannot be colinear. They do not represent linear measures in Euclidean space.... A chi-square test can be used to test for independence of categorical variables

*collin age agesq female wbhox* married ownchildx* empl pubsect union multjob statex* educx* occ_m03x*, corr

*acprplot age, lowess 
*acprplot female, lowess
*acprplot wbhox*, lowess

quietly regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, robust
estimates store regpool
*quietly qreg lwage4 age agesq female i.wbho married i.ownchild pubsect union multjob i.state i.educ i.occ_m03 agric manuf hourslw, quantile(.1)
*estimates store qr1pool
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, quantile(.25)
estimates store qr2pool
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, quantile(.5)
estimates store qr3pool
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, quantile(.75)
estimates store qr4pool
*quietly qreg lwage4 age agesq female wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, quantile(.9)
*estimates store qr5pool
estimates table regpool qr2pool qr3pool qr4pool, equations(1) b(%8.4f) stats(N ll aic bic) stfmt(%8.0f) star  

quietly regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, robust
estimates store regfemale
*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(.1)
*estimates store qr1native
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(.25)
estimates store qr2female
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(.5)
estimates store qr3female
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(.75)
estimates store qr4female
*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(.9)
*estimates store qr5native
estimates table regfemale qr2female qr3female qr4female, equations(1) b(%8.4f) stats(N ll aic bic) stfmt(%8.0f) star  

quietly regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, robust
estimates store regmale
*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(.1)
*estimates store qr1male
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(.25)
estimates store qr2male
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(.5)
estimates store qr3male
quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(.75)
estimates store qr4male
*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(.9)
*estimates store qr5male
estimates table regmale qr2male qr3male qr4male, equations(1) b(%8.4f) stats(N ll aic bic) stfmt(%8.0f) star  

*sysdir set PERSONAL W:\ado

quietly regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, robust
himod, ds

quietly regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, robust
lomod, ds
decomp

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(0.1)
*himod, ds

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(0.1)
*lomod, ds
*decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(0.25)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.25)
lomod, ds
decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(0.5)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.5)
lomod, ds
decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(0.75)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.75)
lomod, ds
decomp

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(0.9)
*himod, ds

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(0.9)
*lomod, ds
*decomp

regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, robust
himod, ds

regress lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, robust
lomod, ds
decomp

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(0.1)
*himod, ds

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(0.1)
*lomod, ds
*decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.25)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(0.25)
lomod, ds
decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.5)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==1, quantile(0.5)
lomod, ds
decomp

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw if female==0, quantile(0.75)
himod, ds

quietly qreg lwage4 age wbhox2-wbhox4 married ownchildx1-ownchildx2 ownchildx4-ownchildx11 pubsect union multjob statex1-statex6 statex8-statex51 educx1 educx3-educx5 occ_m03x1 occ_m03x3-occ_m03x10 agric manuf hourslw, quantile(0.75)
lomod, ds
decomp


*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==0, quantile(0.9)
*himod, ds

*quietly qreg lwage4 age agesq wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw if female==1, quantile(0.9)
*lomod, ds
*decomp

*sysdir set PERSONAL W:\

set matsize 1000 

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) eform detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) eform swap detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) swap detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) pooled detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) pooled eform detail

oaxaca lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) swap pooled detail

decompose lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, by(female) detail

set matafavor speed

cdeco lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, group(female) quantiles(0.25 0.5 0.75) reps(5)
matrix results=r(results)
matrix se=r(se)
svmat results, names(col)
svmat se, names(col)
twoway (line total_differential quantile) (line characteristics quantile) (line coefficients quantile), title(Decomposition of differences in distribution) ytitle(Log wage effects) xtitle(Quantile) legend(order(1 "Total differential" 2 "Effects of characteristics" 3 "Effects of coefficients"))
generate lo_coef=coefficients-1.96*se_coefficients
generate hi_coef=coefficients+1.96*se_coefficients
twoway (rarea hi_coef lo_coef quantile, bcolor(gs13) legend(off)) (line coefficients quantile),title(Effects of coefficients (discrimination)) ytitle(Log wage effects) xtitle(Quantile)

cdeco_jmp lwage4 age wbhox* married ownchildx* pubsect union multjob statex* educx* occ_m03x* agric manuf hourslw, group(female) quantiles(0.25 0.5 0.75) reps(5)




