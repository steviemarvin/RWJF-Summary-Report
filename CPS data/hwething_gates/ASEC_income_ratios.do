****first, generate hhid (a unique variable within a given year/month
use "cps_00018.dta", clear
**revise year to be in survey years
replace year= year-1

*gen black nothispanic
gen black=(race==200)
replace race=0 if hispan>0

**create unique hosehold and family ID
egen unique_hhid=group(serial year month)
egen unique_famid=group(unique_hhid famid)
**identify subfamilies
gen temp=(ftype==4)
replace temp=1 if (ftype==5 | ftype==2)
bys unique_hhid: egen subhhold=max(temp)

**find subfamilies
gen temp2=(ftype==3)
bys unique_hhid: egen relsubhhold=max(temp2)


bys unique_hhid: gen hhsize=_N
bys unique_famid: gen fsize=_N

*ftotval gives family income for their predefined family units (DOES NOT MAP ONTO CENSUS BASIC MONTHLY INCOME QUESTION)
*primary family
gen iprim=ftotval if ftype==1
*nonfamily
gen inonfam=ftotval if ftype==2
*related sub
gen irelated=ftotval if ftype==3
*unrelated sub
gen iunrel=ftotval if ftype==4
*secondary in
gen isecond=ftotval if ftype==5

bys unique_hhid: egen prim=max(iprim)
bys unique_hhid: egen nonfam=max(inonfam)
bys unique_hhid: egen related=max(irelated)
bys unique_hhid: egen unrel=max(iunrel)
bys unique_hhid: egen second=max(isecond)
**generate ratio of incomes from primary family/individuals to secondary individuals or unrelated subfmailies
gen ratio_famrel=  related / prim
gen ratio_famunrel=  unrel / prim
gen ratio_famsec=  second / prim
gen ratio_nhhunrel = nonfam / unrel
gen ratio_nhhsec = nonfam / second
tempfile asec_20152023
save `asec_20152023'

collapse (median) rat_famrel= ratio_famrel rat_famunrel= ratio_famunrel rat_famsec = ratio_famsec  rat_nhhunrel=ratio_nhhunrel  rat_nhhsec=ratio_nhhsec [aweight=asecwt], by(year)
tempfile ratio
save `ratio'
export delim using "${output}ASEC_inc_ratios.csv", replace
use `ratio', clear
keep if year >= 2014 & year <= 2018
collapse (mean) rat_famunrel rat_famsec rat_nhhunrel rat_nhhsec
gen year=2014
save "${data}ASEC_inc_ratios.dta", replace
use `asec_20152023', clear


merge m:1 year using "${data}ASEC_inc_ratios.dta"
drop ratio* _merge

foreach var in famunrel famsec  nhhunrel nhhsec{
	sum rat_`var'
	gen ratio_`var' = r(mean)
	drop rat_`var'
	}

order unique_hhid  hhsize unique_famid famsize pernum ftype famrel famid ftotval

gen income_cons= prim if ( ftype==1 | ftype==3 )
replace income_cons=prim * ratio_famunrel if ftype==4 & prim!=.
replace income_cons=prim * ratio_famsec if ftype==5 & prim!=.
replace income_cons = nonfam if ftype==2
replace income_cons= nonfam * ratio_nhhunrel if ftype==4 & nonfam!=.
replace income_cons= nonfam * ratio_nhhsec if ftype==5 & nonfam!=.

order unique_hhid  hhsize unique_famid famsize pernum ftype famrel famid ftotval income_cons  offtotval


gen cutoff200=offcutoff*2

gen pov100= (offtotval<offcutoff)
tab pov100 [aweight=asecwt]
gen pov200=(offtotval<cutoff200)
tab pov200 [aweight=asecwt]

gen simpov100= (income_cons<offcutoff)
tab simpov100 [aweight=asecwt]
gen simpov200=(income_cons<cutoff200)
tab simpov200 [aweight=asecwt]

keep if age >=16 & age < 65

keep if (schlcoll==5 |schlcoll==0)

*save restricted sample
tempfile asec_20102023
save `asec_20102023'

***COMPARE POVERTY WITH IMPUTED INCOME
	collapse (sum)  pov100   pov200  (mean) povrate100=pov100  povrate200=pov200  [pw=asecwt], by (year)
	tempfile data1
	save `data1'
use `asec_20102023', clear
collapse (sum)  pov100   pov200  (mean) povrate100=pov100 povrate200=pov200  [pw=asecwt], by (year sex)
	tempfile data2
	save `data2'
use `asec_20102023', clear
collapse (sum)  pov100   pov200  (mean) povrate100=pov100 povrate200=pov200  [pw=asecwt], by (year black)
	tempfile data3
	save `data3'
use `data1', clear
append using `data2'
append using `data3'
export delim using "${output}ASEC_benchmarking2.csv", replace


use `asec_20102023', clear
	collapse (sum)  pov100 simpov100  pov200 simpov200 (mean) povrate100=pov100 simrate100=simpov100 povrate200=pov200 simrate200=simpov200  [pw=asecwt], by (year)
	tempfile data1
	save `data1'
use `asec_20102023', clear
collapse (sum)  pov100 simpov100  pov200 simpov200 (mean) povrate100=pov100 simrate100=simpov100 povrate200=pov200 simrate200=simpov200  [pw=asecwt], by (year sex)
	tempfile data2
	save `data2'
use `asec_20102023', clear
collapse (sum)  pov100 simpov100  pov200 simpov200 (mean) povrate100=pov100 simrate100=simpov100 povrate200=pov200 simrate200=simpov200  [pw=asecwt], by (year black)
	tempfile data3
	save `data3'
use `data1', clear
append using `data2'
append using `data3'
export delim using "${output}ASEC_benchmarking.csv", replace

