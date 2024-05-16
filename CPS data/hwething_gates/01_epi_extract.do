set more off
clear all

***Use FPL from 2019 
use "fpl_u65.dta", clear
rename fpl fpl2019
gen mergevar = "merge"
tempfile fpl
save `fpl'

*** Expand FPL data to 2010-2023 using CPI-U
*** first, pull in CPI-U and generate inflation ratio
sysuse cpi_annual
keep if year>2007
keep year cpi_u
sum cpi_u if year == 2019
gen cpi2019 = r(mean)
gen inflate=cpi_u/cpi2019
gen mergevar = "merge"`'

*join fpl to cpi to get all cpi-year fpl pairs
joinby mergevar using `fpl'

gen fpl = fpl2019 * inflate
drop fpl2019 cpi2019 mergevar
tempfile poverty
save `poverty'


****Load CPS Basic
merge_rawextracts, begin(2007m1) end(2009m12) sample(basic) keepextracts(hhid year month minsamp basicwgt finalwgt wbho wbhao age female lfstat emp unemp agechild ownchild orgwgt)  keepraw(hufaminc pwfmwgt perrp prfamnum prfamrel prfamtyp pudis )
rename hufaminc hefaminc
tempfile 2007_2009_rawextracts
save `2007_2009_rawextracts'
merge_rawextracts, begin(2010m1) end(2023m12) sample(basic) keepextracts(hhid year month minsamp basicwgt finalwgt wbho wbhao age female lfstat emp unemp agechild ownchild orgwgt)  keepraw(hefaminc pwfmwgt perrp prfamnum prfamrel prfamtyp hxfaminc pudis )
tempfile 2010_2023_rawextracts
save `2010_2023_rawextracts'

use `2007_2009_rawextracts', clear
append using `2010_2023_rawextracts'

****Generate hhid (a unique variable within a given year/month) & hhsize
egen unique_hhid=group(hhid year month)
bys unique_hhid: gen hhsize=_N

**second, generate family id
**Hhold members that are not family members are coded as 0 so there can be multiple zeros in 1 hhold. 
**to create seperate families for hhold members that are not members of a primary family or subfamily
** generate unique id for these members
tab prfamnum
bys prfamnum: gen id= _n
	replace id=id*10
gen famnum= id if prfamnum==0

***for primary families, famnum== prfamnum, if the subfamilies are unrelated to primary families, famnum== prfamnum
replace famnum=prfamnum if prfamnum>0
****if the subfamily is related to primary family, recode to have the same number as primary family
replace famnum=1 if prfamnum>1 & prfamtyp==3

**generate unique family id
egen fid=group(hhid year month famnum)

***generate family size variable
bys fid: gen famsize=_N
order unique_hhid year month fid prfamnum famsize

***** identify number of children in each family
gen child=(prfamrel==3 & age< 18)
bys fid: egen childsize= sum(child)

order unique_hhid year month fid prfamnum

**categorize families  by Census FPL family types
gen famtype =10 if famsize==1 & childsize==0
replace famtype=20 if famsize==2 & childsize==0
replace famtype=30 if famsize==3 & childsize==0
replace famtype=40 if famsize==4 & childsize==0
replace famtype=50 if famsize==5 & childsize==0
replace famtype=60 if famsize==6 & childsize==0
replace famtype=70 if famsize==7 & childsize==0
replace famtype=80 if famsize==8 & childsize==0
replace famtype=90 if famsize>=9 & childsize==0

forvalues x=2/8{
	replace famtype = `x'1 if famsize==`x' & childsize==1
	}
forvalues x=3/8{
	replace famtype = `x'2 if famsize==`x' & childsize==2
	}
forvalues x=4/8{
	replace famtype = `x'3 if famsize==`x' & childsize==3
	}
forvalues x=5/8{
	replace famtype = `x'4 if famsize==`x' & childsize==4
	}
forvalues x=6/8{
	replace famtype = `x'5 if famsize==`x' & childsize==5
	}
forvalues x=7/8{
	replace famtype = `x'6 if famsize==`x' & childsize==6
	}
forvalues x=8/8{
	replace famtype = `x'7 if famsize==`x' & childsize==7
	}
forvalues x=1/8 {
	replace famtype=9`x' if famsize>=9 & childsize==`x'
	}
replace famtype=98 if famsize>9 & childsize>8


***Recode family income variables
gen maxinc=.
replace maxinc=4999 if hefaminc==1
replace maxinc=7499 if hefaminc==2
replace maxinc = 9999 if hefaminc==3
replace maxinc=12499 if hefaminc==4
replace maxinc=14999 if hefaminc==5
replace maxinc= 19999 if hefaminc==6
replace maxinc=24999 if hefaminc==7
replace maxinc=29999 if hefaminc==8
replace maxinc=34999 if hefaminc==9
replace maxinc=39999 if hefaminc==10
replace maxinc=49999 if hefaminc==11
replace maxinc=59999 if hefaminc==12
replace maxinc=74999 if hefaminc==13
replace maxinc = 99999 if hefaminc==14
replace maxinc=149999 if hefaminc==15
replace maxinc= 150000 if hefaminc==16

gen mininc=.
replace mininc=0 if hefaminc==1
replace mininc=5000 if hefaminc==2
replace mininc=7500 if hefaminc==3
replace mininc=10000 if hefaminc==4
replace mininc=12500 if hefaminc==5
replace mininc= 15000 if hefaminc==6
replace mininc=20000 if hefaminc==7
replace mininc=25000 if hefaminc==8
replace mininc=30000 if hefaminc==9
replace mininc=35000 if hefaminc==10
replace mininc=40000 if hefaminc==11
replace mininc=50000 if hefaminc==12
replace mininc=60000 if hefaminc==13
replace mininc=75000 if hefaminc==14
replace mininc=100000 if hefaminc==15
replace mininc=150000 if hefaminc==16

merge m:1 year famtype using `poverty'
drop _merge

gen fpl50=fpl*.5
gen fpl150=fpl*1.5
gen fpl200=fpl*2
gen fpl185=fpl*1.85

******Create poverty measures that map onto Census imputed income bands
gen pov100=( maxinc<fpl)
gen pov185= ( maxinc<fpl185 )
gen pov200= ( maxinc<fpl200)


****** Generate simulated income for unrelated subfamilies and secondary individuals
gen iprim=maxinc if prfamtyp==1
*nonfamily
gen inonfam=maxinc if prfamtyp==2
*related sub
gen irelated=maxinc if prfamtyp==3
*unrelated sub
gen iunrel=maxinc if prfamtyp==4
*secondary 
gen isecond=maxinc if prfamtyp==5

bys unique_hhid: egen prim=max(iprim)
bys unique_hhid: egen nonfam=max(inonfam)
bys unique_hhid: egen related=max(irelated)
bys unique_hhid: egen unrel=max(iunrel)
bys unique_hhid: egen second=max(isecond)
drop  iprim inonfam irelated iunrel isecond

*Merge in ratios from ASEC of primary individ/family income/unrelated subfmaily or secondary individ
merge m:1 year using "ASEC_inc_ratios.dta"
foreach var in famunrel famsec  nhhunrel nhhsec{
	egen ratio_`var' =max(rat_`var')
	drop rat_`var'
	}
***generate new max and min income variable  = max/min income if person is in primary family or a primary individual
*** new max/min income variable = primary family/individual income * ratio if unrelated subfamily or secondary individual
gen i_maxinc=maxinc if  prfamtyp ==1
replace i_maxinc=maxinc if prfamtyp==3
replace i_maxinc=maxinc *ratio_famunrel if prim!=. & prfamtyp==4
replace i_maxinc=maxinc *ratio_famsec if prim!=. & prfamtyp==5
replace i_maxinc=maxinc if prfamtyp==2
replace i_maxinc=maxinc * ratio_nhhunrel if nonfam!=. & prfamtyp==4
replace i_maxinc=maxinc * ratio_nhhsec if nonfam!=. & prfamtyp==5

gen i_mininc=mininc if  prfamtyp ==1
replace i_mininc=mininc if prfamtyp==3
replace i_mininc=mininc *ratio_famunrel if prim!=. & prfamtyp==4
replace i_mininc=mininc *ratio_famsec if prim!=. & prfamtyp==5
replace i_mininc=mininc if prfamtyp==2
replace i_mininc=mininc * ratio_nhhunrel if nonfam!=. & prfamtyp==4
replace i_mininc=mininc * ratio_nhhsec if nonfam!=. & prfamtyp==5

******Create poverty measures that map onto ratio imputed bands
gen spov50=(i_maxinc<fpl50)
gen spov100=( i_maxinc<fpl)
gen spov150=(i_maxinc<fpl150)
gen spov200= ( i_maxinc<fpl200)


drop hrhhid hrhhid2 ratio_*

save  "data/CPS_families_2007_2023.dta", replace
clear
