

*load_epiextracts, begin(2019m1) end(2019m12) sample(basic) 
merge_rawextracts, begin(2019m1) end(2019) sample(basic) keepextracts(hhid year month basicwgt finalwgt wbho age educ female)  keepraw(hefaminc pwfmwgt  prfamnum prfamrel prfamtyp) 


****first, generate hhid (a unique variable within a given year/month
egen unique_hhid=group(hhid year month)

**second, generate family id-- there can be several families within a household
gen n=1
bys unique_hhid: egen hhsize=sum(n)


**** generate famid (a unique variabls within a given hhid year month and perfamnum
**first, make all 0s in prfamnum (not afamily member) unique IDs
bys prfamnum: gen id= _n
replace id=id*10


***generate unique families within households
*** if prfamnum==0, you are your own singular family (not a family member, primary individual or secondary individual)
gen famnum= id if prfamnum==0

***for primary families and subfamilies, if all subfamilies are unrelated to primary families, famnum== prfamnum
replace famnum=prfamnum if prfamnum>0
****if the subfamily is realted to primary family, but be considered part of primary family
replace famnum=1 if prfamnum>1 & prfamtyp==3
egen fid=group(hhid year month famnum)

***generate family size variable
bys fid: gen famsize=_N
order unique_hhid year month fid prfamnum famsize

***** identify number of children
gen child=(prfamrel==3 & age< 18)
bys fid: egen childsize= sum(child)

order unique_hhid year month fid prfamnum

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




****************family income variables

*****generate top-level income band from code

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


merge m:1 year famtype using "/projects/hwething/gates/data/fpl_threshold.dta"
***for >65 make minor changes
gen a65=(prfamrel==1 & age>=65)
bys fid: egen f65=max(a65)

replace fpl=12261 if famtype==10 & f65 ==1
replace fpl=17120 if famtype == 20 & f65 ==0
replace fpl=15453 if famtype== 20 & f65 ==1
replace fpl=17622 if famtype == 21 & f65 ==0
replace fpl=17555 if famtype== 21 & f65 ==1


gen fpl200=fpl*2
gen fpl185=fpl*1.85

******Create poverty measures that map onto income bands
gen pov100=( maxinc<fpl)
gen pov185= ( maxinc<fpl185 )
gen pov200= ( maxinc<fpl200)

*******generate some stats to benchmark the data
keep if age >16 & age < 64


tempfile basic_1994_2019
save `basic_1994_2019'
	collapse (sum) pov100 (mean) povrate=pov100 [pw=basicwgt/12], by (year)
	tempfile data1
	save `data1'

use `basic_1994_2019', clear
	collapse (sum) pov100 (mean) povrate=pov100 [pw=basicwgt/12], by (year female)
	tempfile data2
	save `data2'
	
use `basic_1994_2019', clear
	collapse (sum) pov100 (mean) povrate=pov100 [pw=basicwgt/12], by (year wbho)
	tempfile data3
	save `data3'
use `basic_1994_2019', clear
	collapse (sum) pov100 (mean) povrate=pov100 [pw=basicwgt/12], by (year educ)
	tempfile data4
	save `data4'

use `data1', clear
append using `data2'
append using `data3'
append using `data4'

export delim using "/projects/hwething/gates/output/CPS_benchmarking.csv", replace
