**reshaping mixing_module_dataset
cd "C:\Users\Matt\Google Drive\Research\github\kenya_mixing"

*wide
use Data/mixing_module_dataset_may1420_wide.dta , clear
sort hhid
destring hhid,replace

sort hhid
merge m:1 hhid using Data\COVID19_Merged_3rds_clean_formerge.dta
drop if _merge==2
*tab hh_size

gen part_id=hhid
gen part_age=q101_age
*gen agecat=q101_age
recode agecat (0/17=1) (18/29=2) (30/39 =3) (40/49=4) (50/59=5) (60 /100=6)



save Data\mixing_module_dataset_may1420_wide_b.dta, replace

*long
use Data\mixing_module_dataset_may1420_long.dta , clear
gen contact_survey=1
sort hhid
destring hhid,replace

merge m:1 hhid using Data\COVID19_Merged_3rds_clean_formerge.dta
tab _merge

*gen hh_size=q127sp

sort hhid hh_size
*by hhid: replace hh_size=hh_size[_n-1] if hh_size==.
*drop if contact_survey!=1
*drop if _merge==2

*gen agecat=q101_age
recode agecat (0/17=1) (18/29=2) (30/39 =3) (40/49=4) (50/59=5) (60 /100=6)

gen part_id=hhid
gen part_age=q101_age
gen part_sex=q100_sex
gen cnt_age=q2_age
gen contact_detail=0
replace contact_detail=1 if cnt_age<99 

gen cnt_agebin_18=0
replace cnt_agebin_18=1 if cnt_age==101 
replace cnt_agebin_18=1 if cnt_age<19
gen cnt_agebin_59=0
replace cnt_agebin_59=1 if cnt_age==102 
replace cnt_agebin_59=1 if cnt_age>=19 & cnt_age<60
gen cnt_agebin_60=0
replace cnt_agebin_60=1 if cnt_age==103 
replace cnt_agebin_60=1 if cnt_age>=60 & cnt_age<90

drop if cnt_age==.

gen cnt_place=q5_place_contact
recode cnt_place(5=15) (9=15) (11=16) (14=15)
tab cnt_place
_crcslbl cnt_place q5_place_contact

save Data\mixing_module_dataset_may1420_long_b.dta, replace
export delimited using Data\mixing_module_dataset_may1420_long_master.csv, replace

egen n_contacts= count(hhid), by(hhid)
sort hhid n_contacts
by hhid: replace n_contacts=hh_size[_n-1] if n_contacts==.

*drop all rows except first row of each hhid
bysort hhid: keep if _n==1
export delimited using "Data\contacts_pseudowide.csv", replace
save  "Data\contacts_pseudowide.dta", replace

*making just hh and non hh contacts wide datasets
*household
import delimited "Data\mixing_module_dataset_may1420_long_master.csv", clear
keep if contact_kind=="Household Contacts" 
egen n_contacts= count(hhid), by(hhid)
sort hhid n_contacts
by hhid: replace n_contacts=hh_size[_n-1] if n_contacts==.
bysort hhid: keep if _n==1
export delimited using "Data\contacts_pseudowide_hh.csv", replace
*non household
import delimited "Data\mixing_module_dataset_may1420_long_master.csv", clear
keep if contact_kind=="Non-Household Contacts" 
egen n_contacts= count(hhid), by(hhid)
sort hhid n_contacts
by hhid: replace n_contacts=hh_size[_n-1] if n_contacts==.
bysort hhid: keep if _n==1
export delimited using "Data\contacts_pseudowide_nonhh.csv", replace



*ANALYSIS 1 - Making values for crude imputation

use "Data\mixing_module_dataset_may1420_long_b.dta" , clear

replace cnt_age=10 if cnt_age==101
replace cnt_age=40 if cnt_age==102
replace cnt_age=70 if cnt_age==103

gen cnt_age_cont=cnt_age
drop cnt_age

gen cnt_age=""
replace cnt_age="0-17" if cnt_age_cont<18
replace cnt_age="18-60" if cnt_age_cont>=18 & cnt_age_cont<=60
replace cnt_age="61-120" if cnt_age_cont>60

gen cnt_age_est_min=.
replace cnt_age_est_min=0 if cnt_agebin_18==1
replace cnt_age_est_min=19 if cnt_agebin_59==1
replace cnt_age_est_min=60 if cnt_agebin_60==1

gen cnt_age_est_max=.
replace cnt_age_est_max=18 if cnt_agebin_18==1
replace cnt_age_est_max=59 if cnt_agebin_59==1
replace cnt_age_est_max=80 if cnt_agebin_60==1

gen    part_agebin_18=0
gen     part_agebin_59=0
replace part_agebin_59=1 if part_age>=19 & part_age<60
gen     part_agebin_60=0

replace part_agebin_60=1 if part_age>=60 & part_age<90
gen     part_age_cont=part_age
drop    part_age
gen     part_age=""

replace part_age="(0,17]" if part_age_cont<=17
replace part_age="(18,50]" if part_age_cont>17 & part_age_cont<=50
replace part_age="(50,80]" if part_age_cont>50
gen     part_age_est_min=.
replace part_age_est_min=0 if  part_agebin_18==1
replace part_age_est_min=19 if part_agebin_59==1
replace part_age_est_min=60 if part_agebin_60==1
gen     part_age_est_max=.
replace part_age_est_max=18 if part_agebin_18==1
replace part_age_est_max=59 if part_agebin_59==1
replace part_age_est_max=80 if part_agebin_60==1
gen part_age_group=part_age
export delimited using "Data\imputed_justcategory_60plus.csv", replace

*ANALYSIS 1 - 50+ - Making values for crude imputation 50+

use "Data\mixing_module_dataset_may1420_long_b.dta" , clear

replace cnt_age=. if cnt_age==101
replace cnt_age=. if cnt_age==102
replace cnt_age=. if cnt_age==103

gen cnt_age_exact=cnt_age if cnt_age<100

*gen cnt_age_cont=cnt_age
*drop cnt_age

*gen cnt_age=.
*replace cnt_age="0-18" if cnt_age_cont<=18
*replace cnt_age=. if cnt_age_cont>18 & cnt_age_cont<=60
*replace cnt_age="61-120" if cnt_age_cont>60

gen cnt_age_est_min=.
replace cnt_age_est_min=0 if cnt_agebin_18==1
replace cnt_age_est_min=19 if cnt_agebin_59==1
replace cnt_age_est_min=60 if cnt_agebin_60==1

gen cnt_age_est_max=.
replace cnt_age_est_max=18 if cnt_agebin_18==1
replace cnt_age_est_max=59 if cnt_agebin_59==1
replace cnt_age_est_max=80 if cnt_agebin_60==1

gen    part_agebin_18=0
gen     part_agebin_49=0
replace part_agebin_49=1 if part_age>=19 & part_age<50
gen     part_agebin_50=0
replace part_agebin_50=1 if part_age>=50 & part_age<90
gen     part_age_cont=part_age
drop    part_age
gen     part_age=""

replace part_age="(0,17]" if part_age_cont<=17
replace part_age="(18,50]" if part_age_cont>17 & part_age_cont<=50
replace part_age="(50,80]" if part_age_cont>50
gen     part_age_est_min=.
replace part_age_est_min=0 if  part_agebin_18==1
replace part_age_est_min=19 if part_agebin_49==1
replace part_age_est_min=50 if part_agebin_50==1
gen     part_age_est_max=.
replace part_age_est_max=18 if part_agebin_18==1
replace part_age_est_max=59 if part_agebin_49==1
replace part_age_est_max=50 if part_agebin_50==1
gen part_age_group=part_age

export delimited using "Data\imputed_justcategory_50plus.csv", replace



*ANALYSIS 3 - letting socialmixr sample age estimates

use "Data\mixing_module_dataset_may1420_long_b.dta" , clear
gen cnt_age_exact=cnt_age
replace cnt_age_exact=. if cnt_age==101|cnt_age==102|cnt_age==103

gen cnt_age_est_min=.
replace cnt_age_est_min=0 if cnt_agebin_18==1
replace cnt_age_est_min=19 if cnt_agebin_59==1
replace cnt_age_est_min=60 if cnt_agebin_60==1

gen cnt_age_est_max=.
replace cnt_age_est_max=18 if cnt_agebin_18==1
replace cnt_age_est_max=59 if cnt_agebin_59==1
replace cnt_age_est_max=80 if cnt_agebin_60==1

drop cnt_age

export delimited using "Data\socialmixr_toimpute.csv", replace


import delimited "Data\mixing_module_dataset_may1420_wide.csv", clear 

*First, 60+ categories
gen    part_agebin_18=0
gen     part_agebin_59=0
replace part_agebin_59=1 if part_age>=19 & part_age<60
gen     part_agebin_60=0

replace part_agebin_60=1 if part_age>=60 & part_age<90
gen     part_age_cont=part_age
drop    part_age
gen     part_age=""


drop part_age
gen part_age=""
replace part_age="0-17" if part_age_cont<=17
replace part_age="18-59" if part_age_cont>17 & part_age_cont<60
replace part_age="60-80" if part_age_cont>=60
gen     part_age_est_min=.
replace part_age_est_min=0 if  part_agebin_18==1
replace part_age_est_min=19 if part_agebin_59==1
replace part_age_est_min=60 if part_agebin_60==1
gen     part_age_est_max=.
replace part_age_est_max=18 if part_agebin_18==1
replace part_age_est_max=59 if part_agebin_59==1
replace part_age_est_max=80 if part_agebin_60==1
gen part_age_group=part_age
export delimited using "Data\mixing_module_dataset_may1420_wide_60plus.csv", replace

import delimited "Data\mixing_module_dataset_may1420_wide.csv", clear 
*Now, 50+ categories
gen    part_agebin_18=0
gen     part_agebin_49=0
replace part_agebin_49=1 if part_age>=19 & part_age<50
gen     part_agebin_50=0

replace part_agebin_50=1 if part_age>=50 & part_age<90
gen     part_age_cont=part_age
drop    part_age
gen     part_age=""


drop part_age
gen part_age=""
replace part_age="0-17" if part_age_cont<=17
replace part_age="18-49" if part_age_cont>17 & part_age_cont<50
replace part_age="50-80" if part_age_cont>=50
gen     part_age_est_min=.
replace part_age_est_min=0 if  part_agebin_18==1
replace part_age_est_min=19 if part_agebin_49==1
replace part_age_est_min=50 if part_agebin_50==1
gen     part_age_est_max=.
replace part_age_est_max=18 if part_agebin_18==1
replace part_age_est_max=49 if part_agebin_49==1
replace part_age_est_max=80 if part_agebin_50==1
gen part_age_group=part_age
export delimited using "Data\mixing_module_dataset_may1420_wide_50plus.csv", replace














