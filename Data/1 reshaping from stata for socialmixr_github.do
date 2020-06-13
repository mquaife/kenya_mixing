**reshaping mixing_module_dataset
cd "C:\Users\matt_\Google Drive\Research\github\kenya_mixing"

* what to keep from KAP survey data

use "C:\Users\matt_\Google Drive\Research\COVID\Pop council\Data\COVID19_MergedWithCohortRosters_Rounds1and2and3_withDemoForMatt.dta", clear
merge m:m hhid using "C:\Users\matt_\Google Drive\Research\COVID\Pop council\Data\hhid_education.dta"
drop _merge
*use Data\COVID19_R123.dta, clear
gen hh_size=q127sp
sort hhid hh_size
by hhid: replace hh_size=hh_size[_n-1] if hh_size==.

gen agecat=q101_age
recode agecat (0/17=1) (18/29=2) (30/39 =3) (40/49=4) (50/59=5) (60 /100=6)

drop if round!=3

*generating variables to export to social contact survey
gen perc_highrisk=0
replace perc_highrisk=1 if q107_inf_chance==3
gen perc_vsevere=0
replace perc_vsevere=1 if q301==1
gen left_location=0
replace left_location=1 if q205c>0 & q205c!=. 
gen skipped_meal=0
replace skipped_meal=1 if q216==2
gen received_help=0
replace received_help=1 if q220==2
gen see_fam_less=0
replace see_fam_less=1 if q226a==2
gen see_friends_less=0
replace see_friends_less=1 if q226b==2
gen partial_income_loss=0
replace partial_income_loss=1 if q226e==2
gen complete_income_loss=0
replace complete_income_loss=1 if q226d==2
gen income_loss=0
replace income_loss=1 if q226e==1 |q226d==2

*piped water to compound
gen piped_water=0
replace piped_water=1 if demo_water_source=="1. piped:to dwelling"|demo_water_source=="2. piped:to compound/plot"|demo_water_source=="PIPED INTO DWELLING"|demo_water_source=="PIPED TO COMPOUND/PLOT"
*toilet which flushes to pipe or elsewhere
gen toilet_flush=0
replace toilet_flush=1 if demo_toilet_type=="1. flush:to piped sewer system"|demo_toilet_type=="2. flush:to septic tank"|demo_toilet_type=="3. flush:to pit latrine"|demo_toilet_type=="4. flush:to somewhere else"|demo_toilet_type=="5. flush:to unknown place"|demo_toilet_type=="FLUSH TO PIPED SEWER SYSTEM"|demo_toilet_type=="FLUSH TO PIT LATRINE"|demo_toilet_type=="FLUSH TO SEPTIC TANK"|demo_toilet_type=="FLUSH TO SOMEWHERE ELSE"|demo_toilet_type=="FLUSH, DON'T KNOW WHERE"
*private toilet (i.e. not shared with others)
gen priv_toilet=0
replace priv_toilet=1 if demo_toilet_shared=="2. No"|demo_toilet_shared=="No"
*kerosene or gas cooking fuel
gen kerosene_gas=0
replace kerosene_gas=1 if demo_cooking_fuel=="2. lpg/natural gas"|demo_cooking_fuel=="3. biogas"|demo_cooking_fuel=="4. kerosene"|demo_cooking_fuel=="BIOGAS"|demo_cooking_fuel=="KEROSENE"|demo_cooking_fuel=="LPG/NATURAL GAS"
*finished floor
gen finished_floor=0
replace finished_floor=1 if demo_floor_type=="5. finished:parquet/polished wood"|demo_floor_type=="6. finished:vinyl/asphalt strips"|demo_floor_type=="7. finished:ceramic tiles"|demo_floor_type=="8. finished:cement"|demo_floor_type=="9. finished:carpet"|demo_floor_type=="CARPETED"|demo_floor_type=="CEMENT"|demo_floor_type=="CERAMIC TILES"|demo_floor_type=="PARQUET OR POLISHED WOOD"|demo_floor_type=="VINYL OR ASPHALT STRIPS"
*concrete  or tiles used in ceiling
gen concrete_ceiling=0
replace concrete_ceiling=1 if demo_roof_type=="6. finished:concrete"|demo_roof_type=="7. finished:tiles"|demo_roof_type=="CONCRETE"|demo_roof_type=="TILES"
gen iron_roof=0
replace iron_roof=1 if demo_roof_type=="3. rudimentary:corrugated iron(mabati)"|demo_roof_type=="CORRUGATED IRON (MABATI)"
*one room
gen one_room=0
replace one_room=1 if demo_number_of_rooms=="1"
*electricity
gen electricity=0
replace electricity=1 if demo_asset_electricity=="1. Yes"|demo_asset_electricity=="Yes"
*tv
gen tv=0
replace tv=1 if demo_asset_television=="1. Yes"|demo_asset_television=="Yes"
*phone
gen phone=0
replace phone=1 if demo_asset_mobile=="1. Yes"|demo_asset_mobile=="Yes"

pca piped_water toilet_flush priv_toilet kerosene_gas finished_floor concrete_ceiling iron_roof one_room electricity tv phone
*by putting only one name; we get eigenvectors, or weights, of the first principal component, which we assume is reflective of wealth
predict wealthscore_s1
*make quintiles
xtile quintile_s1= wealthscore_s1, nq(5) 
tab quintile_s1,gen(quint_)


*keep hhid consent_mixingmod hh_size education q127 q127sp perc_highrisk perc_vsevere left_location skipped_meal received_help see_fam_less see_friends_less partial_income_loss complete_income_loss income_loss
sort hhid


*hh_size has 2004 missing, which I am assuming lines up with 2010 hh interviewed in round 1
*drop if hh_size==.
*sort hhid

save Data\COVID19_Merged_3rds_clean_formerge.dta, replace


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


gen part_age_cat=0
replace part_age_cat=1 if part_age>=18 & part_age<=29
replace part_age_cat=2 if part_age>=30 & part_age<=39
replace part_age_cat=3 if part_age>=40 & part_age<=49
replace part_age_cat=4 if part_age>=50 & part_age<=59
replace part_age_cat=5 if part_age>=60





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














