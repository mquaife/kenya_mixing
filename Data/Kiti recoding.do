cd "C:\Users\matt_\Google Drive\Research\COVID\Pop council\kenyamix"

import delimited "Data\kiti mixing data.CSV", clear

*Kiti age cats
*1   2   3    4     5     6
*<1 1-5 6-15 16-19 20-49 50+
*0.5 2.5 10   17.5  35   51

gen age_class_part_2=age_class_part
recode age_class_part_2 (1=0.5) (2=2.5) (3 =10.5) (4=17.5) (5=35.5) (6=50.5)
gen part_age_exact=age_class_part_2

*gen part_age=age_class_part_2

gen part_id=csid

gen     part_age_est_min=.
replace part_age_est_min=0 if  age_class_part==1
replace part_age_est_min=1 if  age_class_part==2
replace part_age_est_min=6 if  age_class_part==3
replace part_age_est_min=16 if age_class_part==4
replace part_age_est_min=20 if age_class_part==5
replace part_age_est_min=50 if age_class_part==6

gen     part_age_est_max=.
replace part_age_est_max=1 if  age_class_part==1
replace part_age_est_max=5 if  age_class_part==2
replace part_age_est_max=15 if age_class_part==3
replace part_age_est_max=19 if age_class_part==4
replace part_age_est_max=49 if age_class_part==5
replace part_age_est_max=80 if age_class_part==6

gen part_age_cat=""
replace part_age_cat="0-18" if 	age_class_part<=4
replace part_age_cat="19-49" if age_class_part==5
replace part_age_cat="50-80" if age_class_part==6

gen part_age_group=part_age_cat
*Kiti cnt age cats
*Infant Pre-School Primary Seconday Adult Older 

gen cnt_age=.
replace cnt_age=0.5 if age_class_cont=="Infant"
replace cnt_age=2.5 if age_class_cont=="Pre-School" 
replace cnt_age=10.5 if age_class_cont=="Primary"
replace cnt_age=17.5 if age_class_cont=="Secondary"
replace cnt_age=35.5 if age_class_cont=="Adult"
replace cnt_age=50.5 if age_class_cont=="Older"


gen cnt_age_est_min=.
replace cnt_age_est_min=0 if age_class_cont=="Infant"
replace cnt_age_est_min=1 if age_class_cont=="Pre-School"
replace cnt_age_est_min=6 if age_class_cont=="Primary"
replace cnt_age_est_min=16 if age_class_cont=="Secondary"
replace cnt_age_est_min=20 if age_class_cont=="Adult"
replace cnt_age_est_min=50 if age_class_cont=="Older"

gen cnt_age_est_max=.
replace cnt_age_est_max=1 if age_class_cont=="Infant"
replace cnt_age_est_max=5 if age_class_cont=="Pre-School"
replace cnt_age_est_max=15 if age_class_cont=="Primary"
replace cnt_age_est_max=19 if age_class_cont=="Secondary"
replace cnt_age_est_max=49 if age_class_cont=="Adult"
replace cnt_age_est_max=80 if age_class_cont=="Older"

gen cnt_age_cat=""
replace cnt_age_cat="0-18" if  age_class_cont=="Infant"|age_class_cont=="Pre-School"|age_class_cont=="Primary"|age_class_cont=="Secondary"
replace cnt_age_cat="19-49" if age_class_cont=="Adult"
replace cnt_age_cat="50-80" if age_class_cont=="Older"
gen cnt_age_group=cnt_age_cat

gen cnt_age_exact=cnt_age

*gen cnt_age_cont=cnt_age
*drop cnt_age

*gen cnt_age=""
*replace cnt_age="0-18" if cnt_age_est_min<18
*replace cnt_age="19-49" if cnt_age_est_min>=18 & cnt_age_est_min<=49
*replace cnt_age="50-80" if cnt_age_est_min>49



gen country="Kenya"

export delimited using "Data\kiti_data_long.csv", replace
bysort csid: keep if _n==1
export delimited using "Data\kiti_data_wide_csv.csv", replace


import delimited "Data\kiti_data_long.csv", clear
keep if loc_stat==1
export delimited using "Data\kiti_data_long_urban.csv", replace
bysort csid: keep if _n==1
export delimited using "Data\kiti_data_wide_csv_urban.csv", replace

import delimited "Data\kiti_data_long.csv", clear
keep if loc_stat==0
export delimited using "Data\kiti_data_long_rural.csv", replace
bysort csid: keep if _n==1
export delimited using "Data\kiti_data_wide_csv_rural.csv", replace



