**reshaping mixing_module_dataset
**Nb these data not publically available


cd "C:\Users\matt_\Google Drive\Research\COVID\Pop council"

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