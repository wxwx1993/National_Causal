* Written by R;
*  write.foreign(national_merged2016, datafile = "/nfs/home/X/xwu/shared_space/ci3_nsaph/XiaoWu/National_Causal/data2016/national_merged2016.csv",  ;

DATA  national2016 ;
LENGTH
 zip $ 5
 dual $ 1
 statecode $ 2
 region $ 9
;

INFILE  "/nfs/home/X/xwu/shared_space/ci3_nsaph/XiaoWu/National_Causal/data2016_temp/national_merged2016.csv" 
     DSD 
     LRECL= 324 ;
INPUT
 year
 zip
 sex
 race
 age
 dual
 entry_age_break
 statecode
 followup_year
 followup_year_plus_one
 dead
 pm25_ensemble
 mean_bmi
 smoke_rate
 hispanic
 pct_blk
 medhouseholdincome
 medianhousevalue
 poverty
 education
 popdensity
 pct_owner_occ
 summer_tmmx
 winter_tmmx
 summer_rmax
 winter_rmax
 region $ 
;
RUN;

/****************************************************/
/*full analysis*/
ODS HTML FILE='national_main_2016.html' path='/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/Code2016_temp/';

PROC phreg data=national2016;
	class region entry_age_break year/ ref=first;
	model (followup_year,followup_year_plus_one)* dead(0) = pm25_ensemble mean_bmi smoke_rate 
hispanic pct_blk medhouseholdincome medianhousevalue poverty 
education popdensity pct_owner_occ summer_tmmx winter_tmmx summer_rmax winter_rmax region year/rl ties=efron;
	strata entry_age_break sex race dual;
	title "with year and temp;main analysis 2000-2016";
run;

ODS HTML CLOSE;
