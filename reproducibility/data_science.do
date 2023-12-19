cd "E:/data_science_reproducibility/" 

global mainoutcomes z_midline z_endline
global covariates z_baseline age female father_jhs mother_jhs ses1 class_size t_female t_age t_exp t_4yrcoll t_cert t_major_math t_rank
global t_covariates t_female t_age2 t_4yrcoll t_rank t_cert t_exp t_major_math
global sec_outcomes_stu_end e_dropout e_self_concept e_anxiety e_intr_motiv e_instrument_motiv e_time_math 
global sec_outcomes_tea_end t_practice_end t_care_end t_mgmt_end t_comm_end
global sec_outcomes_tea2_end z_t_math_end t_intr_mot_end t_prosoc_mot_end e_t_math_t_direct e_t_math_t_active e_t_math_fixed e_t_math_rules e_t_math_inquiry	

use "E:/data_science_reproducibility/NTTP---except-Table-5.dta", replace
log using data_science.log, replace


/**********/
/*ANALYSES*/
/**********/

/**************************************************************************/
/*TABLES 1 AND 2 ֠IMPACTS ON STUDENT ACHIEVEMENT (AT MIDLINE AND ENDLINE)*/
/**************************************************************************/

/*compare GUOPEI (treatment == 2) with GUOPEI + FOLLOW-UP (treatment == 1) with CONTROL */
foreach var of varlist $mainoutcomes {
	regress `var' i.treatment z_baseline i.blockZ if spillover == 0, vce(cluster schid)
	lincom 1.treatment - 2.treatment
	local tstat= r(estimate)/r(se)
    local pval = tprob(r(df), abs(`tstat'))
    outreg2 using "3ie_guopei_control_stu_zscore.xls", adds(GUOPEI + FOLLOW-UP vs. guopei, r(estimate), p-val,`pval') dec(3) excel append		
	
	regress `var' i.treatment $covariates i.blockZ if spillover == 0, vce(cluster schid)
	lincom 1.treatment - 2.treatment
	local tstat=r(estimate)/r(se)
    local pval = tprob(r(df), abs(`tstat'))
    outreg2 using "3ie_guopei_control_stu_zscore.xls", adds(GUOPEI + FOLLOW-UP vs. guopei, r(estimate), p-val,`pval') dec(3) excel append		
}

/*compare in-person (in NTTP only or NTTP + follow-up) VERSUS no-in-person (in NTTP only or NTTP + follow-up)*/
foreach var of varlist $mainoutcomes {
	regress `var' i.in_person z_baseline i.blockZZ if treatment!= 0 & spillover == 0, vce(cluster schid)
	outreg2 using "3ie_inpersonYN_stu_zscore.xls", dec(3) excel append

	regress `var' i.in_person $covariates i.blockZZ if treatment!= 0 & spillover == 0, vce(cluster schid)
	outreg2 using "3ie_inpersonYN_stu_zscore_cov.xls", dec(3) excel append
}



/*compare in-person with control*/
foreach var of varlist $mainoutcomes {
	regress `var' i.in_person z_baseline i.blockZ if spillover == 0 & ((treatment == 1 & in_person == 1) | (treatment == 2 & in_person == 1) | treatment == 0), vce(cluster schid)
	outreg2 using "3ie_inpersonY_control_stu_zscore.xls", dec(3) excel append

	regress `var' i.in_person $covariates i.blockZ if spillover == 0 & ((treatment == 1 & in_person == 1) | (treatment == 2 & in_person == 1) | treatment == 0), vce(cluster schid)
	outreg2 using "3ie_inpersonY_control_stu_zscore_cov.xls", dec(3) excel append
}

/* Compare in-person (in NTTP only or NTTP + follow-up) VERSUS no-in-person (in NTTP only or NTTP + follow-up) */
foreach var of varlist $mainoutcomes {
    regress `var' i.in_person z_baseline i.blockZZ if treatment != 0 & spillover == 0, vce(cluster schid)
    estimates store model1

    regress `var' i.in_person $covariates i.blockZZ if treatment != 0 & spillover == 0, vce(cluster schid)
    estimates store model2

    /* Save the regression results in separate models and then create the table */
    eststo clear
    eststo model1
    eststo model2
    esttab, ///
    cells("b(fmt(%9.3f)) se(fmt(%9.3f))") label unstack noobs ///
    mlabels("PD" "PD + Follow-up") ///
    stats(coef se) replace
    
    /* Repeat the process for different comparisons */
}

/* Compare in-person with control */
foreach var of varlist $mainoutcomes {
    regress `var' i.in_person z_baseline i.blockZ if spillover == 0 & ((treatment == 1 & in_person == 1) | (treatment == 2 & in_person == 1) | treatment == 0), vce(cluster schid)
    estimates store model3

    regress `var' i.in_person $covariates i.blockZ if spillover == 0 & ((treatment == 1 & in_person == 1) | (treatment == 2 & in_person == 1) | treatment == 0), vce(cluster schid)
    estimates store model4

    /* Save the regression results in separate models and then create the table */
    eststo clear
    eststo model3
    eststo model4
    esttab, ///
    cells("b(fmt(%9.3f)) se(fmt(%9.3f))") label unstack noobs ///
    mlabels("PD" "PD + Follow-up") ///
    stats(coef se) replace
    
    /* Repeat the process for different comparisons */
}
/*********************************************************************************************/
/*TABLE 6 ֠IMPACTS ON STUDENT ACHIEVEMENT BY STUDENT AND TEACHER GROUP TERCILES (AT ENDLINE)*/
/*TABLE 7 - IMPACTS ON STUDENT ACHIEVEMENT BY TEACHER CHARACTERISTICS (AT ENDLINE)			 */
/*********************************************************************************************/

foreach het of varlist ses_base_terc math_base_terc t_training_hours_terc t_female t_4yrcoll t_major_math {
	/*compare GUOPEI (treatment == 2) with GUOPEI + FOLLOW-UP (treatment == 1) with CONTROL */
	foreach var of varlist z_endline {
		regress `var' i.treatment##i.`het' $covariates i.blockZ if spillover == 0, vce(cluster schid)
		outreg2 using "3ie_guopei_control_stu_zscore_`het'.xls", dec(3) excel append		
	}

	/*compare in-person (in NTTP only or NTTP + follow-up) VERSUS no-in-person (in NTTP only or NTTP + follow-up)*/
	foreach var of varlist z_endline {
		regress `var' i.in_person##i.`het' $covariates i.blockZZ if treatment!= 0 & spillover == 0, vce(cluster schid)
		outreg2 using "3ie_inpersonYN_stu_zscore_`het'.xls", dec(3) excel append
	}

	/*compare in-person with control*/
	foreach var of varlist z_endline {
		regress `var' i.in_person##i.`het' $covariates i.blockZ if spillover == 0 & ((treatment == 1 & in_person == 1) | (treatment == 2 & in_person == 1) | treatment == 0), vce(cluster schid)
		outreg2 using "3ie_inpersonY_control_stu_zscore_`het'.xls", dec(3) excel append
	}
}


	generate het1 = t_female
	generate treatment_sch1_het1 = treatment_sch1*t_female
	generate treatment_sch2_het1 = treatment_sch2*t_female
	
	generate het2 = t_4yrcoll
	generate treatment_sch1_het2 = treatment_sch1*t_4yrcoll
	generate treatment_sch2_het2 = treatment_sch2*t_4yrcoll
	
	generate het3 = t_major_math
	generate treatment_sch1_het3 = treatment_sch1*t_major_math
	generate treatment_sch2_het3 = treatment_sch2*t_major_math

	generate het4 = 1 - t_female
	generate treatment_sch1_het4 = treatment_sch1*het4
	generate treatment_sch2_het4 = treatment_sch2*het4
	
	generate het5 = 1 - t_4yrcoll
	generate treatment_sch1_het5 = treatment_sch1*het5
	generate treatment_sch2_het5 = treatment_sch2*het5
	
	generate het6 = 1 - t_major_math
	generate treatment_sch1_het6 = treatment_sch1*het6
	generate treatment_sch2_het6 = treatment_sch2*het6
	
	generate in_person_sch_het1 = in_person_sch*t_female
	generate in_person_sch_het2 = in_person_sch*t_4yrcoll
	generate in_person_sch_het3 = in_person_sch*t_major_math
	generate in_person_sch_het4 = in_person_sch*het4
	generate in_person_sch_het5 = in_person_sch*het5
	generate in_person_sch_het6 = in_person_sch*het6
	
/*
	generate het1 = t_female
	generate treatment_sch1_het1 = treatment_sch1*t_female
	generate treatment_sch2_het1 = treatment_sch2*t_female
	
	generate het2 = t_4yrcoll
	generate treatment_sch1_het2 = treatment_sch1*t_4yrcoll
	generate treatment_sch2_het2 = treatment_sch2*t_4yrcoll
	
	generate het3 = t_major_math
	generate treatment_sch1_het3 = treatment_sch1*t_major_math
	generate treatment_sch2_het3 = treatment_sch2*t_major_math

	generate het4 = 1 - t_female
	generate treatment_sch1_het4 = treatment_sch1*het4
	generate treatment_sch2_het4 = treatment_sch2*het4
	
	generate het5 = 1 - t_4yrcoll
	generate treatment_sch1_het5 = treatment_sch1*het5
	generate treatment_sch2_het5 = treatment_sch2*het5
	
	generate het6 = 1 - t_major_math
	generate treatment_sch1_het6 = treatment_sch1*het6
	generate treatment_sch2_het6 = treatment_sch2*het6
	
	generate in_person_sch_het1 = in_person_sch*t_female
	generate in_person_sch_het2 = in_person_sch*t_4yrcoll
	generate in_person_sch_het3 = in_person_sch*t_major_math
	generate in_person_sch_het4 = in_person_sch*het4
	generate in_person_sch_het5 = in_person_sch*het5
	generate in_person_sch_het6 = in_person_sch*het6
	
* multiple hypothesis adjustment - heterogeneous effects on endline math scores
fsdrm (z_endline treatment_sch1 treatment_sch2 treatment_sch1_het1 treatment_sch2_het1 het1 $covariates i.blockZ) ///  
(z_endline treatment_sch1 treatment_sch2 treatment_sch1_het2 treatment_sch2_het2 het2 $covariates i.blockZ) ///
(z_endline treatment_sch1 treatment_sch2 treatment_sch1_het3 treatment_sch2_het3 het3 $covariates i.blockZ) ///
(z_endline treatment_sch1 treatment_sch2 treatment_sch1_het4 treatment_sch2_het4 het4 $covariates i.blockZ) ///
(z_endline treatment_sch1 treatment_sch2 treatment_sch1_het5 treatment_sch2_het5 het5 $covariates i.blockZ) ///
(z_endline treatment_sch1 treatment_sch2 treatment_sch1_het6 treatment_sch2_het6 het6 $covariates i.blockZ) ///
if spillover == 0, testvar(treatment_sch1 treatment_sch2) reps(1000) cluster(schid) permstrata(blockZ) export("end_PD_het_fsdrm_pvs")

fsdrm (z_endline in_person_sch in_person_sch_het1 het1 $covariates i.blockZZ) ///
(z_endline in_person_sch in_person_sch_het2 het2 $covariates i.blockZZ) ///
(z_endline in_person_sch in_person_sch_het3 het3 $covariates i.blockZZ) ///
(z_endline in_person_sch in_person_sch_het4 het4 $covariates i.blockZZ) ///
(z_endline in_person_sch in_person_sch_het5 het5 $covariates i.blockZZ) ///
(z_endline in_person_sch in_person_sch_het6 het6 $covariates i.blockZZ) ///
if treatment!= 0 & spillover == 0, testvar(in_person_sch) reps(1000) cluster(schid) permstrata(blockZZ) export("end_inperson_het_fsdrm_pvs")

fsdrm (z_endline in_person_sch in_person_sch_het1 het1 $covariates i.blockZ) ///
(z_endline in_person_sch in_person_sch_het2 het2 $covariates  i.blockZ) ///
(z_endline in_person_sch in_person_sch_het3 het3 $covariates  i.blockZ) ///
(z_endline in_person_sch in_person_sch_het4 het4 $covariates  i.blockZ) ///
(z_endline in_person_sch in_person_sch_het5 het5 $covariates  i.blockZ) ///
(z_endline in_person_sch in_person_sch_het6 het6 $covariates  i.blockZ) ///
if spillover == 0 & ((treatment == 1 & in_person_sch == 1) | (treatment == 2 & in_person_sch == 1) | treatment == 0), testvar(in_person_sch) reps(1000) cluster(schid) permstrata(blockZ) export("end_inperson_cont_het_fsdrm_pvs")

*/

log end
