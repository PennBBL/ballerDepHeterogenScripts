#This script, constructed 1/15/2019, will run Azeez's cwasmdmr
#It will use the 27 parcellations (functionally defined from nback

source("/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/cwasmdmr/cwasmdmr_NEW_FROM_AZEEZ_20190109.R")
#inputs
#modelfile: csv with the subjectID, and all covariates
#formula: ~Hydra_k3 + ...
#subjectlist: contains paths to concatenated time series in same order as model file
#factor2perm: Hydra_k3
#nperm: leave as default 499

#modelfile <- '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3_order_by_scanid.csv'
#form <- '~Hydra_k3+age_in_years+sex'
#subject_list <- '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_n312_concat_rest_nback_idemo_4mm_27parcels.csv'
#factor2perm <- 'Hydra_k3'
#mf <- read.csv(modelfile, header = TRUE)
#sl <- read.csv(subject_list, header = FALSE)
#cwas_output <- cwas_mdmr(modelfile = modelfile, formula = form, subjectlist = subject_list, factor2perm = factor2perm, nperm = 499)
cwas_output <- cwas_mdmr(modelfile = '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3_order_by_scanid.csv',
                         subjectlist = '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_n312_concat_rest_nback_idemo_4mm_27parcels.csv',
                         formula = ~Hydra_k3+age_in_years+sex,
                         factor2perm = 'Hydra_k3',
                         nperm = 15000)
write.csv(x = cwas_output, file = "/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/cwas_output_from_R_script_20190122_pandz.csv")

