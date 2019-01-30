source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/fc_communities_three_scans/fc_and_mdmr_functions.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'


#This file was made by the script on Erica's local machine: PrepForNetworks_20180706.
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_FC_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 

#n = 325 , rest scan and T1 qa only
fc_sample_qa_newkey <- readRDS('/data/jux/BBL/projects/ballerDepHeterogen/data/subset_with_T1_FC_and_dem_with_clusters.rds')



#make matrices and append

#make matrix, starts out empty
As <- NULL

#################################
###Maing files for use Cedrics' script###
#################################

#get abs and weighted lists for concatenated scans
list_abs_and_weighted <- get_abs_and_weighted_sample(sample_from_HYDRA = fc_sample_qa_newkey)

sample_abs <- list_abs_and_weighted[[1]] #n = 305
sample_weighted <- list_abs_and_weighted[[2]] #n = 312


sample <- sample_weighted

#covariates for cedric's scripts
Xs <- data.frame(sample$bblid, 
                 sample$Hydra_k3, 
                 sample$age_in_years, 
                 sample$sex,
                 sample$race_binarized,
                 sample$medu1)
names(Xs) <- c("bblid", "Hydra_k3", "age_in_years", "sex", "race_binarized", "medu1")

fc_net_schaefer_r400_concat <- get_net_from_sample_concat(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'fc')
As <- list_to_matrix(list = fc_net_schaefer_r400_concat)

save(Xs, As, sample, file = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/sparse_connectivity_with_cedrics_stuff_20180904/XsandAs_weighted_n312_schaefer400_com7.RData")