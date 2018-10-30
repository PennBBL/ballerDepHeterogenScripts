source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181023_schaefer400_and_fc_communities/fc_and_mdmr_functions_NEW_NBACK.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'


#This file was made by the script on Erica's local machine: PrepForNetworks_20180706.
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_FC_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 

#n = 325 , rest scan and T1 qa only
fc_sample_qa_newkey <- readRDS('/data/jux/BBL/projects/ballerDepHeterogen/data/subset_with_T1_FC_and_dem_with_clusters.rds')

#fc_sample_qa_newkey <- fc_sample_qa_newkey[1:5,]
#################################
###Maing files for use in MDMR###
#################################

#get abs and weighted lists for concatenated scans
list_abs_and_weighted <- get_abs_and_weighted_sample(sample_from_HYDRA = fc_sample_qa_newkey)

sample_abs <- list_abs_and_weighted[[1]] #n = 305
sample_weighted <- list_abs_and_weighted[[2]] #n = 312

# make sub samples for MDMR, which can't handle big files, and has to have everything as numeric

subsample_abs <- data.frame(sample_abs$bblid, sample_abs$scanid, sample_abs$age_in_years, sample_abs$sex, sample_abs$medu1, sample_abs$race_binarized, sample_abs$Hydra_k3)
names(subsample_abs) <- c("bblid", "scanid", "age_in_years", "sex", "medu1", "race_binarized", "Hydra_k3")
subsample_abs <- sapply(subsample_abs, as.numeric)

subsample_weighted <- data.frame(sample_weighted$bblid, sample_weighted$scanid, sample_weighted$age_in_years, sample_weighted$sex, sample_weighted$medu1, sample_weighted$race_binarized, sample_weighted$Hydra_k3)
names(subsample_weighted) <- c("bblid", "scanid", "age_in_years", "sex", "medu1", "race_binarized", "Hydra_k3")
subsample_weighted <- sapply(subsample_weighted, as.numeric)

abs_BBLID <- sample_abs[,1]
weighted_BBLID <- sample_weighted[,1]

abs_SCANID <- sample_abs[,2]
weighted_SCANID <- s ample_weighted[,2]

####prep filenames and save all samples to subject list folders
num_sample_abs <-nrow(sample_abs)
num_sample_weighted <- nrow(sample_weighted)

abs_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_abs_n", num_sample_abs, ".csv")
weighted_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n", num_sample_weighted, ".csv")
abs_filename_with_headers <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_abs_n", num_sample_abs, "_with_headers.csv")
weighted_filename_with_headers <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n", num_sample_weighted, "_with_headers.csv")
abs_filename_with_headers_just_covs_and_Hydra_k3 <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_abs_n", num_sample_abs, "_with_headers_just_covs_and_Hydra_k3.csv")
weighted_filename_with_headers_just_covs_and_Hydra_k3 <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n", num_sample_weighted, "_with_headers_just_covs_and_Hydra_k3.csv")


abs_BBLID_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_abs_BBLID_n", num_sample_abs, ".csv")
weighted_BBLID_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_BBLID_n", num_sample_weighted, ".csv")

abs_SCANID_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_abs_SCANID_n", num_sample_abs, ".csv")
weighted_SCANID_filename <- paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_SCANID_n", num_sample_weighted, ".csv")

###write whole csvs
write.table(sample_abs, file = abs_filename, row.names = FALSE, col.names = FALSE)
write.table(sample_weighted, file = weighted_filename, row.names = FALSE, col.names = FALSE)
write.table(sample_abs, file = abs_filename_with_headers, row.names = TRUE, col.names = TRUE, sep = ",", quote = FALSE)
write.table(sample_weighted, file = weighted_filename_with_headers, row.names = TRUE, col.names = TRUE, sep = ",", quote = FALSE)
write.table(subsample_abs, file = abs_filename_with_headers_just_covs_and_Hydra_k3, row.names =TRUE, col.names = TRUE, sep = ",", quote = FALSE)
write.table(subsample_weighted, file = weighted_filename_with_headers_just_covs_and_Hydra_k3, row.names = TRUE, col.names = TRUE, sep = ",", quote = FALSE)


#write BBLID
write.table(abs_BBLID, file = abs_BBLID_filename, row.names = FALSE, col.names = FALSE)
write.table(weighted_BBLID, file = weighted_BBLID_filename, row.names = FALSE, col.names = FALSE)

#write SCANSID
write.table(abs_SCANID, file = abs_SCANID_filename, row.names = FALSE, col.names = FALSE)
write.table(weighted_SCANID, file = weighted_SCANID_filename, row.names = FALSE, col.names = FALSE)
################

#make a list of ALL samples together for use in lapply later
list_all_samples <- list(fc_sample_qa_newkey, sample_abs, sample_weighted)
#-----------------
#('/data/jux/BBL/projects/prsConnectivity/result/sample_qa_newkey.RData')
#sample <- fc_sample_qa_newkey[1:10,]


#for concatenated scans
  
#com7
sample <- sample_abs
com_net_flat_r400_concat_list_com7<- lapply(list_abs_and_weighted, function(sample){
  fc_net_schaefer_r400_concat <- get_net_from_sample_concat(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'fc')
  fc_net_schaefer_r400_matrix <- list_to_matrix(list = fc_net_schaefer_r400_concat)
  fc_net_schaefer_r400_average <- apply(fc_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
  community_info_r400_com7 <-  get_schaefer_node_info(resolution = 400,num_com = 7)
  
  #within/between
  com_net_names_com7 <- get_community_net_names(community_info_r400_com7$CommunityName)  
  com_net_r400_com7<-lapply(fc_net_schaefer_r400_concat,function(netmat) get_community_net(netmat,community_info_r400_com7$CommunityName,community_info_r400_com7$CommunityAffiliation))
  plot_com_r400_com7 <- better_levelplot(adj = com_net_r400_com7[[1]],node_names = community_info_r400_com7$CommunityName,title = 'Community matrix r400, com7') 
  com_net_flat_r400_com7<-t(sapply(com_net_r400_com7,function(net) net[upper.tri(net,diag = TRUE)])) 
  colnames(com_net_flat_r400_com7) <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)] 
  com_net_names_flat_r400_com7 <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)] 
  com_net_flat_r400_com7 <- cbind(com_net_flat_r400_com7,sample)
  
}) 

#com17
com_net_flat_r400_concat_list_com17<- lapply(list_abs_and_weighted, function(sample){
  fc_net_schaefer_r400_concat <- get_net_from_sample_concat(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'fc')
  fc_net_schaefer_r400_matrix <- list_to_matrix(list = fc_net_schaefer_r400_concat)
  fc_net_schaefer_r400_average <- apply(fc_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
  community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)
 
  #within/between
  com_net_names_com17 <- get_community_net_names(community_info_r400_com17$CommunityName)
  com_net_r400_com17<-lapply(fc_net_schaefer_r400_concat,function(netmat) get_community_net(netmat,community_info_r400_com17$CommunityName,community_info_r400_com17$CommunityAffiliation))
  plot_com_r400_com17 <- better_levelplot(adj = com_net_r400_com17[[1]],node_names = community_info_r400_com17$CommunityName,title = 'Community matrix r400, com17')
  com_net_flat_r400_com17<-t(sapply(com_net_r400_com17,function(net) net[upper.tri(net,diag = TRUE)]))
  colnames(com_net_flat_r400_com17) <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
  com_net_names_flat_r400_com17 <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
  com_net_flat_r400_com17 <- cbind(com_net_flat_r400_com17,sample)
  
})  

#extract individual data frames from concatenated matrices
com_net_flat_concat_abs_r400_com7 <- com_net_flat_r400_concat_list_com7[[1]]
com_net_flat_concat_weighted_r400_com7 <- com_net_flat_r400_concat_list_com7[[2]]
com_net_flat_concat_abs_r400_com17 <- com_net_flat_r400_concat_list_com17[[1]]
com_net_flat_concat_weighted_r400_com17 <- com_net_flat_r400_concat_list_com17[[2]]

#for standard, just restbold
sample <- fc_sample_qa_newkey

fc_net_schaefer_r200 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 200,modality = 'fc')
fc_net_schaefer_r400 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'fc')

fc_net_schaefer_r200_matrix <- list_to_matrix(list = fc_net_schaefer_r200)
fc_net_schaefer_r400_matrix <- list_to_matrix(list = fc_net_schaefer_r400)

fc_net_schaefer_r200_average <- apply(fc_net_schaefer_r200_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
fc_net_schaefer_r400_average <- apply(fc_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))

community_info_r200_com7 <-  get_schaefer_node_info(resolution = 200,num_com = 7)
community_info_r200_com17 <-  get_schaefer_node_info(resolution = 200,num_com = 17)
community_info_r400_com7 <-  get_schaefer_node_info(resolution = 400,num_com = 7)
community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)
plot <- better_levelplot(adj = fc_net_schaefer_r200_average,node_names = NULL,title = 'Community Matrix, r200, com7, no names')
plot
plot <- better_levelplot(adj = fc_net_schaefer_r200_average,node_names = community_info_r200_com7$NodeName$V1,title = 'Community Matrix, r200, com7')
plot

### 2.  calculate within/between community ###

com_net_names_com7 <- get_community_net_names(community_info_r200_com7$CommunityName)
com_net_names_com17 <- get_community_net_names(community_info_r200_com17$CommunityName)

com_net_r200_com7<-lapply(fc_net_schaefer_r200,function(netmat) get_community_net(netmat,community_info_r200_com7$CommunityName,community_info_r200_com7$CommunityAffiliation))
com_net_r200_com17<-lapply(fc_net_schaefer_r200,function(netmat) get_community_net(netmat,community_info_r200_com17$CommunityName,community_info_r200_com17$CommunityAffiliation))
com_net_r400_com7<-lapply(fc_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com7$CommunityName,community_info_r400_com7$CommunityAffiliation))
com_net_r400_com17<-lapply(fc_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com17$CommunityName,community_info_r400_com17$CommunityAffiliation))


plot_com_r200_com7 <- better_levelplot(adj = com_net_r200_com7[[1]],node_names = community_info_r200_com7$CommunityName,title = 'Community matrix r200, com7')
plot_com_r200_com17 <- better_levelplot(adj = com_net_r200_com17[[1]],node_names = community_info_r200_com17$CommunityName,title = 'Community matrix r200, com17')
plot_com_r400_com7 <- better_levelplot(adj = com_net_r400_com7[[1]],node_names = community_info_r400_com7$CommunityName,title = 'Community matrix r400, com7')
plot_com_r400_com17 <- better_levelplot(adj = com_net_r400_com17[[1]],node_names = community_info_r400_com17$CommunityName,title = 'Community matrix r400, com17')

com_net_flat_r200_com7<-t(sapply(com_net_r200_com7,function(net) net[upper.tri(net,diag = TRUE)]))
com_net_flat_r200_com17<-t(sapply(com_net_r200_com17,function(net) net[upper.tri(net,diag = TRUE)]))
com_net_flat_r400_com7<-t(sapply(com_net_r400_com7,function(net) net[upper.tri(net,diag = TRUE)]))
com_net_flat_r400_com17<-t(sapply(com_net_r400_com17,function(net) net[upper.tri(net,diag = TRUE)]))

colnames(com_net_flat_r200_com7) <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)]
colnames(com_net_flat_r200_com17) <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
colnames(com_net_flat_r400_com7) <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)]
colnames(com_net_flat_r400_com17) <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]

com_net_names_flat_r200_com7 <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)]
com_net_names_flat_r200_com17 <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
com_net_names_flat_r400_com7 <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)]
com_net_names_flat_r400_com17 <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]

com_net_flat_r200_com7 <- cbind(com_net_flat_r200_com7,sample)
com_net_flat_r200_com17 <- cbind(com_net_flat_r200_com17,sample)
com_net_flat_r400_com7 <- cbind(com_net_flat_r400_com7,sample)
com_net_flat_r400_com17 <- cbind(com_net_flat_r400_com17,sample)


#This file was made by the script on Erica's local machine: PrepForNetworks_20180706.
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_FC_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 
#fc_sample_qa_newkey <- readRDS('~/from_cedric/subset_with_T1_FC_and_dem_with_clusters.rds')

#('/data/jux/BBL/projects/prsConnectivity/result/sample_qa_newkey.RData')
#test_sample <- fc_sample_qa_newkey[1:10,]
#test_sample <- fc_sample_qa_newkey
#test <- get_net_from_sample(sample = test_sample,parcellation = 'schaefer',resolution = 200,modality = 'fc')
#test_matrix <- list_to_matrix(list = test)
#test_average <- apply(test_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
#community_info <-  get_schaefer_node_info(resolution = 200,num_com = 7)
#plot <- better_levelplot(adj = test_average,node_names = NULL,title = 'Erica\'s first matrix')
#plot
#plot <- better_levelplot(adj = test_average,node_names = community_info$NodeName$V1,title = 'Erica\'s first matrix with node names')
#plot

### 2.  calculate within/between community ###

#com_net_names <- get_community_net_names(community_info$CommunityName)
#com_net<-lapply(test,function(netmat) get_community_net(netmat,community_info$CommunityName,community_info$CommunityAffiliation))
#plot_com <- better_levelplot(adj = com_net[[1]],node_names = community_info$CommunityName,title = 'Erica\'s Community matrix')
#plot_com
#com_net_flat<-t(sapply(com_net,function(net) net[upper.tri(net,diag = TRUE)]))
#colnames(com_net_flat) <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
#com_net_names_flat <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
#com_net_flat <- cbind(com_net_flat,test_sample)

#### From cedric's stuff
#fc_s200_com7_out<-prs_within_between_com_analysis(fc_s200_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',200,7,fc_sample_qa_key)
#fc_s200_com17_out<-prs_within_between_com_analysis(fc_s200_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',200,17, fc_sample_qa_key)
#fc_s400_com7_out<-prs_within_between_com_analysis(fc_s400_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',400,7,fc_sample_qa_key)
#fc_s400_com17_out<-prs_within_between_com_analysis(fc_s400_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',400,17,fc_sample_qa_key)

#write.csv(com_net_flat, '~/from_cedric/com_net_flat_r200_com7.csv')
write.csv(com_net_flat_r200_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/fc_r200_com7/fc_r200_com7.csv')
write.csv(com_net_flat_r200_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/fc_r200_com17/fc_r200_com17.csv')
write.csv(com_net_flat_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/fc_r400_com7/fc_r400_com7.csv')
write.csv(com_net_flat_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/fc_r400_com17/fc_r400_com17.csv')
#write.csv(com_net_flat_concat_abs_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_concat_abs_r400_com7/fc_concat_abs_r400_com7.csv')
#write.csv(com_net_flat_concat_weighted_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_concat_weighted_r400_com7/fc_concat_weighted_r400_com7.csv')
#write.csv(com_net_flat_concat_abs_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_concat_abs_r400_com17/fc_concat_abs_r400_com17.csv')
#write.csv(com_net_flat_concat_weighted_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_concat_weighted_r400_com17/fc_concat_weighted_r400_com17.csv')
