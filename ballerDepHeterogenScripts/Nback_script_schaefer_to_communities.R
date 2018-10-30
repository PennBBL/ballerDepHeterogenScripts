source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181023_schaefer400_and_fc_communities/fc_and_mdmr_functions_NEW_NBACK.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'


#This file was made by the script on Erica's local machine: PrepForNetworks_20180706.
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_FC_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 


### CHanged as of 10/24/2018 ####
#n = 325 , rest scan and T1 qa only
sample_qa <- readRDS('/data/jux/BBL/projects/ballerDepHeterogen/data/subset_with_T1_FC_and_dem_with_clusters.rds')
#read in Nback qa
nback_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/nback/n1601_NbackConnectQAData_20170718.csv'))
#merge , new n = 312
sample<- merge(sample_qa, nback_qa[which(nback_qa$nbackRelMeanRMSMotionExclude == 0),], by = c("bblid", "scanid"))

#fc_sample_qa_newkey <- fc_sample_qa_newkey[1:5,]

#com7 and 17 - just get the comunity info, send straight to RDS for working it out in local environment

  nback_net_schaefer_r400 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'nback')
  nback_net_schaefer_r400_matrix <- list_to_matrix(list = nback_net_schaefer_r400)
  nback_net_schaefer_r400_average <- apply(nback_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
  community_info_r400_com7 <-  get_schaefer_node_info(resolution = 400,num_com = 7)
  community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)
  saveRDS(object = community_info_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer400_com7_mapping.RDS')
  saveRDS(object = community_info_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer400_com17_mapping.RDS')


nback_mean_by_community <- lapply(nback_net_schaefer_r400,function(netmat) get_community_net_nback(netmat,community_info_r400_com7$CommunityName,community_info_r400_com7$CommunityAffiliation))

#------- Has not been changed------
  #within/between
  com_net_names_com7 <- get_community_net_names(community_info_r400_com7$CommunityName)  
  com_net_r400_com7<-lapply(nback_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com7$CommunityName,community_info_r400_com7$CommunityAffiliation))
  plot_com_r400_com7 <- better_levelplot(adj = com_net_r400_com7[[1]],node_names = community_info_r400_com7$CommunityName,title = 'Community matrix r400, com7') 
  com_net_flat_r400_com7<-t(sapply(com_net_r400_com7,function(net) net[upper.tri(net,diag = TRUE)])) 
  colnames(com_net_flat_r400_com7) <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)] 
  com_net_names_flat_r400_com7 <- com_net_names_com7[upper.tri(com_net_names_com7,diag = TRUE)] 
  com_net_flat_r400_com7 <- cbind(com_net_flat_r400_com7,sample)


#com17
com_net_flat_r400_list_com17<- lapply(list_abs_and_weighted, function(sample){
  nback_net_schaefer_r400 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'nback')
  nback_net_schaefer_r400_matrix <- list_to_matrix(list = nback_net_schaefer_r400)
  nback_net_schaefer_r400_average <- apply(nback_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
  community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)
 
  #within/between
  com_net_names_com17 <- get_community_net_names(community_info_r400_com17$CommunityName)
  com_net_r400_com17<-lapply(nback_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com17$CommunityName,community_info_r400_com17$CommunityAffiliation))
  plot_com_r400_com17 <- better_levelplot(adj = com_net_r400_com17[[1]],node_names = community_info_r400_com17$CommunityName,title = 'Community matrix r400, com17')
  com_net_flat_r400_com17<-t(sapply(com_net_r400_com17,function(net) net[upper.tri(net,diag = TRUE)]))
  colnames(com_net_flat_r400_com17) <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
  com_net_names_flat_r400_com17 <- com_net_names_com17[upper.tri(com_net_names_com17,diag = TRUE)]
  com_net_flat_r400_com17 <- cbind(com_net_flat_r400_com17,sample)
  
})  

#extract individual data frames from concatenated matrices
com_net_flat_abs_r400_com7 <- com_net_flat_r400_list_com7[[1]]
com_net_flat_weighted_r400_com7 <- com_net_flat_r400_list_com7[[2]]
com_net_flat_abs_r400_com17 <- com_net_flat_r400_list_com17[[1]]
com_net_flat_weighted_r400_com17 <- com_net_flat_r400_list_com17[[2]]

#for standard, just restbold
sample <- nback_sample_qa_newkey

nback_net_schaefer_r200 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 200,modality = 'nback')
nback_net_schaefer_r400 <- get_net_from_sample(sample = sample,parcellation = 'schaefer',resolution = 400,modality = 'nback')

nback_net_schaefer_r200_matrix <- list_to_matrix(list = nback_net_schaefer_r200)
nback_net_schaefer_r400_matrix <- list_to_matrix(list = nback_net_schaefer_r400)

nback_net_schaefer_r200_average <- apply(nback_net_schaefer_r200_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
nback_net_schaefer_r400_average <- apply(nback_net_schaefer_r400_matrix,c(2,3),function(adj) mean(adj,na.rm=T))

community_info_r200_com7 <-  get_schaefer_node_info(resolution = 200,num_com = 7)
community_info_r200_com17 <-  get_schaefer_node_info(resolution = 200,num_com = 17)
community_info_r400_com7 <-  get_schaefer_node_info(resolution = 400,num_com = 7)
community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)
plot <- better_levelplot(adj = nback_net_schaefer_r200_average,node_names = NULL,title = 'Community Matrix, r200, com7, no names')
plot
plot <- better_levelplot(adj = nback_net_schaefer_r200_average,node_names = community_info_r200_com7$NodeName$V1,title = 'Community Matrix, r200, com7')
plot

### 2.  calculate within/between community ###

com_net_names_com7 <- get_community_net_names(community_info_r200_com7$CommunityName)
com_net_names_com17 <- get_community_net_names(community_info_r200_com17$CommunityName)

com_net_r200_com7<-lapply(nback_net_schaefer_r200,function(netmat) get_community_net(netmat,community_info_r200_com7$CommunityName,community_info_r200_com7$CommunityAffiliation))
com_net_r200_com17<-lapply(nback_net_schaefer_r200,function(netmat) get_community_net(netmat,community_info_r200_com17$CommunityName,community_info_r200_com17$CommunityAffiliation))
com_net_r400_com7<-lapply(nback_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com7$CommunityName,community_info_r400_com7$CommunityAffiliation))
com_net_r400_com17<-lapply(nback_net_schaefer_r400,function(netmat) get_community_net(netmat,community_info_r400_com17$CommunityName,community_info_r400_com17$CommunityAffiliation))


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
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_nback_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 
#nback_sample_qa_newkey <- readRDS('~/from_cedric/subset_with_T1_nback_and_dem_with_clusters.rds')

#('/data/jux/BBL/projects/prsConnectivity/result/sample_qa_newkey.RData')
#test_sample <- nback_sample_qa_newkey[1:10,]
#test_sample <- nback_sample_qa_newkey
#test <- get_net_from_sample(sample = test_sample,parcellation = 'schaefer',resolution = 200,modality = 'nback')
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
#nback_s200_com7_out<-prs_within_between_com_analysis(nback_s200_nback_sample_net,nback_sample_qa_newkey_chip,'schaefer',200,7,nback_sample_qa_key)
#nback_s200_com17_out<-prs_within_between_com_analysis(nback_s200_nback_sample_net,nback_sample_qa_newkey_chip,'schaefer',200,17, nback_sample_qa_key)
#nback_s400_com7_out<-prs_within_between_com_analysis(nback_s400_nback_sample_net,nback_sample_qa_newkey_chip,'schaefer',400,7,nback_sample_qa_key)
#nback_s400_com17_out<-prs_within_between_com_analysis(nback_s400_nback_sample_net,nback_sample_qa_newkey_chip,'schaefer',400,17,nback_sample_qa_key)

#write.csv(com_net_flat, '~/from_cedric/com_net_flat_r200_com7.csv')
write.csv(com_net_flat_r200_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_nback/nback_r200_com7/nback_r200_com7.csv')
write.csv(com_net_flat_r200_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_nback/nback_r200_com17/nback_r200_com17.csv')
write.csv(com_net_flat_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_nback/nback_r400_com7/nback_r400_com7.csv')
write.csv(com_net_flat_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_nback/nback_r400_com17/nback_r400_com17.csv')
#write.csv(com_net_flat_abs_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_abs_r400_com7/fc_abs_r400_com7.csv')
#write.csv(com_net_flat_weighted_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_weighted_r400_com7/fc_weighted_r400_com7.csv')
#write.csv(com_net_flat_abs_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_abs_r400_com17/fc_abs_r400_com17.csv')
#write.csv(com_net_flat_weighted_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_nback_fc/concatenate_rest_nback_idemo/fc_weighted_r400_com17/fc_weighted_r400_com17.csv')
