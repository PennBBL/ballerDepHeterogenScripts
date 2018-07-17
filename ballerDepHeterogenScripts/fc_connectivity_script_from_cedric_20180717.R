source('/home/eballer/from_cedric/prs_source_code/prs_functions.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'


#This file was made by the script on Erica's local machine: PrepForNetworks_20180706.
#The output was : saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/subset_with_T1_FC_and_dem_with_clusters.rds")
#It was then transfered up to the cluser, and loaded below: 
fc_sample_qa_newkey <- readRDS('~/from_cedric/subset_with_T1_FC_and_dem_with_clusters.rds')

#('/data/jux/BBL/projects/prsConnectivity/result/sample_qa_newkey.RData')
#sample <- fc_sample_qa_newkey[1:10,]
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
write.csv(com_net_flat_r200_com7, '/home/eballer/from_cedric/output_of_fc/fc_r200_com7/fc_r200_com7.csv')
write.csv(com_net_flat_r200_com17, '/home/eballer/from_cedric/output_of_fc/fc_r200_com17/fc_r200_com17.csv')
write.csv(com_net_flat_r400_com7, '/home/eballer/from_cedric/output_of_fc/fc_r400_com7/fc_r400_com7.csv')
write.csv(com_net_flat_r400_com17, '/home/eballer/from_cedric/output_of_fc/fc_r400_com17/fc_r400_com17.csv')