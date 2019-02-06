source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/fc_communities_three_scans/fc_and_mdmr_functions_NEW_NBACK_27parcels.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'




#######################################
#####       Connectivity          #####
#######################################
sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_27roi <- get_net_from_sample_concat(sample = sample,parcellation = '27roi',resolution = 400,modality = 'fc')
fc_net_27roi_matrix <- list_to_matrix(list = fc_net_27roi)
fc_net_27roi_average <- apply(fc_net_27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_27roi_average,node_names = community_info_27roi$NodeName$V1,title = 'Community Matrix, 27roi')

#######################################
###     THis does within/between    ###
### For 27 functional parcellations ###
#######################################

com_net_names_27roi <- get_community_net_names(community_info_27roi$CommunityName)
com_net_27roi<-lapply(fc_net_27roi,function(netmat) get_community_net(netmat,community_info_27roi$CommunityName,community_info_27roi$CommunityAffiliation))
plot_com_27roi <- better_levelplot(adj = com_net_27roi[[1]],node_names = community_info_27roi$CommunityName,title = 'Community matrix 27roi')

com_net_flat_27roi<-t(sapply(com_net_27roi,function(net) net[upper.tri(net,diag = TRUE)]))
colnames(com_net_flat_27roi) <- com_net_names_27roi[upper.tri(com_net_names_27roi,diag = TRUE)]
com_net_names_flat_27roi <- com_net_names_27roi[upper.tri(com_net_names_27roi,diag = TRUE)]
com_net_flat_27roi <- cbind(com_net_flat_27roi,sample)

write.csv(com_net_flat_27roi, '/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/fc_27roi_20190122.csv')

#######################################
###     Standard Connectivity       ###
###     Areas Diff in Nback         ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_8roi <- get_net_from_sample_concat(sample = sample,parcellation = '8roi',resolution = 400,modality = 'fc')
fc_net_8roi_matrix <- list_to_matrix(list = fc_net_8roi)
fc_net_8roi_average <- apply(fc_net_8roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_8roi <-  get_8roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_8roi_average,node_names = community_info_8roi$NodeName$V1,title = 'Community Matrix, 8roi defined as Nback sig FDR <0.05')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_8roi <- lapply(fc_net_8roi, function(fc_mat) fc_mat[upper.tri(fc_mat, diag=FALSE)])
fc_flat_8roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_8roi)
colnames(fc_flat_8roi_matrix) <- get_node_names_edge_analysis(community_info_8roi$NodeName)

#merge new matrix with old data frame
df_8roi <- cbind(sample,fc_flat_8roi_matrix)

#write output
write.csv(df_8roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_8roi_20190125.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))

#######################################
### Edge Analysis - 8 sig dif ROIs  ###
###   vs 27 function Nback Parcels  ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_8x27roi <- get_net_from_sample_concat(sample = sample,parcellation = '8x27roi',resolution = 400,modality = 'fc')
fc_net_8x27roi_matrix <- list_to_matrix(list = fc_net_8x27roi)
fc_net_8x27roi_average <- apply(fc_net_8x27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
community_info_8roi <- get_8roi_concat_node_info()
plot <- better_levelplot_edge(adj = fc_net_8x27roi_average,node_names_x = community_info_8roi$NodeName$V1, node_names_y = community_info_27roi$NodeName$V1, title = 'Community Matrix, 8roi defined as Nback sig FDR <0.05')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_8x27roi <- lapply(fc_net_8x27roi, function(fc_mat) matrix_to_flat_for_edge_analysis(fc_mat, community_info_8roi$NodeName, community_info_27roi$NodeName))
fc_flat_8x27roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_8x27roi)
colnames(fc_flat_8x27roi_matrix) <- get_node_names_edge_analysis_diff_dim(community_info_8roi$NodeName, community_info_27roi$NodeName)

#merge new matrix with old data frame
df_8x27roi <- cbind(sample,fc_flat_8x27roi_matrix)

#write output
write.csv(df_8x27roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_8x27roi_20190129.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))
#######################################
###     Edge Analysis               ###
###     27x27 Nback Func Parcels    ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_27roi <- get_net_from_sample_concat(sample = sample,parcellation = '27roi',resolution = 400,modality = 'fc')
fc_net_27roi_matrix <- list_to_matrix(list = fc_net_27roi)
fc_net_27roi_average <- apply(fc_net_27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_27roi_average,node_names = community_info_27roi$NodeName$V1,title = 'Community Matrix, 27 Functional ROIS')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_27roi <- lapply(fc_net_27roi, function(fc_mat) fc_mat[upper.tri(fc_mat, diag=FALSE)])
fc_flat_27roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_27roi)
colnames(fc_flat_27roi_matrix) <- get_node_names_edge_analysis(community_info_27roi$NodeName)

#merge new matrix with old data frame
df_27roi <- cbind(sample,fc_flat_27roi_matrix)

#write output
write.csv(df_27roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_27roi_edge_analysis_20190129.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))
#######################################
#####  Connectivity 14 parcels    #####
#######################################
sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_an
d_Hydra_k3.csv", header=TRUE)
fc_net_14roi <- get_net_from_sample_concat(sample = sample,parcellation = '14roi',resolution = 400,modality = 'fc')
fc_net_14roi_matrix <- list_to_matrix(list = fc_net_14roi)
fc_net_14roi_average <- apply(fc_net_14roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_14roi <-  get_14roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_14roi_average,node_names = community_info_14roi$NodeName$V1,title = 'Community Matrix, 14roi')

#######################################
###     THis does within/between    ###
### For 14 functional parcellations ###
#######################################

com_net_names_14roi <- get_community_net_names(community_info_14roi$CommunityName)
com_net_14roi<-lapply(fc_net_14roi,function(netmat) get_community_net(netmat,community_info_14roi$CommunityName,community_info_14roi$CommunityAffiliation))
plot_com_14roi <- better_levelplot(adj = com_net_14roi[[1]],node_names = community_info_14roi$CommunityName,title = 'Community matrix 14roi')

com_net_flat_14roi<-t(sapply(com_net_14roi,function(net) net[upper.tri(net,diag = TRUE)]))
colnames(com_net_flat_14roi) <- com_net_names_14roi[upper.tri(com_net_names_14roi,diag = TRUE)]
com_net_names_flat_14roi <- com_net_names_14roi[upper.tri(com_net_names_14roi,diag = TRUE)]
com_net_flat_14roi <- cbind(com_net_flat_14roi,sample)

write.csv(com_net_flat_14roi, '/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/fc_14roi_20190125.csv')

#######################################


#######################################
#####    All with 2mm scans      ######
#######################################

#######################################
#####       Connectivity          #####
#######################################
sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_27roi <- get_net_from_sample_concat(sample = sample,parcellation = '27roi',resolution = '2mm',modality = 'fc')
fc_net_27roi_matrix <- list_to_matrix(list = fc_net_27roi)
fc_net_27roi_average <- apply(fc_net_27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_27roi_average,node_names = community_info_27roi$NodeName$V1,title = 'Community Matrix, 27roi')

#######################################
###     THis does within/between    ###
### For 27 functional parcellations ###
#######################################

com_net_names_27roi <- get_community_net_names(community_info_27roi$CommunityName)
com_net_27roi<-lapply(fc_net_27roi,function(netmat) get_community_net(netmat,community_info_27roi$CommunityName,community_info_27roi$CommunityAffiliation))
plot_com_27roi <- better_levelplot(adj = com_net_27roi[[1]],node_names = community_info_27roi$CommunityName,title = 'Community matrix 27roi')

com_net_flat_27roi<-t(sapply(com_net_27roi,function(net) net[upper.tri(net,diag = TRUE)]))
colnames(com_net_flat_27roi) <- com_net_names_27roi[upper.tri(com_net_names_27roi,diag = TRUE)]
com_net_names_flat_27roi <- com_net_names_27roi[upper.tri(com_net_names_27roi,diag = TRUE)]
com_net_flat_27roi <- cbind(com_net_flat_27roi,sample)

write.csv(com_net_flat_27roi, '/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/fc_27roi_20190205_2mm.csv')

#######################################
###     Standard Connectivity       ###
###     Areas Diff in Nback         ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_8roi <- get_net_from_sample_concat(sample = sample,parcellation = '8roi',resolution = '2mm',modality = 'fc')
fc_net_8roi_matrix <- list_to_matrix(list = fc_net_8roi)
fc_net_8roi_average <- apply(fc_net_8roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_8roi <-  get_8roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_8roi_average,node_names = community_info_8roi$NodeName$V1,title = 'Community Matrix, 8roi defined as Nback sig FDR <0.05')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_8roi <- lapply(fc_net_8roi, function(fc_mat) fc_mat[upper.tri(fc_mat, diag=FALSE)])
fc_flat_8roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_8roi)
colnames(fc_flat_8roi_matrix) <- get_node_names_edge_analysis(community_info_8roi$NodeName)

#merge new matrix with old data frame
df_8roi <- cbind(sample,fc_flat_8roi_matrix)

#write output
write.csv(df_8roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_8roi_20190205_2mm.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))

#######################################
### Edge Analysis - 8 sig dif ROIs  ###
###   vs 27 function Nback Parcels  ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_8x27roi <- get_net_from_sample_concat(sample = sample,parcellation = '8x27roi',resolution = '2mm',modality = 'fc')
fc_net_8x27roi_matrix <- list_to_matrix(list = fc_net_8x27roi)
fc_net_8x27roi_average <- apply(fc_net_8x27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
community_info_8roi <- get_8roi_concat_node_info()
plot <- better_levelplot_edge(adj = fc_net_8x27roi_average,node_names_x = community_info_8roi$NodeName$V1, node_names_y = community_info_27roi$NodeName$V1, title = 'Community Matrix, 8roi defined as Nback sig FDR <0.05')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_8x27roi <- lapply(fc_net_8x27roi, function(fc_mat) matrix_to_flat_for_edge_analysis(fc_mat, community_info_8roi$NodeName, community_info_27roi$NodeName))
fc_flat_8x27roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_8x27roi)
colnames(fc_flat_8x27roi_matrix) <- get_node_names_edge_analysis_diff_dim(community_info_8roi$NodeName, community_info_27roi$NodeName)

#merge new matrix with old data frame
df_8x27roi <- cbind(sample,fc_flat_8x27roi_matrix)

#write output
write.csv(df_8x27roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_8x27roi_20190205_2mm.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))
#######################################
###     Edge Analysis               ###
###     27x27 Nback Func Parcels    ###
#######################################

sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_27roi <- get_net_from_sample_concat(sample = sample,parcellation = '27roi',resolution = '2mm',modality = 'fc')
fc_net_27roi_matrix <- list_to_matrix(list = fc_net_27roi)
fc_net_27roi_average <- apply(fc_net_27roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_27roi <-  get_27roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_27roi_average,node_names = community_info_27roi$NodeName$V1,title = 'Community Matrix, 27 Functional ROIS')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_27roi <- lapply(fc_net_27roi, function(fc_mat) fc_mat[upper.tri(fc_mat, diag=FALSE)])
fc_flat_27roi_matrix <- list_to_matrix_edge_analysis(list = fc_flat_27roi)
colnames(fc_flat_27roi_matrix) <- get_node_names_edge_analysis(community_info_27roi$NodeName)

#merge new matrix with old data frame
df_27roi <- cbind(sample,fc_flat_27roi_matrix)

#write output
write.csv(df_27roi,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_27roi_edge_analysis_20190205_2mm.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))
#######################################
#####  Connectivity 14 parcels    #####
#######################################
sample <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers_just_covs_and_Hydra_k3.csv", header=TRUE)
fc_net_14roi <- get_net_from_sample_concat(sample = sample,parcellation = '14roi',resolution = '2mm',modality = 'fc')
fc_net_14roi_matrix <- list_to_matrix(list = fc_net_14roi)
fc_net_14roi_average <- apply(fc_net_14roi_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_14roi <-  get_14roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_14roi_average,node_names = community_info_14roi$NodeName$V1,title = 'Community Matrix, 14roi')

#######################################
###     THis does within/between    ###
### For 14 functional parcellations ###
#######################################

com_net_names_14roi <- get_community_net_names(community_info_14roi$CommunityName)
com_net_14roi<-lapply(fc_net_14roi,function(netmat) get_community_net(netmat,community_info_14roi$CommunityName,community_info_14roi$CommunityAffiliation))
plot_com_14roi <- better_levelplot(adj = com_net_14roi[[1]],node_names = community_info_14roi$CommunityName,title = 'Community matrix 14roi')

com_net_flat_14roi<-t(sapply(com_net_14roi,function(net) net[upper.tri(net,diag = TRUE)]))
colnames(com_net_flat_14roi) <- com_net_names_14roi[upper.tri(com_net_names_14roi,diag = TRUE)]
com_net_names_flat_14roi <- com_net_names_14roi[upper.tri(com_net_names_14roi,diag = TRUE)]
com_net_flat_14roi <- cbind(com_net_flat_14roi,sample)

write.csv(com_net_flat_14roi, '/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/fc_14roi_20190205_2mm.csv')

