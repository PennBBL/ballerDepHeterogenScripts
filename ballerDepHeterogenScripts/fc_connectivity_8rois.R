source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/fc_communities_three_scans/fc_and_mdmr_functions_NEW_NBACK_27parcels.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'

#orginal sample n = 368
sample <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/subset_with_T1_FC_and_dem_with_clusters_20181121_OLD_HYDRA_with_new_script_from_toni.rds")

#scan ids of people who actually had scans- n= 359
scanids <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest/rest_2mm/ts_files_from_3dROIstats_8rois/scanids")

#rename column for merge
names(scanids) <- c("scanid")

#merge
rest_sample <- merge(sample, scanids, by = "scanid" )

#subset only bblid, scanid, age_in_years, ageSq, race, medu, 
rest_sample <- data.frame(cbind(rest_sample$bblid, rest_sample$scanid, rest_sample$Hydra_k3, rest_sample$age_in_years, rest_sample$ageSq, rest_sample$sex, rest_sample$race_binarized, rest_sample$medu1))
names(rest_sample) <- c("bblid", "scanid", "Hydra_k3", "age_in_years", "ageSq", "sex", "race_binarized", "medu1")

fc_net_8roi_rest <- get_net_from_sample(sample = rest_sample, parcellation = '8roi_rest', resolution= 400, modality = 'fc')
fc_net_8roi_rest_matrix <- list_to_matrix(list = fc_net_8roi_rest)
fc_net_8roi_rest_average <- apply(fc_net_8roi_rest_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
community_info_8roi <-  get_8roi_concat_node_info()
plot <- better_levelplot(adj = fc_net_8roi_rest_average,node_names = community_info_8roi$NodeName$V1,title = 'Community Matrix, 8roi defined as Nback sig FDR <0.05')

#grab the upper triangle of matrix, excluding diagonal, and flatten
fc_flat_8roi_rest <- lapply(fc_net_8roi_rest, function(fc_mat) fc_mat[upper.tri(fc_mat, diag=FALSE)])
fc_flat_8roi_matrix_rest <- list_to_matrix_edge_analysis(list = fc_flat_8roi_rest)
colnames(fc_flat_8roi_matrix_rest) <- get_node_names_edge_analysis(community_info_8roi$NodeName)

#merge new matrix with old data frame
df_8roi_rest <- cbind(rest_sample,fc_flat_8roi_matrix_rest)

#write output
write.csv(df_8roi_rest,'/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_8roi_rest_20190315.csv')
#net_flat_8roi_from_matrix <-(t(sapply(fc_net_8roi_matrix,function(net) net[upper.tri(net,diag=TRUE)])))



########################################
######### Nodal Strength ###############
########################################




########################################

### Task active 8 areas ####

#extract the part of the matrix for task active
node_strength_8rois_task_active <- lapply(fc_net_8roi_rest, function(matrix) {
  matrix_to_nodal_strength_array(matrix)
})

node_strength_8rois_task_active_mat <- data.frame(matrix(unlist(node_strength_8rois_task_active), ncol=8,byrow=TRUE))
com8_info <- get_8roi_concat_node_info()
names(node_strength_8rois_task_active_mat) <- as.character(com8_info$NodeName$V1)

task_active <- cbind(node_strength_8rois_task_active_mat, rest_sample)
write.csv(task_active, "/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/df_task_active_node_strength_8roi_rest_20190315.csv")


