source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181023_schaefer400_and_fc_communities/fc_and_mdmr_functions_NEW_NBACK.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'

community_info_r100_com7 <-  get_schaefer_node_info(resolution = 100,num_com = 7)
community_info_r100_com17 <-  get_schaefer_node_info(resolution = 100,num_com = 17)

community_info_r200_com7 <-  get_schaefer_node_info(resolution = 200,num_com = 7)
community_info_r200_com17 <-  get_schaefer_node_info(resolution = 200,num_com = 17)

community_info_r400_com7 <-  get_schaefer_node_info(resolution = 400,num_com = 7)
community_info_r400_com17 <-  get_schaefer_node_info(resolution = 400,num_com = 17)

saveRDS(object = community_info_r100_com7, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer100_com7_mapping.RDS')
saveRDS(object = community_info_r100_com17, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer100_com17_mapping.RDS')

saveRDS(object = community_info_r200_com7, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer200_com7_mapping.RDS')
saveRDS(object = community_info_r200_com17, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer200_com17_mapping.RDS')

saveRDS(object = community_info_r400_com7, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer400_com7_mapping.RDS')
saveRDS(object = community_info_r400_com17, '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/schaefer_to_community_mapping_RDSs/schaefer400_com17_mapping.RDS')
