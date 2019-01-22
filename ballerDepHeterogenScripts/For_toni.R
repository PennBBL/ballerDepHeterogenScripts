pairwise_contrasts_generic_num_clusters <- function(data_frame_lm, fdr_anova, num_clusters) {
  #for 3 clusters
  print(fdr_anova)
  emmodel_df <- lapply(data_frame_lm, function(x) {as.list(ref_grid(x))})
  emgrid_df <- lapply(emmodel_df, function(x) {as.emmGrid(x)})
  
  #run emmeans
  hydra_var <- paste0("Hydra_k", num_clusters)
  emmeans_df <- lapply(emgrid_df, function(x) {emmeans(x, hydra_var)})
  
  #run pairwise contrasts
  empairs_df <- lapply(emmeans_df, function(x) {pairs(x)})
  
  
  #Only include stuff that was fdr corrected (i.e., only keep parts of the model (or only display) ones that are corrected),this will be null if nothing was corrected
  empairs_FDR_corrected <- empairs_df[fdr_anova[,1]]
  
  # print(empairs_FDR_corrected)
  #contrast names, -1 = controls, other numbers are clusters
  contrast_names <- make_pairwise_contrast_names(num_clusters = num_clusters)
  #go through each fdr corrected brain region, and extract p values
  #contrast_table <- lapply(fdr_anova[,1], function(x) {round(summary(x)$p.value,3)})
  contrast_table <- lapply(empairs_FDR_corrected, function(x) {round(summary(x)$p.value,3)})
  #get the names of the brain regions that were fdr corrected
  brain_regions <- names(contrast_table)
  #build table that will hold the name of the brain region and the p values
  #number of columns (or number of unique pairs is)
  pairwise_table <- data.frame(matrix(nrow = length(brain_regions), ncol = length(contrast_names)))
  #give the appropriate names
  rownames(pairwise_table) <- brain_regions
  colnames(pairwise_table) <- contrast_names
  
  #loop through each brain region, and manually assign the columns to be the p values
  for (region in brain_regions)
  {
    pair_pval <- contrast_table[[region]]
    pairwise_table[region,] <- pair_pval
  }
  pairwise_table_with_fdr <- pairwise_table
  pairwise_table_with_fdr$p_FDR_corr <- fdr_anova$p_FDR_corr
  
  pairwise <- list(empairs_df, empairs_FDR_corrected, pairwise_table, pairwise_table_with_fdr)
  return(pairwise)
  
}

#################
#Structure of LM#
#################

#this is a list with each measure I ran.  It is the output from this command:


#cnb_score_cluster_stats_lm_agesq_AG_matched <- lapply(cnb_measure_names, function(x) 
#{
 # lm(substitute(i ~ Hydra_k3 + sex + age_in_years + ageSq , list(i = as.name(x))), data = subset_with_clusters_AG_matched)
#})
#names(cnb_score_cluster_stats_lm_agesq_AG_matched) <- cnb_measure_names


####For you, the cnb_measure_names would be the brain regions

####################
#Structure of ANOVA#
####################
# This is what I send as the fdr_corrected anova into the pairwise contrasts model.  You can put it together any way you would like

#cnb_measure p_FDR_corr
#1        abf_z          0
#2        att_z          0
#3         wm_z          0
#4       vmem_z          0
#5       fmem_z          0
#6       smem_z          0
#7        lan_z          0
#8        nvr_z          0
#9        spa_z          0
#10       eid_z      0.023
#11       edi_z          0
#12       adi_z          0
#13     abf_s_z          0
#14     att_s_z          0
#15      wm_s_z          0
#16    vmem_s_z          0
#17    fmem_s_z          0
#18    smem_s_z          0
#19     lan_s_z          0
#20     nvr_s_z          0
#21     spa_s_z          0
#22     eid_s_z          0
#23     edi_s_z          0
#24     adi_s_z          0
#25     mot_s_z          0
#26      sm_s_z          0

