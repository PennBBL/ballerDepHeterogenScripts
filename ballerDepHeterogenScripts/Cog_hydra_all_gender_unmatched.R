library(visreg)
library(mgcv)
library(tableone)
library(dplyr)
library(plm)
library(MatchIt)
library(tidyr)
library(ggplot2)
library(reshape)
theme_update(plot.title = element_text(hjust = 0.5))
###### This script reads in demographics, cnb_scores, health and psych summaries, merges them, removes NAs, codes and separates by depression#####


#This script goes through demographics, cnb scores, health, and psych summaries, adds clustering information, runs statistics and makes graphs from results.

#Part 1 : Read in csv.s
#-This script reads in demographics, cnb_scores, health and psych summaries, merges them, removes NAs, codes and separates by depression.

#Part 2 : merge with hydra
#-It then merges these documents with hydra output (made in cbica), adding Hydra_k1 through Hydra_k10 columns (which represent the number of clusters)
#-The script reads in 3 different types of groups (matched, unmatched, and residualized unmatched groups), and also does all gender together as well as separating them by gender.

#Part 3 : LM
#-The script then runs LM on each cognitive score (cnb_measure ~ hydra_group).  
#-There is a test option that does this for all cnb measures and all hydra groups, but for the remainder of the analysis, Hydra_k2 was the only classification more deeply explored.

#Part 4 : Anova
#-Anovas were also run on the results of the LM of each cnb value by cluster.

#Part 5 : Graphing
#- Graphs were then made.  
#*For continuous variables(age, medu1), the graphs represent means, with SEM as error bars
#*For categorical variables (race, sex) the graphs are percentages (caucasian, male) per group, with chisq used to calculate significance

#Part 6 : FDR Correction
#-FDR correction was calculated for each cnb measure ANOVA output
#-A table of the results was extracted

#Part 7 : Demographics tables
#- Demographics tables for each group (matched, unmatched, resid) were produced

#######################################################
############ READ IN, MERGE AND SUBSET DATA############
#######################################################

#read in csvs
demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv
cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv
cnb_factor_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_factor_scores_fr_20170202.csv", header = TRUE, sep = ",")

#read in Hydra cluster data -> AG (all_gender), M(males), F(females) 
hydra_AG_matched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_all_gender_matched_n1424_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_AG_unmatched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_all_gender_unmatched_n3022_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_AG_residuals_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_all_gender_unmatched_resid_n3022_HydraSubtypes.csv", header = TRUE, sep = ",") 

hydra_F_matched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_females_matched_n712_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_F_unmatched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_females_unmatched_n1588_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_F_residuals_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_females_unmatched_resid_n1588_HydraSubtypes.csv", header = TRUE, sep = ",") 

hydra_M_matched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_males_matched_n712_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_M_unmatched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_males_unmatched_n1434_HydraSubtypes.csv", header = TRUE, sep = ",")
hydra_M_residuals_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/CogData_males_unmatched_resid_n1434_HydraSubtypes.csv", header = TRUE, sep = ",") 

#Coerce hydra values into factor format
hydra_names <- names(hydra_AG_matched_clusters[2:11])
hydra_AG_matched_clusters[hydra_names] <- lapply(hydra_AG_matched_clusters[hydra_names], factor)
hydra_AG_unmatched_clusters[hydra_names] <- lapply(hydra_AG_unmatched_clusters[hydra_names], factor)
hydra_AG_residuals_clusters[hydra_names] <- lapply(hydra_AG_residuals_clusters[hydra_names], factor)

hydra_F_matched_clusters[hydra_names] <- lapply(hydra_F_matched_clusters[hydra_names], factor)
hydra_F_unmatched_clusters[hydra_names] <- lapply(hydra_F_unmatched_clusters[hydra_names], factor)
hydra_F_residuals_clusters[hydra_names] <- lapply(hydra_F_residuals_clusters[hydra_names], factor)

hydra_M_matched_clusters[hydra_names] <- lapply(hydra_M_matched_clusters[hydra_names], factor)
hydra_M_unmatched_clusters[hydra_names] <- lapply(hydra_M_unmatched_clusters[hydra_names], factor)
hydra_M_residuals_clusters[hydra_names] <- lapply(hydra_M_residuals_clusters[hydra_names], factor)


############
#prepping and merging big CNB and demographics stuff
############

#remove people with NA for race, age, or sex.  START WITH N = 9498
demographics_noNA_race <- demographics[!is.na(demographics$race),] #everyone has a race, N = 9498
demographics_noNA_race_age <- demographics_noNA_race[!is.na(demographics_noNA_race$ageAtClinicalAssess1),] # 86 people do not have age at clinical assessment.  N = 9412
demographics_noNA_race_age_sex <- demographics_noNA_race_age[!is.na(demographics_noNA_race_age$sex),] #everyone has a sex, N = 9412
demographics_noNA_race_age_andCNBage_sex <- demographics_noNA_race_age_sex[!is.na(demographics_noNA_race_age_sex$ageAtCnb1),] #6 people do not have ageAtCnb1, N = 9406

#remove people with NA for depression or total psych score, START WITH N = 9498
psych_summary_no_NA_dep <- psych_summary[!is.na(psych_summary$smry_dep),] #take out those with NA for depression, 87 people N = 9411
psych_summary_no_NA_dep_and_smry_psych_overall <- psych_summary_no_NA_dep[!is.na(psych_summary_no_NA_dep$smry_psych_overall_rtg),] #take out those with NA for overall psych rtg, no additional people lost, N = 9411

#merge demographics and cnb #this is if we want to include people without full demographic data
dem_cnb <- merge(demographics_noNA_race_age_andCNBage_sex, cnb_scores, by = "bblid") #merge demographics and cnb, N = 9406
psych_health <- merge(psych_summary_no_NA_dep_and_smry_psych_overall, health, by = "bblid") #merge psych and health, N = 9411
dem_cnb_psych_health_merged <- merge (dem_cnb, psych_health, by = "bblid") #merge all 4 csvs, lost 1 person [134716] (had demographics, but no psych ratings): N = 9405
dem_cnb_psych_health_factor_scores_merged <- merge(dem_cnb_psych_health_merged, cnb_factor_scores, by = "bblid")

#make subsets
subset_just_dep_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_factor_scores_merged, (medicalratingExclude == 0) & (smry_dep == 4), select = "bblid") #subset people who were not medically excluded and who are depressed, N = 776
subset_no_psych_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_factor_scores_merged, (medicalratingExclude == 0) & (smry_psych_overall_rtg < 4), select = "bblid") #subset people who are psychiatrically healthy, N = 2508
subset_dep_or_no_psych_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_factor_scores_merged, (medicalratingExclude == 0) & ((smry_dep == 4) | (smry_psych_overall_rtg <4))) #subset including both depressed and healthies, good for regressions, N = 3284

#would binarize depression smry score to -1 (less than 4, not depressed) and 1 (score 4 , depressed)
dep_binarized <- ifelse(subset_dep_or_no_psych_and_no_medicalratingExclude$smry_dep == 4, 1, -1)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- cbind(subset_dep_or_no_psych_and_no_medicalratingExclude, dep_binarized) #N = 3284

#make depression and gender into factor scores
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex)

#divide ageAtCNB by 12 for age
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$age_in_years <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$ageAtCnb1/12

#race binarized for plotting purposes, caucasian 1, non-caucasion 0
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race_binarized <- ifelse(as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race) == 1, 1, 0)

#remove people with NA in their cognitive measures
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]

################################################
### Merge clustering data with subsetted data ##
################################################

#AG (all gender), M (males), F (females) 
#AG Matched - n 1424
#AG Unmatched n = 3022
#AG Resid n = 3022
subset_with_clusters_AG_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_matched_clusters, by = "bblid")
subset_with_clusters_AG_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_unmatched_clusters, by = "bblid")
subset_with_clusters_AG_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_residuals_clusters, by = "bblid")

#F Matched - n 954
#F Unmatched n = 1588
#F Resid n = 1588
subset_with_clusters_F_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_matched_clusters, by = "bblid")
subset_with_clusters_F_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_unmatched_clusters, by = "bblid")
subset_with_clusters_F_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_residuals_clusters, by = "bblid")

#M Matched - n 470
#M Unmatched n = 1434
#M Resid n = 1434
subset_with_clusters_M_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_matched_clusters, by = "bblid")
subset_with_clusters_M_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_unmatched_clusters, by = "bblid")
subset_with_clusters_M_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_residuals_clusters, by = "bblid")

#save objects
saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_matched_cnb.rds")
saveRDS(object = subset_with_clusters_AG_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_unmatched_cnb.rds")
saveRDS(object = subset_with_clusters_AG_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_resid_cnb.rds")

saveRDS(object = subset_with_clusters_F_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_matched_cnb.rds")
saveRDS(object = subset_with_clusters_F_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_unmatched_cnb.rds")
saveRDS(object = subset_with_clusters_F_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_resid_cnb.rds")

saveRDS(object = subset_with_clusters_M_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_matched_cnb.rds")
saveRDS(object = subset_with_clusters_M_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_unmatched_cnb.rds")
saveRDS(object = subset_with_clusters_M_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_resid_cnb.rds")


#################################
# Linear Model for each measure #
##### Results stored in list ####
#################################

#get CNB measure names
cnb_measure_names <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("_z", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name
cluster_names <- colnames(hydra_AG_matched_clusters[,2:11])

cnb_measure_names_list <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("_z", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name
cluster_names_list <- colnames(hydra_AG_matched_clusters[,2:11])

#### All Hydra clusters in embedded lis t######
CNB_cog_score_cluster_stats_lm_AG_unmatched_by_cluster_1through10 <- lapply(cluster_names_list, function(cluster_name)
{
  CNB_cog_score_cluster_stats_lm_AG_unmatched_withincluster<- lapply(cnb_measure_names_list, cluster=as.name(cluster_name), function(cnb_measure_name, cluster) 
  {
    cnb_measure <- as.name(cnb_measure_name)
    lm(substitute(cnb_measure ~ cluster, list(cnb_measure = cnb_measure, cluster = cluster)), data = subset_with_clusters_AG_unmatched)
  })
  setNames(CNB_cog_score_cluster_stats_lm_AG_unmatched_withincluster, cnb_measure_names_list)
})
names(CNB_cog_score_cluster_stats_lm_AG_unmatched_by_cluster_1through10) <- cluster_names_list

##### Just Hydra_2 clusters ######
CNB_cog_score_cluster_stats_lm_AG_matched <- lapply(cnb_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_matched)
})

CNB_cog_score_cluster_stats_lm_AG_unmatched <- lapply(cnb_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched)
})

CNB_cog_score_cluster_stats_lm_AG_resid <- lapply(cnb_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_resid)
})


##############################
####### Statistics ###########
##############################

######## All hydra clusters, Anova results ########
CNB_cog_score_cluster_stats_anova_AG_unmatched_by_cluster_1through10 <- lapply(cluster_names_list, function(cluster_name)
{
  CNB_cog_score_cluster_stats_anova_AG_withincluster <- lapply(cnb_measure_names_list, cluster=as.name(cluster_name), function(cnb_measure_name, cluster) 
  {
    cnb_measure <- as.name(cnb_measure_name)
    anova(lm(substitute(cnb_measure ~ cluster, list(cnb_measure = cnb_measure, cluster = cluster)), data = subset_with_clusters_AG_unmatched))
  })
  setNames(CNB_cog_score_cluster_stats_anova_AG_withincluster, cnb_measure_names_list)
})
names(CNB_cog_score_cluster_stats_anova_AG_unmatched_by_cluster_1through10) <- cluster_names_list


#####Just Hydra 2 clusters ANOVA ########
CNB_cog_score_cluster_stats_anova_AG_matched <- lapply(cnb_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_matched))
})

CNB_cog_score_cluster_stats_anova_AG_unmatched <- lapply(cnb_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched))
})

CNB_cog_score_cluster_stats_anova_AG_resid <- lapply(cnb_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_resid))
})

names(CNB_cog_score_cluster_stats_lm_AG_matched) <- cnb_measure_names
names(CNB_cog_score_cluster_stats_lm_AG_unmatched) <- cnb_measure_names
names(CNB_cog_score_cluster_stats_lm_AG_resid) <- cnb_measure_names

names(CNB_cog_score_cluster_stats_anova_AG_matched) <- cnb_measure_names
names(CNB_cog_score_cluster_stats_anova_AG_unmatched) <- cnb_measure_names
names(CNB_cog_score_cluster_stats_anova_AG_resid) <- cnb_measure_names


#WILL HAVE TO DO THIS FOR MALES AND FEMALES#

###################################################
### Average Z-scores for accuracy and speed #######
###################################################

#Get stats of mean accuracy, processing speed and efficiency - TD in the middle, group 1 lower accuracy, processing speed and efficiency, 2 is high on all 3
df_mean_accuracy_z <-c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                       mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1)]),
                       mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2)]))

df_mean_processing_speed_z <- c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                                mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1)]),
                                mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2)]))

df_mean_efficiency_z <- c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                          mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1)]),
                          mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2)]))

names(df_mean_accuracy_z) <- c("TD", "Cluster 1", "Cluster 2")
names(df_mean_processing_speed_z) <- c("TD", "Cluster 1", "Cluster 2")
names(df_mean_efficiency_z) <- c("TD", "Cluster 1", "Cluster 2")

df_mean_acc_speed_eff <- rbind(df_mean_accuracy_z, df_mean_processing_speed_z, df_mean_efficiency_z)

#error bars: will go through each type of symptom, get mean and standard deviation, then divide to get sem
cnb_data_acc_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2"), mean_acc = c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==-1)]),
                                                                                     mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                                                                     mean(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])),
                                  sd_acc = c(sd(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                                             sd(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                             sd(subset_with_clusters_AG_unmatched$NAR_Overall_Accuracy[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])))

cnb_data_speed_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2"), mean_speed = c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==-1)]),
                                                                                         mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                                                                         mean(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])),
                                    sd_speed = c(sd(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                                                 sd(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                                 sd(subset_with_clusters_AG_unmatched$NAR_Overall_Speed[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])))

cnb_data_eff_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2"), mean_eff = c(mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==-1)]),
                                                                                     mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                                                                     mean(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])),
                                  sd_eff = c(sd(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]),
                                             sd(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==1)]),
                                             sd(subset_with_clusters_AG_unmatched$NAR_Overall_Efficiency[which(subset_with_clusters_AG_unmatched$Hydra_k2 ==2)])))


cnb_data_acc_sd_sem$sem = cnb_data_acc_sd_sem$sd_acc/sqrt(nrow(subset_with_clusters_AG_unmatched))
cnb_data_speed_sd_sem$sem = cnb_data_speed_sd_sem$sd_speed/sqrt(nrow(subset_with_clusters_AG_unmatched))
cnb_data_eff_sd_sem$sem = cnb_data_eff_sd_sem$sd_eff/sqrt(nrow(subset_with_clusters_AG_unmatched))

#make a data frame with all standard errors
all_sem <- data.frame(cnb_data_acc_sd_sem$sem, cnb_data_speed_sd_sem$sem, cnb_data_eff_sd_sem$sem)
#make a single column of sem in the data frame mood (TD, Cluster1, Cluster 2), psychosis(TD, cluster 1, cluster 2), etc
all_sem_one_col <- data.frame(sem=unlist(all_sem, use.names = FALSE))

cnb_all_measures <- data.frame(cl=c("TD", "Cluster1", "Cluster2"), acc = df_mean_accuracy_z, speed = df_mean_processing_speed_z, eff = df_mean_efficiency_z)
cnb_measures_for_plot <- melt(cnb_all_measures, id.vars = "cl")
names(cnb_measures_for_plot) <- c("cluster", "cnb", "z_score")
cnb_measures_for_plot$sem <- all_sem_one_col

ggplot(data = cnb_measures_for_plot, aes(x = cnb, y = z_score, group = cluster)) + 
  geom_line(aes(color=cluster)) +
  geom_point(aes(color=cluster)) + 
  geom_errorbar(aes(ymin=z_score-sem, ymax=z_score+sem), width=.1) +
  ggtitle("Hydra_k2 Cognitive Measures")

#######Chi-square for males/females and race########
##########By males, and by caucasians ##############

#Chi squared sex - significance p = 0.2.2 x 10-16
#if just want to look at clusters, not TD, look at commented line below, otherwise keep as is
###chisq_unmatched_sex <- chisq.test(subset_with_clusters_AG_unmatched$sex, (subset_with_clusters_AG_unmatched$Hydra_k2 != -1))
chisq_unmatched_sex <- chisq.test(subset_with_clusters_AG_unmatched$sex, subset_with_clusters_AG_unmatched$Hydra_k2)
chisq_unmatched_sex_pvalue <- chisq_unmatched_sex$p.value
total_people_Hydra_k2 <- c(length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1)]), 
                           length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1)]), 
                           length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2)])) 
#length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 3)]))

num_men_all_clusters <- c(length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1 & subset_with_clusters_AG_unmatched$sex == 1)]), 
                          length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1 & subset_with_clusters_AG_unmatched$sex == 1)]), 
                          length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2 & subset_with_clusters_AG_unmatched$sex == 1)])) 
# length(subset_with_clusters_AG_unmatched$sex[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 3 & subset_with_clusters_AG_unmatched$sex == 1)]))
percent_men_all_clusters <- (num_men_all_clusters/total_people_Hydra_k2) * 100


#Chi square race - p-value = 7.494 x 10-7
chisq_unmatched_race <- chisq.test(subset_with_clusters_AG_unmatched$race_binarized, subset_with_clusters_AG_unmatched$Hydra_k2)
chisq_unmatched_race_pvalue <- chisq_unmatched_race$p.value
num_caucasian_all_clusters <- c(length(subset_with_clusters_AG_unmatched$race_binarized[which(subset_with_clusters_AG_unmatched$Hydra_k2 == -1 & subset_with_clusters_AG_unmatched$race_binarized == 1)]), 
                                length(subset_with_clusters_AG_unmatched$race_binarized[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 1 & subset_with_clusters_AG_unmatched$race_binarized == 1)]), 
                                length(subset_with_clusters_AG_unmatched$race_binarized[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 2 & subset_with_clusters_AG_unmatched$race_binarized == 1)])) 
# length(subset_with_clusters_AG_unmatched$race_binarized[which(subset_with_clusters_AG_unmatched$Hydra_k2 == 3 & subset_with_clusters_AG_unmatched$race_binarized == 1)]))
percent_caucasian_all_clusters <- (num_caucasian_all_clusters/total_people_Hydra_k2)*100

p_values <- c("", chisq_unmatched_sex_pvalue, "", chisq_unmatched_race_pvalue)
dat_sex_race <- data.frame(cl = c("TD", "Cluster1", "Cluster2", "Significance"), 
                           num_males = c(num_men_all_clusters, "---"),
                           percent_males = round(c(percent_men_all_clusters, chisq_unmatched_sex_pvalue), 2), 
                           num_caucasians = c(num_caucasian_all_clusters, "---"),
                           percent_caucasian = round(c(percent_caucasian_all_clusters, chisq_unmatched_race_pvalue),2))
dat_sex_race_no_significance <-  data.frame(cl = c("TD", "Cluster1", "Cluster2"), 
                                            num_males = (num_men_all_clusters),
                                            percent_males = percent_men_all_clusters, 
                                            num_caucasians = num_caucasian_all_clusters,
                                            percent_caucasian = percent_caucasian_all_clusters)                 

percentages <- data.frame(cl=c("TD", "Cluster1", "Cluster2"), percent_males = percent_men_all_clusters, percent_caucasians = percent_caucasian_all_clusters)
percentages_for_plot <- melt(percentages, id.vars = "cl")
names(percentages_for_plot) <- c("cluster", "group", "percent")
ggplot(data = percentages_for_plot, aes(x = group, y = percent, group = cluster)) + 
  geom_line(aes(color=cluster)) +
  geom_point(aes(color=cluster)) + 
  ggtitle("Hydra_k2 Percentages")

ggplot(dat_sex_race_no_significance, aes(x = cl, y = percent_males, fill=cl)) + geom_col() +
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2")) + ylim(0, 100) + xlab("Clusters") + ylab("% Male") + 
  ggtitle("Hydra_k2 % Male, p = 2.2x10-6") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2")) +
  guides(fill=guide_legend(title=NULL))

ggplot(dat_sex_race_no_significance, aes(x = cl, y = percent_caucasian, fill=cl)) + geom_col() + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2")) + ylim(0, 100) + xlab("Clusters") + ylab("% Caucasian") + 
  ggtitle("Hydra_k2 % Caucasian, p = 7.49 x 10-7") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2")) + 
  guides(fill=guide_legend(title=NULL))



#############################
######### Bar Graphs ########
#############################

dat <- data.frame(cluster=c(subset_with_clusters_AG_unmatched$Hydra_k2), age=c(subset_with_clusters_AG_unmatched$age_in_years), medu1=c(subset_with_clusters_AG_unmatched$medu1), race=c(subset_with_clusters_AG_unmatched$race_binarized), sex_males=c(subset_with_clusters_AG_unmatched$sex))
#n becomes 2998 when remove people that don't have Medu1
dat <- dat[!is.na(dat$medu1),]

#dat_age_sd_sem is the data frame that will be cluster(vertical), columns (age, medu), last rows are standard dec and sem
#dat_age_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2", "Cluster3"), age = c(mean(dat$age_in_years[which(dat$cluster ==-1)]), mean(dat$age_in_years[which(dat$cluster ==1)]), mean(dat$age_in_years[which(dat$cluster ==2)]), mean(dat$age_in_years[which(dat$cluster ==3)])), age_sd = c(sd(dat$age_in_years[which(dat$cluster ==-1)]), sd(dat$age_in_years[which(dat$cluster ==1)]), sd(dat$age_in_years[which(dat$cluster ==2)]), sd(dat$age_in_years[which(dat$cluster ==3)])))
dat_age_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2"), 
                             age = c(mean(dat$age[which(dat$cluster ==1)]), 
                                     mean(dat$age[which(dat$cluster ==2)]), 
                                     mean(dat$age[which(dat$cluster ==3)])), 
                             age_sd = c(sd(dat$age[which(dat$cluster ==1)]),
                                        sd(dat$age[which(dat$cluster ==2)]), 
                                        sd(dat$age[which(dat$cluster ==3)])))

dat_age_sd_sem$sem <- dat_age_sd_sem$age_sd/sqrt(nrow(dat)) 
dat_medu_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2"), 
                              medu = c(mean(dat$medu1[which(dat$cluster ==1)]),
                                       mean(dat$medu1[which(dat$cluster ==2)]), 
                                       mean(dat$medu1[which(dat$cluster ==3)])), 
                              medu_sd = c(sd(dat$medu1[which(dat$cluster ==1)]), 
                                          sd(dat$medu1[which(dat$cluster ==2)]), 
                                          sd(dat$medu1[which(dat$cluster ==3)]))) 


dat_medu_sd_sem$sem <- dat_medu_sd_sem$medu_sd/sqrt(nrow(dat))
dat_age_medu_sem_melted <- melt(c(dat_age_sd_sem$sem,dat_medu_sd_sem$sem), id.vars = "cl")

#dat_cont <- data.frame(cluster=c(dat$cluster), age=c(dat$age), medu1=c(dat$medu1))
#dat_cat <- data.frame(cluster=c(dat$cluster), race=c(dat$race_binarized), sex=c(dat$sex))
dat.m <- melt(dat, id.vars='cluster')
#dat_cont.m <- melt(dat_cont, id.vars='cluster')
#dat_cat.m <- melt(dat_cat, id.vars='cluster')

#ggplot(dat.m, aes(fill=cluster, x=cluster, y=value))+ geom_bar(stat="identity") + facet_grid(.~variable) + 
# labs(x='Clusters_unmatched',y='') + scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) 

age = c(mean(dat$age[which(dat$cluster ==1)]), 
        mean(dat$age[which(dat$cluster ==2)]), 
        mean(dat$age[which(dat$cluster ==3)])) 

medu = c(mean(dat$medu1[which(dat$cluster ==1)]),
         mean(dat$medu1[which(dat$cluster ==2)]), 
         mean(dat$medu1[which(dat$cluster ==3)]))

#get everything into the right format
age_and_medu <- data.frame(cl=c("TD", "Cluster1", "Cluster2"), age = age, medu = medu)
dat_age_medu_sem_melted <- melt(c(dat_age_sd_sem$sem,dat_medu_sd_sem$sem), id.vars = "cl")
age_and_medu_for_plot <- melt(age_and_medu, id.vars = "cl")
age_and_medu_for_plot$sem <- dat_age_medu_sem_melted
names(age_and_medu_for_plot) <- c("cluster", "group", "years", "sem")
ggplot(data = age_and_medu_for_plot, aes(x = group, y = years, group = cluster)) + 
  geom_line(aes(color=cluster)) +
  geom_point(aes(color=cluster)) + 
  geom_errorbar(aes(ymin=years-sem, ymax=years+sem), width=.1) +
  ggtitle("Hydra_k2 Ages")

ggplot(dat_age_sd_sem, aes(x = cl, y = age, fill = cl)) + geom_col() + 
  geom_errorbar(aes(ymin=age-sem, ymax=age+sem),width=.2,position=position_dodge(.9)) + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2")) + ylim(0, 18) + xlab("Clusters") + ylab("Age in Years") + 
  ggtitle("Age by Cluster") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2")) +
  guides(fill=guide_legend(title=NULL)) 

ggplot(dat_medu_sd_sem, aes(x = cl, y = medu, fill = cl)) + geom_col() + 
  geom_errorbar(aes(ymin=medu-sem, ymax=medu+sem),width=.2,position=position_dodge(.9)) + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2")) + ylim(0, 18) + xlab("Clusters") + ylab("Maternal Ed in Years") + 
  ggtitle("Maternal Edu by Cluster") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2")) +
  guides(fill=guide_legend(title=NULL)) 


###################################################
## Extracting anovas with significant p values ####
###################################################

#Looking at Hydra_k2, but can redo with nested list
#Look at model summaries
models_anova <- lapply(CNB_cog_score_cluster_stats_anova_AG_unmatched, summary)

#Pull p-values
p_anova <- sapply(CNB_cog_score_cluster_stats_anova_AG_unmatched, function(v) v$"Pr(>F)"[1]) #$coef[,"Pr(>F)"][2]) #get the p value for dep binarized

#Convert to data frame
p_anova <- as.data.frame(p_anova)

#Print original p-values to three decimal places
p_round_anova <- round(p_anova,3)

#FDR correct p-values
pfdr_anova <- p.adjust(p_anova[,1],method="fdr")

#Convert to data frame
pfdr_anova <- as.data.frame(pfdr_anova)
row.names(pfdr_anova) <- cnb_measure_names

#To print fdr-corrected p-values to three decimal places
pfdr_round_anova <- round(pfdr_anova,3)

#List the NMF components that survive FDR correction
CNB_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]

#make a data frame with names and fdr values (rounded to 3 decimals)
CNB_names_and_fdr_values_anova <- data.frame(cbind(CNB_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))

#add titles to names_and_fdr tables
names(CNB_names_and_fdr_values_anova) <- c("CNB_measure", "p_FDR_corr")

#write the results of the mass univariate stats to files
#write.csv(CNB_names_and_fdr_values_anova, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/n3284_dep776_nondep2508_mass_univ_FDR_corrected_anova_20171211.csv")


#############################
####### Demographics ########
#############################

#######Matched group all clusters ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_Hydra_k2_AG_matched <- data.frame(subset_with_clusters_AG_matched$race_binarized, subset_with_clusters_AG_matched$sex, subset_with_clusters_AG_matched$medu1, subset_with_clusters_AG_matched$age_in_years, subset_with_clusters_AG_matched$dep_binarized, subset_with_clusters_AG_matched$Hydra_k2)
names(demo_Hydra_k2_AG_matched) <- c(listVars)

#Change categorical values to have names
demo_Hydra_k2_AG_matched$Depression <- ifelse(demo_Hydra_k2_AG_matched$Depression == 1, "Depressed", "Non-depressed")
demo_Hydra_k2_AG_matched$Race <- ifelse(demo_Hydra_k2_AG_matched$Race == 1, "Caucasian", "Non-caucasian")
demo_Hydra_k2_AG_matched$Sex <- ifelse(demo_Hydra_k2_AG_matched$Sex == 1, "Male", "Female")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")
title <- c("Hydra_k2 demographics")

#create demographics table
demo_Hydra_k2_AG_matched_table <- CreateTableOne(vars = listVars, data = demo_Hydra_k2_AG_matched, factorVars = cat_variables, strata = c("Cluster"))
print(demo_Hydra_k2_AG_matched_table, showAllLevels = TRUE)

#######Unmatched group ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_Hydra_k2_AG_unmatched <- data.frame(subset_with_clusters_AG_unmatched$race_binarized, subset_with_clusters_AG_unmatched$sex, subset_with_clusters_AG_unmatched$medu1, subset_with_clusters_AG_unmatched$age_in_years, subset_with_clusters_AG_unmatched$dep_binarized, subset_with_clusters_AG_unmatched$Hydra_k2)
names(demo_Hydra_k2_AG_unmatched) <- c(listVars)

#Change categorical values to have names
demo_Hydra_k2_AG_unmatched$Depression <- ifelse(demo_Hydra_k2_AG_unmatched$Depression == 1, "Depressed", "Non-depressed")
demo_Hydra_k2_AG_unmatched$Race <- ifelse(demo_Hydra_k2_AG_unmatched$Race == 1, "Caucasian", "Non-caucasian")
demo_Hydra_k2_AG_unmatched$Sex <- ifelse(demo_Hydra_k2_AG_unmatched$Sex == 1, "Male", "Female")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")

title <- c("Hydra_k2 demographics")

#create demographics table
demo_Hydra_k2_AG_unmatched_table <- CreateTableOne(vars = listVars, data = demo_Hydra_k2_AG_unmatched, factorVars = cat_variables, strata = c("Cluster"))
print(demo_Hydra_k2_AG_unmatched_table, showAllLevels = TRUE)

#######Unmatched residuals group ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_Hydra_k2_AG_resid <- data.frame(subset_with_clusters_AG_resid$race_binarized, subset_with_clusters_AG_resid$sex, subset_with_clusters_AG_resid$medu1, subset_with_clusters_AG_resid$age_in_years, subset_with_clusters_AG_resid$dep_binarized, subset_with_clusters_AG_resid$Hydra_k2)
names(demo_Hydra_k2_AG_resid) <- c(listVars)

#Change categorical values to have names
demo_Hydra_k2_AG_resid$Depression <- ifelse(demo_Hydra_k2_AG_resid$Depression == 1, "Depressed", "Non-depressed")
demo_Hydra_k2_AG_resid$Race <- ifelse(demo_Hydra_k2_AG_resid$Race == 1, "Caucasian", "Non-caucasian")
demo_Hydra_k2_AG_resid$Sex <- ifelse(demo_Hydra_k2_AG_resid$Sex == 1, "Male", "Female")

#make variable list
#table_titles <- c("Cluster 1", "Cluster 2")
#table_titles <- c("Cluster 1", "Depressed", "P-value")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")

title <- c("Hydra_k2 demographics")

#create demographics table
demo_Hydra_k2_AG_resid_table <- CreateTableOne(vars = listVars, data = demo_Hydra_k2_AG_resid, factorVars = cat_variables, strata = c("Cluster"))
print(demo_Hydra_k2_AG_resid_table, showAllLevels = TRUE)

#checkmodel with visreg, uncomment when want to check
invisible(lapply(CNB_cog_score_cluster_stats_lm_AG_matched, function(x) {visreg(x)})) 
invisible(lapply(CNB_cog_score_cluster_stats_lm_AG_unmatched, function(x) {visreg(x)})) 
invisible(lapply(CNB_cog_score_cluster_stats_lm_AG_resid, function(x) {visreg(x)})) 
