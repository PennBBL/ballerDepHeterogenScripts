library(visreg)
library(mgcv)
library(tableone)
library(dplyr)
library(plm)
library(MatchIt)
library(tidyr)
library(ggplot2)
library(reshape)
###### This script reads in demographics, imaging_scores, health and psych summaries, merges them, removes NAs, codes and separates by depression#####
####Also preps for Hydra, both using a typical GAM model and matching (we lose a lot of people) and also with residuals plotted so we don't have to match#########
########Also provides unmatched data sets and tests them, if we decide to use them.  It significantly reduces N to match (dataset from 3022 to 1424)


#######################################################
############ READ IN, MERGE AND SUBSET DATA############
#######################################################

#read in csvs
demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv
imaging_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_itemwise_bifactor_scores_20161219.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_itemwise_bifactor_scores_20161219.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv

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


############################################################
##prepping and merging big imaging and demographics stuff ##
############################################################

#remove people with NA for race, age, or sex.  START WITH N = 9498
demographics_noNA_race <- demographics[!is.na(demographics$race),] #everyone has a race, N = 9498
demographics_noNA_race_age <- demographics_noNA_race[!is.na(demographics_noNA_race$ageAtClinicalAssess1),] # 86 people do not have age at imaging assessment.  N = 9412
demographics_noNA_race_age_sex <- demographics_noNA_race_age[!is.na(demographics_noNA_race_age$sex),] #everyone has a sex, N = 9412

#remove people with NA for depression or total psych score, START WITH N = 9498
psych_summary_no_NA_dep <- psych_summary[!is.na(psych_summary$smry_dep),] #take out those with NA for depression, 87 people N = 9411
psych_summary_no_NA_dep_and_smry_psych_overall <- psych_summary_no_NA_dep[!is.na(psych_summary_no_NA_dep$smry_psych_overall_rtg),] #take out those with NA for overall psych rtg, no additional people lost, N = 9411

#merge demographics and imaging #this is if we want to include people without full demographic data
dem_imaging <- merge(demographics_noNA_race_age_sex, imaging_scores, by = "bblid") #merge demographics and imaging, N = 9350
psych_health <- merge(psych_summary_no_NA_dep_and_smry_psych_overall, health, by = "bblid") #merge psych and health, N = 9411
dem_imaging_psych_health_merged <- merge (dem_imaging, psych_health, by = "bblid") #merge all 3 csvs, lost 1 person [134716] (had demographics, but no psych ratings): N = 9350

#make subsets
subset_just_dep_and_no_medicalratingExclude <- subset.data.frame(dem_imaging_psych_health_merged, (medicalratingExclude == 0) & (smry_dep == 4), select = "bblid") #subset people who were not medically excluded and who are depressed, N = 776
subset_no_psych_no_medicalratingExclude <- subset.data.frame(dem_imaging_psych_health_merged, (medicalratingExclude == 0) & (smry_psych_overall_rtg < 4), select = "bblid") #subset people who are psychiatrically healthy, N = 2508
subset_dep_or_no_psych_and_no_medicalratingExclude <- subset.data.frame(dem_imaging_psych_health_merged, (medicalratingExclude == 0) & ((smry_dep == 4) | (smry_psych_overall_rtg <4))) #subset including both depressed and healthies, good for regressions, N = 3284

#would binarize depression smry score to -1 (less than 4, not depressed) and 1 (score 4 , depressed)
dep_binarized <- ifelse(subset_dep_or_no_psych_and_no_medicalratingExclude$smry_dep == 4, 1, -1)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- cbind(subset_dep_or_no_psych_and_no_medicalratingExclude, dep_binarized) #N = 3284

#make depression and gender into factor scores
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex)

#divide ageAtimaging by 12 for age
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$age_in_years <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$ageAtClinicalAssess1/12

#race binarized for plotting purposes, caucasian 1, non-caucasion 0
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race_binarized <- ifelse(as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race) == 1, 1, 0)

#remove people with NA in their cognitive measures
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]

################################################
### Merge clustering data with subsetted data ##
################################################

#AG (all gender), M (males), F (females) 
#AG Matched - n 1423
#AG Unmatched n = 3014
#AG Resid n = 3014
subset_with_clusters_AG_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_matched_clusters, by = "bblid")
subset_with_clusters_AG_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_unmatched_clusters, by = "bblid")
subset_with_clusters_AG_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_AG_residuals_clusters, by = "bblid")

#F Matched - n 952
#F Unmatched n = 1584
#F Resid n = 1585
subset_with_clusters_F_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_matched_clusters, by = "bblid")
subset_with_clusters_F_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_unmatched_clusters, by = "bblid")
subset_with_clusters_F_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_F_residuals_clusters, by = "bblid")

#M Matched - n 470
#M Unmatched n = 1430
#M Resid n = 1430
subset_with_clusters_M_matched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_matched_clusters, by = "bblid")
subset_with_clusters_M_unmatched <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_unmatched_clusters, by = "bblid")
subset_with_clusters_M_resid <- merge(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED, hydra_M_residuals_clusters, by = "bblid")

#save objects
saveRDS(object = subset_with_clusters_AG_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_matched_imaging.rds")
saveRDS(object = subset_with_clusters_AG_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_unmatched_imaging.rds")
saveRDS(object = subset_with_clusters_AG_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_resid_imaging.rds")

saveRDS(object = subset_with_clusters_F_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_matched_imaging.rds")
saveRDS(object = subset_with_clusters_F_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_unmatched_imaging.rds")
saveRDS(object = subset_with_clusters_F_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_F_resid_imaging.rds")

saveRDS(object = subset_with_clusters_M_matched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_matched_imaging.rds")
saveRDS(object = subset_with_clusters_M_unmatched, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_unmatched_imaging.rds")
saveRDS(object = subset_with_clusters_M_resid, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_M_resid_imaging.rds")

###################################################
### Run visreg on clustered data, using lapply ####
###################################################

#get imaging measure names (grep factor)  do I need to grep smry?
imaging_measure_names <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("factor", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name
cluster_names <- colnames(hydra_AG_matched_clusters[,2:11])

imaging_measure_names_list <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("factor", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name
cluster_names_list <- colnames(hydra_AG_matched_clusters[,2:11])


#################################
# Linear Model for each measure #
##### Results stored in list ####
#################################

#### All Hydra clusters in embedded lis t######
imaging_cog_score_cluster_stats_lm_AG_matched_by_cluster_1through10 <- lapply(cluster_names_list, function(cluster_name)
{
  imaging_cog_score_cluster_stats_lm_AG_matched_withincluster<- lapply(imaging_measure_names_list, cluster=as.name(cluster_name), function(imaging_measure_name, cluster) 
  {
    imaging_measure <- as.name(imaging_measure_name)
    lm(substitute(imaging_measure ~ cluster, list(imaging_measure = imaging_measure, cluster = cluster)), data = subset_with_clusters_AG_matched)
  })
  setNames(imaging_cog_score_cluster_stats_lm_AG_matched_withincluster, imaging_measure_names_list)
})
names(imaging_cog_score_cluster_stats_lm_AG_matched_by_cluster_1through10) <- cluster_names_list

##### Just Hydra_3 clusters ######
imaging_cog_score_cluster_stats_lm_AG_matched <- lapply(imaging_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_matched)
})

imaging_cog_score_cluster_stats_lm_AG_unmatched <- lapply(imaging_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched)
})

imaging_cog_score_cluster_stats_lm_AG_resid <- lapply(imaging_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_resid)
})


##############################
####### Statistics ###########
##############################

######## All hydra clusters, Anova results ########
imaging_cog_score_cluster_stats_anova_AG_matched_by_cluster_1through10 <- lapply(cluster_names_list, function(cluster_name)
{
  imaging_cog_score_cluster_stats_anova_AG_withincluster <- lapply(imaging_measure_names_list, cluster=as.name(cluster_name), function(imaging_measure_name, cluster) 
  {
    imaging_measure <- as.name(imaging_measure_name)
    anova(lm(substitute(imaging_measure ~ cluster, list(imaging_measure = imaging_measure, cluster = cluster)), data = subset_with_clusters_AG_matched))
  })
  setNames(imaging_cog_score_cluster_stats_anova_AG_withincluster, imaging_measure_names_list)
})
names(imaging_cog_score_cluster_stats_anova_AG_matched_by_cluster_1through10) <- cluster_names_list


#####Just Hydra 3 clusters ANOVA ########
imaging_cog_score_cluster_stats_anova_AG_matched <- lapply(imaging_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_matched))
})

imaging_cog_score_cluster_stats_anova_AG_unmatched <- lapply(imaging_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched))
})

imaging_cog_score_cluster_stats_anova_AG_resid <- lapply(imaging_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k3, list(i = as.name(x))), data = subset_with_clusters_AG_resid))
})

names(imaging_cog_score_cluster_stats_lm_AG_matched) <- imaging_measure_names
names(imaging_cog_score_cluster_stats_lm_AG_unmatched) <- imaging_measure_names
names(imaging_cog_score_cluster_stats_lm_AG_resid) <- imaging_measure_names

names(imaging_cog_score_cluster_stats_anova_AG_matched) <- imaging_measure_names
names(imaging_cog_score_cluster_stats_anova_AG_unmatched) <- imaging_measure_names
names(imaging_cog_score_cluster_stats_anova_AG_resid) <- imaging_measure_names

#checkmodel with visreg, uncomment when want to check
lapply(imaging_cog_score_cluster_stats_lm_AG_matched, function(x) {visreg(x)}) 
lapply(imaging_cog_score_cluster_stats_lm_AG_unmatched, function(x) {visreg(x)}) 
lapply(imaging_cog_score_cluster_stats_lm_AG_resid, function(x) {visreg(x)}) 

#WILL HAVE TO DO THIS FOR MALES AND FEMALES#


#######Chi-square for males/females and race########
##########By males, and by caucasians ##############

#Chi squared sex - significance p = 0.12
#if just want to look at clusters, not TD, look at commented line below, otherwise keep as is
###chisq_matched_sex <- chisq.test(subset_with_clusters_AG_matched$sex, (subset_with_clusters_AG_matched$Hydra_k3 != -1))
chisq_matched_sex <- chisq.test(subset_with_clusters_AG_matched$sex, subset_with_clusters_AG_matched$Hydra_k3)
chisq_matched_sex_pvalue <- chisq_matched_sex$p.value
total_people_Hydra_k3 <- c(length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == -1)]), 
                           length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 1)]), 
                           length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 2)]), 
                           length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 3)]))

num_men_all_clusters <- c(length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == -1 & subset_with_clusters_AG_matched$sex == 1)]), 
                          length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 1 & subset_with_clusters_AG_matched$sex == 1)]), 
                          length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 2 & subset_with_clusters_AG_matched$sex == 1)]), 
                          length(subset_with_clusters_AG_matched$sex[which(subset_with_clusters_AG_matched$Hydra_k3 == 3 & subset_with_clusters_AG_matched$sex == 1)]))
percent_men_all_clusters <- (num_men_all_clusters/total_people_Hydra_k3) * 100


#Chi square race - p-value = 4.854e-13
chisq_matched_race <- chisq.test(subset_with_clusters_AG_matched$race_binarized, subset_with_clusters_AG_matched$Hydra_k3)
chisq_matched_race_pvalue <- chisq_matched_race$p.value
num_caucasian_all_clusters <- c(length(subset_with_clusters_AG_matched$race_binarized[which(subset_with_clusters_AG_matched$Hydra_k3 == -1 & subset_with_clusters_AG_matched$race_binarized == 1)]), 
                                length(subset_with_clusters_AG_matched$race_binarized[which(subset_with_clusters_AG_matched$Hydra_k3 == 1 & subset_with_clusters_AG_matched$race_binarized == 1)]), 
                                length(subset_with_clusters_AG_matched$race_binarized[which(subset_with_clusters_AG_matched$Hydra_k3 == 2 & subset_with_clusters_AG_matched$race_binarized == 1)]), 
                                length(subset_with_clusters_AG_matched$race_binarized[which(subset_with_clusters_AG_matched$Hydra_k3 == 3 & subset_with_clusters_AG_matched$race_binarized == 1)]))
percent_caucasian_all_clusters <- (num_caucasian_all_clusters/total_people_Hydra_k3)*100

p_values <- c("", chisq_matched_sex_pvalue, "", chisq_matched_race_pvalue)
dat_sex_race <- data.frame(cl = c("TD", "Cluster1", "Cluster2", "Cluster3", "Significance"), 
                           num_males = c(num_men_all_clusters, "---"),
                           percent_males = round(c(percent_men_all_clusters, chisq_matched_sex_pvalue), 2), 
                           num_caucasians = c(num_caucasian_all_clusters, "---"),
                           percent_caucasian = round(c(percent_caucasian_all_clusters, chisq_matched_race_pvalue),2))
dat_sex_race_no_significance <-  data.frame(cl = c("TD", "Cluster1", "Cluster2", "Cluster3"), 
                                            num_males = (num_men_all_clusters),
                                            percent_males = percent_men_all_clusters, 
                                            num_caucasians = num_caucasian_all_clusters,
                                            percent_caucasian = percent_caucasian_all_clusters)                 
ggplot(dat_sex_race_no_significance, aes(x = cl, y = percent_males, fill=cl)) + geom_col() +
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) + ylim(0, 100) + xlab("Clusters") + ylab("% Male") + 
  ggtitle("Hydra_k3 % Male, p = 0.122") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2", "Cluster3")) +
  guides(fill=guide_legend(title=NULL))
ggplot(dat_sex_race_no_significance, aes(x = cl, y = percent_caucasian, fill=cl)) + geom_col() + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) + ylim(0, 100) + xlab("Clusters") + ylab("% Caucasian") + 
  ggtitle("Hydra_k3 % Caucasian, p = 4.854e-13") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2", "Cluster3")) + 
  guides(fill=guide_legend(title=NULL))

#############################
######### Bar Graphs ########
#############################

dat <- data.frame(cluster=c(subset_with_clusters_AG_matched$Hydra_k3), age=c(subset_with_clusters_AG_matched$age_in_years), medu1=c(subset_with_clusters_AG_matched$medu1), race=c(subset_with_clusters_AG_matched$race_binarized), sex_males=c(subset_with_clusters_AG_matched$sex))

#dat_age_sd_sem is the data frame that will be cluster(vertical), columns (age, medu), last rows are standard dec and sem
dat_age_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2", "Cluster3"), age = c(mean(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==-1)]), mean(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==1)]), mean(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==2)]), mean(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==3)])), age_sd = c(sd(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==-1)]), sd(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==1)]), sd(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==2)]), sd(subset_with_clusters_AG_matched$age_in_years[which(subset_with_clusters_AG_matched$Hydra_k3 ==3)])))
dat_age_sd_sem$sem <- dat_age_sd_sem$age_sd/sqrt(nrow(dat)) 
dat_medu_sd_sem <- data.frame(cl = c("TD", "Cluster1", "Cluster2", "Cluster3"), medu = c(mean(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==-1)]), mean(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==1)]), mean(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==2)]), mean(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==3)])), age_sd = c(sd(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==-1)]), sd(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==1)]), sd(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==2)]), sd(subset_with_clusters_AG_matched$medu1[which(subset_with_clusters_AG_matched$Hydra_k3 ==3)])))
dat_medu_sd_sem$sem <- dat_medu_sd_sem$age_sd/sqrt(nrow(dat))

dat_cont <- data.frame(cluster=c(subset_with_clusters_AG_matched$Hydra_k3), age=c(subset_with_clusters_AG_matched$age_in_years), medu1=c(subset_with_clusters_AG_matched$medu1))
dat_cat <- data.frame(cluster=c(subset_with_clusters_AG_matched$Hydra_k3), race=c(subset_with_clusters_AG_matched$race_binarized), sex=c(subset_with_clusters_AG_matched$sex))
dat.m <- melt(dat, id.vars='cluster')
dat_cont.m <- melt(dat_cont, id.vars='cluster')
dat_cat.m <- melt(dat_cat, id.vars='cluster')

#ggplot(dat.m, aes(fill=cluster, x=cluster, y=value))+ geom_bar(stat="identity") + facet_grid(.~variable) + 
# labs(x='Clusters_matched',y='') + scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) 

ggplot(dat_age_sd_sem, aes(x = cl, y = age, fill = cl)) + geom_col() + 
  geom_errorbar(aes(ymin=age-sem, ymax=age+sem),width=.2,position=position_dodge(.9)) + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) + ylim(0, 18) + xlab("Clusters") + ylab("Age in Years") + 
  ggtitle("Age by Cluster") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2", "Cluster3")) +
  guides(fill=guide_legend(title=NULL)) 

ggplot(dat_medu_sd_sem, aes(x = cl, y = medu, fill = cl)) + geom_col() + 
  geom_errorbar(aes(ymin=medu-sem, ymax=medu+sem),width=.2,position=position_dodge(.9)) + 
  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3")) + ylim(0, 18) + xlab("Clusters") + ylab("Maternal Ed in Years") + 
  ggtitle("Maternal Edu by Cluster") + scale_fill_discrete(breaks=c("TD", "Cluster1", "Cluster2", "Cluster3")) +
  guides(fill=guide_legend(title=NULL)) 

#ggplot(dat_cont.m, aes(fill=cluster, x=cluster, y=value)) + geom_bar(stat='identity') + 
# facet_grid(.~variable) + labs(x='Clusters_matched',y='') + 
#scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3"))

#ggplot(dat_cat.m, aes(fill=cluster, x=cluster, y=value)) + geom_bar(stat='identity') + 
# facet_grid(.~variable) + labs(x='Clusters_matched',y='') +
#  scale_x_discrete(limits=c("TD", "Cluster1", "Cluster2", "Cluster3"))


###################################################
## Extracting anovas with significant p values ####
###################################################

#Looking at Hydra_k3, but can redo with nested list
#Look at model summaries
models_anova <- lapply(imaging_cog_score_cluster_stats_anova_AG_matched, summary)

#Pull p-values
p_anova <- sapply(imaging_cog_score_cluster_stats_anova_AG_matched, function(v) v$"Pr(>F)"[1]) #$coef[,"Pr(>F)"][2]) #get the p value for dep binarized

#Convert to data frame
p_anova <- as.data.frame(p_anova)

#Print original p-values to three decimal places
p_round_anova <- round(p_anova,3)

#FDR correct p-values
pfdr_anova <- p.adjust(p_anova[,1],method="fdr")

#Convert to data frame
pfdr_anova <- as.data.frame(pfdr_anova)
row.names(pfdr_anova) <- imaging_measure_names

#To print fdr-corrected p-values to three decimal places
pfdr_round_anova <- round(pfdr_anova,3)

#List the NMF components that survive FDR correction
imaging_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]

#make a data frame with names and fdr values (rounded to 3 decimals)
imaging_names_and_fdr_values_anova <- data.frame(cbind(imaging_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))

#add titles to names_and_fdr tables
names(imaging_names_and_fdr_values_anova) <- c("imaging_measure", "p_FDR_corr")

#write the results of the mass univariate stats to files
write.csv(imaging_names_and_fdr_values_anova, file = "/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_k3_imaging_20180315.csv")


#############################
####### Demographics ########
#############################

#######Matched group all clusters ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_hydra_k3_AG_matched <- data.frame(subset_with_clusters_AG_matched$race_binarized, subset_with_clusters_AG_matched$sex, subset_with_clusters_AG_matched$medu1, subset_with_clusters_AG_matched$age_in_years, subset_with_clusters_AG_matched$dep_binarized, subset_with_clusters_AG_matched$Hydra_k3)
names(demo_hydra_k3_AG_matched) <- c(listVars)

#Change categorical values to have names
demo_hydra_k3_AG_matched$Depression <- ifelse(demo_hydra_k3_AG_matched$Depression == 1, "Depressed", "Non-depressed")
demo_hydra_k3_AG_matched$Race <- ifelse(demo_hydra_k3_AG_matched$Race == 1, "Caucasian", "Non-caucasian")
demo_hydra_k3_AG_matched$Sex <- ifelse(demo_hydra_k3_AG_matched$Sex == 1, "Male", "Female")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")
title <- c("hydra_k3 demographics")

#create demographics table
demo_hydra_k3_AG_matched_table <- CreateTableOne(vars = listVars, data = demo_hydra_k3_AG_matched, factorVars = cat_variables, strata = c("Cluster"))
print(demo_hydra_k3_AG_matched_table, showAllLevels = TRUE)

#######Unmatched group ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_hydra_k3_AG_unmatched <- data.frame(subset_with_clusters_AG_unmatched$race_binarized, subset_with_clusters_AG_unmatched$sex, subset_with_clusters_AG_unmatched$medu1, subset_with_clusters_AG_unmatched$age_in_years, subset_with_clusters_AG_unmatched$dep_binarized, subset_with_clusters_AG_unmatched$Hydra_k3)
names(demo_hydra_k3_AG_unmatched) <- c(listVars)

#Change categorical values to have names
demo_hydra_k3_AG_unmatched$Depression <- ifelse(demo_hydra_k3_AG_unmatched$Depression == 1, "Depressed", "Non-depressed")
demo_hydra_k3_AG_unmatched$Race <- ifelse(demo_hydra_k3_AG_unmatched$Race == 1, "Caucasian", "Non-caucasian")
demo_hydra_k3_AG_unmatched$Sex <- ifelse(demo_hydra_k3_AG_unmatched$Sex == 1, "Male", "Female")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")

title <- c("hydra_k3 demographics")

#create demographics table
demo_hydra_k3_AG_unmatched_table <- CreateTableOne(vars = listVars, data = demo_hydra_k3_AG_unmatched, factorVars = cat_variables, strata = c("Cluster"))
print(demo_hydra_k3_AG_unmatched_table, showAllLevels = TRUE)

#######Ununmatched residuals group ##############
#subset demographics
listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
demo_hydra_k3_AG_resid <- data.frame(subset_with_clusters_AG_resid$race_binarized, subset_with_clusters_AG_resid$sex, subset_with_clusters_AG_resid$medu1, subset_with_clusters_AG_resid$age_in_years, subset_with_clusters_AG_resid$dep_binarized, subset_with_clusters_AG_resid$Hydra_k3)
names(demo_hydra_k3_AG_resid) <- c(listVars)

#Change categorical values to have names
demo_hydra_k3_AG_resid$Depression <- ifelse(demo_hydra_k3_AG_resid$Depression == 1, "Depressed", "Non-depressed")
demo_hydra_k3_AG_resid$Race <- ifelse(demo_hydra_k3_AG_resid$Race == 1, "Caucasian", "Non-caucasian")
demo_hydra_k3_AG_resid$Sex <- ifelse(demo_hydra_k3_AG_resid$Sex == 1, "Male", "Female")

#make variable list
#table_titles <- c("Cluster 1", "Cluster 2")
#table_titles <- c("Cluster 1", "Depressed", "P-value")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex", "Cluster")

title <- c("Hydra_k3 demographics")

#create demographics table
demo_hydra_k3_AG_resid_table <- CreateTableOne(vars = listVars, data = demo_hydra_k3_AG_resid, factorVars = cat_variables, strata = c("Cluster"))
print(demo_hydra_k3_AG_resid_table, showAllLevels = TRUE)
