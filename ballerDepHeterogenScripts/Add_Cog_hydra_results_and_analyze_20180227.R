library(visreg)
library(mgcv)
library(tableone)
library(dplyr)
library(plm)
library(MatchIt)

###### This script reads in demographics, cnb_scores, health and psych summaries, merges them, removes NAs, codes and separates by depression#####
####Also preps for hydra, both using a typical GAM model and matching (we lose a lot of people) and also with residuals plotted so we don't have to match#########
########Also provides matched data sets and tests them, if we decide to use them.  It significantly reduces N to match (dataset from 3022 to 1424)


#######################################################
############ READ IN, MERGE AND SUBSET DATA############
#######################################################

#read in csvs
demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv
cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv

#read in hydra cluster data -> AG (all_gender), M(males), F(females) 
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

#make subsets
subset_just_dep_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, (medicalratingExclude == 0) & (smry_dep == 4), select = "bblid") #subset people who were not medically excluded and who are depressed, N = 776
subset_no_psych_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, (medicalratingExclude == 0) & (smry_psych_overall_rtg < 4), select = "bblid") #subset people who are psychiatrically healthy, N = 2508
subset_dep_or_no_psych_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, (medicalratingExclude == 0) & ((smry_dep == 4) | (smry_psych_overall_rtg <4))) #subset including both depressed and healthies, good for regressions, N = 3284

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
### Merge clustering data with subsetted data###
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


###################################################
####Run visreg on clustered data, using lapply#####
###################################################

#get CNB measure names
cnb_measure_names <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("_z", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name

#using lm, results stored in list

CNB_cog_score_cluster_stats_lm_AG_matched <- lapply(cnb_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_matched)
})

CNB_cog_score_cluster_stats_lm_AG_unmatched <- lapply(cnb_measure_names, function(x) 
{
   lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched)
 # lm(substitute(i ~ Hydra_k2 + race + age_in_years, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched)
})

CNB_cog_score_cluster_stats_lm_AG_resid <- lapply(cnb_measure_names, function(x) 
{
  lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_resid)
})

CNB_cog_score_cluster_stats_anova_AG_matched <- lapply(cnb_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_matched))
})

CNB_cog_score_cluster_stats_anova_AG_unmatched <- lapply(cnb_measure_names, function(x) 
{
  anova(lm(substitute(i ~ Hydra_k2, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched))
#  anova(lm(substitute(i ~ Hydra_k2 + race + age_in_years, list(i = as.name(x))), data = subset_with_clusters_AG_unmatched))
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


#checkmodel with visreg
lapply(CNB_cog_score_cluster_stats_lm_AG_matched, function(x) {visreg(x)}) 
lapply(CNB_cog_score_cluster_stats_lm_AG_unmatched, function(x) {visreg(x)}) 
lapply(CNB_cog_score_cluster_stats_lm_AG_resid, function(x) {visreg(x)}) 


####From prep script, will obviously need to be edited)
#Make table 1 (demographics)
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep

demo_data.matched <- data.frame(data.matched$race_binarized, data.matched$sex, data.matched$medu1, data.matched$age_in_years, data.matched$dep_binarized)
names(demo_data.matched) <- c(listVars)

#Change categorical values to have names
demo_data.matched$Depression <- ifelse(demo_data.matched$Depression == 1, "Depressed", "Non-depressed")
demo_data.matched$Race <- ifelse(demo_data.matched$Race == 1, "Caucasian", "Non-caucasian")
demo_data.matched$Sex <- ifelse(demo_data.matched$Sex == 1, "Male", "Female")

#make variable list
table_titles <- c("Non-depressed", "Depressed", "P-value")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex")

#create demographics table
demo_data.matched_table <- CreateTableOne(vars = listVars, data = demo_data.matched, factorVars = cat_variables, strata = c("Depression"))
print(demo_data.matched_table, showAllLevels = TRUE)

