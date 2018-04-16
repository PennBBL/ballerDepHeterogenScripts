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
cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_frar_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv

#remove people with NA for race, age, or sex.  START WITH N = 9498
demographics_noNA_race <- demographics[!is.na(demographics$race),] #everyone has a race, N = 9498
demographics_noNA_race_age <- demographics_noNA_race[!is.na(demographics_noNA_race$ageAtClinicalAssess1),] # 86 people do not have age at clinical assessment.  N = 9412
demographics_noNA_race_age_sex <- demographics_noNA_race_age[!is.na(demographics_noNA_race_age$sex),] #everyone has a sex, N = 9412
demographics_noNA_race_age_andCNBage_sex <- demographics_noNA_race_age_sex[!is.na(demographics_noNA_race_age_sex$ageAtCnb1),] #6 people do not have ageAtCnb1, N = 9406

#remove people with NA for depression or total psych score, START WITH N = 9498
psych_summary_no_NA_dep <- psych_summary[!is.na(psych_summary$smry_dep),] #take out those with NA for depression, 87 people N = 9411
psych_summary_no_NA_dep_and_smry_psych_overall <- psych_summary_no_NA_dep[!is.na(psych_summary_no_NA_dep$smry_psych_overall_rtg),] #take out those with NA for overall psych rtg, no additional people lost, N = 9411

#merge the csvs
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

#age demeaned and squared, from Toni
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$ageSq <- as.numeric(I(scale(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$age_in_years, scale = FALSE, center = TRUE)^2))

#race binarized for plotting purposes, caucasian 1, non-caucasion 0
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race_binarized <- ifelse(as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race) == 1, 1, 0)

#remove people with NA in their cognitive measures
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]

########################################
###### Using residuals age######
########################################

#GAM to get residuals for hydra since too hard to match on age and race
cnb_measure_names <- names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)[grep("_z", names(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED))] #get the names of all the columns with _z in the name

CNB_cog_score_stats_gam_age_and_sex <- lapply(cnb_measure_names, function(x) 
{
  gam(substitute(i ~ s(age_in_years) + sex, list(i = as.name(x))), data = subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)
  #lapply(CNB_cog_score_stats_gam_age_and_sex, function(x) {visreg(x)})
}) 

CNB_cog_score_stats_gam_age <- lapply(cnb_measure_names, function(x) 
{
  #gam(substitute(i ~ s(age_in_years) + race_binarized, list(i = as.name(x))), data = subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)
  gam(substitute(i ~ s(age_in_years), list(i = as.name(x))), data = subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED)
  #lapply(CNB_cog_score_stats_gam_age, function(x) {visreg(x)})
}) 

names(CNB_cog_score_stats_gam_age_and_sex) <- cnb_measure_names
names(CNB_cog_score_stats_gam_age) <- cnb_measure_names

#set CNB_cog_score_residuals_age_and_sex, and CNB_cog_score_residuals_age to original data frame and remove NAs
CNB_cog_scores_residuals_age_and_sex <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]
CNB_cog_scores_residuals_age <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]

#substitute residuals_age_and_sex from CNB_cog_score_stats_gam for results.  Did not work with for loop when indexing by cog score, did work with while loop and using numbers 'cnt'

cnt = 14
cnt_gam = 1
#for(cog_score in cnb_measure_names) did not work as for loop

#get residuals_age_and_sex, put these in to hydra instead of main #s
while(cnt < 40)
{
  CNB_cog_scores_residuals_age_and_sex[[cnt]] <- as.vector(residuals(CNB_cog_score_stats_gam_age_and_sex[[cnt_gam]]))
  CNB_cog_scores_residuals_age[[cnt]] <- as.vector(residuals(CNB_cog_score_stats_gam_age[[cnt_gam]]))
  
  cnt = cnt + 1
  cnt_gam = cnt_gam + 1
  
}

#Subset only variables needed for hydra analysis, use _r to make sure it is clear that these are residuals_age_and_sex we are looking at
#(BBLID, cognitive variables, depression), also do by males(1555)/females(1729) separately(use only age regressed residuals, not sex + age)
subset_bblidAndCog_features <- data.frame(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[1], subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39], subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[77])
subset_bblidAndCog_features_age_and_sex_r <- data.frame(cbind(CNB_cog_scores_residuals_age_and_sex[1], CNB_cog_scores_residuals_age_and_sex[14:39], CNB_cog_scores_residuals_age_and_sex[77]))
subset_bblidAndCog_features_age_r <- data.frame(cbind(CNB_cog_scores_residuals_age[1], CNB_cog_scores_residuals_age[14:39], CNB_cog_scores_residuals_age[77]))

#remove NAs from hydra - n(3022), male (1434)/ female (1588); IF WE want to keep these people, comment out these lines
subset_bblidAndCog_features <- subset_bblidAndCog_features[complete.cases(subset_bblidAndCog_features[2:27]),]
subset_bblidAndCog_features_age_and_sex_r <- subset_bblidAndCog_features_age_and_sex_r[complete.cases(subset_bblidAndCog_features_age_and_sex_r[2:27]),]
subset_bblidAndCog_features_age_r <- subset_bblidAndCog_features_age_r[complete.cases(subset_bblidAndCog_features_age_r[2:27]),]

#covariates, age
subset_bblidAndCovariates_age <- data.frame(cbind(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[78]))

#save files for hydra
write.csv(subset_bblidAndCog_features, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_all_gender/Features.csv", row.names = FALSE, quote = FALSE)
write.csv(subset_bblidAndCovariates_age, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_all_gender/residuals/Covariates.csv", row.names = FALSE, quote = FALSE)
write.csv(subset_bblidAndCog_features_age_and_sex_r, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_all_gender/residuals/age_and_sex/Features.csv", row.names = FALSE, quote = FALSE)
write.csv(subset_bblidAndCog_features_age_r, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_all_gender/residuals/age/Features.csv", row.names = FALSE, quote = FALSE)


################################
######## MATCHING ##############
################################

############################
## Standard, no residuals ##
############################

#match with matchit n(3022), male (1434)/ female (1588), this is NOT residuals
data.unmatched = subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),]
data.unmatched$unmatchedRows =rownames(data.unmatched)
dataset = data.unmatched

# Some preprocessing
#dataset = dplyr::select(dataset, sex, age_in_years, ageSq, medu1, race, dep_binarized, unmatchedRows)
dataset = dplyr::select(dataset, sex, age_in_years, ageSq, medu1, race_binarized, dep_binarized, unmatchedRows)

#dataset = dplyr::filter(dataset, !is.na(group))
# Dep: 1, Health = 0 
dataset$dep_binarized = 1*(dataset$dep_binarized==1) 

#"male": 1, "female": 0
dataset$sex = 1*(dataset$sex==1)

# Remove subjects with NA for maternal edu, new N = 3256, males = 1539, females 1717
dataset <- dataset[!is.na(dataset$medu1),]

# Plot prematch
plot(dataset$age_in_years,jitter(dataset$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")

legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

#race binarized, age in years and sex covaried out, and taking it out from exact N=1542 (M=522, F = 1020)
m.out <-matchit(dep_binarized ~ age_in_years, data=dataset, method="nearest", distance="mahalanobis")

plot(m.out)

#return the matched dataset 
#(with race binarized, before binarized, N = 1518, males 518, females 1000). 
#WIth race binarized, N = 1530, males = 522, females = 1008 (both largest and random for m.order)
#with medu1 in formula, and exact on race and sex, N = 1542 (male 522, female 1020)
m.data <- match.data(m.out)

# Test for significant difference in age between groups
#results: Trend for race by depression, significant sex by depression, no diff age by depression
t.test(age_in_years~dep_binarized, data=m.data)
t.test(race_binarized~dep_binarized, data=m.data)
t.test(sex~dep_binarized, data=m.data)
t.test(medu1~dep_binarized, data=m.data)

# Re-plot
plot(m.data$age_in_years,jitter(m.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")

legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

# Make the final matched data set
data.matched = data.unmatched[data.unmatched$unmatchedRows%in%m.data$unmatchedRows,]
data.matched$unmatchedRows = NULL
saveRDS(data.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched//Matched_age_20180403_matched.rds')

#####################################################
## With residuals, both age_and_sex, and age alone ##
#####################################################

#match with matchit n(3022), male (1434)/ female (1588), this is NOT residuals

data_age_and_sex.unmatched = CNB_cog_scores_residuals_age_and_sex[complete.cases(CNB_cog_scores_residuals_age_and_sex[14:39]),]
data_age_and_sex.unmatched$unmatchedRows =rownames(data_age_and_sex.unmatched)
dataset_age_and_sex = data_age_and_sex.unmatched

data_age.unmatched = CNB_cog_scores_residuals_age[complete.cases(CNB_cog_scores_residuals_age[14:39]),]
data_age.unmatched$unmatchedRows =rownames(data_age.unmatched)
dataset_age = data_age.unmatched


# Some preprocessing
#dataset = dplyr::select(dataset, sex, age_in_years, ageSq, medu1, race, dep_binarized, unmatchedRows)
dataset_age_and_sex = dplyr::select(dataset, sex, age_in_years, ageSq, medu1, race_binarized, dep_binarized, unmatchedRows)
dataset_age = dplyr::select(dataset, sex, age_in_years, ageSq, medu1, race_binarized, dep_binarized, unmatchedRows)

#dataset = dplyr::filter(dataset, !is.na(group))
# Dep: 1, Health = 0 
dataset_age_and_sex$dep_binarized = 1*(dataset_age_and_sex$dep_binarized==1) 
dataset_age$dep_binarized = 1*(dataset_age$dep_binarized==1) 

#"male": 1, "female": 0
dataset_age_and_sex$sex = 1*(dataset_age_and_sex$sex==1)
dataset_age$sex = 1*(dataset_age$sex==1)

# Remove subjects with NA for maternal edu, new N = 3256, males = 1539, females 1717
dataset_age_and_sex <- dataset_age_and_sex[!is.na(dataset_age_and_sex$medu1),]
dataset_age <- dataset_age[!is.na(dataset_age$medu1),]

# Plot prematch
plot(dataset_age_and_sex$age_in_years,jitter(dataset_age_and_sex$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(dataset_age$age_in_years,jitter(dataset_age$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

#race binarized, age in years and sex covaried out, and taking it out from exact N=1542 (M=522, F = 1020)
m_age_and_sex.out <-matchit(dep_binarized ~ age_in_years + sex, data=dataset_age_and_sex, method="nearest", distance="mahalanobis")
m_age.out <-matchit(dep_binarized ~ age_in_years, data=dataset_age, method="nearest", distance="mahalanobis")


plot(m_age_and_sex.out)
plot(m_age.out)

#return the matched dataset 
#(with race binarized, before binarized, N = 1518, males 518, females 1000). 
#WIth race binarized, N = 1530, males = 522, females = 1008 (both largest and random for m.order)
#with medu1 in formula, and exact on race and sex, N = 1542 (male 522, female 1020)
m_age_and_sex.data <- match.data(m_age_and_sex.out)
m_age.data <- match.data(m_age.out)

# Test for significant difference in age between groups
#results: Trend for race by depression, significant sex by depression, no diff age by depression
t.test(age_in_years~dep_binarized, data=m_age_and_sex.data)
t.test(race_binarized~dep_binarized, data=m_age_and_sex.data)
t.test(sex~dep_binarized, data=m_age_and_sex.data)
t.test(medu1~dep_binarized, data=m_age_and_sex.data)
t.test(age_in_years~dep_binarized, data=m_age.data)
t.test(race_binarized~dep_binarized, data=m_age.data)
t.test(sex~dep_binarized, data=m_age.data)
t.test(medu1~dep_binarized, data=m_age.data)

# Re-plot
#plot(m.data$age_in_years,jitter(m.data$medu1, factor=3), col=2*m.data$race+m.data$dep_binarized+1,pch=16, ylab="Maternal Edu", xlab="Age")
plot(m_age_and_sex.data$age_in_years,jitter(m.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_age.data$age_in_years,jitter(m.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

# Make the final matched data set
data_age_and_sex.matched = data_age_and_sex.unmatched[data_age_and_sex.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL
saveRDS(data_age_and_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched_age_sex_and_z_resid/Matched_age_and_sex.rds')

data_age.matched = data_age.unmatched[data_age.unmatched$unmatchedRows%in%m_age.data$unmatchedRows,]
data_age.matched$unmatchedRows = NULL
saveRDS(data_age.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched_age_and_z_resid//Matched_age.rds')

####FOR HYDRA Matched, non-residualized #######

#all genders together
for_hydra_data.matched_features <- data.frame(cbind(data.matched[1], data.matched[14:39], data.matched[77]))
for_hydra_data.matched_cov = subset.data.frame(data.frame(cbind(data.matched[1], data.matched[78])))

write.csv(for_hydra_data.matched_features, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/Features.csv", row.names = FALSE, quote = FALSE)
write.csv(for_hydra_data.matched_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/Covariates.csv", row.names = FALSE, quote = FALSE)

#Make table 1 (demographics) for matched data
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep

demo_data.matched <- data.frame(data.matched$race_binarized, data.matched$sex, data.matched$medu1, data.matched$age_in_years, data.matched$dep_binarized)
#demo_data.matched <- data.frame(for_hydra_data.matched_cov$race_binarized, for_hydra_data.matched_cov$sex, for_hydra_data.matched_cov$medu1, for_hydra_data.matched_cov$age_in_years, for_hydra_data.matched_cov$dep_binarized)

names(demo_data.matched) <- c(listVars)

#Change categorical values to have names
demo_data.matched$Depression <- ifelse(demo_data.matched$Depression == 1, "Depressed", "Non-depressed")
demo_data.matched$Race <- ifelse(demo_data.matched$Race == 1, "Caucasian", "Non-caucasian")
demo_data.matched$Sex <- ifelse(demo_data.matched$Sex == 1, "Male", "Female")
#subset_demographics_for_table1_males$Depression <- ifelse(subset_demographics_for_table1_males$Depression == 1, "Depressed", "Non-depressed")
#subset_demographics_for_table1_males$Race <- ifelse(subset_demographics_for_table1_males$Race == "1", "Caucasian","Non-caucasian")
#subset_demographics_for_table1_females$Depression <- ifelse(subset_demographics_for_table1_females$Depression == 1, "Depressed", "Non-depressed")
#subset_demographics_for_table1_females$Race <- ifelse(subset_demographics_for_table1_females$Race == "1", "Caucasian","Non-caucasian")

#make variable list
table_titles <- c("Non-depressed", "Depressed", "P-value")
#table_titles_males <- c("Males Non-depressed", "Males depressed", "p-value")
#table_titles_males <- c("Females Non-depressed", "Females depressed", "p-value")

#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex")
#cat_variables <- c("Race")

#create demographics table
demo_data.matched_table <- CreateTableOne(vars = listVars, data = demo_data.matched, factorVars = cat_variables, strata = c("Depression"))
print(demo_data.matched_table, showAllLevels = TRUE)
#demographics_table_males <- CreateTableOne(vars = listVars, data = subset_demographics_for_table1_males, factorVars = cat_variables, strata = c("Depression"))
#print(demographics_table_males, showAllLevels = TRUE)

#females depressed = 514, non-depressed 1215
#demographics_table_females <- CreateTableOne(vars = listVars, data = subset_demographics_for_table1_females, factorVars = cat_variables, strata = c("Depression"))
#print(demographics_table_females, showAllLevels = TRUE)


####FOR HYDRA Matched, residualized age and sex, and age  THIS NEEDS TO BE EDITED#######

#all genders together
for_hydra_data_age_and_sex.matched_features <- data.frame(cbind(data_age_and_sex.matched[1], data_age_and_sex.matched[14:39], data_age_and_sex.matched[77]))
#for_hydra_data_age_and_sex.matched_cov = subset.data.frame(data.frame(cbind(data_age_and_sex.matched[1], data_age_and_sex.matched[78])))

for_hydra_data_age.matched_features <- data.frame(cbind(data_age.matched[1], data_age.matched[14:39], data_age.matched[77]))
#for_hydra_data_age.matched_cov = subset.data.frame(data.frame(cbind(data_age.matched[1], data_age.matched[78])))

write.csv(for_hydra_data_age_and_sex.matched_features, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched_age_sex_and_z_resid/Features.csv", row.names = FALSE, quote = FALSE)
write.csv(for_hydra_data_age.matched_features, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched_age_and_z_resid/Features.csv", row.names = FALSE, quote = FALSE)

#Make table 1 (demographics) for matched data
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep

demo_data_age_and_sex.matched <- data.frame(data_age_and_sex.matched$race_binarized, data_age_and_sex.matched$sex, data_age_and_sex.matched$medu1, data_age_and_sex.matched$age_in_years, data_age_and_sex.matched$dep_binarized)
names(demo_data_age_and_sex.matched) <- c(listVars)

demo_data_age.matched <- data.frame(data_age.matched$race_binarized, data_age.matched$sex, data_age.matched$medu1, data_age.matched$age_in_years, data_age.matched$dep_binarized)
names(demo_data_age.matched) <- c(listVars)

#Change categorical values to have names
demo_data_age_and_sex.matched$Depression <- ifelse(demo_data_age_and_sex.matched$Depression == 1, "Depressed", "Non-depressed")
demo_data_age_and_sex.matched$Race <- ifelse(demo_data_age_and_sex.matched$Race == 1, "Caucasian", "Non-caucasian")
demo_data_age_and_sex.matched$Sex <- ifelse(demo_data_age_and_sex.matched$Sex == 1, "Male", "Female")

demo_data_age.matched$Depression <- ifelse(demo_data_age.matched$Depression == 1, "Depressed", "Non-depressed")
demo_data_age.matched$Race <- ifelse(demo_data_age.matched$Race == 1, "Caucasian", "Non-caucasian")
demo_data_age.matched$Sex <- ifelse(demo_data_age.matched$Sex == 1, "Male", "Female")

#make variable list
table_titles <- c("Non-depressed", "Depressed", "P-value")


#Define Categorical Variables
cat_variables <- c("Race", "Depression", "Sex")


#create demographics table
demo_data_age_and_sex.matched_table <- CreateTableOne(vars = listVars, data = demo_data_age_and_sex.matched, factorVars = cat_variables, strata = c("Depression"))
print(demo_data_age_and_sex.matched_table, showAllLevels = TRUE)

demo_data_age.matched_table <- CreateTableOne(vars = listVars, data = demo_data_age.matched, factorVars = cat_variables, strata = c("Depression"))
print(demo_data_age.matched_table, showAllLevels = TRUE)

