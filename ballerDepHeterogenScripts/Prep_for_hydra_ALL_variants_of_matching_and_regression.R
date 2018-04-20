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
dem_cnb_psych_health_merged <- merge(dem_cnb, psych_health, by = "bblid") #merge all 4 csvs, lost 1 person [134716] (had demographics, but no psych ratings): N = 9405

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

#Will do a variety of matchings, using all variables combined, then age, sex, medu and race separately, than combinging age and sex N=1542 (M=522, F = 1020)
m_all.out <-matchit(dep_binarized ~ age_in_years + race_binarized + medu1 + sex, data=dataset, method="nearest", distance="mahalanobis")
m_age.out <-matchit(dep_binarized ~ age_in_years, data=dataset, method="nearest", distance="mahalanobis")
m_sex.out <-matchit(dep_binarized ~ sex, data=dataset, method="nearest", distance="mahalanobis")
m_medu.out <-matchit(dep_binarized ~ medu1, data=dataset, method="nearest", distance="mahalanobis")
m_race.out <-matchit(dep_binarized ~ race_binarized, data=dataset, method="nearest", distance="mahalanobis")
m_age_and_sex.out <-matchit(dep_binarized ~ age_in_years + sex, data=dataset, method="nearest", distance="mahalanobis")


#going back to old match
#m.out <-matchit(dep_binarized ~ age_in_years + medu1, exact=c("race_binarized","sex"), data=dataset, method="nearest", distance="mahalanobis")


plot(m_all.out)
plot(m_age.out)
plot(m_sex.out)
plot(m_medu.out)
plot(m_race.out)
plot(m_age_and_sex.out)

#return the matched dataset 
#(with race binarized, before binarized, N = 1518, males 518, females 1000). 
#All 4 binarized, N = , males = , females = 
#age:  males = , females = 
#sex:  males = , females = 
#medu:  males = , females = 
#race :  males = , females = 
#age and sex:  males = , females = 

#WIth race binarized, N = 1530, males = 522, females = 1008 (both largest and random for m.order)
#with medu1 in formula, and exact on race and sex, N = 1542 (male 522, female 1020)
m_all.data <- match.data(m_all.out)
m_age.data <- match.data(m_age.out)
m_sex.data <- match.data(m_sex.out)
m_medu.data <- match.data(m_medu.out)
m_race.data <- match.data(m_race.out)
m_age_and_sex.data <- match.data(m_age_and_sex.out)

# Test for significant difference in age between groups
#results: Trend for race by depression, significant sex by depression, no diff age by depression
#t.test(age_in_years~dep_binarized, data=m.data)
#t.test(race_binarized~dep_binarized, data=m.data)
#t.test(sex~dep_binarized, data=m.data)
#t.test(medu1~dep_binarized, data=m.data)

# Re-plot
plot(m_all.data$age_in_years,jitter(m_all.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age all matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_age.data$age_in_years,jitter(m_age.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age age matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_sex.data$age_in_years,jitter(m_sex.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age sex matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_medu.data$age_in_years,jitter(m_medu.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age medu matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_race.data$age_in_years,jitter(m_race.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age race matched ")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

plot(m_age_and_sex.data$age_in_years,jitter(m_age_and_sex.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age age and sex matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))


# Make the final matched data set
data_all.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_all.data$unmatchedRows,]
data_all.matched$unmatchedRows = NULL
saveRDS(data_all.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/Matched_all.rds')

data_age.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age.data$unmatchedRows,]
data_age.matched$unmatchedRows = NULL
saveRDS(data_age.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/Matched_age.rds')

data_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_sex.data$unmatchedRows,]
data_sex.matched$unmatchedRows = NULL
saveRDS(data_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/Matched_sex.rds')

data_medu.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_medu.data$unmatchedRows,]
data_medu.matched$unmatchedRows = NULL
saveRDS(data_medu.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/Matched_medu.rds')

data_race.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_race.data$unmatchedRows,]
data_race.matched$unmatchedRows = NULL
saveRDS(data_race.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/Matched_race.rds')

data_age_and_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL
saveRDS(data_age_and_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/matched_age_and_sex.rds')

##############
#Demo Tablets#
##############
#Make table 1 (demographics) for matched data
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
matched_versions <- c("data_all", "data_age", "data_sex", "data_medu", "data_race", "data_age_and_sex")

for(vers in matched_versions) {
  demo_string_to_eval <- paste("data.frame(", vers , ".matched$race_binarized, ", vers, ".matched$sex, ", vers, ".matched$medu1, ", vers, ".matched$age_in_years, ", vers, ".matched$dep_binarized)", sep ="")
  demo_data.matched <- eval(parse(text=as.name(demo_string_to_eval)))
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
  print(paste("Version matching on ", vers))
  print(demo_data.matched_table, showAllLevels = TRUE)
  
})


##############################################
###### Post match age and sex residualizing###
##############################################

#GAM to get residuals for hydra since too hard to match on age and race
#cnb_measure_names <- names(data.matched)[grep("_z", names(data.matched))] #get the names of all the columns with _z in the name

#CNB_cog_score_stats_gam_age_and_sex <- lapply(cnb_measure_names, function(measure) 
#{
 # gam(substitute(i ~ s(age_in_years) + sex, list(i = as.name(measure))), data = data.matched)
  #lapply(CNB_cog_score_stats_gam_age_and_sex, function(x) {visreg(x)})
#}) 

#CNB_cog_score_stats_gam_age <- lapply(cnb_measure_names, function(measure) 
#{
  #gam(substitute(i ~ s(age_in_years) + race_binarized, list(i = as.name(x))), data = data.matched)
 # gam(substitute(i ~ s(age_in_years), list(i = as.name(measure))), data = data.matched)
  #lapply(CNB_cog_score_stats_gam_age, function(x) {visreg(x)})
#}) 

#names(CNB_cog_score_stats_gam_age_and_sex) <- cnb_measure_names
#names(CNB_cog_score_stats_gam_age) <- cnb_measure_names

#set CNB_cog_score_residuals_age_and_sex, and CNB_cog_score_residuals_age to original data frame and remove NAs
#CNB_cog_scores_residuals_age_and_sex <- data.matched[complete.cases(data.matched[14:39]),]
#CNB_cog_scores_residuals_age <- data.matched[complete.cases(data.matched[14:39]),]

#substitute residuals_age_and_sex from CNB_cog_score_stats_gam for results.  Did not work with for loop when indexing by cog score, did work with while loop and using numbers 'cnt'

#cnt = 14
#cnt_gam = 1
#for(cog_score in cnb_measure_names) did not work as for loop

#get residuals_age_and_sex, put these in to hydra instead of main #s
#while(cnt < 40)
#{
 # CNB_cog_scores_residuals_age_and_sex[[cnt]] <- as.vector(residuals(CNB_cog_score_stats_gam_age_and_sex[[cnt_gam]]))
  #CNB_cog_scores_residuals_age[[cnt]] <- as.vector(residuals(CNB_cog_score_stats_gam_age[[cnt_gam]]))
  
  #cnt = cnt + 1
  #cnt_gam = cnt_gam + 1
  
#}


############################
####make csvs for hydra#####
############################

#Subset only variables needed for hydra analysis
for(vers in matched_versions) {
  print(vers)
  string_to_eval <- paste("data.frame(", vers, ".matched[1], ", vers, ".matched[14:39], ", vers, ".matched[77])", sep = "")
  subset_bblidAndCog_features <- eval(parse(text = as.name(string_to_eval)))
  string_for_csv <- paste("write.csv(subset_bblidAndCog_features, file=\"/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/no_covariance/", vers, ".csv\" , row.names = FALSE, quote = FALSE)", sep = "")
  eval(parse(text = as.name(string_for_csv)))
}
