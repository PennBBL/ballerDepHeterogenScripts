library(visreg)
library(mgcv)
library(tableone)
library(dplyr)
library(plm)
library(MatchIt)

####on 6/11, now look at R markdown for updates


###### This script reads in demographics, cnb_scores, health, psych summaries, and imaging, merges them, removes NAs, codes and separates by depression#####
########Also provides matched data sets and tests them, if we decide to use them.  It significantly reduces N to match (dataset from 3022 to 1424)
######## Uses double match to preferentially keep people who have imaging

#######################################################
############ READ IN, MERGE AND SUBSET DATA############
#######################################################

#to get a reproducible result from matchit
set.seed(1)

#read in csvs
demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv
cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv
imaging_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/neuroimaging/t1struct/n1601_t1QaData_20170306.csv")

#remove people with NA for race, age, or sex.  START WITH N = 9498
demographics_noNA_race <- demographics[!is.na(demographics$race),] #everyone has a race, N = 9498
demographics_noNA_race_age <- demographics_noNA_race[!is.na(demographics_noNA_race$ageAtClinicalAssess1),] # 86 people do not have age at clinical assessment.  N = 9412
demographics_noNA_race_age_sex <- demographics_noNA_race_age[!is.na(demographics_noNA_race_age$sex),] #everyone has a sex, N = 9412
demographics_noNA_race_age_andCNBage_sex <- demographics_noNA_race_age_sex[!is.na(demographics_noNA_race_age_sex$ageAtCnb1),] #6 people do not have ageAtCnb1, N = 9406

#remove people with NA for depression or total psych score, START WITH N = 9498
psych_summary_no_NA_dep <- psych_summary[!is.na(psych_summary$smry_dep),] #take out those with NA for depression, 87 people N = 9411
psych_summary_no_NA_dep_and_smry_psych_overall <- psych_summary_no_NA_dep[!is.na(psych_summary_no_NA_dep$smry_psych_overall_rtg),] #take out those with NA for overall psych rtg, no additional people lost, N = 9411

#only include good imaging scans, n = 1540
imaging_include <- subset.data.frame(imaging_summary, (t1Exclude == 0))

#merge the csvs
#merge demographics and cnb #this is if we want to include people without full demographic data
dem_cnb <- merge(demographics_noNA_race_age_andCNBage_sex, cnb_scores, by = "bblid") #merge demographics and cnb, N = 9406
psych_health <- merge(psych_summary_no_NA_dep_and_smry_psych_overall, health, by = "bblid") #merge psych and health, N = 9411
dem_cnb_psych_health_merged <- merge(dem_cnb, psych_health, by = "bblid") #merge all 4 csvs, lost 1 person [134716] (had demographics, but no psych ratings): N = 9405
dem_merged_with_imaging_as_well <- merge(dem_cnb_psych_health_merged, imaging_include, by = "bblid")

#make subsets, 
subset_just_dep_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, ((medicalratingExclude == 0) & (smry_dep == 4))) #subset people who were not medically excluded and who are depressed, N = 776
subset_no_psych_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, ((medicalratingExclude == 0) & (smry_psych_overall_rtg < 4))) #subset people who are psychiatrically healthy, N = 2508
subset_nondep_imaging <-  subset.data.frame(dem_merged_with_imaging_as_well, ((medicalratingExclude == 0) & (smry_dep < 4))) #n = 1266
subset_dep_imaging <-  subset.data.frame(dem_merged_with_imaging_as_well, ((medicalratingExclude == 0) & (smry_dep == 4)))#n = 200

#subsets with depressed AND non depressed, without OR with imaging :  without imaging n = 3284, with imaging, n = 703
subset_dep_or_no_psych_and_no_medicalratingExclude <- subset.data.frame(dem_cnb_psych_health_merged, (medicalratingExclude == 0) & ((smry_dep == 4) | (smry_psych_overall_rtg <4))) #subset including both depressed and healthies, N = 3284
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging <- subset.data.frame(dem_merged_with_imaging_as_well, (medicalratingExclude == 0) & ((smry_dep == 4) | (smry_psych_overall_rtg <4))) #subset including both depressed and healthies, N = 703

#would binarize depression smry score to -1 (less than 4, not depressed) and 1 (score 4 , depressed)
dep_binarized <- ifelse(subset_dep_or_no_psych_and_no_medicalratingExclude$smry_dep == 4, 1, -1)
dep_binarized_with_imaging <- ifelse(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging$smry_dep == 4, 1, -1)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- cbind(subset_dep_or_no_psych_and_no_medicalratingExclude, dep_binarized) #N = 3284
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED <- cbind(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging, dep_binarized_with_imaging) #N = 703

#make depression and gender into factor scores 
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$dep_binarized)
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$dep_binarized <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$dep_binarized)

subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$sex)
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$sex <- as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$sex)

#divide ageAtCNB by 12 for age 
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$age_in_years <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$ageAtCnb1/12
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$age_in_years <- subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$ageAtCnb1/12

#age demeaned and squared
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$ageSq <- as.numeric(I(scale(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$age_in_years, scale = FALSE, center = TRUE)^2))
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$ageSq <- as.numeric(I(scale(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$age_in_years, scale = FALSE, center = TRUE)^2))

#race binarized for plotting purposes, caucasian 1, non-caucasion 0
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race_binarized <- ifelse(as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$race) == 1, 1, 0)
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$race_binarized <- ifelse(as.factor(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED$race) == 1, 1, 0)

#remove people with NA in their cognitive measures, we lose 252 for whole group, and 57 with imaging at this stage, n for NO imaging = 3022, n for WITH IMAGING = 646(dep = 187, non-dep = 459)
subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[14:39]),] #n=3022
subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED <- subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED[14:39]),] # n = 646


################################
######## MATCHING ##############
################################

############################
## Standard, no residuals ##
############################

#Steps for this section
####1. Match depress w/ imaging to controls w/imaging
####2. Removed these from full group
####3. Match remaining depressed without imaging to controls who weren't previously matched
####4. Combine depressed w/imaging to depressed w/out imaging and controls w/imaging to controls w/out imaging
####5. Check distribution of these groups

#########Let's start by matching only depressed people with imaging to controls with imaging###
#match with matchit depressed n =187
#### matching on all: dep: m/f (70/117), non dep:m/f (71/116)
#### matching on age: dep: m/f (70/117), non dep:m/f (90/97)
#### matching on sex: dep: m/f (70/117), non dep:m/f (70/117)
#### matching on medu: dep: m/f (70/117), non dep:m/f (90/97)
#### matching on race: dep: m/f (70/117), non dep:m/f (90/97)
#### matching on age_and_sex: dep: m/f (70/117), non dep:m/f (70/117)

data.unmatched = subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED[complete.cases(subset_dep_or_no_psych_and_no_medicalratingExclude_with_imaging_DEPBINARIZED[14:39]),]
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
saveRDS(data_all.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_imaging_all.rds')

data_age.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age.data$unmatchedRows,]
data_age.matched$unmatchedRows = NULL
saveRDS(data_age.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_imgaging_age.rds')

data_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_sex.data$unmatchedRows,]
data_sex.matched$unmatchedRows = NULL
saveRDS(data_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_imaging_sex.rds')

data_medu.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_medu.data$unmatchedRows,]
data_medu.matched$unmatchedRows = NULL
saveRDS(data_medu.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_imaging_medu.rds')

data_race.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_race.data$unmatchedRows,]
data_race.matched$unmatchedRows = NULL
saveRDS(data_race.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_imaging_race.rds')

data_age_and_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL
saveRDS(data_age_and_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/matched_age_and_sex_imaging.rds')


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
  
  #rename the data***.matched to have an imaging end to it, so we can use it to combine
  rename_string_to_eval <- paste(vers, ".matched_imaging <- ", vers, ".matched", sep = "")
  eval(parse(text = as.name(rename_string_to_eval)))
}

############Remove people from main group who have imaging ##############
#number remaining  =3022(before match) - 374 (num people matched through imaging match) = 2648
people_from_imaging_match_all <- data_all.matched$bblid
people_from_imaging_match_age <- data_age.matched$bblid
people_from_imaging_match_sex <- data_sex.matched$bblid
people_from_imaging_match_medu <- data_medu.matched$bblid
people_from_imaging_match_race <- data_race.matched$bblid
people_from_imaging_match_age_and_sex <- data_age_and_sex.matched$bblid

subset_people_who_were_not_included_in_imaging_match_all <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_all)),]
subset_people_who_were_not_included_in_imaging_match_age <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_age)),]
subset_people_who_were_not_included_in_imaging_match_sex <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_sex)),]
subset_people_who_were_not_included_in_imaging_match_medu <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_medu)),]
subset_people_who_were_not_included_in_imaging_match_race <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_race)),]
subset_people_who_were_not_included_in_imaging_match_age_and_sex <- subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED[!(subset_dep_or_no_psych_and_no_medicalratingExclude_DEPBINARIZED$bblid %in% c(people_from_imaging_match_age_and_sex)),]

###############Match remainder ####################
#match with matchit n(3022), male (1434)/ female (1588), this is NOT residuals  -> just do for age right now

######## Ted, is this right???? #####
data.unmatched = subset_people_who_were_not_included_in_imaging_match_age_and_sex[complete.cases(subset_people_who_were_not_included_in_imaging_match_age_and_sex[14:39]),]
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

#Will do a variety of matchings, using all variables combined, then age, sex, medu and race separately
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
saveRDS(data_all.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_all_after_imaging_match.rds')

data_age.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age.data$unmatchedRows,]
data_age.matched$unmatchedRows = NULL
saveRDS(data_age.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_age_after_imaging_match.rds')

data_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_sex.data$unmatchedRows,]
data_sex.matched$unmatchedRows = NULL
saveRDS(data_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_sex_after_imaging_match.rds')

data_medu.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_medu.data$unmatchedRows,]
data_medu.matched$unmatchedRows = NULL
saveRDS(data_medu.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_medu_after_imaging_match.rds')

data_race.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_race.data$unmatchedRows,]
data_race.matched$unmatchedRows = NULL
saveRDS(data_race.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/Matched_race_after_imaging_match.rds')

data_age_and_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL
saveRDS(data_age_and_sex.matched, file='/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/matched_age_and_sex_after_imaging_match.rds')


##############
#Demo Tablets#
##############
#Make table 1 (demographics) for matched data after removing imaging (525 depressed, 525 non-depressed)

#data_all : dep: m/f 165/360, non-dep: m/f 165/360
#data_age : dep: m/f 165/360, non-dep: m/f 280/245
#data_sex : dep: m/f 165/360, non-dep: m/f 165/360
#data_medu : dep: m/f 165/360, non-dep: m/f 265/260
#data_race : dep: m/f 165/360, non-dep: m/f 287/238
#data_age_and_sex : dep: m/f 165/360, non-dep: m/f 165/360

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
  
  #rename the data***.matched to have a non-imaging end to it, so we can use it to combine
  rename_string_to_eval <- paste(vers, ".matched_non_imaging <- ", vers, ".matched", sep = "")
  eval(parse(text = as.name(rename_string_to_eval)))
  
}

###############Combine imaging matched and imaging non-matched groups ####################

#first remove imaging info from the imaging group
#Gets you a group of 1424 , just like original.... YAY!
data_all.matched_imaging_without_imaging_data_in_data_frame <- data_all.matched_imaging[,-c(77:89)]
data_age.matched_imaging_without_imaging_data_in_data_frame <- data_age.matched_imaging[,-c(77:89)]
data_sex.matched_imaging_without_imaging_data_in_data_frame <- data_sex.matched_imaging[,-c(77:89)]
data_medu.matched_imaging_without_imaging_data_in_data_frame <- data_medu.matched_imaging[,-c(77:89)]
data_race.matched_imaging_without_imaging_data_in_data_frame <- data_race.matched_imaging[,-c(77:89)]
data_age_and_sex.matched_imaging_without_imaging_data_in_data_frame <- data_age_and_sex.matched_imaging[,-c(77:89)]

imaging_plus_non_imaging_matched_groups_data_all <- rbind(data_all.matched_imaging_without_imaging_data_in_data_frame, data_all.matched_non_imaging)
imaging_plus_non_imaging_matched_groups_data_age <- rbind(data_age.matched_imaging_without_imaging_data_in_data_frame, data_age.matched_non_imaging)
imaging_plus_non_imaging_matched_groups_data_sex <- rbind(data_sex.matched_imaging_without_imaging_data_in_data_frame, data_sex.matched_non_imaging)
imaging_plus_non_imaging_matched_groups_data_medu <- rbind(data_medu.matched_imaging_without_imaging_data_in_data_frame, data_medu.matched_non_imaging)
imaging_plus_non_imaging_matched_groups_data_race <- rbind(data_race.matched_imaging_without_imaging_data_in_data_frame, data_race.matched_non_imaging)
imaging_plus_non_imaging_matched_groups_data_age_and_sex <- rbind(data_age_and_sex.matched_imaging_without_imaging_data_in_data_frame, data_age_and_sex.matched_non_imaging)

####Demographics check#####
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
matched_versions <- c("data_all", "data_age", "data_sex", "data_medu", "data_race", "data_age_and_sex")
short <- "imaging_plus_non_imaging_matched_groups_"

for(vers in matched_versions) {
  demo_string_to_eval <- paste("data.frame(", short, vers , "$race_binarized, ", short, vers, "$sex, ", short, vers, "$medu1, ", short, vers, "$age_in_years, ", short, vers, "$dep_binarized)", sep ="")
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
  
  #rename the data***.matched to have a non-imaging end to it, so we can use it to combine
  rename_string_to_eval <- paste(vers, ".matched_non_imaging <- ", vers, ".matched", sep = "")
  eval(parse(text = as.name(rename_string_to_eval)))
  
}

############################
####make csvs for hydra#####
############################

#Subset only variables needed for hydra analysis
short <- "imaging_plus_non_imaging_matched_groups_"
matched_versions <- c("data_all", "data_age", "data_sex", "data_medu", "data_race", "data_age_and_sex")

for(vers in matched_versions) {
  print(vers)
  #bblid, cognitive scores, dep_binarized
  string_to_eval_cog <- paste("data.frame(", short, vers, "[1], ", short, vers, "[14:39], ", short, vers, "[77])", sep = "")
  subset_bblidAndCog_features <- eval(parse(text = as.name(string_to_eval_cog)))
  string_for_csv <- paste("write.csv(subset_bblidAndCog_features, file=\"/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/", vers, ".csv\" , row.names = FALSE, quote = FALSE)", sep = "")
  eval(parse(text = as.name(string_for_csv)))
  
}

#covary only the things relevant to each analysis

#all
string_to_eval_cov <- paste("data.frame(", short, "data_all[1:2], ", short, "data_all[78], ", short, "data_all[10], ", short, "data_all[80])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_all.csv", row.names = FALSE, quote = FALSE)

#age
string_to_eval_cov <- paste("data.frame(", short, "data_age[1], ", short, "data_age[78])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_age.csv", row.names = FALSE, quote = FALSE)

#sex
string_to_eval_cov <- paste("data.frame(", short, "data_sex[1], ", short, "data_sex[2])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_sex.csv", row.names = FALSE, quote = FALSE)

#medu
string_to_eval_cov <- paste("data.frame(", short, "data_medu[1], ", short, "data_medu[10])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_medu.csv", row.names = FALSE, quote = FALSE)

#race
string_to_eval_cov <- paste("data.frame(", short, "data_race[1], ", short, "data_race[80])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_race.csv", row.names = FALSE, quote = FALSE)

#age_and_sex
string_to_eval_cov <- paste("data.frame(", short, "data_age_and_sex[1], ", short, "data_age_and_sex[78], ", short, "data_age_and_sex[2])", sep = "")
subset_bblidAndCog_cov <- eval(parse(text = as.name(string_to_eval_cov)))
write.csv(subset_bblidAndCog_cov, file="/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/hydra_matched/matched_imaging_plus_non_imaging/cov_age_and_sex.csv", row.names = FALSE, quote = FALSE)

