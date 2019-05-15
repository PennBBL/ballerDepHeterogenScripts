####################################
###          Libraries           ###
####################################
library(tableone)
library(dplyr)
library(plm)
library(MatchIt)
library(tidyr)
library(ggplot2)
library(reshape)
library(emmeans)
library(MASS)
require(cowplot)
require(stringr)
require(rasterVis)
require(lattice)

####################################
###           Functions          ###
####################################
source("~/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Hydra_functions.R")

####################################
###      Baseline Settings       ###
####################################

theme_update(plot.title = element_text(hjust = 0.5))
num_clusters <- 3
total_num_groups <- num_clusters + 1
cluster_titles <- get_cluster_titles(hydra_cluster = num_clusters)
numeric_vector <- get_cluster_numerical_vector(hydra_cluster = num_clusters)
groups_list <- data.frame(cbind(c(cluster_titles), c(numeric_vector)))
names(groups_list) <- c("cl", "numeric")

####################################
###      Sample Construction     ###
####################################

demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv

cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv

health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv

psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv

imaging_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/neuroimaging/t1struct/n1601_t1QaData_20170306.csv")

dem_cnb <- merge(demographics, cnb_scores, by = "bblid")
health_psych <- merge(health, psych_summary, by = "bblid")
overall_data <- merge(dem_cnb, health_psych, by = "bblid")

#remove people with NA for race, age, or sex.  START WITH N = 9498, final N = 9406
overall_data <- overall_data[!is.na(overall_data$race),] #everyone has a race, N = 9498
overall_data <- overall_data[!is.na(overall_data$ageAtClinicalAssess1),] # 86 people do not have age at clinical assessment.  N = 9412
overall_data <- overall_data[!is.na(overall_data$sex), ] #everyone has a sex, N = 9412
overall_data <- overall_data[!is.na(overall_data$ageAtCnb1), ] #6 people do not have ageAtCnb1, N = 9406


#remove people with NA for depression or total psych score, START WITH N = 9406, final N = 9405
overall_data <- overall_data[!is.na(overall_data$smry_dep),] #lost 1, N = 9405
overall_data<- overall_data[!is.na(overall_data$smry_psych_overall_rtg),] #take out those with NA for overall psych rtg, no additional people lost, N = 9405


#remove people with medical illnesses that could affect brain functioning, Original N = 9405, final N = 7079
overall_data <- subset.data.frame(overall_data, (medicalratingExclude == 0)) #lost 2326

#remove people without complete 26 cognitive measures, start with N = 7079, final n = 6327
overall_data <- overall_data[complete.cases(overall_data[14:39]),] #lost 752, n = 6327

#only keep people who are depressed or psychiatrically healthy (i.e exclude those with psychosis, bipolar), start with N = 6327, final N = 3022
overall_data <- subset.data.frame(overall_data, ((smry_dep == 4) | ((smry_dep < 4) & (smry_psych_overall_rtg < 4)))) #lost 3305

#only include good imaging scans, n = 1540
imaging_include <- subset.data.frame(imaging_summary, (t1Exclude == 0))

####################################
###       Prepping Dataset       ###
####################################

#would binarize depression smry score to -1 (less than 4, not depressed) and 1 (score 4 , depressed)
dep_binarized <- ifelse(overall_data$smry_dep == 4, 1, -1)
overall_data <- cbind(overall_data, dep_binarized) #N = 3284

#binarize race
race_binarized <- ifelse(overall_data$race == 1, 1, 2)
overall_data <-cbind(overall_data, race_binarized)

#make depression and gender into factor scores 
overall_data$dep_binarized <- as.factor(overall_data$dep_binarized)
overall_data$sex <- as.factor(overall_data$sex)

#divide ageAtCNB by 12 for age 
overall_data$age_in_years <- overall_data$ageAtCnb1/12

#race binarized for plotting purposes, caucasian 1, non-caucasion 0
overall_data$race_binarized <- ifelse(as.factor(overall_data$race) == 1, 1, 0)

#get subset with imaging, merge overall data (N = 3022) with imaging (n = 1540), Final n = 646
overall_data_imaging <- merge(overall_data, imaging_include, by = "bblid")

####################################
###    Matching with Matchit     ###
####################################


####1. Match depress w/ imaging to controls w/imaging
####2. Removed these from full group
####3. Match remaining depressed without imaging to controls who weren't previously matched
####4. Combine depressed w/imaging to depressed w/out imaging and controls w/imaging to controls w/out imaging
####5. Check distribution of these groups

#########First: matching only depressed people with imaging to controls with imaging###
#match with matchit depressed n =187
#### matching on age_and_sex: dep: m/f (70/117), non dep:m/f (70/117)

data.unmatched = overall_data[complete.cases(overall_data[14:39]),]
data.unmatched$unmatchedRows =rownames(data.unmatched)
dataset = data.unmatched

# Some preprocessing
dataset = dplyr::select(dataset, sex, age_in_years, medu1, race_binarized, dep_binarized, unmatchedRows)

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

#post_match
m_age_and_sex.out <-matchit(dep_binarized ~ age_in_years + sex, data=dataset, method="nearest", distance="mahalanobis")
plot(m_age_and_sex.out)
m_age_and_sex.data <- match.data(m_age_and_sex.out)
plot(m_age_and_sex.data$age_in_years,jitter(m_age_and_sex.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age and sex matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))


# Make the final matched data set
data_age_and_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL

##############
#Demo Tablets#
##############
#subset demographics

listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
matched_versions <- c("data_age_and_sex")

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

### Match non-imaging depressed 

people_from_imaging_match_age_and_sex <- data_age_and_sex.matched$bblid
subset_people_who_were_not_included_in_imaging_match_age_and_sex <- overall_data[!(overall_data$bblid %in% c(people_from_imaging_match_age_and_sex)),]

data.unmatched = subset_people_who_were_not_included_in_imaging_match_age_and_sex[complete.cases(subset_people_who_were_not_included_in_imaging_match_age_and_sex[14:39]),]
data.unmatched$unmatchedRows =rownames(data.unmatched)
dataset = data.unmatched

# Some preprocessing
dataset = dplyr::select(dataset, sex, age_in_years, medu1, race_binarized, dep_binarized, unmatchedRows)


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
m_age_and_sex.out <-matchit(dep_binarized ~ age_in_years + sex, data=dataset, method="nearest", distance="mahalanobis")
plot(m_age_and_sex.out)
m_age_and_sex.data <- match.data(m_age_and_sex.out)

# Re-plot
plot(m_age_and_sex.data$age_in_years,jitter(m_age_and_sex.data$medu1, factor=3), pch=c(15, 7, 18, 9), col=c(1,2,3,4), ylab="Maternal Edu", xlab="Age age and sex matched")
legend("bottomright",c("Non-white, non-depressed", "Non-white, depressed", "White, non-depressed", "White, depressed"),pch=c(15, 7, 18, 9), col=c(1,2,3,4))

data_age_and_sex.matched = data.unmatched[data.unmatched$unmatchedRows%in%m_age_and_sex.data$unmatchedRows,]
data_age_and_sex.matched$unmatchedRows = NULL

### New Demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
matched_versions <- c("data_age_and_sex")

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
#n= 1424
data_age_and_sex.matched_imaging_without_imaging_data_in_data_frame <- data_age_and_sex.matched_imaging[,-c(77:89)]
imaging_plus_non_imaging_matched_groups_data_age_and_sex <- rbind(data_age_and_sex.matched_imaging_without_imaging_data_in_data_frame, data_age_and_sex.matched_non_imaging)

####Demographics check#####
#subset demographics
listVars <- c("Race_binarized", "Sex", "Maternal Ed", "Age", "Depression") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
matched_versions <- c("data_age_and_sex")
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
