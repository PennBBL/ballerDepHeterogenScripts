###################################################################################################
##########################         GRMPY - Generate Structual RDS        ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 11/08/2017                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
# Use #

# This script is used in order to combine the non-neuroimaging data into a single dataframe and assign the nominal
# variables as factors. This is needed before performing analysis with the roiWrapperCall.sh.

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

#####################################################################
##### Reads in the Demographic, Irritability, and structQA Data #####
#####################################################################

demographics <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_demographics_go1_20161212.csv
cnb_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_cnb_zscores_fr_20170202.csv
health <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_health_20170405.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_health_20170405.csv
psych_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv", header = TRUE, sep = ",") #from /data/joy/BBL/projects/ballerDepHeterogen/data/n9498_goassess_psych_summary_vars_20131014.csv
t1_qa_scores <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header = TRUE, sep = ",") 
jlfCT_summary <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv", header = TRUE, sep = ",") 
hydra_AG_matched_clusters <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/hydra_output_from_cbica/hydra_matched/matched_imaging_plus_non_imaging/CogData_matched_age_and_sex_n1424_matched_imaging_plus_non_imaging_HydraSubtypes.csv", header = TRUE, sep = ",")

#demographics <- read.csv("/data/joy/BBL/tutorials/exampleData/roiWrapper/n20_Demographics_20171108.csv")

#ARI <- read.csv("/data/joy/BBL/tutorials/exampleData/roiWrapper/n20_Irritability_20171108.csv")

#structQA <- read.csv("/data/joy/BBL/tutorials/exampleData/roiWrapper/n20_structQAFlags_20171108.csv")
structQA <- t1_qa_scores[c("bblid","scanid", "T1exclude")]

###########################################
##### Merges the DataSets Into in One #####
###########################################

dataSub <- merge(demographics, ARI, by=c("bblid","scanid"))
dataSub <- merge(dataSub, structQA, by=c("bblid","scanid"))


######################################################
##### Assign the Nominal Variables to be Factors #####
######################################################

dataSub$sex <- as.factor(dataSub$sex)

dataSub$ManualRating <- ordered(dataSub$ManualRating)

dataSub$race <- as.factor(dataSub$race)

#####################################
##### Write the Output RDS File #####
#####################################

saveRDS(dataSub, "/data/joy/BBL/tutorials/exampleData/roiWrapper/n20_Demo+ARI+QA_20171108.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
