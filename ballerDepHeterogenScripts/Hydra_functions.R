require('visreg')
require('mgcv')
require('tableone')
require('dplyr')
require('plm')
require('MatchIt')
require('tidyr')
require('ggplot2')
require('reshape')
require('emmeans')

## set directories ##
#local_wkdir <- '~/Google Drive/TDSlab/SCZ_gene_imaging/'
#remote_wkdir <- '~/Desktop/BBL/data/joy/BBL/studies/pnc/'

#############################
###Make Demographics Table###
#############################
#############################
####### Demographics ########
#############################

#######Matched group all clusters ##############

make_demographics_table<- function(data_frame, hydra_cluster) {
  #subset demographics
  cluster <- paste0("Hydra_k", hydra_cluster)
  listVars <- c("Race", "Sex", "Maternal Ed", "Age", "Depression", "Cluster") #Race 1 = caucasian, Maternal Ed = years, age = years, dep 1 = dep, 0 = non_dep
  for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex, data_frame$medu1, data_frame$age_in_years, data_frame$dep_binarized, data_frame$", cluster, ")")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)

  #Change categorical values to have names
  demo$Depression <- ifelse(demo$Depression == 1, "Depressed", "Non-depressed")
  demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")

  #Define Categorical Variables
  cat_variables <- c("Race", "Depression", "Sex", "Cluster")
  title <- c("Hydra_k3 demographics")

  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Cluster"))
  print(demo_table, showAllLevels = TRUE)
}

total_people_per_cluster <- function(data_frame, hydra_cluster)
{
  #returns vector with a number for each cluster
  if (hydra_cluster > 1 & hydra_cluster < 10) {
    length_controls <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == -1))")))
    cluster_num_vector <- c(length_controls)
    for (cluster_counter in 1:hydra_cluster){
      length_cluster <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, "))")))
      cluster_num_vector <- c(cluster_num_vector, length_cluster)
    }
    return(cluster_num_vector)
  }
  else {
    print("Error: Cluster number must be between 2 and 10")
  }
}

total_people_per_cluster_by_group <- function(data_frame, variable, hydra_cluster, group_val)
{
  #returns vector with a number for each cluster per group
  #make sure variable is in quotes
  #group_val is the group you'd like. For exampke, if variable= sex and group_val = 1, it will send back number of men, 
       #if variable = race_binarized and group_val = 1, it will send back #caucasians
  if (hydra_cluster > 1 & hydra_cluster < 10) {
    length_controls <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == -1 & data_frame$", variable, " == ", group_val, "))")))
    cluster_num_vector <- c(length_controls)
    for (cluster_counter in 1:hydra_cluster){
      length_cluster <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, " & data_frame$", variable, " == ", group_val, "))")))
      cluster_num_vector <- c(cluster_num_vector, length_cluster)
    }
    return(cluster_num_vector)
  }
  else {
    print("Error: Cluster number must be between 2 and 10")
  }
}

chi_sq <- function(data_frame, variable, hydra_cluster) {
  #make sure variable is in quotes
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", data_frame$Hydra_k", hydra_cluster, ")")))
  return(chisq)
}

chi_sq_no_controls <- function(data_frame, variable, hydra_cluster)
{
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", (data_frame$Hydra_k", hydra_cluster, " != -1))")))
  return(chisq)
}

chi_sq_p <- function(data_frame, variable, hydra_cluster) {
 #make sure variable is in quotes
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", data_frame$Hydra_k", hydra_cluster, ")")))
  print(chisq)
  chisq_pvalue <- chisq$p.value
  return(chisq_pvalue)
}

