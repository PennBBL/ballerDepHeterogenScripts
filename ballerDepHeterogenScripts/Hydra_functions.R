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

get_cluster_titles <- function(hydra_cluster){
  # build vector of titles
  cluster_titles <- c("TD")
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0("Cluster", cluster_counter)
    cluster_titles <- c(cluster_titles, title_to_add)
  }
  return(cluster_titles)
}

get_cluster_titles_no_TD <- function(hydra_cluster){
  # build vector of titles
  cluster_titles <- NULL
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0("Cluster", cluster_counter)
    cluster_titles <- c(cluster_titles, title_to_add)
  }
  return(cluster_titles)
}

get_cluster_numerical_vector <- function(hydra_cluster){
  # build vector of titles
  cluster_vector <- c("-1")
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0(cluster_counter)
    cluster_vector <- c(cluster_vector, title_to_add)
  }
  return(cluster_vector)
}
 
get_cluster_numerical_vector_no_TD <- function(hydra_cluster){
  # build vector of titles
  cluster_vector <- NULL
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0(cluster_counter)
    cluster_vector <- c(cluster_vector, title_to_add)
  }
  return(cluster_vector)
}

get_variable_mean_vector <- function(data_frame, variable, hydra_cluster){
  #returns a vector of means, depends on # hydra clusters
  means <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  for (cluster_counter in 1:hydra_cluster){
    mean_to_add <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    means <- c(means, mean_to_add)
  }
  return(means)
}

get_variable_mean_vector_no_TD <- function(data_frame, variable, hydra_cluster){
  #returns a vector of means, depends on # hydra clusters
  #means <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  means <- NULL
  for (cluster_counter in 1:hydra_cluster){
    mean_to_add <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    means <- c(means, mean_to_add)
  }
  return(means)
}

get_variable_sd_vector <- function(data_frame, variable, hydra_cluster){
  #returns vector of sds, depends on # hydra clusters
  sds <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  for (cluster_counter in 1:hydra_cluster){
    sd_to_add <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    sds <- c(sds, sd_to_add)
  }
  return(sds)
}

get_variable_sd_vector_no_TD <- function(data_frame, variable, hydra_cluster){
  #returns vector of sds, depends on # hydra clusters
  #sds <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  sds <- NULL
  for (cluster_counter in 1:hydra_cluster){
    sd_to_add <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    sds <- c(sds, sd_to_add)
  }
  return(sds)
}

get_num_subj_per_cluster <- function(data_frame, hydra_cluster)
{
  nums <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == -1)))")))
  for (cluster_counter in 1:hydra_cluster){
    num_to_add <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")))")))
    nums <- c(nums, num_to_add)
  }
  return(nums)
}

get_num_subj_per_cluster_no_TD <- function(data_frame, hydra_cluster)
{
 # nums <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == -1)))")))
  nums <- NULL
  for (cluster_counter in 1:hydra_cluster){
    num_to_add <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")))")))
    nums <- c(nums, num_to_add)
  }
  return(nums)
}

data_frame_mean_sd_sem <- function(data_frame, variable, hydra_cluster){
  cluster_titles <- get_cluster_titles(hydra_cluster = hydra_cluster)
  variable_mean <- get_variable_mean_vector(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  variable_sd <- get_variable_sd_vector(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  num_per_cluster <- get_num_subj_per_cluster(data_frame = data_frame, hydra_cluster = hydra_cluster)
  variable_sem <- variable_sd/sqrt(num_per_cluster)
  
  #put all together in one data frame
  df_mean_sd_sem <- data.frame(cl = cluster_titles, mean = variable_mean, sd = variable_sd, sem = variable_sem)
  return(df_mean_sd_sem)
}

data_frame_mean_sd_sem_no_TD <- function(data_frame, variable, hydra_cluster){
  cluster_titles <- get_cluster_titles_no_TD(hydra_cluster = hydra_cluster)
  variable_mean <- get_variable_mean_vector_no_TD(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  variable_sd <- get_variable_sd_vector_no_TD(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  num_per_cluster <- get_num_subj_per_cluster_no_TD(data_frame = data_frame, hydra_cluster = hydra_cluster)
  variable_sem <- variable_sd/sqrt(num_per_cluster)
  
  #put all together in one data frame
  df_mean_sd_sem <- data.frame(cl = cluster_titles, mean = variable_mean, sd = variable_sd, sem = variable_sem)
  return(df_mean_sd_sem)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_continuous_variables <- function(data_frame, var1, var2, hydra_cluster, optional_variable_name_string){
#makes bar plots and line plot with error bars, specifically for plotting numerical things like age and medu. 
  #NOT for categorical variables like sex/race 
  
  num_total_groups <- hydra_cluster + 1
  cluster_titles <- get_cluster_titles(hydra_cluster = hydra_cluster)
  dat_var1_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var1, hydra_cluster = hydra_cluster)
  dat_var2_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var2, hydra_cluster = hydra_cluster)
  dat_var1_var2_sem_melted <- melt(c(dat_var1_sd_sem$sem,dat_var2_sd_sem$sem), id.vars = "cl")


  #get everything into the right format
  var1_and_var2 <- data.frame(cl=cluster_titles, var1 = dat_var1_sd_sem$mean, var2 = dat_var2_sd_sem$mean)
  var1_and_var2_for_plot <- melt(var1_and_var2, id.vars = "cl")
  var1_and_var2_for_plot$sem <- dat_var1_var2_sem_melted$value
  names(var1_and_var2_for_plot) <- c("cluster", "group", "years", "sem")
  
  #change names from var1 and var2 to their actual values
  if (!missing(optional_variable_name_string))
  {
    var1 = optional_variable_name_string[1]
    var2 = optional_variable_name_string[2]
  }
  
  replace_group_names <- c(rep(var1, num_total_groups), rep(var2, num_total_groups))
  #replace_group_names <- c(rep(var1, 4), rep(var2, 4))
  var1_and_var2_for_plot$group <- replace_group_names

  #plot
  title_of_plot <- paste0("Hydra_k", hydra_cluster, " ", var1, " and ", var2)
  p1 <- ggplot(data = var1_and_var2_for_plot, aes(x = group, y = years, group = cluster)) + 
    geom_line(aes(color=cluster)) +
    geom_point(aes(color=cluster)) + 
    geom_errorbar(aes(ymin=years-sem, ymax=years+sem), width=.1) +
    ggtitle(title_of_plot)

  p2 <- ggplot(dat_var1_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + ylim(0, 18) + xlab("Clusters") + ylab(paste0(var1, " in Years")) + 
    ggtitle(paste0(var1, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
  
  p3 <- ggplot(dat_var2_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + ylim(0, 18) + xlab("Clusters") + ylab(paste0(var2, " in Years")) + 
    ggtitle(paste0(var2, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
    
  #send back all three to the rmarkdown document that I called
  list_to_return <- list(p1, p2, p3)
   # multiplot(p1,p2,p3, cols=3)
  return(list_to_return)
  
}

fdr_anova <- function(data_frame) {
  models_anova <- lapply(data_frame, summary)
  
  #Pull p-values
  p_anova <- sapply(data_frame, function(v) v$"Pr(>F)"[1]) #$coef[,"Pr(>F)"][2]) #get the p value for dep binarized
  
  #Convert to data frame
  p_anova <- as.data.frame(p_anova)
  
  #print BEFORE FDR correction 
  print("Anova scores, BEFORE FDR correction: i.e., uncorrected")
  print(p_anova)
  
  #Print original p-values to three decimal places
  p_round_anova <- round(p_anova_lm_agesq,3)
  
  #FDR correct p-values
  pfdr_anova <- p.adjust(p_anova[,1],method="fdr")
  
  #Convert to data frame
  pfdr_anova <- as.data.frame(pfdr_anova)
  row.names(pfdr_anova) <- names(data_frame)
  
  #To print fdr-corrected p-values to three decimal places
  pfdr_round_anova <- round(pfdr_anova,3)
  
  #List the components that survive FDR correction
  components_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]
  
  #make a data frame with names and fdr values (rounded to 3 decimals)
  names_and_fdr_values_anova <- data.frame(cbind(components_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))
  
  #add titles to names_and_fdr tables
  names(names_and_fdr_values_anova) <- c("component", "p_FDR_corr")
  
  print("Mean centered age that was then squared, FDR corrected")
  print(names_and_fdr_values_anova)
  return(names_and_fdr_values_anova)
  
}

pairwise_contrasts_3clusters <- function(data_frame_lm, fdr_anova) {
 #for 3 clusters
  print(fdr_anova)
  emmodel_df <- lapply(data_frame_lm, function(x) {as.list(ref_grid(x))})
  emgrid_df <- lapply(emmodel_df, function(x) {as.emmGrid(x)})
  
  #run emmeans
  emmeans_df <- lapply(emgrid_df, function(x) {emmeans(x, "Hydra_k3")})
  
  #run pairwise contrasts
  empairs_df <- lapply(emmeans_df, function(x) {pairs(x)})
  

  #Only include stuff that was fdr corrected (i.e., only keep parts of the model (or only display) ones that are corrected),this will be null if nothing was corrected
  empairs_FDR_corrected <- empairs_df[fdr_anova[,1]]
  
  print(empairs_FDR_corrected)
  #contrast names, -1 = controls, 1-3 are clusters
  contrast_names <- c("-1 - 1", "-1 - 2", "-1 - 3", "1 - 2", "1 - 3", "2 - 3")
  #go through each fdr corrected brain region, and extract p values
  #contrast_table <- lapply(fdr_anova[,1], function(x) {round(summary(x)$p.value,3)})
  contrast_table <- lapply(empairs_FDR_corrected, function(x) {round(summary(x)$p.value,3)})
  #get the names of the brain regions that were fdr corrected
  brain_regions <- names(contrast_table)
  #build table that will hold the name of the brain region and the p values
  pairwise_table <- data.frame(matrix(nrow = length(brain_regions), ncol = 6))
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
