require('stringr')
require('R.matlab')
require('mgcv')
require('visreg')
require('rasterVis')

## set directories ##
#local_wkdir <- '/home/eballer/from_cedric/'
remote_wkdir <- '/data/joy/BBL/studies/pnc/'
#remote_wkdir <- '/home/eballer/from_cedric/imaging/'
local_wkdir <- '/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/from_cedric/'
#remote_wkdir <- '/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/from_cedric/imaging/'
data_dir <- '/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/'


#############################
#### Sample Construction ####
#############################

### For 3 concatenated scans###
#Input: Data frame with all subjects who met criteria for T1 struct and rest qa
#Output: List
      #Item 1: Data frame with only subjects who passed T1, rest, nback and idemo qa
      #Item 2: Data frame with weights based on quality of T1, rest, nback, idemo qa

#weighted sample ()

get_abs_and_weighted_sample <- function(sample_from_HYDRA) {

  #load qa for each different scan type
  rest_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv'))
  nback_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/nback/n1601_NbackConnectQAData_20170718.csv'))
  idemo_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/idemo/n1601_IdemoConnectQAData_20170718.csv'))
  
  #set num volumes  
  rest_nvol <- 120
  nback_nvol <- 225
  idemo_nvol <- 204
  
  #do a weighted relative mean motion, putting more emphasis on scans with more volume
  weightedRelMeanMotion <- (rest_nvol*rest_qa$restRelMeanRMSMotion + 
                            nback_nvol*nback_qa$nbackRelMeanRMSMotion + 
                            idemo_nvol*idemo_qa$idemoRelMeanRMSMotion)/
  (idemo_nvol+nback_nvol+rest_nvol)

  #start with the resting task
  rest_task_qa <- data.frame(bblid=rest_qa$bblid,scanid= rest_qa$bblid,weightedRelMeanMotion=weightedRelMeanMotion)

  #assume that everything will be excluded by weighted rel mean
  rest_task_qa$weightedRelMeanExclude <- 1

  #set criteria for motion to be less tha <= 0.2mm and only select people who meet criteria
  #lose 465, new n = 1136
  weightedRelMeanMotion_criteria <- which(rest_task_qa$weightedRelMeanMotion<=0.2)

  #set the weighted mean exclude motion variable to be 0 if they meet criteria
  #i.e., if you move less than 0.2mm, you are included
  rest_task_qa$weightedRelMeanExclude[weightedRelMeanMotion_criteria] <- 0

  #set absolute rel mean motion to exclude
  rest_task_qa$AbsRelMeanExclude <- 1

  #set criteria to be that you have to not have been exluded for motion in rest, nback or idemo
  #lost 601 people, new n = 1000
  AbsRelMeanExclude_criteria <- which(rest_qa$restRelMeanRMSMotionExclude != 1 & 
                                      nback_qa$nbackRelMeanRMSMotionExclude != 1 & 
                                      idemo_qa$idemoRelMeanRMSMotionExclude != 1)

  #for those people who met criteria, set their exclude code to 0 
  #i.e. if they were perfect in all three scans, keep em
  rest_task_qa$AbsRelMeanExclude[AbsRelMeanExclude_criteria] <- 0

  #merge the rest task info, which now has new columns for rel and abs exlude, with our HYDRA sample
  #n = 325
  rest_task_qa_FROM_HYDRA <- merge(sample_from_HYDRA,rest_task_qa,by='bblid')

  #since merging gives us scanid.x, reset the name of scan ID
  colnames(rest_task_qa_FROM_HYDRA)[c(2)] <- c('scanid')

#rest_task_master_key <- merge(rest_task_qa_hx_t1_demo,prs_chip_key,by= 'org_bblid' )
#colnames(rest_task_master_key)[which(colnames(rest_task_master_key) == 'bblid')] <- 'new_bblid'
#colnames(rest_task_master_key)[which(colnames(rest_task_master_key) == 'org_bblid')] <- 'bblid'

  #Get our sample that meets the weighted criteria, lost 13, n = 312
  weighted_rest_task_sample <- subset(rest_task_qa_FROM_HYDRA, weightedRelMeanExclude == 0)

  #Get our sample that meets absolute rule out, lost 20, n = 305
  abs_rest_task_sample <- subset(rest_task_qa_FROM_HYDRA, AbsRelMeanExclude == 0)

  #save these two files into the results section
  #save(weighted_rest_task_sample,abs_rest_task_sample, 
     #file = '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_fc/concatenate_rest_nback_idemo/weighted_and_abs_rest_task_sample.RData')
  list_abs_and_weighted_samples <- list(abs_rest_task_sample, weighted_rest_task_sample)
  return(list_abs_and_weighted_samples)
}

##############################
####                      ####
#### Schaefer Specifics   ####
####                      ####
##############################

get_schaefer_netpath <- function(resolution, modality, bblid, scanid){
  if (modality == 'fc' & (resolution == 200 | resolution == 100)) {
    netpath <- paste0(remote_wkdir,"processedData/restbold/restbold_201607151621/",bblid,"/*",scanid,"/net/Schaefer",resolution,"PNC/*_network.txt")
  } else if (modality == 'fc' & resolution == 400) {
    netpath <- paste0(remote_wkdir,"processedData/restbold/restbold_201607151621/",bblid,"/*",scanid,"/net/SchaeferPNC/*_network.txt")
  } else if (modality == 'sc-d') {
    netpath <- paste0(remote_wkdir,"processedData/diffusion/deterministic_20171118/",bblid,"/*",scanid,"/tractography/connectivity/*_",resolution,"_*_fa_connectivity.mat")
  } else if (modality == 'sc-p' & resolution == 400) {
    netpath <- paste0(remote_wkdir,"processedData/diffusion/probabilistic_20171118/",bblid,"/*",scanid,"/output/connectivity/*Schaefer",resolution,"_17net.mat")
  } else if (modality == 'sc-p' & resolution == 200) {
    netpath <- paste0(remote_wkdir,"processedData/diffusion/probabilistic_20171118/",bblid,"/*",scanid,"/output/schaefer",resolution,"/connectivity/*Schaefer",resolution,"_17net.mat")
  }
  netpath
}

get_schaefer_netpath_concat <- function(resolution, modality, bblid, scanid){
 #resolution and modality to be used if I later use this for other resolutions or dti 
 # netpath <- paste0(data_dir, "SchaeferPNC/", bblid,"/", scanid, "/", bblid, "_", scanid, "_restbold_nback_idemo_concat_ts_transposed.txt")
  netpath <- paste0(data_dir, "SchaeferPNC/", bblid,"/", scanid, "/", bblid, "_", scanid, "_restbold_nback_idemo_concat_ts.txt")
  print(netpath)
  return(netpath)
}

get_schaefer_node_info <- function(resolution,num_com){
  CommunityAffiliation <- read.csv2(paste0(local_wkdir,'imaging/schaefer',resolution,'/schaefer',resolution,'x',num_com,'CommunityAffiliation.1D'),header = FALSE)$V1
  CommunityName <- read.csv2(paste0(local_wkdir,'imaging/schaefer',resolution,'/schaefer',resolution,'x',num_com,'CommunityNames.txt'),header = FALSE)$V1
  NodeName <- read.csv2(paste0(local_wkdir,'imaging/schaefer',resolution,'/schaefer',resolution,'NodeNames.txt'),header = FALSE)
  out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName, NodeName= NodeName)
}


get_wsbm_node_info <- function(CIs){
  CommunityAffiliation <- CIs$consensus.list[,numCom]
  CommunityName <- paste0('community',unique(CommunityAffiliation))
  out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName)
}

get_louvein_node_info <- function(best_consensus){
  CommunityAffiliation <- best_consensus
  CommunityName <- paste0('community',unique(CommunityAffiliation))
  out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName)
}

##############################
####                      ####
####   Power Specific     ####
####                      ####
##############################

get_power_netpath <- function(scanid) {
  netpath <- paste0(remote_wkdir, 'n1601_dataFreeze/neuroimaging/rest/restNetwork_264PowerPNC/264PowerPNCNetworks/',scanid,'*.txt')
}

get_power_netpath_concat <- function(resolution, modality, bblid, scanid){
  #resolution and modality to be used if I later use this for other resolutions or dti 
  netpath <- paste0(data_dir, "264PowerPNC/", bblid,"/", scanid, "/*transposed.txt")
  netpath
}

get_power_node_info <- function(num_com){
  CommunityAffiliation <- read.csv2(paste0(local_wkdir,'imaging/power264/power264CommunityAffiliation.1D'),header = FALSE)$V1
  CommunityName <- read.csv2(paste0(local_wkdir,'imaging/power264/power264CommunityNames.txt'),header = FALSE)$V1
  NodeName <- read.csv2(paste0(local_wkdir,'imaging/power264/power264NodeNames.txt'),header = FALSE)$V1
  NodeCoord <- read.delim(paste0(local_wkdir,'imaging/power264/power264CoorMNI.sclib'),header = F,dec = ',',skip = 2)$V1
  NodeCoord_df <- t(sapply(NodeCoord,function(coord) as.numeric(unlist(strsplit(as.character(coord),"\\,|\\#"))[3:5])))
  colnames(NodeCoord_df) <-c('X','Y','Z')
  if (num_com == 14){
    out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName, NodeName= NodeName, NodeCoord = NodeCoord_df)
  }
  else if (num_com == 6){
    CommunityName2 <- CommunityName
    CommunityAffiliation2 <- array(NA,length(CommunityAffiliation))
    CommunityName2 <- c('somatomotor_aud','COP_SAL_vATT','FrontoParietal','vis','default','dATT')
    CommunityAffiliation2[which(CommunityAffiliation == 1 | CommunityAffiliation == 2 | CommunityAffiliation == 4)] <- 1  # merge somato and aud
    CommunityAffiliation2[which(CommunityAffiliation == 3 | CommunityAffiliation == 9 | CommunityAffiliation == 11)] <- 2  # merge COP_salience_vatt
    CommunityAffiliation2[which(CommunityAffiliation == 8)] <- 3  # fronto
    CommunityAffiliation2[which(CommunityAffiliation  == 7 )] <- 4  # vis
    CommunityAffiliation2[which(CommunityAffiliation == 5 | CommunityAffiliation == 6)] <- 5  # default and memory
    CommunityAffiliation2[which(CommunityAffiliation == 12)] <- 6  # datt
    out <- list(CommunityAffiliation = CommunityAffiliation2, CommunityName = CommunityName2, NodeName= NodeName, NodeCoord = NodeCoord_df)
  }
  else if (num_com == "1.5-0.8"){
    CommunityAffiliation2 <- array(NA,length(CommunityAffiliation))
    CommunityName2 <- c('task-positive','executive','default','somatosensory')
    best_consensus_file <- paste0(local_wkdir,'results/power_community_detection_gamma_1,5_consensus_0.8_subj.mat')
    CommunityAffiliation <- readMat(best_consensus_file)$con
    CommunityAffiliation2[which(CommunityAffiliation == 1 )] <- 1  # task-positive
    CommunityAffiliation2[which(CommunityAffiliation == 2 )] <- 2  # executive 
    CommunityAffiliation2[which(CommunityAffiliation == 3 )] <- 3  # default
    CommunityAffiliation2[which(CommunityAffiliation == 5 )] <- 4  # somatosensory
    out <- list(CommunityAffiliation = CommunityAffiliation2, CommunityName = CommunityName2, NodeName= NodeName, NodeCoord = NodeCoord_df)
  }
  else if (num_com == "dmn3"){
    CommunityAffiliation2 <- array(NA,length(CommunityAffiliation))
    CommunityName2 <- c('dmnA','dmnB','dmnC')
    #best_consensus_file <- paste0(local_wkdir,'results/default_consensus_gamma',1,'_tau_',0.6,'_subj.mat')
    best_consensus_file <- paste0(local_wkdir,'results/wsbm/dmn_power_88.mat')
    #CommunityAffiliation <- readMat(best_consensus_file)$con
    CommunityAffiliation <- readMat(best_consensus_file)$cons
    CommunityAffiliation2[which(CommunityAffiliation == 1 )] <- 1  # dmnA
    CommunityAffiliation2[which(CommunityAffiliation == 2 )] <- 2  # dmnB
    CommunityAffiliation2[which(CommunityAffiliation == 3 )] <- 3  # dmnC
    out <- list(CommunityAffiliation = CommunityAffiliation2, CommunityName = CommunityName2, NodeName= NodeName, NodeCoord = NodeCoord_df)
  } else if (num_com == "dmn4"){
    CommunityAffiliation2 <- array(NA,length(CommunityAffiliation))
    CommunityName2 <- c('dmnA','dmnB','dmnC','dmnD')
    best_consensus_file <- paste0(local_wkdir,'results/default_consensus_gamma',1.25,'_tau_',0.7,'_subj.mat')
    CommunityAffiliation <- readMat(best_consensus_file)$con
    CommunityAffiliation2[which(CommunityAffiliation == 1 )] <- 1  # dmnA
    CommunityAffiliation2[which(CommunityAffiliation == 2 )] <- 2  # dmnB
    CommunityAffiliation2[which(CommunityAffiliation == 3 )] <- 3  # dmnC
    CommunityAffiliation2[which(CommunityAffiliation == 4 )] <- 4  # dmnD
    out <- list(CommunityAffiliation = CommunityAffiliation2, CommunityName = CommunityName2, NodeName= NodeName, NodeCoord = NodeCoord_df)
  }
  
  
  
  out
}


##############################
####                      ####
####  Gordon Specific     ####
####                      ####
##############################
get_gordon_netpath <- function(scanid) {
  netpath <- paste0(remote_wkdir, 'n1601_dataFreeze/neuroimaging/rest/restNetwork_gordon/GordonPNCNetworks/',scanid,'*.txt')
}

get_gordon_netpath_concat <- function(resolution, modality, bblid, scanid){
  #resolution and modality to be used if I later use this for other resolutions or dti 
  netpath <- paste0(data_dir, "GordonPNC/", bblid,"/", scanid, "/*transposed.txt")
  netpath
}

get_gordon_node_info <- function(){
  CommunityAffiliation <- read.csv2(paste0(local_wkdir,'imaging/gordon333/gordon333CommunityAffiliation.1D'),header = FALSE)$V1
  CommunityName <- read.csv2(paste0(local_wkdir,'imaging/gordon333/gordon333CommunityNames.txt'),header = FALSE)$V1
  NodeName <- read.csv2(paste0(local_wkdir,'imaging/gordon333/gordon333NodeNames.txt'),header = FALSE)
  out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName, NodeName= NodeName)
}

##############################
####                      ####
####  Glasser Specific    ####
####                      ####
##############################
get_glasser_netpath <- function(scanid) {
  netpath <- paste0(remote_wkdir, 'n1601_dataFreeze/neuroimaging/rest/restNetwork_GlasserPNC/GlasserPNCNetworks/',scanid,'*.txt')
}

get_glasser_netpath_concat <- function(resolution, modality, bblid, scanid){
  #resolution and modality to be used if I later use this for other resolutions or dti 
  netpath <- paste0(data_dir, "GlasserPNC/", bblid,"/", scanid, "/*transposed.txt")
  netpath
}


get_glasser_node_info <- function(){
  CommunityAffiliation <- read.csv2(paste0(local_wkdir,'imaging/glasser360/glasser360CommunityAffiliation.1D'),header = FALSE)$V1
  CommunityName <- read.csv2(paste0(local_wkdir,'imaging/glasser360/gordon333CommunityNames.txt'),header = FALSE)$V1
  NodeName <- read.csv2(paste0(local_wkdir,'imaging/glasser360/gordon333NodeNames.txt'),header = FALSE)
  out <- list(CommunityAffiliation = CommunityAffiliation, CommunityName = CommunityName, NodeName= NodeName)
}

##############################
####                      ####
#### grab network matrix  ####
####                      ####
##############################

grab_net_from_path <- function(netpath){
  net_file_type = str_sub(netpath,-3)
  if (identical(Sys.glob(netpath), character(0))) {
    temp_net <- NA
  } else  {
    if (net_file_type == 'txt') {  
      temp_net <- as.matrix(read.table(Sys.glob(netpath)))
    } else if (net_file_type == 'mat') {
      temp_net <- readMat(Sys.glob(netpath))
    }
  }
}

get_net_from_sample <- function(sample,parcellation,resolution,modality) {
  n_sample <- dim(sample)[1]
  sample_net<-list()
  for (i in 1:n_sample) {
    bblid = sample[i,'bblid']
    scanid = sample[i,'scanid']
    # set up the correct path by modality, and resolution#
    
    
    # import the network data #
    
    if (parcellation == 'power') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation,' atlas'))
      netpath <- get_power_netpath(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == 'gordon') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation,' atlas'))
      netpath <- get_gordon_netpath(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == 'schaefer') {
      netpath <- get_schaefer_netpath(resolution,modality,bblid,scanid)
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of resolution ', resolution,' in ', modality,' of ', parcellation,' atlas'))
      if (modality == 'sc-d') {
        temp_net <- grab_net_from_path(netpath)
        if (is.na(temp_net)) {
          sample_net[[i]] <- NA
        } else {
          sample_net[[i]] <- temp_net$connectivity
        }
      } else if  (modality == 'sc-p') {
        temp_net <- grab_net_from_path(netpath)
        if (is.na(temp_net)) {
          sample_net[[i]] <- NA
        } else {
          sample_net[[i]] <- temp_net$streamlineCount.mat
        }
      } else if (modality == 'fc') {
        sample_net[[i]] <- grab_net_from_path(netpath)
      }
    }
    
  }
  
  if (parcellation == 'power'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_power_network.RData')
  }
  else if (parcellation == 'schaefer'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_',modality,'_',resolution,'_schaefer_network.RData')
  } else if (parcellation == 'gordon'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_gordon_network.RData')
  }
  #save(sample_net,sample,file = save_file_path)
  sample_net
}

#concatenated
get_net_from_sample_concat <- function(sample,parcellation,resolution,modality) {
  n_sample <- dim(sample)[1]
  sample_net<-list()
  for (i in 1:n_sample) {
    bblid = sample[i,'bblid']
    scanid = sample[i,'scanid']
    
    # set up the correct path by modality, and resolution#
    #### THIS WAS ADDED 8/17
    
    # import the network data #
    
    if (parcellation == 'power') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation,' atlas'))
      netpath <- get_power_netpath_concat(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == 'gordon') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation,' atlas'))
      netpath <- get_gordon_netpath_concat(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == 'schaefer') {
      netpath <- get_schaefer_netpath_concat(resolution,modality,bblid,scanid)
      ts <- read.table(netpath)
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of resolution ', resolution,' in ', modality,' of ', parcellation,' atlas'))
      if (modality == 'sc-d') {
        temp_net <- grab_net_from_path(netpath)
        if (is.na(temp_net)) {
          sample_net[[i]] <- NA
        } else {
          sample_net[[i]] <- temp_net$connectivity
        }
      } else if  (modality == 'sc-p') {
        temp_net <- grab_net_from_path(netpath)
        if (is.na(temp_net)) {
          sample_net[[i]] <- NA
        } else {
          sample_net[[i]] <- temp_net$streamlineCount.mat
        }
      } else if (modality == 'fc') {
        sample_net[[i]] <-  get_net_from_ts(ts = ts)
      }
    }
    
  }
  
  if (parcellation == 'power'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_power_network.RData')
  }
  else if (parcellation == 'schaefer'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_',modality,'_',resolution,'_schaefer_network.RData')
  } else if (parcellation == 'gordon'){
    #save_file_path <- paste0(remote_wkdir,'../../projects/prsConnectivity/result/',deparse(substitute(sample)),'_gordon_network.RData')
  }
  #save(sample_net,sample,file = save_file_path)
  sample_net
}

##############################
####                      ####
## calculate bet/within net ##
####                      ####
##############################


get_community_net_names <- function(CommunityName){
  community_net_names <-  array(NA, dim=c(length(CommunityName),length(CommunityName)))
  for (community_i in 1:length(CommunityName)){
    for (community_j in 1:length(CommunityName)) {
      community_net_names[community_i,community_j] = paste(CommunityName[community_i],CommunityName[community_j],sep = '_')
    }
  }
  community_net_names
}

get_community_var <- function(sample_net,com_name_vector,node_aff_vector){
  community_var  = array(NA, dim=c(length(com_name_vector),length(com_name_vector)))
  for (community_i in 1:length(com_name_vector)){
    for (community_j in 1:length(com_name_vector)) {
      nodes_in_community_i = which(node_aff_vector == community_i)
      nodes_in_community_j = which(node_aff_vector == community_j)
      community_var[community_i,community_j] = get_community_var_in_sample(sample_net,nodes_in_community_i,nodes_in_community_j)
    }
  }
  colnames(community_var) <- com_name_vector
  rownames(community_var) <- com_name_vector
  community_var
}


get_community_net<-function(netmat,com_name_vector,node_aff_vector){
  community_net =  array(NA, dim=c(length(com_name_vector),length(com_name_vector)))
  for (community_i in 1:length(com_name_vector)){
    for (community_j in 1:length(com_name_vector)) {
      nodes_in_community_i = which(node_aff_vector == community_i)
      nodes_in_community_j = which(node_aff_vector == community_j)
      community_net[community_i,community_j] = 
        mean(netmat[nodes_in_community_i,nodes_in_community_j],na.rm=TRUE)
      #community_pair_var = communnity_var[community_i,community_j]
      #community_net[community_i,community_j] = get_subj_mean_of_sqr(netmat,nodes_in_community_i,nodes_in_community_j) - community_pair_var
    }
  }
  colnames(community_net) <- com_name_vector
  rownames(community_net) <- com_name_vector
  community_net
}


##############################
####                      ####
####       PRS GAMs       ####
####                      ####
##############################
prs_img_gam10<-function(vab_of_int, prs_df, modality){
  if (modality == 'fc') {
    out<-gam(vab_of_int ~ ageAtScan1 +  sex + scz_prs + restRelMeanRMSMotion + chip +
               pc1 + pc2 + pc3 + pc4 + pc5 +
               pc6 + pc7 + pc8 + pc9 + pc10, data = prs_df, method="REML")
  } else if (modality == 'sc') {
    out<-gam(vab_of_int ~ s(ageAtScan1) +  sex + scz_prs + dti32MeanRelRMS + chip +
               pc1 + pc2 + pc3 + pc4 + pc5 +
               pc6 + pc7 + pc8 + pc9 + pc10, data = prs_df, method="REML")
  }
}

prs_img_gam_wei<-function(vab_of_int, prs_df, modality){
  if (modality == 'fc') {
    out<-gam(vab_of_int ~ ageAtScan1 +  sex + scz_prs + weightedRelMeanMotion + chip +
               pc1 + pc2 + pc3 + pc4 + pc5 +
               pc6 + pc7 + pc8 + pc9 + pc10, data = prs_df, method="REML")
  } else if (modality == 'sc') {
    out<-gam(vab_of_int ~ s(ageAtScan1) +  sex + scz_prs + dti32MeanRelRMS + chip +
               pc1 + pc2 + pc3 + pc4 + pc5 +
               pc6 + pc7 + pc8 + pc9 + pc10, data = prs_df, method="REML")
  }
}


prs_gam<-function(vab_of_int, prs_df){
  out<-gam(vab_of_int ~ s(ageAtScan1, k =4) + sex +  scz_prs , data = prs_df, method="REML")
}

apply_prs_gam <-function(prs_voi_df,voi_names,modality) {
  prs_voi_reml<-lapply(voi_names,function(vab_of_int) prs_img_gam10(prs_voi_df[,vab_of_int],prs_voi_df,modality))
  #prs_voi_reml<-lapply(voi_names,function(vab_of_int) prs_img_gam_wei(prs_voi_df[,vab_of_int],prs_voi_df,modality))
  prs_voi_pval<-sapply(prs_voi_reml, function(reml_result) {summary(reml_result)$p.pv})
  colnames(prs_voi_pval) <- voi_names
  voi_of_sig <- prs_voi_pval[,which(prs_voi_pval['scz_prs',] <0.05)]
  out <- list(data = prs_voi_df, reml_result = prs_voi_reml,pval = prs_voi_pval, sig_voi = voi_of_sig)
}

get_gams <- function(community_info,sample_net, sample,modality){
  com_net_names <- get_community_net_names(community_info$CommunityName)
  com_net<-lapply(sample_net,function(netmat) get_community_net(netmat,community_info$CommunityName,community_info$CommunityAffiliation))
  com_net_flat<-t(sapply(com_net,function(net) net[upper.tri(net,diag = TRUE)]))
  colnames(com_net_flat) <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
  com_net_names_flat <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
  com_net_flat <- cbind(com_net_flat,sample)
  com_net_flat$sex <- as.ordered(as.factor(com_net_flat$sex))
  prs_gams <- apply_prs_gam(com_net_flat,com_net_names_flat,modality)
  out <- list(gam = prs_gams, data = com_net_flat)
}

#################################
####                         ####
#### run sample and get pval ####
####                         ####
#################################
prs_within_between_com_analysis<-function(sample_net,sample_qa,modality,parcellation,resolution,num_com,sub_to_remove) {
  if (parcellation == "schaefer") {
    community_info <-  get_schaefer_node_info(resolution = resolution,num_com = num_com)
  } else if (parcellation == "power"){
    community_info <- get_power_node_info(num_com = num_com)
  } else if (parcellation == "gordon") {
    community_info <- get_gordon_node_info()
  } else if (parcellation == "wsbm"){
    community_info <- get_wsbm_node_info(CIs)
  } else if (parcellation == "schaefer200lv"){
    community_info <- get_louvein_node_info(best_consensus)
  }
  
  
  ##  determine whether there is any empty samples ##  
  empty_net<-which(is.na(sample_net) == T)
  non_empty_sample_net <- sample_net
  non_empty_sample_net[empty_net] <- NULL
  non_empty_sample <- sample_qa[-empty_net,]
  empty_sample<- sample_qa[empty_net,]
  ##  run gam model #
  
  if (length(empty_net) > 0) {
    print(paste('This sample contains',length(empty_net),'subjects without processed data'))
    prs_gams <- get_gams(community_info = community_info, sample_net = non_empty_sample_net, sample = non_empty_sample,modality)
  } else if (length(sub_to_remove) > 0)  {
    new_sample_net <- sample_net
    new_sample_net[sub_to_remove] <- NULL
    new_sample <- sample_qa[-sub_to_remove,]
    prs_gams <- get_gams(community_info = community_info, sample_net = new_sample_net, sample = new_sample,modality)
  } else {
    prs_gams <- get_gams(community_info = community_info, sample_net = sample_net, sample = sample_qa,modality)
  }
  
  
  ##  pull p-values ##  
  
  com_names <- get_community_net_names(CommunityName =community_info$CommunityName )
  within_coms <- diag(com_names)
  within_coms_pval <- prs_gams$gam$pval['scz_prs',within_coms]
  within_coms_fdr <- p.adjust(within_coms_pval,method ='fdr')
  
  between_coms <- com_names[upper.tri(com_names)]
  between_coms_pval<-prs_gams$gam$pval['scz_prs',between_coms]
  between_coms_fdr <- p.adjust(between_coms_pval,method ='fdr')
  
  out <- list(within_coms_pval = within_coms_pval,
              within_coms_fdr = within_coms_fdr,
              between_coms_pval = between_coms_pval,
              between_coms_fdr = between_coms_fdr,
              prs_gams = prs_gams)
}

### other helper functions ##
# convert a list object in R to 3D matrix for matlab processing #
list_to_matrix <- function(list){
  dim1 <- length(list)
  dim2 <- dim(list[[1]])[1]
  dim3 <- dim(list[[1]])[2]
  mat_out <- array(NA,c(dim1,dim2,dim3))
  for (i in 1:dim1){
    mat_out[i,,] <- list[[i]]
    print(paste("Processing......",i,"out of",dim1))
  }
  print(paste("Conversion Successful. Converted matrix has dimensions of",dim(mat_out)))
  return(mat_out)
}

## better_level_plot ##
better_levelplot <- function(adj, node_names, title) {
  adj_norm <- adj/max(abs(adj))
  limit = max(abs(adj_norm))
  keycol=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
  plot<-levelplot(adj_norm, par.settings = BuRdTheme(), 
                  at = seq(limit,-limit,length.out = 12),xlab="",ylab = "", strip = F, contour = F, region= T,main=title,
                  scales=list(x=list(at = 1:length(node_names), labels=node_names,rot=90, tck = 0),
                              y=list(at = 1:length(node_names),labels=node_names, tck = 0)))
  return(plot)
}

get_net_all_cat_tasks <- function(bblid,scanid,parcellation) {
  #concatenates all three types of scans
  
  task = 'restbold'
  rest_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'*201607151621/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  rest_ts <- read.table(rest_ts_path)
  
  task = 'nback'
  nback_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'Connect_*/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  nback_ts <- read.table(nback_ts_path)
  
  task = 'idemo'
  idemo_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'Connect_*/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  idemo_ts <- read.table(idemo_ts_path)
  
  cat_ts <- rbind(rest_ts,nback_ts,idemo_ts)
  
  #This grabs your correlation matrix
  net <- get_net_from_ts(cat_ts)
  #net <- get_net_from_ts(rest_ts)
}

get_net_from_ts <-function(ts) {
  num_node <- dim(ts)[2]
  net <- matrix(NA,nrow = num_node, ncol = num_node)
  for (i in 1:num_node) {
    for (j in 1:num_node) {
      net[i,j] <- cor(ts[,i],ts[,j],method = 'pearson')
    }
  }
  #display the matrix
  net
}

