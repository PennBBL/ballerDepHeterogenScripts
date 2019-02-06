require('stringr')
require('R.matlab')
require('mgcv')
require('visreg')
require('rasterVis')
require('lattice')
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
  nback_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/nback/nbackConnectTaskRegress/n1601_NbackConnectTaskRegressQAData_2018-10-21.csv'))
  idemo_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/idemo/n1601_IdemoConnectQAData_20170718.csv'))
  
  #set num volumes  
  rest_nvol <- 120
  nback_nvol <- 221
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

#############################
####                     ####
#### 27 Funcional ROIS   ####
####                     ####
#############################
get_27roi_concat_netpath <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm/weighted_n312_rest_nback_idemo_tr_20181218/ts_files_from_3dROIstats/', scanid, '*.txt'))
}

get_27roi_concat_node_info <- function() {
  CommunityAffiliation <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/nback27FunctionalROIs/nback27parcelNodeAffiliations.txt", header=FALSE)$V1
  CommunityName<-read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/nback27FunctionalROIs/nback27parcelCommunityNames.txt", header
                           =FALSE)$V1
  NodeName <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/nback27FunctionalROIs/nback27parcelNodeNames.txt", header=FALSE)
  out <- list(CommunityAffiliation=CommunityAffiliation, CommunityName=CommunityName, NodeName = NodeName)
}


#############################
####                     ####
#### 8 Funcional ROIS   ####
####                     ####
#############################
get_8roi_concat_netpath <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/ts_files_from_3dROIstats_8rois/', scanid, '*.txt'))
}

get_8roi_concat_node_info <- function() {
  CommunityAffiliation <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_CommunityAffiliation.txt", header=FALSE)$V1
  CommunityName<-read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_CommunityName.txt", header
                           =FALSE)$V1
  NodeName <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_justnames.txt", header=FALSE)
  out <- list(CommunityAffiliation=CommunityAffiliation, CommunityName=CommunityName, NodeName = NodeName)
}

#############################
####                     ####
#### 14 Funcional ROIS   ####
####                     ####
#############################
get_14roi_concat_netpath <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/ts_files_from_3dROIstats_14rois/', scanid, '*.txt'))
}

get_14roi_concat_node_info <- function() {
  CommunityAffiliation <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/nback14parcelNodeAffiliations.txt", header=FALSE)$V1
  CommunityName<-read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/nback14parcelCommunityNames.txt", header=FALSE)$V1
  NodeName <- read.csv2("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/nback14parcelNodeNames.txt", header=FALSE)
  out <- list(CommunityAffiliation=CommunityAffiliation, CommunityName=CommunityName, NodeName = NodeName)
}

#############################
####                     ####
#### Netpath 2mm 27/8/14 ####
####                     ####
#############################

get_27roi_concat_netpath_2mm <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/concat_rest_nback_idemo_2mm/ts_files_from_3dROIstats/', scanid, 
                             '*.txt'))
}

get_8roi_concat_netpath_2mm <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs_2mm/ts_files_from_3dROIstats_8rois_2mm/', scanid, '*.txt'))
}

get_14roi_concat_netpath_2mm <- function(scanid) {
  netpath <- Sys.glob(paste0('/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs_2mm/ts_files_from_3dROIstats_14rois_2mm/', scanid, '*.txt'))
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
    } else if (parcellation == '27roi') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
      netpath <- get_27roi_concat_netpath(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == '14roi') {
        print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
        netpath <- get_14roi_concat_netpath(scanid)
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
    
    
    # import the network data #
    
    if (parcellation == 'power') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation,' atlas'))
      netpath <- get_power_netpath_concat(scanid)
      sample_net[[i]] <- grab_net_from_path(netpath)
    } else if (parcellation == '27roi') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
      if (resolution == '2mm') {
        netpath <- get_27roi_concat_netpath_2mm(scanid)
      } else { 
        netpath <- get_27roi_concat_netpath(scanid)
      }
      ts <- read.table(netpath)
      print(paste0(netpath," ts dim", dim(ts)[2]))
      net_from_ts <- get_net_from_ts(ts = ts)
      print(paste0("output dim", dim(net_from_ts)[1], " by", dim(net_from_ts)[2]))
      sample_net[[i]] <- net_from_ts
    } else if (parcellation == '8roi') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
      if (resolution == '2mm') {
        netpath <- get_8roi_concat_netpath_2mm(scanid) 
      } else {
        netpath <- get_8roi_concat_netpath(scanid)
      }
      ts <- read.table(netpath)
      print(paste0(netpath," ts dim", dim(ts)[2]))
      net_from_ts <- get_net_from_ts(ts = ts)
      print(paste0("output dim", dim(net_from_ts)[1], " by", dim(net_from_ts)[2]))
      sample_net[[i]] <- net_from_ts
    }else if (parcellation == '14roi') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
      if (resolution == '2mm') {
        netpath <- get_14roi_concat_netpath_2mm(scanid)
      } else {
        netpath <- get_14roi_concat_netpath(scanid)
      }
      ts <- read.table(netpath)
      print(paste0(netpath," ts dim", dim(ts)[2]))
      net_from_ts <- get_net_from_ts(ts = ts)
      print(paste0("output dim", dim(net_from_ts)[1], " by", dim(net_from_ts)[2]))
      sample_net[[i]] <- net_from_ts
    } else if (parcellation == '8x27roi') {
      print(paste0(i,"/",n_sample,": copying ",bblid,'_',scanid, ' of ',parcellation, ' atlas'))
      
      #copy in 27 ROItable
      netpath <- get_27roi_concat_netpath_2mm(scanid)
      ts <- read.table(netpath)
      print(paste0(netpath," ts dim", dim(ts)[2]))
     
       #get 27x27 matrix
      net_from_ts <- get_net_from_ts(ts = ts)
      print(paste0("output dim", dim(net_from_ts)[1], " by", dim(net_from_ts)[2]))
      
      #remove rows not part of 8 functional networks to make 8x27
      roi8_justnumbers <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_justnumbers.txt")
      net_from_ts_8x27 <- net_from_ts[roi8_justnumbers$V1,]
      sample_net[[i]] <- net_from_ts_8x27
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

get_node_names_edge_analysis <- function(node_names){
 #gets back an array of node names for edge analysis, exludes diagonal
  node_names <- c(sapply(node_names[1], as.character))
  node_names<- gsub("nback_func_sc_", "", node_names)
 # print(node_names)
  num_nodes <- length(node_names)
 # print(num_nodes)
  length_array <- (num_nodes * (num_nodes -1))/2
#  print(length_array)
  name_array <- array(NA,length_array)
  nrow <- 1
  num_node_a <- 1
  num_node_b<- 1
  for (node_a in node_names) {
    for (node_b in node_names) {
      #goes by columns, then rows, but writes out row_col, this is to match the upper.tri output
      if (num_node_b < num_node_a) {
        name_array[nrow] <- paste0(node_b, "_", node_a)
        nrow <- nrow + 1
      }
      num_node_b <- num_node_b + 1
    }
    num_node_a <- num_node_a + 1
    num_node_b <- 1
  }
  return(name_array)
}

get_node_names_edge_analysis_diff_dim <- function(node_names_col, node_names_row){
  #gets back an array of node names for edge analysis, exludes diagonal
  node_names_col <- c(sapply(node_names_col[1], as.character))
  node_names_row <- c(sapply(node_names_row[1], as.character))
  node_names_col<- gsub("nback_func_sc_", "", node_names_col)
  node_names_row <- gsub("nback_func_sc_", "", node_names_row)
  # print(node_names)
  num_nodes_col <- length(node_names_col)
  num_nodes_row <- length(node_names_row)
  # print(num_nodes)
  length_array <- (num_nodes_col * (num_nodes_row - num_nodes_col)) + (num_nodes_col * (num_nodes_col - 1))/2 
  #  print(length_array)
  name_array <- array(NA,length_array)
  nrow <- 1
  num_node_r <- 1
  num_node_c<- 1
  for (c in node_names_col) {
    for (r in node_names_row) {
      #goes by columns, then rows, but writes out row_col, this is to match the upper.tri output
      name_to_add <- paste0(c, "_", r)
      reverse <- paste0(r, "_", c)
      
      if ((c != r) & !(reverse %in% name_array)) {
      
        name_array[nrow] <- paste0(c, "_", r)
        nrow <- nrow + 1
      }
      num_node_r <- num_node_r + 1
    }
    num_node_r <- 1
    num_node_c <- num_node_c + 1
  }
  return(name_array)
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

matrix_to_flat_for_edge_analysis <- function(net_mat, col_name, row_name) {
 # makes a flat list, goes down columns
  
  node_names_col <- c(sapply(col_name[1], as.character))
  node_names_row <- c(sapply(row_name[1], as.character))
  node_names_col<- gsub("nback_func_sc_", "", node_names_col)
  node_names_row <- gsub("nback_func_sc_", "", node_names_row)

  num_nodes_col <- length(node_names_col)
  num_nodes_row <- length(node_names_row)

  length_array <- (num_nodes_col * (num_nodes_row - num_nodes_col)) + (num_nodes_col * (num_nodes_col - 1))/2 
 
  name_array <- array(NA,length_array)
  new_flat <- array(NA,length_array)
  nrow <- 1
  num_node_r <- 1
  num_node_c<- 1
  
  for (c in node_names_col) {
    for (r in node_names_row) {
      #goes by columns, then rows, but writes out row_col, this is to match the upper.tri output
      name_to_add <- paste0(c, "_", r)
      reverse <- paste0(r, "_", c)
    #  print(paste0(num_node_r, " ", num_node_c))
      if ((c != r) & !(reverse %in% name_array)) {
        
        name_array[nrow] <- paste0(c, "_", r)
        new_flat[nrow] <- net_mat[num_node_c,num_node_r]
        nrow <- nrow + 1
      }
      num_node_r <- num_node_r + 1
    }
    num_node_r <- 1
    num_node_c <- num_node_c + 1
  }
  
  #nrow <- dim(net_mat)[1]
  #ncol <- dim(net_mat)[2]
  
  #node_names_row <- c(sapply(row_name[1], as.character))
  #node_names_col <- c(sapply(col_name[1], as.character))
  
  #colnames(net_mat) <- node_names_col
  #rownames(net_mat) <- node_names_row
  
  #cnt <- 1
  #total_length_flat <- (nrow* (ncol-nrow)) + (nrow * (nrow-1))/2
  #final_array <- array(NA, dim = total_length_flat)
  #names_array <- array(NA, dim = total_length_flat)
  
  
  #for(c in 1:ncol) {
   # for (r in 1:nrow) {
    
    #  if (c!=r & ) {
    
     #   final_array[cnt] <- net_mat[r,c]
      #  cnt <- cnt + 1
      #}
    #}
  #}
  return(new_flat)
  #return(final_array)
  
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


list_to_matrix_edge_analysis <- function(list) {
  dim1 <- length(list)
  dim2 <- length(list[[1]])
  mat_out <- array(NA, c(dim1,dim2))
  for(i in 1:dim1){
    mat_out[i,] <- list[[i]]
    print(paste("Processing.....",i," out of ", dim1))
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

## better_level_plot ##
better_levelplot_edge <- function(adj, node_names_x, node_names_y, title) {
  adj_norm <- adj/max(abs(adj))
  limit = max(abs(adj_norm))
  keycol=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
  plot<-levelplot(adj_norm, par.settings = BuRdTheme(), 
                  at = seq(limit,-limit,length.out = 12),xlab="",ylab = "", strip = F, contour = F, region= T,main=title,
                  scales=list(x=list(at = 1:length(node_names_x), labels=node_names_x,rot=90, tck = 0),
                              y=list(at = 1:length(node_names_y),labels=node_names_y, tck = 0)))
  return(plot)
}
get_net_all_cat_tasks <- function(bblid,scanid,parcellation) {
  #concatenates all three types of scans
  
  task = 'restbold'
  rest_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'*201607151621/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  rest_ts <- read.table(rest_ts_path)
  
  task = 'nback'
  #nback_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'Connect_*/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  #changed 20181023 to point to good and new nback
  nback_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/xcptaskregressed/', bblid,'/*x',scanid,'/fcon/',parcellation,'/*_ts.1D'))
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

###############################################
###    Get 8rois from 27 roi textfile       ###
###############################################
write_new_ts_file_with_8_rois <- function(ts) {
  rois_to_keep <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_justnumbers.txt")
  timeseries <- read.table(ts)
  timeseries_8rois <- timeseries[,rois_to_keep$V1]
 # timeseries_8rois <- timeseries[,c(1,3,7,11,12,18,19,20)]
  write.table(x = timeseries_8rois, file = ts, row.names = F, col.names = F)
}


#######################################################
###    Get subset of ROIs from textfile, generic    ###
#######################################################
write_new_ts_file_generic_rois <- function(ts, rois_to_keep) {
  rois_to_keep <- read.table(rois_to_keep)
  timeseries <- read.table(ts)
  timeseries_rois <- timeseries[,rois_to_keep$V1]
  write.table(x = timeseries_rois, file = ts, row.names = F, col.names = F)
}
###############################################
###    Get 14rois from 27 roi textfile       ###
###############################################
write_new_ts_file_with_14_rois <- function(ts) {
  rois_to_keep <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/roi14_justnumbers.txt")
  print(paste0("rois to keep -->>> ", rois_to_keep))
  print(paste0("timeseries --->>> ", ts))
  timeseries <- read.table(ts)
  timeseries_14rois <- timeseries[,rois_to_keep$V1]
  # timeseries_8rois <- timeseries[,c(1,3,7,11,12,18,19,20)]
  write.table(x = timeseries_14rois, file = ts, row.names = F, col.names = F)
}

#########################
### make_new_textfiles ##
#########################
write_2mm_text_files <- function(x) {
rois_to_keep_14 <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs/roi14_justnumbers.txt")
rois_to_keep_8 <- read.table("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs/roi8_justnumbers.txt")
indir <- "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/concat_rest_nback_idemo_2mm/ts_files_from_3dROIstats/"
outdir_14 <- "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_14FunctionalROIs_2mm/ts_files_from_3dROIstats_14rois_2mm/"
outdir_8 <- "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs_2mm/ts_files_from_3dROIstats_8rois_2mm/"
files <-  list.files(path=indir, pattern = "*.txt", recursive = FALSE)
lapply(files, function(file) {
  print(file)
   timeseries <- read.table(paste0("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/concat_rest_nback_idemo_2mm/ts_files_from_3dROIstats/", file))
   timeseries_14rois <- timeseries[,rois_to_keep_14$V1]
   timeseries_8rois <- timeseries[,rois_to_keep_8$V1]
   newName_14 <- gsub(pattern = "emo", replacement = "emo_14", x = file)
   newName_8 <- gsub(pattern = "emo", replacement = "emo_8", x = file)
   newFile_14 <- paste0(outdir_14, "/", newName_14)
   newFile_8 <- paste0(outdir_8, "/", newName_8)
   write.table(x = timeseries_14rois, file = newFile_14, row.names = F, col.names = F)
   write.table(x = timeseries_8rois, file = newFile_8, row.names = F, col.names = F)
  })
}