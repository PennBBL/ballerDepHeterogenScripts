source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/from_cedric/prs_source_code/prs_functions.R')

#########################
############
###Now start the code###
############

###########
##### Making the new sample, with all the different movement excludes
###########

local_wkdir <- '/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/fc_communities_three_scans/'
remote_wkdir <- '/data/joy/BBL/studies/pnc/'

#load qa for each different scan type
rest_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv'))
nback_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/nback/n1601_NbackConnectQAData_20170718.csv'))
idemo_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/idemo/n1601_IdemoConnectQAData_20170718.csv'))

#load in sample from hydra (has already been t1, demographic, health history QA'ed)
sample_from_HYDRA <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/data/subset_with_T1_FC_and_dem_with_clusters.rds")

#number of volumes for each type of scan
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
save(weighted_rest_task_sample,abs_rest_task_sample, 
     file = '/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_fc/concatenate_rest_nback_idemo/weighted_and_abs_rest_task_sample.RData')


#################################

###The work horse, actually concatenating the files and making the matrices
########

###### FUNCTIONS ############
######## power #######

#makes a correlation matrix
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

get_net_all_cat_tasks <- function(bblid,scanid,parcellation) {
  #concatenates all three types of scans

  task = 'restbold'
  rest_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'*201607151621/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  rest_ts <- read.table(rest_ts_path)

  print(rest_ts)
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

parcellation = '264PowerPNC'
parcellation = 'GordonPNC'
parcellation = 'SchaeferPNC'

get_sample_net_rest_task <- function(sample,parcellation) {
  ####Get the networks for a WHOLE SAMPLE
  num_sub <- nrow(sample)
  sample_net<-lapply(1:num_sub, function(i) {
    print(paste(i,'/',num_sub,'for',parcellation,'in',deparse(substitute(sample))))
    net<-get_net_all_cat_tasks(sample[i,'bblid'],sample[i,'scanid'],parcellation)
  })
  save_file_path <- paste0('/data/jux/BBL/projects/ballerDepHeterogen/results/output_of_fc/concatenate_rest_nback_idemo/indiv_subj_concat_text_files/',num_sub,'_',parcellation,'_rest_task_net_NEW_NBACK.RData')
  save(sample_net,sample,file = save_file_path)
  sample_net
}


#I have only run weighted_rest_task_schafer_net, not gordon
weighted_rest_task_schafer_net <- get_sample_net_rest_task(weighted_rest_task_sample,'SchaeferPNC')
weighted_rest_task_gordon_net <- get_sample_net_rest_task(weighted_rest_task_sample,'GordonPNC')

abs_rest_task_schafer_net <- get_sample_net_rest_task(abs_rest_task_sample,'SchaeferPNC')
abs_rest_task_gordon_net <- get_sample_net_rest_task(abs_rest_task_sample,'GordonPNC')

parcellation_list = c('264PowerPNC','GordonPNC','SchaeferPNC')

for (i in 1:length(parcellation_list)) {
  get_sample_net_rest_task(weighted_rest_task_sample,parcellation_list[i])
  get_sample_net_rest_task(abs_rest_task_sample,parcellation_list[i])
}

weighted_rest_task_schafer_net<- sample_net
weighted_rest_task_gordon_net <- sample_net

abs_rest_task_schafer_net<- sample_net
abs_rest_task_gordon_net <- sample_net

wei_sub_to_remove <- "NULL"
abs_sub_to_remove <- "NULL"

weighted_rest_task_schafer_r400_com7 <- prs_within_between_com_analysis(sample_net =weighted_rest_task_schafer_net,sample_qa = weighted_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 7,sub_to_remove = weighted_sub_to_remove)
weighted_rest_task_schafer_r400_com17 <- prs_within_between_com_analysis(sample_net =weighted_rest_task_schafer_net,sample_qa = weighted_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 17,sub_to_remove = weighted_sub_to_remove)

abs_rest_task_schafer_r400_com7 <- prs_within_between_com_analysis(sample_net =abs_rest_task_schafer_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 7,sub_to_remove = abs_sub_to_remove)
abs_rest_task_schafer_r400_com17 <- prs_within_between_com_analysis(sample_net =abs_rest_task_schafer_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 17,sub_to_remove = abs_sub_to_remove)

weighted_rest_task_gordon <- prs_within_between_com_analysis(sample_net =weighted_rest_task_gordon_net,sample_qa = weighted_rest_task_sample,modality = 'fc',parcellation = 'gordon', sub_to_remove = weighted_sub_to_remove)
abs_rest_task_gordon <- prs_within_between_com_analysis(sample_net =abs_rest_task_gordon_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'gordon',sub_to_remove = abs_sub_to_remove)