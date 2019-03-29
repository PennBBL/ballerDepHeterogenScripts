source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/from_cedric/prs_source_code/prs_functions.R')

#########################
############
###Now start the code###
############

###########
##### Making the new sample, with all the different movement excludes
###########

local_wkdir <- '/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/fc_communities_rest_and_idemo/'
remote_wkdir <- '/data/joy/BBL/studies/pnc/'

#load qa for each different scan type
rest_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv'))
idemo_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/idemo/n1601_IdemoConnectQAData_20170718.csv'))

#load in sample from hydra (has already been t1, demographic, health history QA'ed)
sample_from_HYDRA <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/data/subset_with_T1_FC_and_dem_with_clusters.rds")

#number of volumes for each type of scan
rest_nvol <- 120
idemo_nvol <- 204

#do a weighted relative mean motion, putting more emphasis on scans with more volume
weightedRelMeanMotion <- (rest_nvol*rest_qa$restRelMeanRMSMotion + 
                            idemo_nvol*idemo_qa$idemoRelMeanRMSMotion)/(idemo_nvol+rest_nvol)

#start with the resting task
rest_task_qa <- data.frame(bblid=rest_qa$bblid,scanid= rest_qa$bblid,weightedRelMeanMotion=weightedRelMeanMotion)

#assume that everything will be excluded by weighted rel mean
#rest_task_qa$weightedRelMeanExclude <- 1

#set criteria for motion to be less tha <= 0.2mm and only select people who meet criteria
#lose 465, new n = 1136
#weightedRelMeanMotion_criteria <- which(rest_task_qa$weightedRelMeanMotion<=0.2)

#set the weighted mean exclude motion variable to be 0 if they meet criteria
#i.e., if you move less than 0.2mm, you are included
#rest_task_qa$weightedRelMeanExclude[weightedRelMeanMotion_criteria] <- 0

#set absolute rel mean motion to exclude
rest_task_qa$AbsRelMeanExclude <- 1

#set criteria to be that you have to not have been exluded for motion in rest, nback or idemo
#lost 601 people, new n = 1000
AbsRelMeanExclude_criteria <- which(rest_qa$restRelMeanRMSMotionExclude != 1 & 
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
#weighted_rest_task_sample <- subset(rest_task_qa_FROM_HYDRA, weightedRelMeanExclude == 0)

#Get our sample that meets absolute rule out, lost 20, n = 305
abs_rest_task_sample <- subset(rest_task_qa_FROM_HYDRA, AbsRelMeanExclude == 0)

#save these two files into the results section
#save(weighted_rest_task_sample,abs_rest_task_sample, 
   #  file = '/data/jux/BBL/projects/ballerDepHeterogen/results/rds/weighted_and_abs_rest_and_idemo.RData')

write.csv(abs_rest_task_sample, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo/subject_lists/rest_idemo_sample.csv")
write.table(abs_rest_task_sample$bblid, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo/subject_lists/rest_idemo_sample_BBLID.csv", row.names = F, col.names = F)
write.table(abs_rest_task_sample$scanid, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo/subject_lists/rest_idemo_sample_SCANID.csv", row.names = F, col.names = F)