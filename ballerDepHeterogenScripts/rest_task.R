## set directories ##
local_wkdir <- '~/Google Drive/TDSlab/SCZ_gene_imaging/'
remote_wkdir <- '~/Desktop/BBL/data/joy/BBL/studies/pnc/'

idemo_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/idemo/n1601_IdemoConnectQAData_20170718.csv'))
nback_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/nback/n1601_NbackConnectQAData_20170718.csv'))
rest_qa <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv'))

chipinfo <-  read.csv('~/Google Drive/TDSlab/SCZ_gene_imaging/genotype/converted_chipinfo_EA.csv')
t1_qa <- read.csv(paste0(remote_wkdir,"n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv"))
hx_qa <- read.csv(paste0(remote_wkdir,"n1601_dataFreeze/health/n1601_health_20161214.csv"))
sample_demo <- read.csv(paste0(remote_wkdir,'n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv'))
prs <- read.csv(paste0(local_wkdir,'genotype/pnc_EA_prs_20170501.csv'))


idemo_nvol <- 204
nback_nvol <- 225
rest_nvol <- 120

weightedRelMeanMotion <- (rest_nvol*rest_qa$restRelMeanRMSMotion + 
                          nback_nvol*nback_qa$nbackRelMeanRMSMotion + 
                          idemo_nvol*idemo_qa$idemoRelMeanRMSMotion)/
                        (idemo_nvol+nback_nvol+rest_nvol)
rest_task_qa <- data.frame(bblid=idemo_qa$bblid,scanid= idemo_qa$bblid,weightedRelMeanMotion=weightedRelMeanMotion)
rest_task_qa$weightedRelMeanExclude <-  1
weightedRelMeanMotion_criteria <- which(rest_task_qa$weightedRelMeanMotion<=0.2)
rest_task_qa$weightedRelMeanExclude[weightedRelMeanMotion_criteria] <- 0
rest_task_qa$AbsRelMeanExclude <- 1
AbsRelMeanExclude_criteria <- which(idemo_qa$idemoRelMeanRMSMotionExclude != 1 & 
                                      nback_qa$nbackRelMeanRMSMotionExclude != 1 & 
                                      rest_qa$restRelMeanRMSMotionExclude != 1)
rest_task_qa$AbsRelMeanExclude[AbsRelMeanExclude_criteria] <- 0
hx_t1_qa <- merge(hx_qa,t1_qa)
hx_t1_qa_demo <- merge(hx_t1_qa,sample_demo)

prs_chip <- merge(prs,chipinfo)
prs_chip_key <- merge(prs_chip, prs_img_match_key,by.x = 'bblid',by.y='new_bblid')

rest_task_qa_hx_t1_demo <- merge(hx_t1_qa_demo,rest_task_qa,by='bblid')
colnames(rest_task_qa_hx_t1_demo)[c(1,2)] <- c('org_bblid','scanid')

rest_task_master_key <- merge(rest_task_qa_hx_t1_demo,prs_chip_key,by= 'org_bblid' )
colnames(rest_task_master_key)[which(colnames(rest_task_master_key) == 'bblid')] <- 'new_bblid'
colnames(rest_task_master_key)[which(colnames(rest_task_master_key) == 'org_bblid')] <- 'bblid'


wei_rest_task_sample <- subset(rest_task_master_key, healthExcludev2 == 0 & t1Exclude == 0 & weightedRelMeanExclude == 0)
abs_rest_task_sample <- subset(rest_task_master_key, healthExcludev2 == 0 & t1Exclude == 0 & AbsRelMeanExclude == 0)
save(wei_rest_task_sample,abs_rest_task_sample, 
     file = paste0(remote_project_wkdir,'/result/sample_qa_rest_task_newkey.RData'))

wei_sub_to_remove = which(wei_rest_task_sample$chip == 'Human1M-Duov3_B' | wei_rest_task_sample$chip == 'HumanHap550_v1')
abs_sub_to_remove = which(abs_rest_task_sample$chip == 'Human1M-Duov3_B' | abs_rest_task_sample$chip == 'HumanHap550_v1')

######## power #######
get_net_rest_task <- function(bblid,scanid,parcellation) {

  task = 'nback'
  nback_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'Connect_*/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  nback_ts <- read.table(nback_ts_path)
  
  task = 'idemo'
  idemo_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'Connect_*/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  idemo_ts <- read.table(idemo_ts_path)
  
  task = 'restbold'
  rest_ts_path <- Sys.glob(paste0(remote_wkdir,'processedData/',task,'/',task,'*201607151621/',bblid,'/*x',scanid,'/net/',parcellation,'/*_ts.1D'))
  rest_ts <- read.table(rest_ts_path)
  
  cat_ts <- rbind(nback_ts,idemo_ts,rest_ts)
  
  get_net_from_ts <-function(ts) {
    num_node <- dim(ts)[2]
    net <- matrix(NA,nrow = num_node, ncol = num_node)
    for (i in 1:num_node) {
      for (j in 1:num_node) {
        net[i,j] <- cor(ts[,i],ts[,j],method = 'pearson')
      }
    }
    net
  }
  
  #net <- get_net_from_ts(cat_ts)
  net <- get_net_from_ts(rest_ts)
}

parcellation = '264PowerPNC'
parcellation = 'GordonPNC'
parcellation = 'SchaeferPNC'

get_sample_net_rest_task <- function(sample,parcellation) {
  num_sub <- nrow(sample)
  sample_net<-lapply(1:num_sub, function(i) {
    print(paste(i,'/',num_sub,'for',parcellation,'in',deparse(substitute(sample))))
    net<-get_net_rest_task(sample[i,'bblid'],sample[i,'scanid'],parcellation)
    })
  save_file_path <- paste0('~/Desktop/BBL/data/jux/BBL/projects/prsConnectivity/result/',num_sub,'_',parcellation,'_rest_task_net.RData')
  #save(sample_net,sample,file = save_file_path)
  sample_net
}

wei_rest_task_schafer_net <- get_sample_net_rest_task(wei_rest_task_sample,'SchaeferPNC')
wei_rest_task_gordon_net <- get_sample_net_rest_task(wei_rest_task_sample,'GordonPNC')

abs_rest_task_schafer_net <- get_sample_net_rest_task(abs_rest_task_sample,'SchaeferPNC')
abs_rest_task_gordon_net <- get_sample_net_rest_task(abs_rest_task_sample,'GordonPNC')

parcellation_list = c('264PowerPNC','GordonPNC','SchaeferPNC')

for (i in 1:length(parcellation_list)) {
  get_sample_net_rest_task(wei_rest_task_sample,parcellation_list[i])
  get_sample_net_rest_task(abs_rest_task_sample,parcellation_list[i])
}

wei_rest_task_schafer_net<- sample_net
wei_rest_task_gordon_net <- sample_net

abs_rest_task_schafer_net<- sample_net
abs_rest_task_gordon_net <- sample_net

wei_rest_task_schafer_7 <- prs_within_between_com_analysis(sample_net =wei_rest_task_schafer_net,sample_qa = wei_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 7,sub_to_remove = wei_sub_to_remove)
wei_rest_task_schafer_17 <- prs_within_between_com_analysis(sample_net =wei_rest_task_schafer_net,sample_qa = wei_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 17,sub_to_remove = wei_sub_to_remove)

abs_rest_task_schafer_7 <- prs_within_between_com_analysis(sample_net =abs_rest_task_schafer_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 7,sub_to_remove = abs_sub_to_remove)
abs_rest_task_schafer_17 <- prs_within_between_com_analysis(sample_net =abs_rest_task_schafer_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'schaefer',resolution = 400,num_com = 17,sub_to_remove = abs_sub_to_remove)

wei_rest_task_gordon <- prs_within_between_com_analysis(sample_net =wei_rest_task_gordon_net,sample_qa = wei_rest_task_sample,modality = 'fc',parcellation = 'gordon', sub_to_remove = wei_sub_to_remove)
abs_rest_task_gordon <- prs_within_between_com_analysis(sample_net =abs_rest_task_gordon_net,sample_qa = abs_rest_task_sample,modality = 'fc',parcellation = 'gordon',sub_to_remove = abs_sub_to_remove)
