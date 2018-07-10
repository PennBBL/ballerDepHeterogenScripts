## match subjects ##
## set directories ##
local_wkdir <- '~/Google Drive/TDSlab/SCZ_gene_imaging/'
remote_study_wkdir <- '~/Desktop/BBL/data/joy/BBL/studies/pnc/'
remote_project_wkdir <- '~/Desktop/BBL/data/jux/BBL/projects/prsConnectivity/'


## import demographcis ##
prs_demo <- read.csv(paste0(local_wkdir,'phenotype/EuropeanAmerican/demographics/n9498_demographics_go1_20161212_EA.csv'))
img_demo <- read.csv(paste0(remote_study_wkdir,'n9498_dataFreeze/demographics/n9498_demographics_go1_20161212.csv'))


## import cnb ##
prs_cnb <- read.csv(paste0(local_wkdir,'phenotype/EuropeanAmerican/cnb/n9498_cnb_zscores_fr_20170202_EA.csv'))
img_cnb <- read.csv(paste0(remote_study_wkdir,'n9498_dataFreeze/cnb/n9498_cnb_zscores_fr_20170202.csv'))

## merge cnb and demo ##
prs_cnb_demo <- merge(prs_demo,prs_cnb, by = 'bblid')
img_cnb_demo <- merge(img_demo,img_cnb, by = 'bblid')


prs_img_matched <- merge(prs_cnb_demo,img_cnb_demo, by = c(colnames(prs_cnb)[2:29],colnames(prs_demo)[2:11]))
dim(prs_img_matched)

## test if merged matches ##
prs_cnb_factor <- read.csv(paste0(local_wkdir,'phenotype/EuropeanAmerican/cnb/n9498_cnb_factor_scores_fr_20170202_EA.csv'))
img_cnb_factor <- read.csv(paste0(remote_study_wkdir,'n9498_dataFreeze/cnb/n9498_cnb_factor_scores_fr_20170202.csv'))

prs_cnb_factor_match <- merge(prs_cnb_factor, prs_img_matched, by.x = 'bblid', by.y = 'bblid.x')
img_cnb_factor_match <- merge(img_cnb_factor, prs_img_matched, by.x = 'bblid', by.y = 'bblid.y')

prs_img_cnb_factor_match <- merge(prs_cnb_factor_match,img_cnb_factor_match, by.x = 'bblid', by.y = 'bblid.x')
plot(prs_img_cnb_factor_match$NAR_Overall_Accuracy.x,prs_img_cnb_factor_match$NAR_Overall_Accuracy.y)
###### !!!! match is successful !!!! ######

prs_img_match_key <- data.frame(org_bblid = prs_img_matched$bblid.y, new_bblid = prs_img_matched$bblid.x)
prs <- read.csv(paste0(local_wkdir,'genotype/pnc_EA_prs_20170501.csv'))
chip <- read.csv(paste0(local_wkdir,'genotype/converted_chipinfo_EA.csv'))
prs_chip <- merge(prs,chip,by='bblid')

## Construct healthy Sample ##
sample_demo <- read.csv(paste0(remote_study_wkdir,'n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv'))
# apply subject-level exclusion
hx_qa <- read.csv(paste0(remote_study_wkdir,"n1601_dataFreeze/health/n1601_health_20161214.csv"))
sample_hx <- merge(sample_demo,hx_qa)
sample_qa <- subset(sample_hx, healthExcludev2 == 0)

# apply strc exclusion
t1_qa <- read.csv(paste0(remote_study_wkdir,"n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv"))
sample_t1 <- merge(sample_qa, t1_qa)
sample_qa <- subset(sample_t1, t1Exclude == 0)

## Construct healthy Sample ##

# load modality exclusion file from the data-freeze
fc_qa <- read.csv(paste0(remote_study_wkdir,"n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv"))
sample_fc <- merge(sample_qa,fc_qa)
fc_sample_qa <- subset(sample_fc, restExclude ==0)  


## Construct SC Sample ##

# load modality exclusion file from the data-freeze
sc_qa <- read.csv(paste0(remote_study_wkdir,"n1601_dataFreeze/neuroimaging/dti/n1601_dti_qa_20170301.csv"))
sample_sc <- merge(sample_qa,sc_qa)
sc_sample_qa <- subset(sample_sc, dti64Exclude ==0)

## Construct FSC sample ##

fc_sc_sample_qa <- merge(fc_sample_qa,sc_sample_qa)

## add new bblid ##
fc_sc_sample_qa_newkey <- merge(fc_sc_sample_qa,prs_img_match_key, by.x = 'bblid', by.y='org_bblid' )
fc_sample_qa_newkey <- merge(fc_sample_qa,prs_img_match_key, by.x = 'bblid', by.y='org_bblid' )
sc_sample_qa_newkey <- merge(sc_sample_qa,prs_img_match_key, by.x = 'bblid', by.y='org_bblid' )

fc_sc_sample_qa_newkey <- merge(fc_sc_sample_qa_newkey,prs_chip, by.x = 'new_bblid',by.y = 'bblid')
fc_sample_qa_newkey <- merge(fc_sample_qa_newkey,prs_chip, by.x = 'new_bblid',by.y = 'bblid')
sc_sample_qa_newkey <- merge(sc_sample_qa_newkey,prs_chip, by.x = 'new_bblid',by.y = 'bblid')

save(fc_sc_sample_qa_newkey,fc_sample_qa_newkey,sc_sample_qa_newkey, file = paste0(remote_project_wkdir,'/result/sample_qa_newkey.RData'))
