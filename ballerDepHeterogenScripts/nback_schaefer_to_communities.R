source('/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181023_schaefer400_and_fc_communities/fc_and_mdmr_functions_NEW_NBACK.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'

# set defaults for looping purposes

communities <- c(7, 17)
schaefer_resolution <- c(100, 200, 400)

#Read CSVs with Schaefer nod
df_r100 <-data.frame(read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/n1601schaefer100.csv"))
df_r200 <-data.frame(read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/n1601schaefer200.csv"))
df_r400 <-data.frame(read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/n1601schaefer400.csv"))

gsub(pattern = "NA", replacement = "0", x = df_r100)

#make new df only with contrast 4
df_r100_con4 <- df_r100[,grep("contrast4", names(df_r100))]
df_r200_con4 <- df_r200[,grep("contrast4", names(df_r200))]
df_r400_con4 <- df_r400[,grep("contrast4", names(df_r400))]

#Obtain community affilitations for schaefer nodes- Don't actually think I need this

# Make matrices for each community
nback_r100_com7_matrix_list <- make_nback_community_matrix(df_r100_con4, 100, 7) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)
nback_r200_com7_matrix_list <- make_nback_community_matrix(df_r200_con4, 200, 7) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)
nback_r400_com7_matrix_list <- make_nback_community_matrix(df_r400_con4, 400, 7) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)

nback_r100_com17_matrix_list <- make_nback_community_matrix(df_r100_con4, 100, 17) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)
nback_r200_com17_matrix_list <- make_nback_community_matrix(df_r200_con4, 200, 17) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)
nback_r400_com17_matrix_list <- make_nback_community_matrix(df_r400_con4, 400, 17) #will return a matrix that has length 1601, 9 columsn (BBLID, SCANID, Comm 1-7)

#merge BBLID, SCANID, and nback matrix
nback_r100_com7_with_BBLID_and_SCANID_matrix <- cbind(df_r100[,2:3], nback_r100_com7_matrix_list$mean)
nback_r200_com7_with_BBLID_and_SCANID_matrix <- cbind(df_r200[,2:3], nback_r200_com7_matrix_list$mean)
nback_r400_com7_with_BBLID_and_SCANID_matrix <- cbind(df_r400[,2:3], nback_r400_com7_matrix_list$mean)

nback_r100_com17_with_BBLID_and_SCANID_matrix <- cbind(df_r100[,2:3], nback_r100_com17_matrix_list$mean)
nback_r200_com17_with_BBLID_and_SCANID_matrix <- cbind(df_r200[,2:3], nback_r200_com17_matrix_list$mean)
nback_r400_com17_with_BBLID_and_SCANID_matrix <- cbind(df_r400[,2:3], nback_r400_com17_matrix_list$mean)

#output to data directory for movement down to local directory
write.table(nback_r100_com7_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r100_com7_means.csv")
write.table(nback_r200_com7_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r200_com7_means.csv")
write.table(nback_r400_com7_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r400_com7_means.csv")
write.table(nback_r100_com17_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r100_com17_means.csv")
write.table(nback_r200_com17_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r200_com17_means.csv")
write.table(nback_r400_com17_with_BBLID_and_SCANID_matrix, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/nback_schaefer_means/n1601_r400_com17_means.csv")
