source('~/Google Drive/TDSlab/SCZ_gene_imaging/code/prs_functions.R')

sub_to_remove = which(fc_sample_qa_newkey_chip$chip == 'Human1M-Duov3_B' | fc_sample_qa_newkey_chip$chip == 'HumanHap550_v1')
fc_power_out<-prs_within_between_com_analysis(fc_power_fc_sample_net,fc_sample_qa_newkey_chip,'power',NA,6,sub_to_remove)
fc_s200_7_out<-prs_within_between_com_analysis(fc_s200_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',200,7,sub_to_remove)
fc_s200_17_out<-prs_within_between_com_analysis(fc_s200_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',200,17,sub_to_remove)
fc_s400_7_out<-prs_within_between_com_analysis(fc_s400_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',400,7,sub_to_remove)
fc_s400_17_out<-prs_within_between_com_analysis(fc_s400_fc_sample_net,fc_sample_qa_newkey_chip,'schaefer',400,17,sub_to_remove)
fc_gordon_out<-prs_within_between_com_analysis(fc_gordon_fc_sample_net,fc_sample_qa_newkey_chip,'gordon',NA,NA,sub_to_remove)

fc_sample_qa_newkey$scz_prs_z <- scale(fc_sample_qa_newkey$scz_prs)
ggplot(fc_sample_qa_newkey,aes(scz_prs_z)) + geom_histogram(fill = '#82B1FF')  +
  xlab('Schizophrenia Polygenic Score')
ggplot(fc_sample_qa_newkey,aes(ageAtScan1/12)) + geom_histogram(fill = '#FFD180')  +
  xlab('Age at Scan 1')

fc_sample_qa_newkey$sex <- as.factor(fc_sample_qa_newkey$sex)
ggplot(fc_sample_qa_newkey,aes(sex,fill = sex)) + geom_histogram(aes(y = ..count..), stat = "count",binwidth=5)
xlab('Sex')


par(mfrow=c(2,3))
for (chip in names(table(fc_sample_qa_newkey_chip$chip))[table(fc_sample_qa_newkey_chip$chip) >0]) {
  plot<-hist(fc_sample_qa_newkey_chip[which(fc_sample_qa_newkey_chip$chip == chip),'scz_prs_z'],main = chip, xlab= 'scz prs', breaks = 10)
  x_unit = (max(plot$breaks)-min(plot$breaks))/4
  text(x = min(plot$breaks)+ x_unit*0.5,y=max(plot$counts),labels = paste0('mean=',round(mean(fc_sample_qa_newkey_chip[which(fc_sample_qa_newkey_chip$chip == chip),'scz_prs_z']),3)))
  text(x = min(plot$breaks)+ x_unit*2,y=max(plot$counts),labels = paste0('sd=',round(sd(fc_sample_qa_newkey_chip[which(fc_sample_qa_newkey_chip$chip == chip),'scz_prs_z']),3)))
  text(x = min(plot$breaks)+ x_unit*3,y=max(plot$counts),labels = paste0('n=',length(fc_sample_qa_newkey_chip[which(fc_sample_qa_newkey_chip$chip == chip),'scz_prs_z'])))
  }


fc_power_out$prs_gams$data$scz_prs_z <- as.numeric(fc_power_out$prs_gams$data$scz_prs_z)
DMN_out <- gam(default_default ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip +
                 pc1 + pc2 + pc3 + pc4 + pc5 +
                 pc6 + pc7 + pc8 + pc9 + pc10, data = fc_power_out$prs_gams$data, method = 'REML')
plotdata <- visreg(DMN_out,'scz_prs_z',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "DMN", 
                       x=plotdata$res$scz_prs_z,
                       y=plotdata$res$visregRes)

power_DMN_prs
ggplot() +
  geom_point(data = predicts, aes(x, y), alpha= 1, colour = '#616161'  ) +
  geom_line(data = smooths, aes(x = x, y = smooth), size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = 'black', alpha = 0.75, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = 'black', alpha = 0.75, size = 0.9) +
  labs(x = "Schizophrenia Polygenic Score", y = "Within DMN Connectivty") 

library("abind")

all.matrix <- abind(sample_net, along=3)
all<-apply(all.matrix, c(1,2), mean)
keycol=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
clean = order(power_node$CommunityAffiliation[which(power_node$CommunityAffiliation > 0)])
diag(all) <- 0
ave_adj_plot <-levelplot(all[clean,clean], 
                         col.regions=rev(keycol),at = seq(0.8,-0.6,length.out = 9),xlab="",ylab = "")



fc_s200_17_out$prs_gams$data$scz_prs_z <- as.numeric(fc_s200_17_out$prs_gams$data$scz_prs_z)
dmnC_out_200 <- gam(defaultC_defaultC ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip +
                      pc1 + pc2 + pc3 + pc4 + pc5 +
                      pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnC_out,'scz_prs_z')
dmnA_out <- gam(defaultA_defaultA ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip  +
                  pc1 + pc2 + pc3 + pc4 + pc5 +
                  pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnA_out,'scz_prs_z')
dmnB_out <- gam(defaultB_defaultB ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip +
                  pc1 + pc2 + pc3 + pc4 + pc5 +
                  pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnB_out,'scz_prs_z')


plotdata <- visreg(dmnC_out,'scz_prs_z',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "DMN", 
                       x=plotdata$res$scz_prs_z,
                       y=plotdata$res$visregRes)

ggplot() +
  geom_point(data = predicts, aes(x, y), alpha= 1, colour = '#FFEE58'  ) +
  geom_line(data = smooths, aes(x = x, y = smooth), size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = 'black', alpha = 0.75, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = 'black', alpha = 0.75, size = 0.9) +
  labs(x = "Schizophrenia Polygenic Score", y = "Within DMN C Connectivty") 


fc_s400_17_out$prs_gams$data$scz_prs_z <- as.numeric(fc_s400_17_out$prs_gams$data$scz_prs_z)
dmnC_out <- gam(defaultC_defaultC ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip +
                  pc1 + pc2 + pc3 + pc4 + pc5 +
                  pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnC_out,'scz_prs_z')
dmnA_out <- gam(defaultA_defaultA ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip +
                  pc1 + pc2 + pc3 + pc4 + pc5 +
                  pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnA_out,'scz_prs_z')
dmnB_out <- gam(defaultB_defaultB ~ s(ageAtScan1) +  sex + scz_prs_z + restRelMeanRMSMotion + chip + 
                  pc1 + pc2 + pc3 + pc4 + pc5 +
                  pc6 + pc7 + pc8 + pc9 + pc10, data = fc_s200_17_out$prs_gams$data, method = 'REML')
visreg(dmnB_out,'scz_prs_z')


plotdata <- visreg(dmnA_out,'scz_prs_z',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "DMN", 
                       x=plotdata$res$scz_prs_z,
                       y=plotdata$res$visregRes)

ggplot() +
  geom_point(data = predicts, aes(x, y), alpha= 1, colour = '#F9A825'  ) +
  geom_line(data = smooths, aes(x = x, y = smooth), size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = 'black', alpha = 0.75, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = 'black', alpha = 0.75, size = 0.9) +
  labs(x = "Schizophrenia Polygenic Score", y = "Within DMN C Connectivty") 
