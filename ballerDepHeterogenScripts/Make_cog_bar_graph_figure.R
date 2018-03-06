######################
#### READ IN DATA ####
######################
subjData_matched <- readRDS("/Users/eballer/BBL/from_chead/ballerDepHeterogen/data/subset_with_clusters_AG_matched.rds")

#########################################
#### MAKE CLUSTER VARIABLES ####
#########################################

##Calculate Sds, sds, percentage of females, percentage of White, and Ns for all clusters (start with Hydra_3)

#Cluster 1, TD(typically developing)
numTD<-length(subjData_matched$Hydra_k3[which(subj_data_matched$Hydra_k3==-1)]) 
ageTDMean<-mean(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==-1)])
ageTDSd<-sd(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==-1)])
femTD<-length(which(subjData_matched$sex==2 & subjData_matched$Hydra_k3==-1))/numTD
whiteTD<-(length(which(subjData_matched$race==1 & subjData_matched$Hydra_k3==-1)))/numTD
meduTDMean<-mean(subjData_matched$medu1[which(subjData_matched$Hydra_k3==-1)])
meduTDSd<-sd(subjData_matched$medu1[which(subjData_matched$Hydra_k3==-1)])

#Cluster 2, Depression
numCluster2<-length(subjData_matched$Hydra_k3[which(subj_data_matched$Hydra_k3==1)]) 
ageCluster2Mean<-mean(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==1)])
ageCluster2Sd<-sd(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==1)])
femCluster2<-length(which(subjData_matched$sex==2 & subjData_matched$Hydra_k3==1))/numCluster2
whiteCluster2<-(length(which(subjData_matched$race==1 & subjData_matched$Hydra_k3==1)))/numCluster2
meduCluster2Mean<-mean(subjData_matched$medu1[which(subjData_matched$Hydra_k3==1)])
meduCluster2Sd<-sd(subjData_matched$medu1[which(subjData_matched$Hydra_k3==1)])

#Cluster 3, Depression
numCluster3<-length(subjData_matched$Hydra_k3[which(subj_data_matched$Hydra_k3==2)]) 
ageCluster3Mean<-mean(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==2)])
ageCluster3Sd<-sd(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==2)])
femCluster3<-length(which(subjData_matched$sex==2 & subjData_matched$Hydra_k3==2))/numCluster3
whiteCluster3<-(length(which(subjData_matched$race==1 & subjData_matched$Hydra_k3==2)))/numCluster3
meduCluster3Mean<-mean(subjData_matched$medu1[which(subjData_matched$Hydra_k3==2)])
meduCluster3Sd<-sd(subjData_matched$medu1[which(subjData_matched$Hydra_k3==2)])

#Cluster 4, Depression
numCluster4<-length(subjData_matched$Hydra_k3[which(subj_data_matched$Hydra_k3==3)]) 
ageCluster4Mean<-mean(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==3)])
ageCluster4Sd<-sd(subjData_matched$age_in_years[which(subjData_matched$Hydra_k3==3)])
femCluster4<-length(which(subjData_matched$sex==2 & subjData_matched$Hydra_k3==3))/numCluster4
whiteCluster4<-(length(which(subjData_matched$race==1 & subjData_matched$Hydra_k3==3)))/numCluster4
meduCluster4Mean<-mean(subjData_matched$medu1[which(subjData_matched$Hydra_k3==3)])
meduCluster4Sd<-sd(subjData_matched$medu1[which(subjData_matched$Hydra_k3==3)])

###############################
#### TABLE 1: DEMOGRAPHICS ####
###############################
#Combine variables
bblid_and_cluster_names<-c("bblid","Hydra_k1","Hydra_k2","Hydra_k3","Hydra_k4")
clusterDf<-subjData_matched[,bblid_and_cluster_names]

numComb<-c(numTD,numCluster2,numCluster3,numCluster4)
ageMeanComb<-round(c(ageTDMean,ageCluster2Mean,ageCluster3Mean,ageCluster4Mean), 2)
ageSdComb<-round(c(ageTDSd,ageCluster2Sd,ageCluster3Sd,ageCluster4Sd), 2)

femComb<-c(femTD,femCluster2,femCluster3,femCluster4)
whiteComb<-c(whiteTD,whiteCluster2,whiteCluster3,whiteCluster4)

meduMeanComb<-round(c(meduTDMean,meduCluster2Mean,meduCluster3Mean,meduCluster4Mean), 2)
meduSdComb<-round(c(meduTDSd,meduCluster2Sd,meduCluster3Sd,meduCluster4Sd), 2)

#MAKE TABLE
cluster_namesFull<-c("Typically Developing","Cluster 1","Cluster 2","Cluster 3")
table1<-as.data.frame(matrix(nrow=4,ncol=7))
row.names(table1)<-cluster_namesFull
colnames(table1)[1]<-"N"
colnames(table1)[2]<-"Female (%)"
colnames(table1)[3]<-"Caucasian (%)"
colnames(table1)[4]<-"Mean Age"
colnames(table1)[5]<-"SD Age"
colnames(table1)[6]<-"Mean Maternal Education (Years)"
colnames(table1)[7]<-"SD Maternal Education"
table1[,1]<-numComb
table1[,2]<-femComb
table1[,3]<-whiteComb
table1[,4]<-ageMeanComb
table1[,5]<-ageSdComb
table1[,6]<-meduMeanComb
table1[,7]<-meduSdComb

#PRINT TABLE
print(table1)

#SAVE TABLE
write.csv(table1,"/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/table1_demo_matched.rds",row.names=TRUE,quote=FALSE)

####################################################
#### FIGURE 1: BIFACTORS BY SCREENING CATEGORY ####  THIS IS TONI's need to update past this
####################################################

#Create table with bifactor means and sds only for diagnoses with >20 subjects.
facTbl2<-as.data.frame(matrix(nrow=4,ncol=10))
colnames(facTbl2)[1]<-"OverallMean"
colnames(facTbl2)[2]<-"OverallSem"
colnames(facTbl2)[3]<-"TDMean"
colnames(facTbl2)[4]<-"TDSem"
colnames(facTbl2)[5]<-"Cluster2Mean"
colnames(facTbl2)[6]<-"Cluster2Sem"
colnames(facTbl2)[7]<-"Cluster3Mean"
colnames(facTbl2)[8]<-"Cluster3Sem"
colnames(facTbl2)[9]<-"Cluster4Mean"
colnames(facTbl2)[10]<-"Cluster4Sem"

#Name the rows
clusterNames<-c("TD","Cluster2","Cluster3","Cluster4")

row.names(facTbl2)<-clusterNames

#Calculate means and standard deviations
#for (i in 2:5){
  cluster<-bblid_and_cluster_names[i]
  print(cluster)
  y<-subjData_matched[,cluster]
  print(y)
 # facTbl2[i,1]<-mean(subjData$Bifactor_Overall_Psychopathology[which(y==1)],na.rm=TRUE)
#  facTbl2[i,2]<-sd(subjData$Bifactor_Overall_Psychopathology[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))
  
 # facTbl2[i,3]<-mean(subjData$Bifactor_Mood[which(y==1)],na.rm=TRUE)
#  facTbl2[i,4]<-sd(subjData$Bifactor_Mood[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))
  
#  facTbl2[i,5]<-mean(subjData$Bifactor_Psychosis[which(y==1)],na.rm=TRUE)
#  facTbl2[i,6]<-sd(subjData$Bifactor_Psychosis[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))
  
#  facTbl2[i,7]<-mean(subjData$Bifactor_Externalizing[which(y==1)],na.rm=TRUE)
#  facTbl2[i,8]<-sd(subjData$Bifactor_Externalizing[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))
  
#  facTbl2[i,9]<-mean(subjData$Bifactor_Fear[which(y==1)],na.rm=TRUE)
#  facTbl2[i,10]<-sd(subjData$Bifactor_Fear[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))
  
#}


#Reshape to long format
library(reshape2)
facTbl2$group<-as.factor(dxs2)
facTbl2Sem<-facTbl2[,c(2,4,6,8,10,11)]
facTbl2Mean<-facTbl2[,c(1,3,5,7,9,11)]
facTbl2MeanLong<-melt(facTbl2Mean,id.vars="group",variable.name="factor",value.name="meanScore")
facTbl2SemLong<-melt(facTbl2Sem,id.vars="group",variable.name="factor",value.name="semScore")
facTbl2Long<-facTbl2MeanLong
facTbl2Long$semScore<-facTbl2SemLong$semScore

#Remove empty factors
facTbl2Long$group<-factor(facTbl2Long$group)


#Plot w/ ggplot
library(ggplot2)
library(grid)

#Colors used: #329444 = green (OverallPsych), #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear)

Fig1b<-ggplot(facTbl2Long, aes(x=group, y=meanScore,fill=factor)) +
  ylab("Factor Score (z)") + xlab("") + ggtitle("Orthogonal Dimensions of Psychopathology by Screening Diagnosis") +
  geom_bar(stat="identity",position=position_dodge()) +
  scale_fill_manual(values=c("#329444","#325194","#943282","#B3141C","#F58311"), breaks=c("OverallMean","MoodMean","PsychosisMean","ExternalizingMean","PhobiasMean"),
                    labels=c("Overall Psychopathology", "Anxious-Misery", "Psychosis", "Behavioral", "Fear")) +
  theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + theme(legend.text = element_text(size = 10), legend.justification=c(0.5,0.5), legend.position=c(.9,.9)) +
  theme(plot.title = element_text(size = rel(1.5), vjust = 0, hjust = .5)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black")) +
  theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
  scale_x_discrete(breaks=c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td"), labels=c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD",
                                                                                                                   "Psychosis","PTSD","Separation Anx","Social Anx","Specific Phobia","TD")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))

#see colors used in plot (leave "data" specified as is)
Plot1b<-ggplot_build(Fig1b)$data

ggsave(file="/data/joy/BBL/projects/pncAslAcrossDisorder/TablesFigures/Figure1_BifactorsByDiag.png", width = 15, height = 5, units = "in", dpi = 300)
