######################
#### READ IN DATA ####
######################

subjData<-readRDS("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/n1042_JLF_aslVox11andUp_subjData_clusters.rds")

#########################################
#### MAKE DIAGNOSIS FACTOR VARIABLES ####
#########################################

##Calculate means, sds, percentage of females, percentage of White, and Ns for all diagnoses. 

#ADHD
numAdd<-sum(subjData$Add, na.rm=TRUE)
ageAddMean<-mean(subjData$age[which(subjData$Add==1)],na.rm=T)
ageAddSd<-sd(subjData$age[which(subjData$Add==1)],na.rm=T)
femAdd<-length(which(subjData$sex=="female" & subjData$Add==1))/numAdd
whiteAdd<-(length(which(subjData$race==1 & subjData$Add==1)))/numAdd
meduAddMean<-mean(subjData$meduCnbGo1[which(subjData$Add==1)],na.rm=T)
meduAddSd<-sd(subjData$meduCnbGo1[which(subjData$Add==1)],na.rm=T)

#Agoraphobia
numAgr<-sum(subjData$Agr, na.rm=TRUE)
ageAgrMean<-mean(subjData$age[which(subjData$Agr==1)],na.rm=T)
ageAgrSd<-sd(subjData$age[which(subjData$Agr==1)],na.rm=T)
femAgr<-length(which(subjData$sex=="female" & subjData$Agr==1))/numAgr
whiteAgr<-(length(which(subjData$race==1 & subjData$Agr==1)))/numAgr
meduAgrMean<-mean(subjData$meduCnbGo1[which(subjData$Agr==1)],na.rm=T)
meduAgrSd<-sd(subjData$meduCnbGo1[which(subjData$Agr==1)],na.rm=T)

#Anorexia
numAno<-sum(subjData$Ano, na.rm=TRUE)
ageAnoMean<-mean(subjData$age[which(subjData$Ano==1)],na.rm=T)
ageAnoSd<-sd(subjData$age[which(subjData$Ano==1)],na.rm=T)
femAno<-length(which(subjData$sex=="female" & subjData$Ano==1))/numAno
whiteAno<-(length(which(subjData$race==1 & subjData$Ano==1)))/numAno
meduAnoMean<-mean(subjData$meduCnbGo1[which(subjData$Ano==1)],na.rm=T)
meduAnoSd<-sd(subjData$meduCnbGo1[which(subjData$Ano==1)],na.rm=T)

#Bulimia
numBul<-sum(subjData$Bul, na.rm=TRUE)
ageBulMean<-mean(subjData$age[which(subjData$Bul==1)],na.rm=T)
ageBulSd<-sd(subjData$age[which(subjData$Bul==1)],na.rm=T)
femBul<-length(which(subjData$sex=="female" & subjData$Bul==1))/numBul
whiteBul<-(length(which(subjData$race==1 & subjData$Bul==1)))/numBul
meduBulMean<-mean(subjData$meduCnbGo1[which(subjData$Bul==1)],na.rm=T)
meduBulSd<-sd(subjData$meduCnbGo1[which(subjData$Bul==1)],na.rm=T)

#Conduct Disorder
numCon<-sum(subjData$Con, na.rm=TRUE)
ageConMean<-mean(subjData$age[which(subjData$Con==1)],na.rm=T)
ageConSd<-sd(subjData$age[which(subjData$Con==1)],na.rm=T)
femCon<-length(which(subjData$sex=="female" & subjData$Con==1))/numCon
whiteCon<-(length(which(subjData$race==1 & subjData$Con==1)))/numCon
meduConMean<-mean(subjData$meduCnbGo1[which(subjData$Con==1)],na.rm=T)
meduConSd<-sd(subjData$meduCnbGo1[which(subjData$Con==1)],na.rm=T)

#Generalized Anxiety Disorder
numGad<-sum(subjData$Gad, na.rm=TRUE)
ageGadMean<-mean(subjData$age[which(subjData$Gad==1)],na.rm=T)
ageGadSd<-sd(subjData$age[which(subjData$Gad==1)],na.rm=T)
femGad<-length(which(subjData$sex=="female" & subjData$Gad==1))/numGad
whiteGad<-(length(which(subjData$race==1 & subjData$Gad==1)))/numGad
meduGadMean<-mean(subjData$meduCnbGo1[which(subjData$Gad==1)],na.rm=T)
meduGadSd<-sd(subjData$meduCnbGo1[which(subjData$Gad==1)],na.rm=T)

#Major Depressive Disorder
numMdd<-sum(subjData$Mdd, na.rm=TRUE)
ageMddMean<-mean(subjData$age[which(subjData$Mdd==1)],na.rm=T)
ageMddSd<-sd(subjData$age[which(subjData$Mdd==1)],na.rm=T)
femMdd<-length(which(subjData$sex=="female" & subjData$Mdd==1))/numMdd
whiteMdd<-(length(which(subjData$race==1 & subjData$Mdd==1)))/numMdd
meduMddMean<-mean(subjData$meduCnbGo1[which(subjData$Mdd==1)],na.rm=T)
meduMddSd<-sd(subjData$meduCnbGo1[which(subjData$Mdd==1)],na.rm=T)

#Mania
numMan<-sum(subjData$Man, na.rm=TRUE)
ageManMean<-mean(subjData$age[which(subjData$Man==1)],na.rm=T)
ageManSd<-sd(subjData$age[which(subjData$Man==1)],na.rm=T)
femMan<-length(which(subjData$sex=="female" & subjData$Man==1))/numMan
whiteMan<-(length(which(subjData$race==1 & subjData$Man==1)))/numMan
meduManMean<-mean(subjData$meduCnbGo1[which(subjData$Man==1)],na.rm=T)
meduManSd<-sd(subjData$meduCnbGo1[which(subjData$Man==1)],na.rm=T)

#OCD
numOcd<-sum(subjData$Ocd, na.rm=TRUE)
ageOcdMean<-mean(subjData$age[which(subjData$Ocd==1)],na.rm=T)
ageOcdSd<-sd(subjData$age[which(subjData$Ocd==1)],na.rm=T)
femOcd<-length(which(subjData$sex=="female" & subjData$Ocd==1))/numOcd
whiteOcd<-(length(which(subjData$race==1 & subjData$Ocd==1)))/numOcd
meduOcdMean<-mean(subjData$meduCnbGo1[which(subjData$Ocd==1)],na.rm=T)
meduOcdSd<-sd(subjData$meduCnbGo1[which(subjData$Ocd==1)],na.rm=T)

#Oppositional Defiant Disorder
numOdd<-sum(subjData$Odd, na.rm=TRUE)
ageOddMean<-mean(subjData$age[which(subjData$Odd==1)],na.rm=T)
ageOddSd<-sd(subjData$age[which(subjData$Odd==1)],na.rm=T)
femOdd<-length(which(subjData$sex=="female" & subjData$Odd==1))/numOdd
whiteOdd<-(length(which(subjData$race==1 & subjData$Odd==1)))/numOdd
meduOddMean<-mean(subjData$meduCnbGo1[which(subjData$Odd==1)],na.rm=T)
meduOddSd<-sd(subjData$meduCnbGo1[which(subjData$Odd==1)],na.rm=T)

#Panic Disorder
numPan<-sum(subjData$Pan, na.rm=TRUE)
agePanMean<-mean(subjData$age[which(subjData$Pan==1)],na.rm=T)
agePanSd<-sd(subjData$age[which(subjData$Pan==1)],na.rm=T)
femPan<-length(which(subjData$sex=="female" & subjData$Pan==1))/numPan
whitePan<-(length(which(subjData$race==1 & subjData$Pan==1)))/numPan
meduPanMean<-mean(subjData$meduCnbGo1[which(subjData$Pan==1)],na.rm=T)
meduPanSd<-sd(subjData$meduCnbGo1[which(subjData$Pan==1)],na.rm=T)

#Psychosis
numPs<-sum(subjData$Ps, na.rm=TRUE)
agePsMean<-mean(subjData$age[which(subjData$Ps==1)],na.rm=T)
agePsSd<-sd(subjData$age[which(subjData$Ps==1)],na.rm=T)
femPs<-length(which(subjData$sex=="female" & subjData$Ps==1))/numPs
whitePs<-(length(which(subjData$race==1 & subjData$Ps==1)))/numPs
meduPsMean<-mean(subjData$meduCnbGo1[which(subjData$Ps==1)],na.rm=T)
meduPsSd<-sd(subjData$meduCnbGo1[which(subjData$Ps==1)],na.rm=T)

#Posttraumatic Stress Disorder
numPtd<-sum(subjData$Ptd, na.rm=TRUE)
agePtdMean<-mean(subjData$age[which(subjData$Ptd==1)],na.rm=T)
agePtdSd<-sd(subjData$age[which(subjData$Ptd==1)],na.rm=T)
femPtd<-length(which(subjData$sex=="female" & subjData$Ptd==1))/numPtd
whitePtd<-(length(which(subjData$race==1 & subjData$Ptd==1)))/numPtd
meduPtdMean<-mean(subjData$meduCnbGo1[which(subjData$Ptd==1)],na.rm=T)
meduPtdSd<-sd(subjData$meduCnbGo1[which(subjData$Ptd==1)],na.rm=T)

#Separation Anxiety Disorder
numSep<-sum(subjData$Sep, na.rm=TRUE)
ageSepMean<-mean(subjData$age[which(subjData$Sep==1)],na.rm=T)
ageSepSd<-sd(subjData$age[which(subjData$Sep==1)],na.rm=T)
femSep<-length(which(subjData$sex=="female" & subjData$Sep==1))/numSep
whiteSep<-(length(which(subjData$race==1 & subjData$Sep==1)))/numSep
meduSepMean<-mean(subjData$meduCnbGo1[which(subjData$Sep==1)],na.rm=T)
meduSepSd<-sd(subjData$meduCnbGo1[which(subjData$Sep==1)],na.rm=T)

#Social Anxiety Disorder
numSoc<-sum(subjData$Soc, na.rm=TRUE)
ageSocMean<-mean(subjData$age[which(subjData$Soc==1)],na.rm=T)
ageSocSd<-sd(subjData$age[which(subjData$Soc==1)],na.rm=T)
femSoc<-length(which(subjData$sex=="female" & subjData$Soc==1))/numSoc
whiteSoc<-(length(which(subjData$race==1 & subjData$Soc==1)))/numSoc
meduSocMean<-mean(subjData$meduCnbGo1[which(subjData$Soc==1)],na.rm=T)
meduSocSd<-sd(subjData$meduCnbGo1[which(subjData$Soc==1)],na.rm=T)

#Specific Phobia
numSph<-sum(subjData$Sph, na.rm=TRUE)
ageSphMean<-mean(subjData$age[which(subjData$Sph==1)],na.rm=T)
ageSphSd<-sd(subjData$age[which(subjData$Sph==1)],na.rm=T)
femSph<-length(which(subjData$sex=="female" & subjData$Sph==1))/numSph
whiteSph<-(length(which(subjData$race==1 & subjData$Sph==1)))/numSph
meduSphMean<-mean(subjData$meduCnbGo1[which(subjData$Sph==1)],na.rm=T)
meduSphSd<-sd(subjData$meduCnbGo1[which(subjData$Sph==1)],na.rm=T)

#Typically Developing
numTd<-sum(subjData$Td, na.rm=TRUE)
ageTdMean<-mean(subjData$age[which(subjData$Td==1)],na.rm=T)
ageTdSd<-sd(subjData$age[which(subjData$Td==1)],na.rm=T)
femTd<-length(which(subjData$sex=="female" & subjData$Td==1))/numTd
whiteTd<-(length(which(subjData$race==1 & subjData$Td==1)))/numTd
meduTdMean<-mean(subjData$meduCnbGo1[which(subjData$Td==1)],na.rm=T)
meduTdSd<-sd(subjData$meduCnbGo1[which(subjData$Td==1)],na.rm=T)



###############################
#### TABLE 1: DEMOGRAPHICS ####
###############################

#Combine variables
dxNames<-c("bblid","Td","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf<-subjData[,dxNames]

numComb<-c(numTd,numAdd,numAgr,numAno,numBul,numCon,numGad,numMdd,numMan,numOcd,numOdd,numPan,numPs,numPtd,numSep,numSoc,numSph)

ageMeanComb<-round(c(ageTdMean,ageAddMean,ageAgrMean,ageAnoMean,ageBulMean,ageConMean,ageGadMean,ageMddMean,ageManMean,ageOcdMean,ageOddMean,agePanMean,agePsMean,agePtdMean,ageSepMean,ageSocMean,ageSphMean),2)

ageSdComb<-round(c(ageTdSd,ageAddSd,ageAgrSd,ageAnoSd,ageBulSd,ageConSd,ageGadSd,ageMddSd,ageManSd,ageOcdSd,ageOddSd,agePanSd,agePsSd,agePtdSd,ageSepSd,ageSocSd,ageSphSd),2)

femComb<-round(c(femTd,femAdd,femAgr,femAno,femBul,femCon,femGad,femMdd,femMan,femOcd,femOdd,femPan,femPs,femPtd,femSep,femSoc,femSph),3)*100

whiteComb<-round(c(whiteTd,whiteAdd,whiteAgr,whiteAno,whiteBul,whiteCon,whiteGad,whiteMdd,whiteMan,whiteOcd,whiteOdd,whitePan,whitePs,whitePtd,whiteSep,whiteSoc,whiteSph),3)*100

meduMeanComb<-round(c(meduTdMean,meduAddMean,meduAgrMean,meduAnoMean,meduBulMean,meduConMean,meduGadMean,meduMddMean,meduManMean,meduOcdMean,meduOddMean,meduPanMean,meduPsMean,meduPtdMean,meduSepMean,meduSocMean,meduSphMean),2)

meduSdComb<-round(c(meduTdSd,meduAddSd,meduAgrSd,meduAnoSd,meduBulSd,meduConSd,meduGadSd,meduMddSd,meduManSd,meduOcdSd,meduOddSd,meduPanSd,meduPsSd,meduPtdSd,meduSepSd,meduSocSd,meduSphSd),2)

#MAKE TABLE
dxNamesFull<-c("Typically Developing","ADHD","Agoraphobia","Anorexia","Bulimia","Conduct Disorder","Generalized Anxiety Disorder","Major Depression","Mania","Obsessive-Compulsive Disorder","Oppositional Defiant Disorder","Panic","Psychosis-spectrum","PTSD","Separation Anxiety","Social Phobia","Specific Phobia")
table1<-as.data.frame(matrix(nrow=17,ncol=7))
row.names(table1)<-dxNamesFull
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

#SAVE TABLE
write.csv(table1,"/data/joy/BBL/projects/pncAslAcrossDisorder/TablesFigures/Table1_Demographics.csv",row.names=TRUE,quote=FALSE)


 
####################################################
#### FIGURE 1: BIFACTORS BY SCREENING CATEGORY ####
####################################################

#Create table with bifactor means and sds only for diagnoses with >20 subjects.
facTbl2<-as.data.frame(matrix(nrow=13,ncol=10))
colnames(facTbl2)[1]<-"OverallMean"
colnames(facTbl2)[2]<-"OverallSem"
colnames(facTbl2)[3]<-"MoodMean"
colnames(facTbl2)[4]<-"MoodSem"
colnames(facTbl2)[5]<-"PsychosisMean"
colnames(facTbl2)[6]<-"PsychosisSem"
colnames(facTbl2)[7]<-"ExternalizingMean"
colnames(facTbl2)[8]<-"ExternalizingSem"
colnames(facTbl2)[9]<-"PhobiasMean"
colnames(facTbl2)[10]<-"PhobiasSem"

#Name the rows
dxNamesShort2<-c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD","Psychosis","PTSD","Separation Anxiety","Social Phobia","Specific Phobia","TD")

row.names(facTbl2)<-dxNamesShort2

dxs2<-c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td")

#Calculate means and standard deviations
for (i in 1:13){
        dx<-dxs2[i]
        print(dx)
        y<-subjData[,dx]

        facTbl2[i,1]<-mean(subjData$Bifactor_Overall_Psychopathology[which(y==1)],na.rm=TRUE)
        facTbl2[i,2]<-sd(subjData$Bifactor_Overall_Psychopathology[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl2[i,3]<-mean(subjData$Bifactor_Mood[which(y==1)],na.rm=TRUE)
        facTbl2[i,4]<-sd(subjData$Bifactor_Mood[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl2[i,5]<-mean(subjData$Bifactor_Psychosis[which(y==1)],na.rm=TRUE)
        facTbl2[i,6]<-sd(subjData$Bifactor_Psychosis[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl2[i,7]<-mean(subjData$Bifactor_Externalizing[which(y==1)],na.rm=TRUE)
        facTbl2[i,8]<-sd(subjData$Bifactor_Externalizing[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl2[i,9]<-mean(subjData$Bifactor_Fear[which(y==1)],na.rm=TRUE)
        facTbl2[i,10]<-sd(subjData$Bifactor_Fear[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

}


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


##helpful ggplot guides:
#ylab- y label title
#xlab- x label title
#ggtitle- plot title
#geom_bar - make bar plot
#geom_errorbar - create error-bars on plot
#scale_fill_manual- manually change the colors and names of the legend labels
#Within theme(axis.title.y = element_text()) or theme(axis.title.x = element_text()) or theme(plot.title = element_text()) you can change:
        #size = size of axis title
        #angle = orientation of text
        #vjust = vertical adjustment of text from axes
        #hjust = horizontal adjustment of text from axes
        #color = change color of text
#Above also works for axis tick labels using theme(axis.text.y=element_text()) or theme(axis.text.x=element_text())
#guides(fill=guide_legend(title=NULL)) - remove legend title
#scale_x_discrete(breaks=c("",""), labels=c("","")) - take the pre-existing axis labels and give them new labels
#theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) - change the margins of the plot to add a little extra room around the entire figure. Note: this requires the function "unit" from the package "grid" to work. unit(c(top,right,bottom,left))
#geom_bar(size=0.3) = changes the thickness of the black line around each bar (1 = thicker, .03 = thinner)
#geom_bar(color="black") = changes the color of the black line around each bar (remove this command to remove the black outlines).
#legend.justification = defines which side of the legend that the legend.position coordinates refer to (can use left, right, centre or numeric value (x = between 0 and 1))
#legend.position = move the legend (accepts left, right, top, bottom or define relative coordinates on plot c(x, y) between 0 and 1)



