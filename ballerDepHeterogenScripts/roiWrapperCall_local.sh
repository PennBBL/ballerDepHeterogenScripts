###################################################################################################
##########################           GRMPY - Run ROI Wrapper             ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 11/03/2017                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
<<USE

This wrapper was orginally created by Angel Garcia and modifed to run on the GRMPY dataset. After inputing
the required files and formulas, then the script will call the gamROI wrapper that computes the analysis.

USE
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

###############################################
##### Define the RDS File and Output Path #####
###############################################

subjDataName="/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/voxelwrapper_20180810/subset_with_T1_NbackFC_and_dem_with_clusters.rds"
OutDirRoot="/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/voxelwrapper_20180824/roiWrapper"  

###################################################################################################
##### Define the name of the Variable that Specific which Subjects to Exclude in the RDS File #####
#####         Subjects To Include are Coded as "1" and the Ones to Exclude are "0"            #####
###################################################################################################

inclusionName="include" #I made this column especially for this analysis, but commented out the line that subsets on it in the actual R script because I account for all of these things when constructing my list. I put something because the command needs it to run

subjID="bblid,scanid"

#######################################################################################
##### Define the Formula to Run, including the amount of knots and the Predictors #####
#####   Do Not Include Method as it will be Defined in gamROI.R (method="REML")   #####    
#######################################################################################

covsFormula="~Hydra_k3+s(age_in_years)+sex+nbackRelMeanRMSMotion"

pAdjustMethod="fdr"
#pAdjustMethod="bonferroni"
ncores=5

residualMap=FALSE

######################################################################
##### If doing Repeated Measures, Include the Following Argument #####
######################################################################

#randomFormula="~(1|bblid)"

# Include in R script Call: -e $randomFormula

########################################################################
##### Define Paths of the Neuroimaging CSV's that will be Analyzed #####
#####  If a varaible is missing more than half the data, then you  #####
#####   will encounter errors, use reduceGMD.R to remove "NA's"    #####
########################################################################

input=/Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/voxelwrapper_20180810/BBLID_SCANID_schaefer_parcellations_n368.csv

########################################################################
##### Calls the " gamROI.R" Rscript that will Perform the Analysis #####
########################################################################

Rscript /Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/voxelwrapper_20180810/gamROI_nback_local.R -c $subjDataName -o $OutDirRoot -p ${input} -i $inclusionName -u $subjID -f $covsFormula -a $pAdjustMethod -n 5 -r $residualMap


###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
