#!/bin/bash
#
###########
today=$(date +"%Y%m%d_%s")
#metric=pearson  # pearson is standard 
# formula='"age + sex + etc"'  no demeaning
#formula='Hydra_k3+age_in_years+sex'
formula='Hydra_k3'
factors=""  #defines which ones you want an MDMR statistical map out for-- limit yourself! requires permutations for each (i.e., just MAP)
formula2=$(echo $formula | cut -d"\"" -f2)
datadir=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm
outdir=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm
covariates=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists/subj_list_concat_weighted_n312_with_headers.csv  #this is a path to a .csv with your subject level data, which includes col headers w/ variables for formula specified above
###############
scriptdir=/data/joy/BBL/applications/Rlibraries/connectir/connectir/scripts
#data/joy/BBL/applications/R-3.2.5/bin
modelname=$(echo $formula2 | sed "s/+/_/g" | sed "s/\*/x/g" | sed "s/ //g" )
inputdir=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm/weighted_n312/n312_pearson


echo $modelname
n=$(echo "$(cat $covariates | wc -l) - 1" | bc)
echo "$n subjects"
echo "model file: $covariates"
#nnew=$n  #for backwards-compatability

#modeldir=n${n} # modelname has number of subjects in the csv used. If any don't have functional images then they won't be in the analysis
# files inside the model folder are named with the n used for analysis.
#echo $modeldir

model=$covariates

rm -rf $modelname

### RUN CONNECTIR_MDMR ###
##########################


Rscript $scriptdir/connectir_mdmr.R \
	--indir=$inputdir \
	--formula="$formula" \
	--model=$model \
        --save-perms \
	--factors2perm=$factors \
	-c 4 -t 4 \
	 $modelname 

#        --subdist=$outdir/$modeldir/n${nnew}_$metric/subdist.desc \
