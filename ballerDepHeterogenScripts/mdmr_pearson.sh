#!/bin/bash
#
###########
today=$(date +"%Y%m%d_%s")
#metric=pearson  # pearson is standard 
# formula='"age + sex + etc"'  no demeaning
formula='logk'
factors="logk"  #defines which ones you want an MDMR statistical map out for-- limit yourself! requires permutations for each (i.e., just MAP)
formula2=$(echo $formula | cut -d"\"" -f2)
datadir=/data/jux/BBL/projects/pncItcNetworks/subjectData
outdir=/data/jux/BBL/projects/pncItcNetworks/subjectData
covariates=$datadir/demographics/n270_nback_rest_idemo.csv #this is a path to a .csv with your subject level data, which includes col headers w/ variables for formula specified above
###############
scriptdir=/data/joy/BBL/applications/R-3.2.5/bin
modelname=$(echo $formula2 | sed "s/+/_/g" | sed "s/\*/x/g" | sed "s/ //g" )
#modelname="$(echo $formula2 | sed "s/+/_/g" | sed "s/\*/x/g" | sed "s/ //g" )"
inputdir=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson


echo $modelname
n=$(echo "$(cat $covariates | wc -l) - 1" | bc)
echo "$n subjects"
echo "model file: $covariates"
#nnew=$n  #for backwards-compatability

#modeldir=n${n} # modelname has number of subjects in the csv used. If any don't have functional images then they won't be in the analysis
# files inside the model folder are named with the n used for analysis.
#echo $modeldir

model=$covariates

### RUN CONNECTIR_MDMR ###
##########################


$scriptdir/Rscript $scriptdir/connectir_mdmr.R \
	--indir=$inputdir \
	--formula="$formula" \
	--model=$model \
        --save-perms \
	--factors2perm=$factors \
	-c 4 -t 4 \
	 $modelname 

#        --subdist=$outdir/$modeldir/n${nnew}_$metric/subdist.desc \
