#!/bin/bash
#
###########
today=$(date +"%Y%m%d")
metric=pearson  # pearson is standard 


datadir=/data/jux/BBL/projects/pncItcNetworks/subjectData
outdir=/data/jux/BBL/projects/pncItcNetworks/subjectData
scriptdir=/data/joy/BBL/applications/R-3.2.5/bin
templatedir=/data/joy/BBL/studies/pnc/template/
brainmask=$templatedir/priors/prior_mask_4mm.nii.gz
background=$templatedir/pnc_template_brain_4mm.nii.gz
imagelist=$datadir/imagelist_272.txt #this is a .txt w/ a list of paths to the 4d, 4mm timeseries in standard space-- MUST be in same order as csv
covariates=$datadir/demographics/n270_nback_rest_idemo.csv  #this is a path to a .csv with your subject level data, which includes col headers w/ variables for formula specified above

###############
n=$(echo "$(cat $covariates | wc -l) - 1" | bc)
echo "$n subjects"
echo "model file: $covariates"
echo "bg template: $background"
echo "template mask: $brainmask"
nnew=$n  #for backwards-compatability
#mkdir  outdir
### MODEL NAME FOR DIRECTORY ###
################################
modeldir=n${n} # modelname has number of subjects in the csv used. If any don't have functional images then they won't be in the analysis
# files inside the model folder are named with the n used for analysis.
echo $modeldir

## PREPARE TO RUN ##
####################
echo ""
echo "*********************"
echo "preparing input files"
echo "*********************"
mkdir $outdir/$modeldir/input_files -p 2>/dev/null
cp $imagelist $outdir/$modeldir/input_files/n${n}_image_list.txt
cp $covariates $outdir/$modeldir/input_files/n${n}_variables.csv

rm -rf  $outdir/$modeldir/${nnew}_$metric

### RUN CONNECTIR_SUBDIST ###
#############################
if [ ! -e $outdir/$modeldir/${nnew}_$metric ]; then
echo ""
echo "*************************"
echo "running connectir_subdist"
echo "*************************"
echo "output directory is $outdir/$modeldir/n${nnew}_$metric"

#mkdir $outdir/$modeldir/n${nnew}_$metric

$scriptdir/Rscript $scriptdir/connectir_subdist.R \
        $outdir/$modeldir/n${nnew}_$metric \
	--infuncs1=$outdir/$modeldir/input_files/n${nnew}_image_list.txt \
	--ztransform \
        --automask1\
	--brainmask1=$brainmask\
	--method="$metric" \
	--memlimit=30 \
        --bg=$background
         
fi

#
