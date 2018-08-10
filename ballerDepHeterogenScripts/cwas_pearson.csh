#!/bin/csh
###########

set nonomatch

#today=$(date +"%Y%m%d")
set metric = pearson  # pearson is standard 
set default_subj_list_dir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists"
set default_outdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"
set scriptdir = "/data/joy/BBL/applications/R-3.2.5/bin"
set templatedir = "/data/joy/BBL/studies/pnc/template/"
set brainmask = "$templatedir/priors/prior_mask_4mm.nii.gz"
set background = "$templatedir/pnc_template_brain_4mm.nii.gz"

echo "num elements on command line" $#argv
echo $scriptdir $templatedir $brainmask $background

if ($#argv < 4) then
	echo "Welcome to a script that does pearson correlation on your 4mm rest/nback/idemo concatenated files, outputed from combine_rest_nback_idemo_volumes.sh."
	echo "Please enter 4 (or 5) pieces of information on the command line: <Option1> <Option2> <imagelist> <OUTPUT DIR> <Covariates, optional>"
	echo "Option1: 1 if you'd like to use default paths, 2 if you'd like to use your own paths"
	echo "Option2: 1 if you'd like to use covariates, 2 if not (input NONE) >" 
#don't actually use covariates file in this analysis, but can move it into appropriate directories if you want
	echo "default subject list dir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists"
	echo "default outdir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"
	echo "check /home/eballer for the logfile"
else
	echo "in here"

	if ($1 == 1) then #set imagelist and outdir
		set imagelist = $default_subj_list_dir/$3
		set outdir = $default_outdir/$4	
	else
		set imagelist = $3
		set outdir = $4
	endif

	if (`ls -d $outdir | wc -l` == 0) then #make outdir if it does not exist
		echo "making directory $outdir"
		mkdir $outdir
	endif

	
	#datadir=/data/jux/BBL/projects/pncItcNetworks/subjectData
	#outdir=/data/jux/BBL/projects/pncItcNetworks/subjectData
	#imagelist=$datadir/imagelist_272.txt #this is a .txt w/ a list of paths to the 4d, 4mm timeseries in standard space-- MUST be in same order as csv
	#covariates=$datadir/demographics/n270_nback_rest_idemo.csv  #this is a path to a .csv with your subject level data, which includes col headers w/ variables for formula specified above

###############
	#n=$(echo "$(cat $imagelist | wc -l) - 1" | bc)
	set n = `cat $imagelist | wc -l`
	echo "$n subjects"

	
	
	echo "bg template: $background"
	echo "template mask: $brainmask"
	set nnew = $n  #for backwards-compatability
	#mkdir  outdir
	### MODEL NAME FOR DIRECTORY ###
	################################
	set modeldir = "n${n}" # modelname has number of subjects in the csv used. If any don't have functional images then they won't be in the analysis
	# files inside the model folder are named with the n used for analysis.
	echo "modeldir "$modeldir

	## PREPARE TO RUN ##
	####################
	echo ""
	echo "*********************"
	echo "preparing input files"
	echo "*********************"
	
	if (`ls -d $outdir/$modeldir/input_files | wc -l` == 0) then
		echo "......making directory input files"
		mkdir $outdir/$modeldir/input_files -p 2>/dev/null
	endif

	echo "......copying imagelist into input files directory"
	cp $imagelist $outdir/$modeldir/input_files/n${n}_image_list.txt

	if (($2 == 1) && ($#argv == 5)) then #set covariates file. Don't actually use it for this analysis
		set covariates = $5
		cp $covariates $outdir/$modeldir/input_files/n${n}_variables.csv
		echo "model file: $covariates"
	else
		echo "no covariate file listed"
	endif
	
	if (`ls -d $outdir/$modeldir/n${nnew}_$metric | wc -l` > 0) then
		echo ".... removing $outdir/$modeldir/n${nnew}_${metric}"
		rm -rf $outdir/$modeldir/n${nnew}_$metric
	endif
	
	echo ".... making $outdir/$modeldir/n${nnew}_${metric}"
	mkdir $outdir/$modeldir/n${nnew}_$metric

	### RUN CONNECTIR_SUBDIST ###
	#############################

	echo ""
	echo "*************************"
	echo "running connectir_subdist"
	echo "************************"
	echo "output directory is $outdir/$modeldir/n${nnew}_$metric"
	
		
	
	$scriptdir/Rscript $scriptdir/connectir_subdist.R $outdir/$modeldir/n${nnew}_$metric \
	--infuncs1=$outdir/$modeldir/input_files/n${nnew}_image_list.txt \
	--ztransform \
     	--automask1 \
	--brainmask1=$brainmask \
	--method="$metric" \
	--memlimit=30 \
    	--bg=$background \
	--overwrite

	echo "finished run"
         

endif

