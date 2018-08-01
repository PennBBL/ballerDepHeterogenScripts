#!/bin/bash
#This script will take a list of scanids to create imageslist required for 
#distance computation

default_subj_list_dir="/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists"

default_outdir="/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"

if (( "$#" < 4)); then
	echo "Welcome to a script that combines your actual .nii rest, nback, and idemo files."
	echo "Please enter 3 pieces of information on the command line: <Option> <BBLID file> <SCANID file> <OUTPUT DIR>"
	echo "Option: 1 if you'd like to use default paths, 2 if you'd like to use your own paths"
	echo "If using option 1, can put in relative paths, and put 1 for <OUTPUT DIR> option"
	echo "If using option 2, make sure you give FULL PATHS FOR EVERYTHING!!!"
	echo "default subject list dir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists Please enter the following:"
	echo "default outdir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"
	
else
	if (($1 == 1)); then
		bblid=$(cat $default_subj_list_dir/$2)
		scanid=$(cat $default_subj_list_dir/$3)
		outputdir=$(ls -d $default_outdir)
		echo "BBLID: $bblid" 
		echo "SCANID $scanid"
		echo "OUTDIR $outdir"		
	else
		bblid=$(cat $2)
		scanid=$(cat $3)
		outputdir=$(ls -d $4)
		echo "BBLID: $bblid" 
		echo "SCANID $scanid"
		echo "OUTDIR $outdir"
	fi


fi

