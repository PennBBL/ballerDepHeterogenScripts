#!/bin/bash
#This script will take a list of scanids to create imageslist required for 
#distance computation

default_subj_list_dir="/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists"

default_outdir="/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"

if (( "$#" < 4)); then
	echo "Welcome to a script that combines your actual .nii rest, nback, and idemo files."
	echo "Please enter 3 pieces of information on the command line: <Option> <BBLID file> <SCANID file> <OUTPUT DIR>"
	echo "Option: 1 if you'd like to use default paths, 2 if you'd like to use your own paths"
	echo "If using option 1, can put in relative paths"
	echo "If using option 2, make sure you give FULL PATHS FOR EVERYTHING!!!"
	echo "default subject list dir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/subject_lists Please enter the following:"
	echo "default outdir: /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm"
	echo "check /home/eballer for the logfile"
else
	if (($1 == 1)); then
		bblid=$(cat $default_subj_list_dir/$2)
		scanid=$(cat $default_subj_list_dir/$3)
		if ((`ls -d $default_outdir/$4 | wc -l` == 0)); then
			mkdir $defaut_outdir/$4
		else
			rm -f $default_outdir/$4/*
		fi
		
		outputdir=$(ls -d $default_outdir/$4)
		output_image_list=`echo $2 | perl -pe 's/(.*)_BBLID_(.*).csv$/$1_IMAGELIST_$2.txt/'`
		
	else
		bblid=$(cat $2)
		scanid=$(cat $3)
		if ((`ls -d $4 | wc -l` == 0)); then
			mkdir $4
		else
			rm -f $4/*
		fi
		outputdir=$(ls -d $4)
		output_image_list=`echo $2 | perl -pe 's/.*\/(.*)_BBLID_(.*).csv$/$1_IMAGELIST_$2.txt/'`
	fi

	#output image list is the name of the BBLID file, grabs the last part, removes BBLID and puts in image list, from PERL command above
	
	echo "Output text file of images: $default_subj_list_dir/$output_image_list"

	#remove textfile if it already exists
	if ((`ls $default_subj_list_dir/$output_image_list | wc -l` > 0)); then
		rm $default_subj_list_dir/$output_image_list
	fi 

	touch $default_subj_list_dir/$output_image_list

	#set imaging directories
	restdir=$(ls -d /data/joy/BBL/studies/pnc/processedData/restbold/restbold_201607151621)
	nbackdir=$(ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/nbackVoxelwiseTimeseries)
	idemodir=$(ls -d /data/joy/BBL/studies/pnc/processedData/idemo/idemoConnect_201707)

#from azeez's stuff below here
 	nsubj=$(echo $bblid | wc | awk '{print $2}')
	
	echo "$nsubj subjects"

	c=0
	for b  in $bblid;  do 
     		dd=$(ls -d $restdir/${b}/*/*.nii.gz)
     		echo "$dd"
    
   		echo "$c out of $nsubj"
     		set -- $scanid
     		arr=($scanid)
     		s=${arr[$c]}
     		echo "$s" 	
     		echo "find the files: rest, nback and idemo"
     		echo "    "
     		img1=$(ls -d $restdir/${b}/*${s}/${b}_*${s}.nii.gz) #rest file 
     		img2=$(ls -d  $nbackdir/${s}_taskRegress_ts36evDespike.nii.gz) #nback file 
     		img3=$(ls -d $idemodir/${b}/*x${s}/${b}_*x${s}.nii.gz) #idemo file 

    		echo "    "
    		echo "split the file to the temporary location"

    		fslsplit $img1 $outputdir/vol1
     		fslsplit $img2 $outputdir/vol2
     		fslsplit $img3 $outputdir/vol3
     
     		echo "    "
     		echo "merge all the volumes"
     
     		fslmerge -t  $outputdir/${s}_rest_nback_idemo.nii.gz $outputdir/vol1* $outputdir/vol2* $outputdir/vol3*
      
     		echo "    "
     		echo "remove all the 2D files to free space"

    		rm -rf $outputdir/vol1* $outputdir/vol2* $outputdir/vol3*

     		echo "    "
     		echo "downsample to 4mm and remove to 2mm "
     
     		3dresample -inset $outputdir/${s}_rest_nback_idemo.nii.gz -dxyz 4 4 4 -prefix  $outputdir/${s}_rest_nback_idemo_4mm.nii.gz

    		rm -rf  $outputdir/${s}_rest_nback_idemo.nii.gz 

 q    		imagelist=$(ls -d $outputdir/${s}_rest_nback_idemo_4mm.nii.gz)

     		c=$(($c+1));

     		echo "$imagelist" >> $default_subj_list_dir/$output_image_list
	done
fi	
exit 
