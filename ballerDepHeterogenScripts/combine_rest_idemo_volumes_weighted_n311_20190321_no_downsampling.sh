#This script will take a list of scanids to create imageslist required for 
#distance computation
 
#scanids 
datadir=$(ls -d /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo)
scanid=$(cat $datadir/subject_lists/rest_idemo_sample_SCANID.csv)
bblid=$(cat $datadir/subject_lists/rest_idemo_sample_BBLID.csv)

#These were all updated based on new processing/directory structures.  12/18/2018
restdir=$(ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/restVoxelwiseTimeseries)
idemodir=$(ls -d /data/joy/BBL/studies/pnc/processedData/idemo/idemoConnect_201707)
outputdir=$(ls -d $datadir/rest_idemo_concat_2mm)
images="subj_list_rest_idemo_IMAGELIST_n311_2mm.txt"

rm -rf $datadir/subject_lists/${images}
#data format in imagedir
#rm $datadir/imageinput1.txt # remove previous output if exit 


nsubj=$(echo $bblid | wc | awk '{print $2}')
	
	echo "$nsubj subjects"

c=0
for b  in $bblid;  do 
     #dd=$(ls -d $restdir/${b}/*/*.nii.gz)
  
    
     echo "$c out of $nsubj"
     set -- $scanid
     arr=($scanid)

     s=${arr[$c]}
     echo "$b/$s" 	
     echo "find the files: rest and idemo"
     echo "    "
     img1=$(ls -d $restdir/${s}_*.nii.gz) #rest file 
	echo "$img1"
     img2=$(ls -d $idemodir/${b}/*x${s}/${b}_*x${s}.nii.gz) #idemo file 
	echo "$img2"

     echo "    "
     echo "split the file to the temporary location"

     fslsplit $img1 $outputdir/vol1
     fslsplit $img2 $outputdir/vol2
     
     echo "    "
     echo "merge all the volumes"
     
     fslmerge -t  $outputdir/${s}_rest_idemo.nii.gz $outputdir/vol1* $outputdir/vol2*
      
     echo "    "
     echo "remove all the 2D files to free space"

     rm -rf $outputdir/vol1* $outputdir/vol2*

     echo "    "
    
     rm -rf  $outputdir/${s}_rest_nback_idemo.nii.gz 

     imagelist=$(ls -d $outputdir/${s}_rest_idemo.nii.gz)

     c=$(($c+1));

     echo "$imagelist" >> $datadir/subject_lists/${images}

done
	
exit 
