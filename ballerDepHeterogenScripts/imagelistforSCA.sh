#This script will take a list of scanids to create imageslist required for 
#distance computation
 
#scanids 
datadir=$(ls -d /data/jux/BBL/projects/pncItcNetworks/subjectData)
scanid=$(cat $datadir/demographics/n270scanid.txt)
bblid=$(cat $datadir/demographics/n270bblid.txt)

mkdir /data/jux/BBL/projects/pncItcNetworks/subjectData/restnifti

restdir=$(ls -d /data/joy/BBL/studies/pnc/processedData/restbold/restbold_201607151621)
nbackdir=$(ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/nbackVoxelwiseTimeseries)
idemodir=$(ls -d /data/joy/BBL/studies/pnc/processedData/idemo/idemoConnect_201707)
outputdir=$(ls -d /data/jux/BBL/projects/pncItcNetworks/subjectData/restnifti)
#data format in imagedir
#rm $datadir/imageinput1.txt # remove previous output if exit 



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

     imagelist=$(ls -d $outputdir/${s}_rest_nback_idemo.nii.gz)

     c=$(($c+1));

     echo "$imagelist" >> $datadir/imagelist_270_SCA.txt
done
	
exit 
