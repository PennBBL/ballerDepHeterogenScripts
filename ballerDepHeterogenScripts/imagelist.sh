#This script will take a list of scanids to create imageslist required for 
# distance computation
 
#scanids 
datadir=$(ls -d /data/jux/BBL/projects/pncItcNetworks/subjectData)
scanid=$(cat $daatadir/demographics/n314scanid.txt)
bblid=$(cat $datadir/demographics/n314bblid.txt)
imagedir=$(ls -d /data/joy/BBL/studies/pnc/processedData/restbold/restbold_201607151621)

#data format in imagedir
rm $datadir/imageinput1.txt # remove previous output if exit 

nsubj=$(echo $scanid | wc | awk '{print $2}')
	
	echo "$nsubj subjects"

c=0
for b  in $bblid;  do 
     dd=$(ls -d $imagedir/${b}/*/*.nii.gz)
     echo "$dd"
    
     echo "$c out of $nsubj"
     set -- $scanid
     arr=($scanid)
     s=${arr[$c]}
     echo "$s" 	
     img=$(ls -d $imagedir/${b}/*${s}/${b}_*${s}.nii.gz)
     fslmaths $img -subsamp2 $datadir/nifti4mm/${b}_image_${s}_4mm.nii.gz
     
    imagelist=$(ls -d $datadir/nifti4mm/${b}_image_${s}_4mm.nii.gz)
     c=$(($c+1));
     echo "$imagelist" >> $datadir/imageinput1.txt
done
	
exit 
