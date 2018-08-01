
# pnc brain mask and image 



templatedir=/data/joy/BBL/studies/pnc/template/priors
mask=$templatedir/prior_mask.nii.gz

datadir=$(ls -d /data/jux/BBL/projects/pncItcNetworks/subjectData)
scanid=$(cat $datadir/demographics/n270scanid.txt)
bblid=$(cat $datadir/demographics/n270bblid.txt)

imagedir=$(ls -d /data/jux/BBL/projects/pncItcNetworks/subjectData/restnifti)

#seed based correlation

# do for logk with two rois
roi1=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI1_2mm_bin.nii.gz
roi2=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI2_2mm_bin.nii.gz
 mkdir /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI1map
 mkdir /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI2map

 outputdir1=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI1map
 outputdir2=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05/easythresh/ROI2map

for b  in $scanid;  do 
    # extract the times series for each ROI 
    3dROIstats -mask $roi1 -1Dformat $imagedir/${b}_rest_nback_idemo.nii.gz > $outputdir1/ROI1timeseries.1D 
    3dROIstats -mask $roi2 -1Dformat $imagedir/${b}_rest_nback_idemo.nii.gz > $outputdir2/ROI2timeseries.1D
    
    ## pearson correlation 
    3dTcorr1D -pearson -mask $mask -prefix $outputdir1/${b}_ROI1map_r.nii.gz $imagedir/${b}_rest_nback_idemo.nii.gz $outputdir1/ROI1timeseries.1D
    3dTcorr1D -pearson -mask $mask -prefix $outputdir2/${b}_ROI2map_r.nii.gz $imagedir/${b}_rest_nback_idemo.nii.gz $outputdir2/ROI2timeseries.1D
    
    ## convert r ro z map 
    3dcalc -a  $outputdir1/${b}_ROI1map_r.nii.gz -expr 'atanh(a)' -prefix $outputdir1/${b}_ROI1map_z.nii.gz
    3dcalc -a  $outputdir2/${b}_ROI2map_r.nii.gz -expr 'atanh(a)' -prefix $outputdir2/${b}_ROI2map_z.nii.gz

    # clear the files not neccessary for next step 
    rm -rf   $outputdir1/ROI1timeseries.1D $outputdir2/ROI2timeseries.1D  $outputdir1/${b}_ROI1map_r.nii.gz  $outputdir2/${b}_ROI2map_r.nii.gz
done
  	
   # group statisttics 
   groupmapdir=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk/cluster_correct_v001_c05
   
   3dttest++ -toz -prefix $groupmapdir/ROI1_group_stat.nii.gz  -setA $outputdir1/*_ROI1map_z.nii.gz 
   3dttest++ -toz -prefix $groupmapdir/ROI2_group_stat.nii.gz  -setA $outputdir2/*_ROI2map_z.nii.gz

exit
