#!/bin/csh
#this goes through each subject and gets mean values for each schaefer parcellation, and puts the results into a file for voxelwrapper

## Subject identifiers - 8/7/2018 n= 368
#bblid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justBBLID.csv
#scanid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justSCANID.csv

set bblid_and_scanid_path = /data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justBBLID_and_SCANID.csv

## Parcellation and Template Paths
set SchaeferPNC_path = /data/joy/BBL/studies/pnc/template/SchaeferPNC.nii.gz
set pnc_2mm_path = /data/joy/BBL/studies/pnc/template/pnc_template_brain_2mm.nii.gz
set SchaeferPNC_2mm_path = /data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181015_schaefer400/Schaefer400PNC_2mm.nii.gz


####################
## Make your lists##
####################

## Define output directory for ROI values
set outdir = /data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/20181015_schaefer400
mkdir -p ${outdir}

set nback_paths = ${outdir}/n368_2back0backStd_paths.txt
set tmp_output = ${outdir}/tmp_Schaefer400PNC_voxelwise_mean_n368_2back0back_sigchange_regional_values.txt
set final_output = ${outdir}/BBLID_SCANID_schaefer_parcellations_n368.txt
echo -n "" >! ${nback_paths}

#set up output file <BBLID> <SCANID> <Mean parcel 1> <Mean Parcel 2> ...
echo -n "bblid scanid" >! $final_output
set counter = 1
while ($counter <= 400)
	echo -n " $counter" >> $final_output
	@ counter++
end
echo "" >> $final_output

#################################
## Create 2mm SchaeferPNC_path ##
#################################
echo "***** applying antsApplytransform to get Schaefer parcellations (2mm)****"
antsApplyTransforms -e 3 -d 3 -i ${SchaeferPNC_path} -r ${pnc_2mm_path} -o ${SchaeferPNC_2mm_path} -n MultiLabel

##########################################################
## Extract 2back-0back ROI activation in Schaefer400PNC ##
##########################################################

echo "****** extracting mean signal for each 2bk-0bk in Schaefer space using c3d*****" 

foreach bblid_and_scanid ("`cat ${bblid_and_scanid_path}`") 
	echo "BBLID AND SCANID ${bblid_and_scanid}"
	set bblid = `echo ${bblid_and_scanid} | cut -d ' ' -f1`
	set scanid = `echo ${bblid_and_scanid} | cut -d ' ' -f2`
	set nback_path = `ls /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/n1601_voxelwiseMaps_nback/*${scanid}_sigchange_contrast4_2back0backStd.nii.gz`
	echo $nback_path >> ${nback_paths}
	c3d "${nback_path}" "${SchaeferPNC_2mm_path}" -lstat >! $tmp_output
	echo -n "${bblid} ${scanid}" >> $final_output
	#go through line by line, grab mean, and stick it in the final_output
	#set num_parcel = 1
	foreach line ("`cat $tmp_output | tail -400`")
		set mean = `echo "$line" | awk '{print $2}'`
		#echo $mean
		echo -n " $mean" >> $final_output
		#@ num_parcel++
	end
	echo "" >> $final_output
end 

#merged_nback_output=${outdir}/merged_n368_sigchange_cope4_2back-0backStd.nii.gz
#mean_nback_output=${outdir}/voxelwise_mean_n368_sigchange_cope4_2back-0backStd.nii.gz

#nback=$(cat ${nback_paths})

## Merge each subject's 3D activation map in PNC template space
#fslmerge -t ${merged_nback_output} ${nback}

## Calculate mean voxel intensity across subjects
#fslmaths ${merged_nback_output} -Tmean ${mean_nback_output}

## Extract ROI values from activation maps
#tmp_output=${outdir}/tmp_Schaefer400PNC_voxelwise_mean_n368_2back-0back_sigchange_regional_values.txt
#c3d "${mean_nback_output}" "${SchaeferPNC_2mm_path}" -lstat >> ${tmp_output}

#final_output=${outdir}/Schaefer400PNC_voxelwise_mean_n368_2back-0back_sigchange_regional_values.txt

## Extract mean ROI values from output (second column)
#awk '{print $2}' ${tmp_output} >> ${final_output}

## Remove header and label 0 stats
#sed -i -e '1,2d' ${final_output}
