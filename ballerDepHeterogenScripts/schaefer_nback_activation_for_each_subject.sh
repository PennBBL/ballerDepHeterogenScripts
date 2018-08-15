#!/bin/sh
#this goes through each subject and gets mean values for each schaefer parcellation, and puts the results into a file for voxelwrapper

## Subject identifiers - 8/7/2018 n= 368
#bblid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justBBLID.csv
#scanid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justSCANID.csv

bblid_and_scanid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justBBLID_and_SCANID.csv

## Parcellation and Template Paths
SchaeferPNC_path=/data/joy/BBL/studies/pnc/template/SchaeferPNC.nii.gz
pnc_2mm_path=/data/joy/BBL/studies/pnc/template/pnc_template_brain_2mm.nii.gz
SchaeferPNC_2mm_path=/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/fc_nback_shaefer400/Schaefer400PNC_2mm.nii.gz

#################################
## Create 2mm SchaeferPNC_path ##
#################################
#antsApplyTransforms -e 3 -d 3 -i ${SchaeferPNC_path} -r ${pnc_2mm_path} -o ${SchaeferPNC_2mm_path} -n MultiLabel

## Define output directory for ROI values
outdir=/data/jux/BBL/projects/ballerDepHeterogen/ballerDepHeterogenScripts/Imaging/matched/nback/activation_by_hydra_cluster_n368
mkdir -p ${outdir}

nback_paths=${outdir}/n368_2back-0backStd_paths.txt

touch ${nback_paths}


##########################################################
## Extract 2back-0back ROI activation in Schaefer400PNC ##
##########################################################

for bblid_and_scanid in "`cat ${bblid_and_scanid_path}`"; do
	echo "BBLID AND SCANID ${bblid_and_scanid}"
	#bblid=`echo ${bblid_and_scanid | cut -d ' ' -f1`
	#scanid=`echo ${bblid_and_scanid | cut -d ' ' -f2`
	echo "BBLID $bblid"
	echo "SCANID $scanid"
done
#for scanid in `cat ${scanid_path}`; do

	# Define path to 2back-0back activation map in PNC template space
#	echo $scanid
#	nback_path=$(ls /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/voxelwiseMaps_nback/"${scanid}"_sigchange_cope4_2back-0backStd.nii.gz)

#	echo $nback_path >> ${nback_paths}
#done

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
