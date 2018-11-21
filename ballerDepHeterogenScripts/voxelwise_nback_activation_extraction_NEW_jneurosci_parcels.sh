#!/bin/sh

## Subject identifiers - 8/7/2018 n= 368
bblid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justBBLID.csv
scanid_path=/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/subset_with_T1_NbackFC_and_dem_with_clusters_justSCANID.csv

## Parcellation and Template Paths- these parcels are the NEW ones, using the same parcels as ted's jneurosci paper but with NEW data
parcels_path=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels/frac2back_n951_parametric_27roi_thr20.nii.gz
pnc_2mm_path=/data/joy/BBL/studies/pnc/template/pnc_template_brain_2mm.nii.gz
#pnc_2mm_path=$FSLDIR/data/standard/MNI152_T1_2mm_brain.nii.gz
parcels_2mm_path=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels/frac2back_n951_parametric_27roi_thr20_2mm.nii.gz
affine_mat=/data/joy/BBL/studies/pnc/template/mni2pnc0GenericAffine.mat
mni2pnc_warp=/data/joy/BBL/studies/pnc/template/mni2pnc1Warp.nii.gz

#################################
## Create 2mm PNC_path ##
#################################
#antsApplyTransforms -e 3 -d 3 -i ${parcels_path} -r ${pnc_2mm_path} -o ${parcels_2mm_path} -n MultiLabel
echo "applying antsApplyTransforms"
antsApplyTransforms -e 3 -d 3 -i ${parcels_path} -o ${parcels_2mm_path} -r ${pnc_2mm_path} -t ${affine_mat} -t ${mni2pnc_warp}
echo "transform complete"

## Define output directory for ROI values
outdir=/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels/roi_vals
mkdir -p ${outdir}

nback_paths=${outdir}/n368_2back0backStd_paths.txt

touch ${nback_paths}

#############################################################################
## Extract 2back-0back ROI activation in Parcel from New Jneurosci parcels ##
#############################################################################
for scanid in `cat ${scanid_path}`; do
	echo $scanid
	# Define path to 2back-0back activation map in PNC template space
	nback_path=$(ls /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/nback/nbackGlmBlockDesign/n1601_voxelwiseMaps_cope/*"${scanid}"_contrast4_2back0backStd.nii.gz) 

	echo $nback_path >> ${nback_paths}
done

merged_nback_output=${outdir}/merged_n368_contrast4_2back0backStd.nii.gz
mean_nback_output=${outdir}/parcels_mean_n368_contrast4_2back0backStd.nii.gz

nback=$(cat ${nback_paths})

## Merge each subject's 3D activation map in PNC template space
fslmerge -t ${merged_nback_output} ${nback}

## Calculate mean voxel intensity across subjects
fslmaths ${merged_nback_output} -Tmean ${mean_nback_output}

## Extract ROI values from activation maps
tmp_output=${outdir}/tmp_parcels_mean_n368_2back0back_sigchange_regional_values.txt
c3d "${mean_nback_output}" "${parcels_2mm_path}" -lstat >> ${tmp_output}

final_output=${outdir}/parcels_mean_n368_2back0back_sigchange_regional_values.txt

## Extract mean ROI values from output (second column)
awk '{print $2}' ${tmp_output} >> ${final_output}

## Remove header and label 0 stats
sed -i -e '1,2d' ${final_output}

