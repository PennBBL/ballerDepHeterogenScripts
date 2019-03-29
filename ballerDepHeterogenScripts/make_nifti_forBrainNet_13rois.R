library(pracma)
library(RNifti)

#read in fdr corrected p values that were calculated on local machine
fvals <- read.csv("/data/jux/BBL/projects/ballerDepHeterogen/results/csvs/nback_f_fdr_corr0s_forBrainNet_13rois.csv", header = FALSE)

#convert ps to ts

#read in nifti with ROIs
roi_nifti <- readNifti("/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels/frac2back_n951_parametric_27roi_thr20_2mm.nii.gz")

#make new output nifti
newNifti <- roi_nifti
newNifti[newNifti>0]=0

#replace
for (roi in 1:27) {
  mask <- roi_nifti[roi_nifti==roi]
  newNifti[mask] <-fvals[roi,2]
  rm(mask)
}
b=newNifti[newNifti>0]
writeNifti(newNifti, "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels/nifti_fvals_for_fdr_corrected_ps_roi27_thresh20_2mm_13rois.nii.gz")
