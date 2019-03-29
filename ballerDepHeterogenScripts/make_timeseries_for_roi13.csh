#Input: timeseries files (27rois)
#Output: timeseries files (13rois)
#Uses: Makes new Timeseries

#This script makes new timeseries files from the concatenated 27 roi files in
#/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/rest_nback_idemo_8FunctionalROIs

#The ROIs were defined by taking areas that showed differences between Hydra_k3 groups during Nback activation

set currdir = `pwd`
set wkdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest/rest_2mm/"
set oldts = $wkdir/ts_files_from_3dROIstats
set newts = $wkdir/ts_files_from_3dROIstats_13rois

#copy old files into this directory
foreach ts (`ls $oldts/*.txt`)
	set ts_root = `echo $ts:r:t`
	cp $ts ${newts}/${ts_root}_13roi.txt
	#change format of new file so it is single space delimited
	more ${newts}/${ts_root}_13roi.txt | sed -e "s/\s\{1,\}/ /g" >! temp
	mv temp ${newts}/${ts_root}_13roi.txt	
	Rscript call_27roi_to_13_roi_r_script.R ${newts}/${ts_root}_13roi.txt
end


