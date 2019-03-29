#This script will do 3dROISTATS to get time series from concatenated rest, nback, idemo
#Done in 2mm space
#OUTPUT /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/concat_rest_nback_idemo_2mm/ts_files_from_3dROIstats

set cwdir = `pwd`
set parceldir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels"
set parcels_2mm = `ls ${parceldir}/frac2back_n951_parametric_27roi_thr20_2mm.nii`
set wkdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo/rest_idemo_concat_2mm"
set outdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/rest_idemo/rest_idemo_concat_2mm/ts_files_from_3dROIstats_13rois"

echo "Parcel path " $parcels_2mm
echo "CWD" $cwdir
cd $wkdir

foreach subj (*nii.gz)
	echo "Subj nii : " $subj
	set prefix = `echo $subj:r:r`
	3dROIstats -mask $parcels_2mm $wkdir/$subj >! temp1
	#process the output to remove the header (Mean_1, Mean_2 ... Mean_27) and the first two columns (name of file and subrick)
	awk '{$1=$2=""; print $0}' temp1 | tail -n +2 >! $outdir/$prefix.txt
	
	#get # lines
	#set numlines = `more temp2 | wc -l`
	#echo "Numlines = " $numlines
	
	#Transpose so file of 27 rows - THIS IS THE TIMESERIES FORMAT I WANT
	#make new file
	#echo -n "" >! $outdir/$prefix.txt
	
	#set while loop counter
	#set colcnt = 2 # has to start on 2 for the cut command - weird stuff
	
	#go through each column, transpose, and add to main file
	#while ($colcnt <= 28) 	
		#echo "colcnt: " $colcnt
	#	more temp2 | sed -e "s/\s\{1,\}/ /g" | cut -f $colcnt -d ' ' | xargs -n$numlines >> $outdir/$prefix.txt
	#	@ colcnt++
	#end
	rm temp*

end
cd $cwdir

