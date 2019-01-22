#This script will do 3dmaskdump to get time series from concatenated rest, nback, idemo
#Done in 4mm space
#Results in /data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm/weighted_n312_rest_nback_idemo_tr_20181218/ts_files_from_3dmaskdump/

set cwdir = `pwd`
set parceldir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/nback/parcels"
set parcels_4mm = `ls ${parceldir}/frac2back_n951_parametric_27roi_thr20_4mm.nii.gz`
set wkdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm/weighted_n312_rest_nback_idemo_tr_20181218"
set outdir = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo/for_cwasmdmr_concat_4mm/weighted_n312_rest_nback_idemo_tr_20181218/ts_files_from_3dROIstats"

echo "Parcel path " $parcels_4mm
echo "CWD" $cwdir
cd $wkdir

foreach subj (*nii.gz)
	echo "Subj nii : " $subj
	set prefix = `echo $subj:r:r`
	3dROIstats -mask $parcels_4mm $wkdir/$subj >! temp1
	#process the output to remove the header (Mean_1, Mean_2 ... Mean_27) and the first two columns (name of file and subrick)
	awk '{$1=$2=""; print $0}' temp1 | tail -n +2 >! temp2
	
	#get # lines
	set numlines = `more temp2 | wc -l`
	#echo "Numlines = " $numlines
	
	#Transpose so file of 27 rows - THIS IS THE TIMESERIES FORMAT I WANT
	#make new file
	echo -n "" >! $outdir/$prefix.txt
	
	#set while loop counter
	set colcnt = 2 # has to start on 2 for the cut command - weird stuff
	
	#go through each column, transpose, and add to main file
	while ($colcnt <= 28) 	
		#echo "colcnt: " $colcnt
		more temp2 | sed -e "s/\s\{1,\}/ /g" | cut -f $colcnt -d ' ' | xargs -n$numlines >> $outdir/$prefix.txt
		@ colcnt++
	end
	rm temp*

end
cd $cwdir

