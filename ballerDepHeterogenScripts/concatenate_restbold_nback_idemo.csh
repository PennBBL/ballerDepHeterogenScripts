set working_directory = "/data/jux/BBL/projects/ballerDepHeterogen/data/neuroimaging/processed_data/concat_restbold_nback_idemo"

if ($#argv < 2) then
    echo "Welcome to Concatenate Script!"
    echo "Please enter a subject list(full path) (FORMAT <BBLID SCANID>) and type of parcellation: "
    echo "1: 264PowerPNC"
    echo "2: GlasserPNC"
    echo "3: GordonPNC"
    echo "4: SchaeferPNC"  
else
    set subj_list = $1
    #set analysis type
    if ($2 == 1) then
	set analysis = "264PowerPNC"
    else if ($2 == 2) then
	set analysis = "GlasserPNC"
    else if ($2 == 3) then
	set analysis = "GordonPNC"
    else if ($2 == 4) then
	set analysis = "SchaeferPNC"
    else
	echo $2 " is not a known analysis type.  Using default = SchaeferPNC"
	set analysis = "SchaeferPNC"
    endif
    set output_directory = "$working_directory/$analysis"
    echo "Output directory of new concatenated files:"$output_directory
    
    #### Make directory where all new concatenated files will go #######
    mkdir $working_directory/$analysis
    
    #go through each bblid/scanid pair, make a new directory within working directory and analysis type, and concatenate 3 runs
    foreach scan ("`cat $subj_list`")
	set bblid = `echo $scan | perl -pe 's/(.*) .*/$1/'`
	set scanid = `echo "$scan" | perl -pe 's/(.*) (.*)/$2/'`
	echo "${bblid}/${scanid}"
	mkdir $working_directory/$analysis/${bblid}
	mkdir $working_directory/$analysis/${bblid}/${scanid}

	set restbold = `ls /data/joy/BBL/studies/pnc/processedData/restbold/restbold_201607151621/${bblid}/*x${scanid}/net/$analysis/*_ts.1D`
	set nback = `ls /data/joy/BBL/studies/pnc/processedData/nback/nbackConnect_*/${bblid}/*x${scanid}/net/$analysis/*_ts.1D`
	set idemo = `ls /data/joy/BBL/studies/pnc/processedData/idemo/idemoConnect_*/${bblid}/*x${scanid}/net/$analysis/*_ts.1D`

#outputs all three concatenated as both text file and 1D files
	cat $restbold $nback $idemo >! $working_directory/$analysis/${bblid}/${scanid}/${bblid}_${scanid}_restbold_nback_idemo_concat_ts.txt
	cat $restbold $nback $idemo >! $working_directory/$analysis/${bblid}/${scanid}/${bblid}_${scanid}_restbold_nback_idemo_concat_ts.1D	
    end
    
endif
