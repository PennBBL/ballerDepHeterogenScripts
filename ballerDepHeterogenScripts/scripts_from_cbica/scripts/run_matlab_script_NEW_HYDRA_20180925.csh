if ($#argv < 4) then
   echo "This script generates ARI and hydra subtypes from complete analysis"
   echo "as of 9/25, for this script, you must provide a date, so that it can look at the appropriate replication file"
    echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching, 3 covary with hydra, or the name of the analysis <ex covaried_before_match>), number of subjects, and date of replication analysis"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	   else if ($2 == 1) then
	      set analysis = "covaried_before_match"
	   else if ($2 == 2) then
	      set analysis = "covaried_after_match"
	   else if ($2 == 3) then
	      set analysis = "covaried_with_hydra"
	   else
	      set analysis = $2

        endif

	set num_subs = $3
	set replication_date = $4

        foreach x (`cat $list`)	 
		cp matlab_script_to_get_subtypes_and_ARI /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/.
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/
		source matlab_script_to_get_subtypes_and_ARI
		#have to extract just the important stuff, cause matlab puts crap in these files
		#set numlines_ARI = `more CogData*ARI.csv | wc -l`
		#set numlines_Subtypes = `more CogData**HydraSubtypes.csv | wc -l`
		#set tail_ARI = `expr $numlines_ARI - 3`
		#set head_ARI = `expr $numlines_Subtypes - 8`
		#set tail_Subtypes = `expr $numlines_Subtypes - 3`
		#set head_Subtypes = `expr $numlines_Subtypes - 8`
		#more CogData*ARI.csv | tail -${tail_ARI} | head -${head_ARI} >! CogData_${x}_n${num_subs}_${analysis}_ARI_edited.csv
		#more CogData*HydraSubtypes.csv | tail -${tail_Subtypes} | head -${head_Subtypes} >! CogData_${x}_n${num_subs}_${analysis}_HydraSubtypes_edited.csv
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/scripts/
	end
endif
