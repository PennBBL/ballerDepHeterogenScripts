if ($#argv < 3) then
   echo "This script generates ARI and hydra subtypes from complete analysis"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching), and number of subjects"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	   else if ($2 == 1) then
	      set analysis = "pre_match_resid"
	      else
	         set analysis = "post_match_resid"
        endif
	set num_subs = $3

        foreach x (`cat $list`)	 
		cp matlab_script_to_get_subtypes_and_ARI /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_$analysis/.
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}/
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