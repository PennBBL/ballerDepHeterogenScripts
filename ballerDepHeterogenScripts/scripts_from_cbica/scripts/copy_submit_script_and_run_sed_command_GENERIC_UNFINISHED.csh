#first, create a list  that contains, one per line, each variable or filename you want to make a new script for
if ($#argv < 4) then
   echo "This script takes a list of different submit scripts you want to make, makes them and does the string replace with sed.  It also makes a directory in results."
   echo "as of 4/21/2018, it will also copy SaveHydraOutput and command to run this to the directories, and change names as appropriate with sed"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching), number of subjects, And whether to match/not match/use residuals(0 matched, 1 unmatched, 2 residualized)"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	else if ($2 == 1) then
	   set analysis = "covaried_before_match"
      	else
	   set analysis = "covaried_after_match" 
        endif
	set num_subs = $3

	if ($4 == 0) then
	    set match_type = "matched"
	else if ($4 == 1) then
       	    set match_type = "unmatched"
	else
	    set match_type = "residualized"
	endif

        foreach x (`cat $list`)	     
		cp GENERIC_Cog_hydra_analysis_Hydra_K2.Rmd  submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}

		mkdir /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}
		cp SaveHydraOutput.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}/SaveHydraOutput.m
		cp csvwrite_with_headers.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}/csvwrite_with_headers.m

		#personalize title 
		sed -i -e 's/<MATCH_TYPE>/${x}/' submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}
		sed -i -e 's/<N>/${num_subs}/' submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}
		sed -i -e 's/<COVARIANCE>/${analysis}/' submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}

		#personalize the submit_run_hydra_experiment file
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}/" submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}		

		#personalize the SaveHydraOutput files, do sed twice, sometimes not greedy
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}/
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}/" SaveHydraOutput.m
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}/" SaveHydraOutput.m
		#mv SaveHydraOutput SaveHydraOutput.m
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/scripts/

	end
endif
