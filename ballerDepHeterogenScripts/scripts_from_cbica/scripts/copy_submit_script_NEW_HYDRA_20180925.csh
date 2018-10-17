#first, create a list  that contains, one per line, each variable or filename you want to make a new script for
if ($#argv < 4) then
   echo "This script takes a list of different submit scripts you want to make, makes them and does the string replace with sed.  It also makes a directory in results."
   echo "as of 4/21/2018, it will also copy SaveHydraOutput and command to run this to the directories, and change names as appropriate with sed"
   echo "As of 20180925, this will also require a separate argument for the date, so that we might map the replication after HYDRA changed in 09/2018"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching, 3 for covaried_with_hydra or another name you'd like to call your analysis), number of subjects, and replication_date"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	else if ($2 == 1) then
	   set analysis = covaried_before_match
	else if ($2 == 2) then
	   set analysis = covaried_after_match 
	else if ($2 == 3) then
           set analysis = covaried_with_hydra
	else
	   set analysis = $2
        endif

	set num_subs = $3
	set replication_date = $4

        foreach x (`cat $list`)	     

		#make a script for each analysis by copying template
		cp submit_run_hydra_NEW_20180925 submit_run_hydra_NEW_${x}_n${num_subs}_${analysis}_${replication_date}
		
		#personalize the submit scripts
		sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/" submit_run_hydra_NEW_${x}_n${num_subs}_${analysis}_${replication_date}
		sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/" submit_run_hydra_NEW_${x}_n${num_subs}_${analysis}_${replication_date}

		#make results directory
		mkdir -p /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}
	
		#copy SaveHydraOutput.m, and csvwrite_with_headers.m to new directory 
		cp SaveHydraOutput_NEW.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/SaveHydraOutput.m
		cp csvwrite_with_headers.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/csvwrite_with_headers.m

		#go into appropriate directory
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/
	
		#personalize the SaveHydraOutput file, both the path, as well as the prefixes for the ARI and HydraSubtypes files

		#path
		sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/" SaveHydraOutput.m
		sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/" SaveHydraOutput.m
		
		#prefix 
		sed -i -e "s/new_hydra_results_prefix/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
		sed -i -e "s/new_hydra_results_prefix/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_${replication_date}\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m

		#go back to scripts directory                                                                                                                                                               
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/scripts/
	end
endif
