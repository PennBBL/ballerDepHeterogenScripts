#first, create a list  that contains, one per line, each variable or filename you want to make a new script for
if ($#argv < 4) then
   echo "This script takes a list of different submit scripts you want to make, makes them and does the string replace with sed.  It also makes a directory in results."
   echo "as of 4/21/2018, it will also copy SaveHydraOutput and command to run this to the directories, and change names as appropriate with sed"
   echo "As of 20180925, this will also require a separate argument for the date, so that we might map the replication after HYDRA changed in 09/2018"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching), number of subjects, and replication_date"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	else if ($2 == 1) then
	   set analysis = covaried_before_match
	    #set analysis = "pre_match_resid"
	else
	   set analysis = covaried_after_match 
	   #set analysis = "post_match_resid"
        endif
	set num_subs = $3
	set replication_date = $4

        foreach x (`cat $list`)	     
		cp submit_run_hydra_experiment_csv_all_gender_matched_n1424 submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}_replication_${replication_date}

		mkdir /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}
		cp SaveHydraOutput.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}_${replication_date}/SaveHydraOutput.m
		cp csvwrite_with_headers.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/csvwrite_with_headers.m

		#personalize the submit_run_hydra_experiment file
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}_replication_${replication_date}/" submit_run_hydra_experiment_csv_${x}_n${num_subs}_${analysis}_replication_${replication_date}		

		#personalize the SaveHydraOutput files, do sed twice, sometimes not greedy
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
		sed -i -e "s/all_gender_matched_n1424/${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
		#mv SaveHydraOutput SaveHydraOutput.m
		cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/scripts/

	end
endif
