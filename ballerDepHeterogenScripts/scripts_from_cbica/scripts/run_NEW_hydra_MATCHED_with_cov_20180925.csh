#first, create a list  that contains, one per line, each variable or filename you want to make a new script for
if ($#argv < 4) then
   echo "This script runs Hydra analysis"
   echo "As of 09/2018, Hydra has changed, so this script also requires you enter in the new path for the replication.  It also has -l centos6 in it as o 9/25"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching, 3 for using hydra for covariance, or the name you would like to use <ex covaried_after_match>), number of subjects, and replication date"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	   set csv_type = "no_covariance"
	else if ($2 == 1) then
	   # set analysis = "pre_match_resid" 
	   set analysis = "covaried_before_match"
	   set csv_type = "covaried_before_match"
	else if($2 == 2) then
	   #set analysis = "post_match_resid"
	   set analysis = "covaried_after_match"
	   set csv_type = "covaried_after_match"
	else if ($2 == 3) then
	   set analysis = "covaried_with_hydra"
           set csv_type = "covaried_with_hydra"
	else
	   set analysis = $2
           set csv_type = $2 
        endif
	set num_subs = $3
	set replication_date = $4
	echo -n "" >! qsub_commands_to_run_NEW_Hydra_${num_subs}_${analysis}_{$replication_date}
        foreach x (`cat $list`)
		set for_csv_name = `echo $x | perl -pe 's/matched_(.*)/$1/'`
		set csv = /cbica/projects/pncHeterogeneity/ballerDepHeterogen/subjectData/hydra_matched/${csv_type}/data_$for_csv_name".csv"
		echo $csv
		if ($3 < 3) then
		    echo "qsub ./submit_run_hydra_NEW_${x}_n${num_subs}_${analysis}_${replication_date} -i $csv -o /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date}" >> qsub_commands_to_run_NEW_Hydra_${num_subs}_${analysis}_${replication_date}
		else
		    set cov_csv = /cbica/projects/pncHeterogeneity/ballerDepHeterogen/subjectData/hydra_matched/${csv_type}/cov_$for_csv_name".csv"
		    echo "qsub ./submit_run_hydra_NEW_${x}_n${num_subs}_${analysis}_${replication_date} -i $csv -o /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_${replication_date} -z $cov_csv" >> qsub_commands_to_run_NEW_Hydra_${num_subs}_${analysis}_${replication_date}
		endif
	end
endif
