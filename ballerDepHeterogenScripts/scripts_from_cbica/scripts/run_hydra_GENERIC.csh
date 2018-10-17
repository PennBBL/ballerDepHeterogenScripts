l#first, create a list  that contains, one per line, each variable or filename you want to make a new script for
if ($#argv < 3) then
   echo "This script runs Hydra analysis"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching), and number of subjects"
else
	set list = $1
	if ($2 == 0) then
	   set analysis = "no_resid"
	   set csv_type = "no_covariance"
	else if ($2 == 1) then
	   # set analysis = "pre_match_resid" 
	   set analysis = "covaried_before_match"
	   set csv_type = "covaried_before_match"
	else
	   #set analysis = "post_match_resid"
	   set analysis = "covaried_after_match"
	   set csv_type = "covaried_after_match"
        endif
	set num_subs = $3
	echo -n "" >! qsub_commands_to_run_${num_subs}_${analysis}
        foreach x (`cat $list`)
		set for_csv_name = `echo $x | perl -pe 's/matched_(.*)/$1/'`
		set csv = /cbica/projects/pncHeterogeneity/ballerDepHeterogen/subjectData/hydra_matched/${csv_type}/data_$for_csv_name".csv"
		echo $csv
		echo "qsub ./submit_run_hydra_experiment_csv_$x"_n"${num_subs}_${analysis} $csv /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/CogData_${x}_n${num_subs}_${analysis}" >> qsub_commands_to_run_${num_subs}_${analysis}
	end
endif
