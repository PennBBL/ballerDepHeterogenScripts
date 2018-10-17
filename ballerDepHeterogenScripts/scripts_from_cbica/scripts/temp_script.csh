set list = list_matched_for_copy_script
set num_subs = 1424
set replication_date = 20181009
set analysis = "covaried_with_hydra"

foreach x (`cat $list`)
	#personalize the SaveHydraOutput files, do sed twice, sometimes not greedy
	cp SaveHydraOutput_NEW.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/SaveHydraOutput.m
        cp csvwrite_with_headers.m /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/csvwrite_with_headers.m


	cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/results/${replication_date}_New_Hydra/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/
	sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
	sed -i -e "s/new_hydra_results_path/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
	sed -i -e "s/new_hydra_results_prefix/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m
	sed -i -e "s/new_hydra_results_prefix/${replication_date}_New_Hydra\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}\/CogData_${x}_n${num_subs}_${analysis}_replication_${replication_date}/" SaveHydraOutput.m

	#go back to scripts directory
	cd /cbica/projects/pncHeterogeneity/ballerDepHeterogen/scripts/

end
