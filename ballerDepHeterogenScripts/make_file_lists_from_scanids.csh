#!/bin/csh

#this script goes through the scanids, created on my local machine under /Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/PrepForFunctionalNetworks/PrepForNbackNetworks_20180706.R 
#make afni command line, both including and without TDs
#echo -n "3dANOVA -levels 4 " >! afni_anova_with_TD_command
#echo -n "3dANOVA -levels 3 " >! afni_anova_without_TD_command

set factor = 1
foreach cluster (cluster1 cluster2 cluster3 TD)
	set scanid_file = `ls nback_just_scanid_${cluster}_n*.csv`
	set num_scanids = `echo $scanid_file | perl -pe 's/.*_n(.*).csv/$1/'`
	echo -n "" >! ${cluster}_n${num_scanids}_filepaths.txt
	foreach scanid (`cat $scanid_file`)
		set filename = `more n368_2back-0backStd_paths.txt | grep $scanid` 
		echo $filename >> ${cluster}_n${num_scanids}_filepaths.txt
		#echo -n "dset ${factor} $filename \" >> afni_anova_with_TD_command
		#if ($cluster != "TD") then
		#	echo -n "dset ${factor} $filename \" >> afni_anova_without_TD_command
		#endif
		
	end

	@ factor++
end

#echo -n "-ftr Clusters \\-mean 1 Cluster1 \\-mean 2 Cluster2 \\-mean 3 Cluster3 \\-mean 4 TD \-diff 1 2 C1vsC2 \-diff 1 3 C1vsC3 \-diff 1 4 C1vsTD \-diff 2 3 C2vsC3 \-diff 2 4 C2vsTD \-diff 3 4 C3vsTD \\" >> afni_anova_with_TD_command
#echo -n "-contr 1 2 C1vsC2 -contr 1 3 C1vsC3 -contr 1 4 C1vsTD -contr 2 3 C2vsC3 -contr 2 4 C2vsTD -contr 3 4 C3vsTD " >> afni_anova_with_TD_command
#echo "-bucket afni_with_TD_ANOVA" >> afni_anova_with_TD_command 
#echo "-ftr Clusters " >> afni_anova_without_TD_command

#echo -n "-ftr Clusters -mean 1 Cluster1 -mean 2 Cluster2 -mean 3 Cluster3 -diff 1 2 C1vsC2 -diff 1 3 C1vsC3 -diff 2 3 C2vsC3 " >> afni_anova_without_TD_command
#echo "-bucket afni_without_TD_ANOVA" >> afni_anova_without_TD_command 

