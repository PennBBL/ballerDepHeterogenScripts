#this takes a file list and renames the RMD files based on specifications... Great if you have to do minor changes
if ($#argv < 4) then
   echo "This script takes a list of files you need to rename, and copies the appropriate RMDs.  As of 4/24, you still need to go into the rmds to change paths, but plan on working in sed command into this script"
   echo "Please enter a list, number (0 for no resids, 1 for resids BEFORE match, 2 for resids AFTER matching), number of subjects, and whether you matched(1 for matched, 2 for not)"
else
    set list = $1
    if ($2 == 0) then
       set analysis = "no_resid"
    else if ($2 == 1) then
          set analysis = covaried_before_match
    else
	  set analysis = covaried_after_match 
    endif
    
    if($3 == 1) then
	set match = "matched"
    else
	set match = "unmatched"
    endif

    set num_subs = $4
    
    foreach type (`cat $list`)
	cp /Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Cog/matched/covariance_before_match/Cog_hydra_analysis_matched_race_n1424_covariance_before_match_Hydra_k2.Rmd Cog_hydra_analysis_${match}_${type}_n${num_subs}_${analysis}_Hydra_k2.Rmd
    #cp /Users/eballer/BBL/from_chead/ballerDepHeterogen/ballerDepHeterogenScripts/Cog/GENERIC_Cog_hydra_analysis_Hydra_K2.Rmd Cog_hydra_analysis_${match}_${type}_n${num_subs}_${analysis}_Hydra_k2.Rmd
    end

endif

