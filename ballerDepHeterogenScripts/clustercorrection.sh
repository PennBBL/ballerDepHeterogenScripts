#!/bin/bash

scriptdir=/data/joy/BBL/applications/R-3.2.5/bin




##  $scriptdir/Rscript $scriptdir/le_correcter.R  ${distance_directory}  ${mdmr_directory}

distancedir=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson
mdmrdir=/data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk

$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_sex
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_motion
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_scanageYrs
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_motion_scanageYrs
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_motion_sex
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_sex_scanageYrs
$scriptdir/Rscript $scriptdir/le_correcter.R  $distancedir  /data/jux/BBL/projects/pncItcNetworks/subjectData/n269/n269_pearson/logk_sex_scanageYrs_motion


	

