KIYILepak
=========

This repository contains the analysis scripts and the draft manuscript for *Age Estimation for Lake Superior Kiyi (Coregonus kiyi)* by Taylor A. Lepak, Derek H. Ogle, and Mark R. Vinson that will be submitted to the Journal of Great Lakes Research.


# Data files (not in the repo)
* `data/KIYILepak_2014.xlsx` -- An excel file that contains two worksheets ...
    * `Ageing` -- Data for individual fish that were sampled for ageing purposes.
    * `LenFreq` -- Lengths of all captured Kiyi from 1972 to 2014.
    * `Stations` -- Information about hte sampling stations.
    * `Regions` -- Location of region boundaries.
* `data/LF_2001_14_forMark_corrected.xlsx` -- An excel file with length frequency records marked that can be used (per USGS protocol).
* `data/glgis_gl_shore_noaa_70k.*` -- Shape files for making the map.

# Scripts
## Initialization
* `aaaInit.R` -- Initial loading of packages, sets up names and colors for the regions, and prepares ggplot2 themes.
* `Data_Init.R` --  and data,  and prepares the `kiyial` data.frame for use in other scripts.  This is sourced by all other scripts except `LenFreq.R`.

## Map
* `Map.R` -- Constructs map of Lake Superior with sampling locations shown (Figure 1).

## Length Frequency Analysis
* `LenFreq.R` -- Process (i.e., expand) the corrected length frequency data.  Constructs 2014 length frequencies by region (Figure 2) and length frequencies for 2001-2014 (Figure 3).

## Age Analyses
* `AgeComparisons.R` -- Compares (bias and precision) scale to otolith ages and otolith ages between readers.  Constructs Figures 4 and 5.
* `ALKComparisons.R` -- Compares age-length keys between sexes within regions and among regions with sexes pooled.  No evidence for any differences in the keys.
* `AgeAnalysis_RegionalALKs.R` -- Makes and applies an age-length key (with consensus otolith ages) by region and then summarizes the age frequency.  Constructs Figure 6.

# Other Files
* `zzzLenFreqPrep.R` -- examined original length frequency to create a file to be corrected per USGS protocol.  DO NOT run this script again.
* `zzzAgeAnalysis_OneALK.R` -- similar to `AgeAnalysis_RegionalALKs.R` except that it uses an ALK pooled across all regions.  Not used in the manuscript.
* `KIYILepak.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
