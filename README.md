KIYILepak
=========

This repository contains data files, analysis scripts, and the draft manuscript for *Age Estimation for Lake Superior Kiyi (Coregonus kiyi)* by Taylor A. Lepak, Derek H. Ogle, and Mark R. Vinson that will be submitted to the Journal of Great Lakes Research.


# Data files (not in the repo)
* `data/KIYILepak_2014.xlsx` -- An excel file that contains two worksheets ...
    * `Ageing` -- Data for individual fish that were sampled for ageing purposes.
    * `LenFreq` -- Lengths of all captured Kiyi from 1972 to 2014.


# Scripts
## Initialization
* `Data_Init.R` -- Initial loading of packages and data, sets up names and colors for the regions, and prepares the `kiyial` data.frame for use in other scripts.  This is sourced by all other scripts except `LenFreq.R`.

## Length Frequency Analysis
* `LenFreq.R` -- Process (i.e., expand) the original length frequency data.  Compute length frequency by month to examine in-season growth and for May-Jul samples from 2001-2014.  This makes **Figure XX** in the manuscript.

## Age, Growth, and Size Analyses
* `AgeComparisons.R` -- Compares (bias and precision) scale to otolith ages and otolith ages between readers.  This makes **Figure XX** in the manuscript
* `ALKComparisons.R` -- Compares age-length keys between sexes within regions and among regions with sexes pooled.  No evidence for any differences in the keys.
* `AgeAnalysis.R` -- makes and applies an age-length key (with consensus otolith ages) and then summarizes the age frequency.

# Other Files
* `KIYILepak.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
