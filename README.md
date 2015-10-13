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
* `Age_Comparisons.R` -- Precision and bias analysis between readers of scales and otoliths, bias analysis between scales and otoliths.  This makes **Table 1**, **Figure 2**, and the results in the **AGE** section of the manuscript.
* `Basic_Summaries.R` -- Basic summaries of length, weight, age, and sex variables.  These results are in the **SIZE** section of the manuscript.
* `Growth.R` -- von Bertalanffy growth analyses.  This makes **Table 2**, **Table 3**, **Table 4**,  **Figure 4**, and the results in the **GROWTH** section of the manuscript.
* `Weight-Length.R` -- Weight-length relationship analysis including comparing between sexes (F,M,U) and developing an overall W-L relationship.  These results are in the **WEIGHT-LENGTH RELATIONSHIP** section of the manuscript.

## Helper Files
* `zzzHelpers.R` -- Helper files used in the above scripts.


# Other Files
* `KIYILepak.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
