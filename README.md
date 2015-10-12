KIYILepak
=========

This repository contains data files, analysis scripts, and the draft manuscript for *Age Estimation for Lake Superior Kiyi (Coregonus kiyi)* by Taylor A. Lepak, Derek H. Ogle, and Mark R. Vinson that will be submitted to the Journal of Great Lakes Research.


# Data files (not in the repo)
* `data/KIYILepak_2014.xlsx` -- An excel file that contains two worksheets ...
    * `Ageing` -- Data for individual fish that were sampled for ageing purposes.
    * `LenFreq` -- Lengths of all captured Kiyi from 1972 to 2014.
* `data/kiyiLens.csv` -- CSV file that contains lengths from `LenFreq` worksheet of  `data/KIYILepak_2014.xlsx` that have been expanded.  Created in `LFdata_Create.R` and used in `LF_Analysis.R` and `Basic_Summaries.R`.


# Scripts
## Initialization
* `Data_Init.R` -- Initial loading of packages and data, initial preparation of data for other scripts.  This is sourced by all other scripts except `Create_LFdata.R` and creates the following data.frames.
    * `pwfLens`: Used in the length frequency analysis.
    * `pwfWL`: Used in the weight-length analysis, length frequency analysis, and sex-ratio analysis.
    * `pwfAgeS`: Used in the scale age comparisons analysis.
    * `pwfAgeO`: Used in the otolith age comparisons analysis.
    * `pwfAgeSO`: Used in the scale-otolith age comparisons analysis.
    * `pwfGrow: Used in the growth analysis.`

## Length Frequency Analysis
* `LF_Create_data.R` -- Process the original length frequency data to produce `kiyiLens.csv` which is then loaded in the `LF_Analysis.R` script.  This is only run once as running it multiple times seems to run into a memory problem that crashes R/RStudio.
* `LF_Analysis.R` -- Length frequency analysis for last XX years of data.  This makes **Figure XX** in the manuscript.

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
