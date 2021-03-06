# compare_surf_tools
A BrainHack Project to compare thickness outputs from different pipelines run on ABIDEI

The principle deliverables of this project are:
1) a set of unified data tables for FreeSurfer (version 5.1 and 5.3) and ANTS anatomic analyses of the ABIDE 1 data, and
2) a set of R analysis scripts that reads and performs simple comparison analyses on these data tables.

## The Data Files
Consolidated from the analysis results provided at http://preprocessed-connectomes-project.org/abide/Description, we provide the following unified data tables in the 'data' directory:
* ABIDE_Phenotype.csv             ! phenotypic data for the subjects
* ABIDE_ants_thickness_data.csv   ! thickness data from ANTS analysis
* ABIDE_fs5.3_LandRvolumes.csv    ! volume data from FreeSurfer 5.3 analysis
* ABIDE_fs5.3_thickness.csv       ! thickness data from FreeSurfer 5.3 analysis
* abide_fs5.1_landrvolumes.csv    ! volume data from FreeSurfer 5.1 analysis
* cortical_fs5.1_measuresenigma_thickavg.csv ! thickness data from FreeSurfer 5.1 analysis
* subject_check.csv               ! summary table of the data available per subject

## The R Scripts
Description of the R Scripts...

In the 'analysis' diretory, there are R the following R scripts:
* abide_ct.R       ! Script to...
* data_prep.R      ! Script to Combine the data sources into one merged data frame with some added labels for grouping the results by hemisphere, lobe, etc.
* heatmap_plot.R   ! Script to check possible mis-alignment of label names
* pairs_plot.R     ! Script to create tool-pair plots with scatterplot in lower triangle, histograms on main diagonal, and correlation coefficients (Pearson) on the upper diagonal.

There are also Rmd scripts in this directory:
* double_check.Rmd      ! A script to double check that the ROIs at least matched up in the same way across the data sources.
* dx_effect_testing.Rmd ! Script to look at diagnostic effects in the thickness data.



In the 'bin' directory, there is the following Rmd script:
* LandRvolumes_comparison.Rmd  ! Script to...


### R package requirements

Some of the scripts here make use of R libraries `ggplot2`, `tidyr` and `dplyr`. The latest versions of these libraries come packaged together inside the "tidyverse" can be installed with:

```
install.packages(tidyverse)
```

### R markdown

Some of these sciprts are written in rmardown, which can be quickly converted in to html or pdf reports. The libararies for working with R markdown come packaged within the Rstudio IDE, so anybody using it should have them already. [Click here for more info on R markdown](http://rmarkdown.rstudio.com/authoring_quick_tour.html#rendering_output). We provide the R markdown scripts and also the rendered .pdf reports in this repo.

If not using Rstudio. You will need the following libraries to render the .rmd file into and .html report
```
install.packages(knitr)
install.packages(rmarkdown)
```
