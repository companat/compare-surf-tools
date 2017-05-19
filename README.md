# compare_surf_tools
A BrainHack Project to compare thickness outputs from different pipelines run on ABIDEI

The principle deliverables of this project are:
1) a set of unified data tables for FreeSurfer (version 5.1 and 5.3) and ANTS anatomic analyses of the ABIDE 1 data, and
2) a set of R analysis scripts that reads and performs simple comparison analyses on these data tables.

## The Data Files
Consolidated from the analysis results provided at http://preprocessed-connectomes-project.org/abide/Description, we provide the following unified data tables:
* ABIDE_Phenotype.csv             ! phenotypic data for the subjects
* ABIDE_ants_thickness_data.csv   ! thickness data from ANTS analysis
* ABIDE_fs5.3_LandRvolumes.csv    ! volume data from FreeSurfer 5.3 analysis
* ABIDE_fs5.3_thickness.csv       ! thickness data from FreeSurfer 5.3 analysis
* abide_fs5.1_landrvolumes.csv    ! volume data from FreeSurfer 5.1 analysis
* cortical_fs5.1_measuresenigma_thickavg.csv ! thickness data from FreeSurfer 5.1 analysis
* subject_check.csv               ! summary table of the data available per subject

## The R Scripts
Description of the R Scripts...

### R package requirements

Some of the scripts here make use of R libraries `ggplot2`, `tidyr` and `dplyr`. The latest versions of these libraries come packaged together inside the "tidyverse" can be installed with:

```
install.packages(tidyverse)
```

### R markdown

Some of these sciprts are written in rmardown, which can be quickly converted in to html or pdf reports. The libararies for working with R markdown come packaged within the Rstudio IDE, so anybody using it should have them already. [Click here for more info on R markdown](http://rmarkdown.rstudio.com/authoring_quick_tour.html#rendering_output). We provide the R markdown scripts and also the rendered .pdf reports in this repo.

If not using Rstudio. You will need the following libraries to render teh .rmd file into and .html report
```
install.packages(knitr)
install.packages(rmarkdown)
```
