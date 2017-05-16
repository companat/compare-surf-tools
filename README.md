# compare_surf_tools
A BrainHack Project to compare thickness outputs from different pipelines run on ABIDEI

## R package requirements

Some of the scripts here make use of R libraries `ggplot2`, `tidyr` and `dplyr`. The latest versions of these libraries come packaged together inside the "tidyverse" can be installed with:

install.packages(tidyverse)

## R markdown

Some of these sciprts are written in rmardown, which can be quickly converted in to html or pdf reports. The libararies for working with R markdown come packaged within the Rstudio IDE, so anybody using it should have them already. [Click here for more info on R markdown](http://rmarkdown.rstudio.com/authoring_quick_tour.html#rendering_output). We provide the R markdown scripts and also the rendered .pdf reports in this repo.

If not using Rstudio. You will need the following libraries to render teh .rmd file into and .html report
```
install.packages(knitr)
install.packages(rmarkdown)
```
