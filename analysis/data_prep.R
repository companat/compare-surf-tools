# 2016-10-26
# 2016-12-21
# ABIDE analysis of average cortical thickness according to analysis method [ANTS, FS_5.1, FS_5.3]
# Source of project is git repository: https://github.com/companat/compare_surf_tools

# Goal: Combine the data sources into one merged data frame with some added labels for grouping the results by hemisphere, lobe, etc.

# new files created are:
# "analysis/abide_ct.RData"



# # # # # # # # # # # # # # # # # #
# set the local working directory that assumes the github repository is cloned as a directory called "compare-surf-tools"
# $ cd compare_surf_tools/
# ... or this, once R is running:
# > setwd("compare-surf-tools/.")
# # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # #
# Load the required package (for reading the FS lookup-table of terms)
require(readxl)
# # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# Load the datasets:
# # # # # # # # # # # # # # # # # #
# ANTS
ants <- read.csv("data/ABIDE_ants_thickness_data.csv", as.is = TRUE, skip = 2)
# remove first column
ants <- ants[-1]
# rename "Structure.Name" column of subject id to "SubjID"
names(ants)[1] <- "SubjID"
# NOTE that the labels have periods replacing the spaces: "left cuneus" -> "left.cuneus". This is how they are represented in the FreeSurfer label look-up table ("lut"; see below)

# fs_5.1 data - "cortical_fs5.1_measuresenigma_thickavg.csv"
fs51 <- read.csv("data/cortical_fs5.1_measuresenigma_thickavg.csv", as.is = TRUE)

# fs_5.3 data - "ABIDE_fs5.3_thickness.csv"
fs53 <- read.csv("data/ABIDE_fs5.3_thickness.csv", as.is = TRUE)



# # # # # # # # # # # # # # # # # #
# Load the label look-up table:
# # # # # # # # # # # # # # # # # #
# get the label names for the DKT atlas in freesurfer (with freesurfer numbers)
library(readxl)
lut <- read_excel("analysis/fs_labels.xlsx", sheet = "dkt_atlas")


# # # # # # # # # # # # # # # # # #
# reshape each dataset from "wide" to "long", paring down just the columns of interest
# # # # # # # # # # # # # # # # # #
# ANTS
ants_long <- reshape(
	ants[, c("SubjID", names(ants)[names(ants) %in% lut$ANTS_labels])],
	direction = "long",
	idvar = "SubjID",
	varying = names(ants)[names(ants) %in% lut$ANTS_labels],
	times = names(ants)[names(ants) %in% lut$ANTS_labels],
	v.names = "thickness",
	timevar = "roi"
)
# reset the row names
row.names(ants_long) <- NULL
# add a column to indicate which method was used to estimate cortical thickness:
ants_long$method <- "ANTS"


# Freesurfer version 5.1
fs51_long <- reshape(
	fs51[, c("SubjID", names(fs51)[names(fs51) %in% lut$FS_5.1_labels])],
	direction = "long",
	idvar = "SubjID",
	varying = names(fs51)[names(fs51) %in% lut$FS_5.1_labels],
	times = names(fs51)[names(fs51) %in% lut$FS_5.1_labels],
	v.names = "thickness",
	timevar = "roi"
)
# reset the row names
row.names(fs51_long) <- NULL
# add a column to indicate which method was used to estimate cortical thickness:
fs51_long$method <- "FS51"


# Freesurfer version 5.3
fs53_long <- reshape(
	fs53[, c("SubjID", names(fs53)[names(fs53) %in% lut$FS_5.3_labels])],
	direction = "long",
	idvar = "SubjID",
	varying = names(fs53)[names(fs53) %in% lut$FS_5.3_labels],
	times = names(fs53)[names(fs53) %in% lut$FS_5.3_labels],
	v.names = "thickness",
	timevar = "roi"
)
# reset the row names
row.names(fs53_long) <- NULL
# add a column to indicate which method was used to estimate cortical thickness:
fs53_long$method <- "FS53"



# # # # # # # # # # # # # # # # # #
# regularize the region of interest names and add other label information:
  # - label_name
  # - label_abbrev
  # - stem
  # - stem_abbrev
  # - lobe
  # - hemi
  # - hemi_abbrev
# # # # # # # # # # # # # # # # # #

# ANTS
ants_long <- merge(ants_long, lut, by.x = "roi", by.y = "ANTS_labels", sort = FALSE, all.x = TRUE)
# Freesurfer version 5.1
fs51_long <- merge(fs51_long, lut, by.x = "roi", by.y = "FS_5.1_labels", sort = FALSE, all.x = TRUE)
# Freesurfer version 5.3
fs53_long <- merge(fs53_long, lut, by.x = "roi", by.y = "FS_5.3_labels", sort = FALSE, all.x = TRUE)


# # # # # # # # # # # # # # # # # #
# join all the dataframes, then remove some extraneous column names
# # # # # # # # # # # # # # # # # #

abide_long <- rbind(ants_long[, 2:13], fs51_long[, 2:13], fs53_long[, 2:13])

# save this
# save(abide, file = "abide_ct.RData")

# check a random few:
# abide[sample(1:dim(abide)[1], 10),]


# 2016-10-31
# add a "Site" column based on the SubjID prefix
# unique(gsub("_[[:digit:]]{7}$", "", abide$SubjID))
 # [1] "Caltech"    "CMU_a"      "CMU_b"      "KKI"        "Leuven_1"   "Leuven_2"  
 # [7] "MaxMun_a"   "MaxMun_b"   "MaxMun_c"   "MaxMun_d"   "NYU"        "OHSU"      
# [13] "Olin"       "Pitt"       "SBL"        "SDSU"       "Stanford"   "Trinity"   
# [19] "UCLA_1"     "UCLA_2"     "UM_1"       "UM_2"       "USM"        "Yale"      
# [25] "UCLA_51232" "UCLA_51233" "UCLA_51242" "UCLA_51243" "UCLA_51244" "UCLA_51245"
# [31] "UCLA_51246" "UCLA_51247" "UCLA_51270" "UCLA_51310"

# test <- cbind(abide$SubjID, gsub("_[[:digit:]]{7}$", "", abide$SubjID), abide$method)
# test[grep("UCLA", test[ ,1]),]
# # The UCLA_????? subjects seem to be in FS51 ... do the full subjid appear in ANTS or FS53?

# abide[grep("UCLA_[[:digit:]]{5}$", abide$SubjID), c("SubjID", "method")]
# unique(abide$method[grep("UCLA_[[:digit:]]{5}$", abide$SubjID)])
# [1] "FS51"
# Nope ... only in FS51

# Do the subject suffixes occur in part in the other methods?
# abide[grepl("51232$", abide$SubjID), c("SubjID", "method")]
# abide[grepl("51245$", abide$SubjID), c("SubjID", "method")]
# abide[grepl("51310$", abide$SubjID), c("SubjID", "method")]
# These numeric suffixes seem to only occur in FS51 method.

# Try to adapt the method to 5 or more occurances of a number
# unique(gsub("_[[:digit:]]{5,}$", "", abide$SubjID))
 # [1] "Caltech"  "CMU_a"    "CMU_b"    "KKI"      "Leuven_1" "Leuven_2" "MaxMun_a" "MaxMun_b"
 # [9] "MaxMun_c" "MaxMun_d" "NYU"      "OHSU"     "Olin"     "Pitt"     "SBL"      "SDSU"    
# [17] "Stanford" "Trinity"  "UCLA_1"   "UCLA_2"   "UM_1"     "UM_2"     "USM"      "Yale"    
# [25] "UCLA"    

# Apply it to the abide dataframe:
abide_long$site <- gsub("_[[:digit:]]{5,}$", "", abide_long$SubjID)
# unique(abide$site)

# # save this
# save(abide, file = "analysis/abide_ct.RData")
# # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# To do: add phenotype data to the thickness data
phen <- read.csv("data/ABIDE_Phenotype.csv", as.is = TRUE, na.strings = "-9999")
# merge by abide$SubjID and phen$Subject_ID
abide <- merge(abide_long, phen, by.x = "SubjID", by.y = "Subject_ID", all.x = TRUE)

# # save this
save(abide, file = "analysis/abide_ct.RData")
# # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # #
# scraps
# clean up
rm(list = c("abide", "ants", "ants_long", "fs51", "fs51_long", "fs53", "fs53_long", "lut", "abide_long", "phen"))
