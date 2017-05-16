# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2016-10-26
# 2016-12-21# correlation heatmaps of ANTS vars with FS5.1 vars
# # ... to check possible mis-alignment of label names
# 1. get correlations of each unique ants labels with each unique fs5.1 label
# 2. get pairwise correlations
# 3. grab list of r-values and p-values
# 4. plot this list as a heatmap

# vars of interest: roi = unique(abide$label_abbrev)
# columns of results:
# xroi = ants roi
# yroi = fs5.1 roi
# rval = pearson correlation
# pval = p-value of pearson correlation

# new files created are:
# 1. The subdirectory "results/heatmaps/" with the files:
  # heatmap_ants_fs51.pdf  # heatmap_ants_fs53.pdf  # heatmap_fs51_fs53.pdf
# 2. Dataframes of the pairwise correlation results (i.e. the numbers behind the colors in the heatmaps) in results/.
  # corrs_ants_fs51.RData  # corrs_ants_fs53.RData  # corrs_fs51_fs53.RData



# # # # # # # # # # # # # # # # # #
# set the local working directory that assumes the github repository is cloned as a directory called "compare-surf-tools"
# insert appropriate name otherwise, such as this for the command line:
# $ cd compare-surf-tools/
# ... or this, once R is running:
# > setwd("compare-surf-tools/.")
# # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Load the required R packages
require(ggplot2)
require(RColorBrewer)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# load the dataset (assumes "data_prep.R" and "pairs_plot.R" has been run)
load("analysis/abide_ct.RData") # -- abide
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# get list of unique parcellation unit names
roi <- unique(abide$label_abbrev)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# location of the plot files
dir.create("results/heatmaps")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Analysis of  ANTS vs FS 5.1

results_list <- list()

for(i in seq_along(roi)){

	# get the ANTS data for roi[i]
	adf <- subset(abide, method == "ANTS" & label_abbrev == roi[i], select = c("SubjID", "thickness", "label_abbrev"))
	cat(roi[i])

	for(j in seq_along(roi)){
		
		# get the fs51 data for roi[j]
		fsdf <- subset(abide, method == "FS51" & label_abbrev == roi[j], select = c("SubjID", "thickness", "label_abbrev"))
		
		# merge the data frames
		ijdf <- merge(adf, fsdf, by = "SubjID")
		
		# Run the correlation and get the rval and pval
		ctest <- cor.test(~ thickness.x + thickness.y, data = ijdf)
		rval <- ctest$estimate[[1]]
		pval <- ctest$p.value

		# paste the output into the results list
		results_list <- c(results_list, paste(roi[i], roi[j], rval, pval, sep = ","))
		# ... and the terminal window
		cat(".")
	}
	cat("\n")
}
# convert the results to a data frame
results_df <- data.frame(do.call(rbind, strsplit(unlist(results_list), split = ",")), stringsAsFactors = FALSE)
results_df[, 3] <- as.numeric(results_df[, 3])
results_df[, 4] <- as.numeric(results_df[, 4])
names(results_df) <- c("ANTS_roi", "FS51_roi", "r_value", "p_value")
results_df$ANTS_roi <- factor(results_df$ANTS_roi)
results_df$FS51_roi <- factor(results_df$FS51_roi)
save(results_df, file = "results/corrs_ants_fs51.RData")
# save as csv file as well
write.csv(results_df, file = "results/corrs_ants_fs51.csv", row.names = FALSE)
# reverse the levels of FS5.1_roi for top-left plotting
results_df$FS51_roi <- factor(results_df$FS51_roi, levels = rev(levels(results_df$FS51_roi)))

# plot the results as a heatmap matrix ... use r_value^2 (i.e. R-Squared) as the whitening term.
library(ggplot2)
library(RColorBrewer)

# dev.new(width = 9, height = 9)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
hm1 <- ggplot(results_df, aes(x = ANTS_roi, y = FS51_roi, fill = r_value, alpha = r_value^2)) +
	geom_tile() + 
	scale_fill_gradientn(name = "Correlation", colours = myPalette(100), limits = c(-1, 1)) +
	guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + 
	# scale_alpha_continuous(name = "R-Squared", range = c(0, 1), limits = c(0, 1)) +
	scale_alpha_continuous(name = "R-Squared") +
	coord_equal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
	labs(x = "ANTS roi", y = "FS 5.1 roi", title = "Correlations among DKT PU thicknesses estimated by ANTS and FS 5.1") + 
	theme(panel.grid.major = element_blank(), panel.background = element_rect(color = "black", fill = "white"))
ggsave(hm1, filename = "results/heatmaps/heatmap_ants_fs51.pdf", width = 9, height = 9)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Do this again for ANTS vs FS 5.3
# optional:
# setwd("~/abide/")
# load("abide_ct.RData") #- abide
roi <- unique(abide$label_abbrev)

results_list <- list()

for(i in seq_along(roi)){

	# get the ANTS data for roi[i]
	adf <- subset(abide, method == "ANTS" & label_abbrev == roi[i], select = c("SubjID", "thickness", "label_abbrev"))
	cat(roi[i])

	for(j in seq_along(roi)){
		
		# get the fs53 data for roi[j]
		fsdf <- subset(abide, method == "FS53" & label_abbrev == roi[j], select = c("SubjID", "thickness", "label_abbrev"))
		
		# merge the data frames
		ijdf <- merge(adf, fsdf, by = "SubjID")
		
		# Run the correlation and get the rval and pval
		ctest <- cor.test(~ thickness.x + thickness.y, data = ijdf)
		rval <- ctest$estimate[[1]]
		pval <- ctest$p.value

		# paste the output into the results list
		results_list <- c(results_list, paste(roi[i], roi[j], rval, pval, sep = ","))
		# ... and the terminal window
		cat(".")
	}
	cat("\n")
}
# convert the results to a data frame
results_df <- data.frame(do.call(rbind, strsplit(unlist(results_list), split = ",")), stringsAsFactors = FALSE)
results_df[, 3] <- as.numeric(results_df[, 3])
results_df[, 4] <- as.numeric(results_df[, 4])
names(results_df) <- c("ANTS_roi", "FS53_roi", "r_value", "p_value")
results_df$ANTS_roi <- factor(results_df$ANTS_roi)
results_df$FS53_roi <- factor(results_df$FS53_roi)
save(results_df, file = "results/corrs_ants_fs53.RData")
# save as csv file as well
write.csv(results_df, file = "results/corrs_ants_fs53.csv", row.names = FALSE)
# reverse the levels of FS5.3_roi for top-left plotting
results_df$FS53_roi <- factor(results_df$FS53_roi, levels = rev(levels(results_df$FS53_roi)))

# plot the results as a heatmap matrix ... use r_value^2 (i.e. R-Squared) as the whitening term.
library(ggplot2)
library(RColorBrewer)

# dev.new(width = 9, height = 9)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
hm2 <- ggplot(results_df, aes(x = ANTS_roi, y = FS53_roi, fill = r_value, alpha = r_value^2)) +
	geom_tile() + 
	scale_fill_gradientn(name = "Correlation", colours = myPalette(100), limits = c(-1, 1)) +
	guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + 
	# scale_alpha_continuous(name = "R-Squared", range = c(0, 1), limits = c(0, 1)) +
	scale_alpha_continuous(name = "R-Squared") +
	coord_equal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
	labs(x = "ANTS roi", y = "FS 5.3 roi", title = "Correlations among DKT PU thicknesses estimated by ANTS and FS 5.3") + 
	theme(panel.grid.major = element_blank(), panel.background = element_rect(color = "black", fill = "white"))
ggsave(hm2, filename = "results/heatmaps/heatmap_ants_fs53.pdf", width = 9, height = 9)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Do this again for FS 5.1 vs FS 5.3
# optional:
# setwd("~/abide/")
# load("abide_ct.RData") #- abide
roi <- unique(abide$label_abbrev)

results_list <- list()

for(i in seq_along(roi)){

	# get the FS51 data for roi[i]
	adf <- subset(abide, method == "FS51" & label_abbrev == roi[i], select = c("SubjID", "thickness", "label_abbrev"))
	cat(roi[i])

	for(j in seq_along(roi)){
		
		# get the fs53 data for roi[j]
		fsdf <- subset(abide, method == "FS53" & label_abbrev == roi[j], select = c("SubjID", "thickness", "label_abbrev"))
		
		# merge the data frames
		ijdf <- merge(adf, fsdf, by = "SubjID")
		
		# Run the correlation and get the rval and pval
		ctest <- cor.test(~ thickness.x + thickness.y, data = ijdf)
		rval <- ctest$estimate[[1]]
		pval <- ctest$p.value

		# paste the output into the results list
		results_list <- c(results_list, paste(roi[i], roi[j], rval, pval, sep = ","))
		# ... and the terminal window
		cat(".")
	}
	cat("\n")
}
# convert the results to a data frame
results_df <- data.frame(do.call(rbind, strsplit(unlist(results_list), split = ",")), stringsAsFactors = FALSE)
results_df[, 3] <- as.numeric(results_df[, 3])
results_df[, 4] <- as.numeric(results_df[, 4])
names(results_df) <- c("FS51_roi", "FS53_roi", "r_value", "p_value")
results_df$FS51_roi <- factor(results_df$FS51_roi)
results_df$FS53_roi <- factor(results_df$FS53_roi)
save(results_df, file = "results/corrs_fs51_fs53.RData")
# save as csv file as well
write.csv(results_df, file = "results/corrs_fs51_fs53.csv", row.names = FALSE)
# reverse the levels of FS5.3_roi for top-left plotting
results_df$FS53_roi <- factor(results_df$FS53_roi, levels = rev(levels(results_df$FS53_roi)))

# plot the results as a heatmap matrix ... use r_value^2 (i.e. R-Squared) as the whitening term.
library(ggplot2)
library(RColorBrewer)

# dev.new(width = 9, height = 9)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
hm3 <- ggplot(results_df, aes(x = FS51_roi, y = FS53_roi, fill = r_value, alpha = r_value^2)) +
	geom_tile() + 
	scale_fill_gradientn(name = "Correlation", colours = myPalette(100), limits = c(-1, 1)) +
	guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + 
	# scale_alpha_continuous(name = "R-Squared", range = c(0, 1), limits = c(0, 1)) +
	scale_alpha_continuous(name = "R-Squared") +
	coord_equal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
	labs(x = "FS 51 roi", y = "FS 5.3 roi", title = "Correlations among DKT PU thicknesses estimated by FS 5.1 and FS 5.3") + 
	theme(panel.grid.major = element_blank(), panel.background = element_rect(color = "black", fill = "white"))
ggsave(hm3, filename = "results/heatmaps/heatmap_fs51_fs53.pdf", width = 9, height = 9)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Clean up and scraps
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# clean up
rm(list = c("abide", "adf", "ctest", "fsdf", "i", "ijdf", "j", "myPalette", "pval", "results_df", "results_list", "roi" , "rval", "hm1", "hm2", "hm3"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# check a few of these correlations for accuracy of the script
# xdat <- subset(abide, method == "ANTS" & label_abbrev == "R.ENT", select = c("SubjID", "thickness", "label_abbrev"))
# ydat <- subset(abide, method == "FS51" & label_abbrev == "R.rosmidF", select = c("SubjID", "thickness", "label_abbrev"))
# mydat <- merge(xdat, ydat, by = "SubjID")
# cor.test(~ thickness.x + thickness.y, data = mydat)

# xdat <- subset(abide, method == "ANTS" & label_abbrev == "R.supT", select = c("SubjID", "thickness", "label_abbrev"))
# ydat <- subset(abide, method == "FS53" & label_abbrev == "R.supT", select = c("SubjID", "thickness", "label_abbrev"))
# mydat <- merge(xdat, ydat, by = "SubjID")
# cor.test(~ thickness.x + thickness.y, data = mydat)

# xdat <- subset(abide, method == "FS51" & label_abbrev == "L.infP", select = c("SubjID", "thickness", "label_abbrev"))
# ydat <- subset(abide, method == "FS53" & label_abbrev == "L.cauantCG", select = c("SubjID", "thickness", "label_abbrev"))
# mydat <- merge(xdat, ydat, by = "SubjID")
# cor.test(~ thickness.x + thickness.y, data = mydat)
# Seem okay.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
