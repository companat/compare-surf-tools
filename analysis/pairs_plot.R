# 2016-10-31
# 2016-12-21
# ABIDE analysis of cortical thickness by method ANTS and FreeSurfer (vs. 5.1 and 5.3).
# Pairs plots with scatterplot in lower triangle, histograms on main diagonal, and correlation coefficients (Pearson) on the upper diagonal.
# Most of this is an import from an earlier test.
# ... the main difference is to now use the "long" dataframe that has an explicit "site" column

# new files created are:
# A directory called "results"
# A subdirectory called "results/pair_plots"
# PDF files of pair-wise plot of each parcellation unit pair as [unit_abbreviation].pdf


# # # # # # # # # # # # # # # # # #
# set the local working directory that assumes the github repository is cloned as a directory called "compare_surf_tools"
# insert appropriate name otherwise, such as this for the command line:
# $ cd compare-surf-tools/
# ... or this, once R is running:
# > setwd("compare-surf-tools/.")
# # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # #
# Load the required R packages
# *** none except base R ***



# load the data  (assumes "data_prep.R" has been run)
load("analysis/abide_ct.RData") # -- abide
# # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # #
# Define the plot functions
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "orange", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "pairwise.complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    # text(0.5, 0.5, txt, cex = cex.cor * r)
    text(0.5, 0.5, txt, cex = 2)
}
panel.smooth.ab <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
        abline(a = 0, b = 1, lty = 3, col = "gray50")
}
# # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # #
# Create a directory to hold the results:
dir.create("results")
dir.create("results/pair_plots")
# # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# Get the list of unique regions to plot
roi <- unique(abide$stem_abbrev)
# hemisphere abbreviaion is "hemi_abbrev"
# # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # #
# The plotting loop
for(i in seq_along(roi)){

	cat(roi[i], "\n")
	# get the data for one roi
	xdf <- subset(abide, subset = stem_abbrev %in% roi[i], select = c("SubjID", "site", "method", "thickness", "hemi_abbrev"))

	# reshape from "long" to "wide, pare out incomplete cases, and adjust the column names to remove "thickness."
	xdf_wide <- reshape(xdf, direction = "wide", idvar = c("SubjID", "hemi_abbrev", "site"), timevar = "method")
	xdf_wide <- xdf_wide[complete.cases(xdf_wide),]
	names(xdf_wide) <- gsub("thickness.", "", names(xdf_wide))

	# get the axis range limits to be used for both L and R plots
	mylim <- range(xdf$thickness, na.rm = TRUE)

	# initiate a pdf file for printing
	pdf(paste0("results/pair_plots/", roi[i], ".pdf"), height = 7, width = 7)

	# left hemi plot
	pairs(
		xdf_wide[xdf_wide$hemi_abbrev == "L", c("ANTS", "FS51", "FS53")],
		gap = 0,
		lower.panel = panel.smooth.ab,
		upper.panel = panel.cor,
		diag.panel = panel.hist,
		xlim = mylim,
		ylim = mylim,
		pch = 21,
		bg = as.factor(xdf_wide$site),
		main = paste0("L ", roi[i])
	)
	# Right hemi plot
	pairs(
		xdf_wide[xdf_wide$hemi_abbrev == "R", c("ANTS", "FS51", "FS53")],
		gap = 0,
		lower.panel = panel.smooth.ab,
		upper.panel = panel.cor,
		diag.panel = panel.hist,
		xlim = mylim,
		ylim = mylim,
		pch = 21,
		bg = as.factor(xdf_wide$site),
		main = paste0("R ", roi[i])
	)
	dev.off()	
}
# done
# # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #
# scraps and cleanup:

# # # # # # # # # # # # # # # # # # # #
# clean up
rm("abide", "i", "mylim", "panel.cor", "panel.hist", "panel.smooth.ab", "roi", "xdf", "xdf_wide")
# # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # #
# dev scraps:
# Try it with one region

# # # get the data for one roi
# xdf <- subset(abide, subset = stem_abbrev %in% roi[1], select = c("SubjID", "site", "method", "thickness", "hemi_abbrev"))

# # reshape from "long" to "wide, pare out incomplete cases, and adjust the column names to remove "thickness."
# xdf_wide <- reshape(xdf, direction = "wide", idvar = c("SubjID", "hemi_abbrev", "site"), timevar = "method")
# xdf_wide <- xdf_wide[complete.cases(xdf_wide),]
# names(xdf_wide) <- gsub("thickness.", "", names(xdf_wide))

# # get the axis range limits to be used for both L and R plots
# mylim <- range(xdf$thickness, na.rm = TRUE)

# # The actual plotting
# dev.new(height = 7, width = 7)
# pairs(
	# xdf_wide[xdf_wide$hemi_abbrev == "L", 4:6],
	# gap = 0,
	# lower.panel = panel.smooth.ab,
	# upper.panel = panel.cor,
	# diag.panel = panel.hist,
	# xlim = mylim,
	# ylim = mylim,
	# pch = 21,
	# bg = as.factor(xdf_wide$site),
	# main = paste0("L ", roi[1])
# )

# pairs(
	# xdf_wide[xdf_wide$hemi_abbrev == "R", 4:6],
	# gap = 0,
	# lower.panel = panel.smooth.ab,
	# upper.panel = panel.cor,
	# diag.panel = panel.hist,
	# xlim = mylim,
	# ylim = mylim,
	# pch = 21,
	# bg = as.factor(xdf_wide$site),
	# main = paste0("R ", roi[1])
# )

# # try printing this to a pdf file
# pdf(paste0("pair_plots/", roi[1], ".pdf"), height = 7, width = 7)
# pairs(
	# xdf_wide[xdf_wide$hemi_abbrev == "L", 4:6],
	# gap = 0,
	# lower.panel = panel.smooth.ab,
	# upper.panel = panel.cor,
	# diag.panel = panel.hist,
	# xlim = mylim,
	# ylim = mylim,
	# pch = 21,
	# bg = as.factor(xdf_wide$site),
	# main = paste0("L ", roi[1])
# )

# pairs(
	# xdf_wide[xdf_wide$hemi_abbrev == "R", 4:6],
	# gap = 0,
	# lower.panel = panel.smooth.ab,
	# upper.panel = panel.cor,
	# diag.panel = panel.hist,
	# xlim = mylim,
	# ylim = mylim,
	# pch = 21,
	# bg = as.factor(xdf_wide$site),
	# main = paste0("R ", roi[1])
# )
# dev.off()
# # works!

