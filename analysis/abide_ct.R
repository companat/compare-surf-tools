# 2016-09-28
# ABIDE analysis of average cortical thickness according to analysis method [ANTS, FS_5.1, FS_5.3]
# Source of project is git repository: https://github.com/edickie/compare_surf_tools

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# the R syntax below is currently not written for common usage
# (see Steve Hodge <steven.hodge@umassmed.edu> for info)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# set the local working directory
setwd("~/abide/.")






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Is there a way to determine if the homologous correlation is the largest for that particular ROI?
# within each x-roi, get the correlation for it's y-roi counterpart.
# Then get the largest n correlations between x roi and all the other y-rois
# Is the x-roi : y-roi pair in the set of top n correlations?

# start clean
rm(list = ls())

# set the local working directory
setwd("~/abide/.")

# load a results data frame that was used to create the heatmap plot
load("corrs_fs51_fs53.RData") #-- results_df
roi <- unique(results_df[, 1])

# results for x-y pairs
res_pair <- subset(results_df, results_df[, 1] == results_df[, 2], select = 1:3)

res_ordered <- results_df[order(results_df[results_df[,1] == roi[1], 3], decreasing = TRUE), c(1:3)]

# Merge can be used to see if a particular row in res_pair is among the (say) top-5 rows of res_ordered:
merge(res_pair[1,], res_ordered[1:5,])
    # FS51_roi   FS53_roi   r_value
# 1 L.cauantCG L.cauantCG 0.8976968
merge(res_pair[1,], res_ordered[11:16,])
# [1] FS51_roi FS53_roi r_value 
# <0 rows> (or 0-length row.names)

# The row dimension is 1 if it is in the subset, 0 otherwise
dim(merge(res_pair[1,], res_ordered[1:5,]))
# [1] 1 3
dim(merge(res_pair[1,], res_ordered[11:16,]))
# [1] 0 3
# So this could be the test of "Top-n"


# NOTE: ordered() returns positions, not row numbers, so ordering a subset of a dataframe cannot be used at the same time as trying to extract a subset. Get the subset first, then order the rows:
res_ordered <- results_df[results_df[,1] == roi[2], 1:3]
res_ordered <- res_ordered[order(res_ordered[, 3], decreasing = TRUE), ]
n <- 5
dim(merge(res_pair[2,], res_ordered[1:n,]))[1] >= 1
# [1] TRUE
# Is it the top-ranked position?
dim(merge(res_pair[2,], res_ordered[1,]))[1] >= 1
# [1] TRUE


# run this in a loop?
load("corrs_fs51_fs53.RData") #-- results_df
# get results for each x-y pair
res_pair <- subset(results_df, results_df[, 1] == results_df[, 2], select = 1:3)
roi <- unique(results_df[, 1])
top_rank <- 5
rank_test <- list()
for(i in seq_along(res_pair[,1])){
	
	x <- results_df[results_df[,1] == roi[i], 1:3]
	xo <- x[order(x[, 3], decreasing = TRUE), ]

	rank_test <- c(rank_test, 
		paste(
			dim(merge(res_pair[i,], xo[1,]))[1] >= 1,
			dim(merge(res_pair[i,], xo[1:top_rank,]))[1] >= 1,
			sep = ","
		)
	)
}
rank_test.df <- data.frame(do.call(rbind, strsplit(unlist(rank_test), split = ",")), stringsAsFactors = FALSE)
names(rank_test.df) <- c("top_rank", paste0("in_top_", top_rank, "_rank"))
rank_test.df
# How to interpret these results?
apply(rank_test.df, 2, function(x) prop.table(table(x)))
     # top_rank in_top_5_rank 
            # 1             1 
write.csv(cbind(res_pair, rank_test.df), file = "corr_rank_fs51_fs53.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# do it again for ants v fs5.1
load("corrs_ants_fs51.RData") #-- results_df

# get results for x-y pairs
res_pair <- subset(results_df, results_df[, 1] == results_df[, 2], select = 1:3)

roi <- unique(results_df[, 1])
top_rank <- 10
rank_test <- list()
for(i in seq_along(res_pair[,1])){
	
	x <- results_df[results_df[,1] == roi[i], 1:3]
	xo <- x[order(x[, 3], decreasing = TRUE), ]

	rank_test <- c(rank_test, 
		paste(
			dim(merge(res_pair[i,], xo[1,]))[1] >= 1,
			dim(merge(res_pair[i,], xo[1:top_rank,]))[1] >= 1,
			sep = ","
		)
	)
}
rank_test.df <- data.frame(do.call(rbind, strsplit(unlist(rank_test), split = ",")), stringsAsFactors = FALSE)
names(rank_test.df) <- c("top_rank", paste0("in_top_", top_rank, "_rank"))
rank_test.df
apply(rank_test.df, 2, function(x) prop.table(table(x)))
       # top_rank in_top_10_rank
# FALSE 0.4193548      0.1129032
# TRUE  0.5806452      0.8870968

write.csv(cbind(res_pair, rank_test.df), file = "corr_rank_ants_fs51.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# do it again for ants v fs5.3
load("corrs_ants_fs53.RData") #-- results_df

# get results for x-y pairs
res_pair <- subset(results_df, results_df[, 1] == results_df[, 2], select = 1:3)

roi <- unique(results_df[, 1])
top_rank <- 10
rank_test <- list()
for(i in seq_along(res_pair[,1])){
	
	x <- results_df[results_df[,1] == roi[i], 1:3]
	xo <- x[order(x[, 3], decreasing = TRUE), ]

	rank_test <- c(rank_test, 
		paste(
			dim(merge(res_pair[i,], xo[1,]))[1] >= 1,
			dim(merge(res_pair[i,], xo[1:top_rank,]))[1] >= 1,
			sep = ","
		)
	)
}
rank_test.df <- data.frame(do.call(rbind, strsplit(unlist(rank_test), split = ",")), stringsAsFactors = FALSE)
names(rank_test.df) <- c("top_rank", paste0("in_top_", top_rank, "_rank"))
rank_test.df
# How to interpret these results?
apply(rank_test.df, 2, function(x) prop.table(table(x)))
      # top_rank in_top_10_rank
# FALSE 0.516129      0.1451613
# TRUE  0.483871      0.8548387
# So ... 48% of the roi pairs have the highest correlation, 85% are in the top 10 of all other xroi - nroi pairs.
write.csv(cbind(res_pair, rank_test.df), file = "corr_rank_ants_fs53.csv", row.names = FALSE)


# histograms of the results_df files
load("corrs_ants_fs51.RData") #-- results_df
dfa <- results_df
dfa_pair <- subset(dfa, dfa[, 1] == dfa[, 2], select = 1:3)
load("corrs_ants_fs53.RData") #-- results_df
dfb <- results_df
dfb_pair <- subset(dfb, dfb[, 1] == dfb[, 2], select = 1:3)
load("corrs_fs51_fs53.RData") #-- results_df
dfc <- results_df
dfc_pair <- subset(dfc, dfc[, 1] == dfc[, 2], select = 1:3)

# plot the R-values as histograms
mylim <- range(dfa$r_value, dfb$r_value, dfc$r_value)

dev.new(height = 10, width = 6)
par(mfcol = c(3, 2))
hist(dfa$r_value, xlim = mylim, col = "skyblue", main = "ANTS v FS5.1", xlab = "r values")
hist(dfb$r_value, xlim = mylim, col = "skyblue", main = "ANTS v FS5.3", xlab = "r values")
hist(dfc$r_value, xlim = mylim, col = "skyblue", main = "FS5.1 v FS5.3", xlab = "r values")
hist(dfa_pair$r_value, xlim = mylim, col = "red", main = "ANTS v FS5.1 (diagonal)", xlab = "r values")
hist(dfb_pair$r_value, xlim = mylim, col = "red", main = "ANTS v FS5.1 (diagonal)", xlab = "r values")
hist(dfc_pair$r_value, xlim = mylim, col = "red", main = "ANTS v FS5.1 (diagonal)", xlab = "r values")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2016-11-01
# Get histograms of the roi-pair correlation coefficients.
# Put density estimates for ANTS-FS51, ANTS-FS53, and FS51-FS53 on the same plot.

# start with the results of the one-to-all correlations
# Load each dataset and merge them.
load("corrs_ants_fs51.RData")
df_A <- results_df[results_df$ANTS_roi == results_df$FS51_roi, 1:3]
load("corrs_ants_fs53.RData")
df_B <- results_df[results_df$ANTS_roi == results_df$FS53_roi, 1:3]
load("corrs_fs51_fs53.RData")
df_C <- results_df[results_df$FS51_roi == results_df$FS53_roi, 1:3]
corr_dat <- cbind(df_A[,c(1, 3)], df_B$r_value, df_C$r_value)

# adjust the column names
row.names(corr_dat) <- NULL
names(corr_dat) <- c("roi", "ants_fs51", "ants_fs53", "fs51_fs53")

# put this in "long" format
corr_dat_long <- reshape(corr_dat, direction = "long", idvar = "roi", varying = c(2:4), v.names = "corr", timevar = "method", times = names(corr_dat)[2:4])
row.names(corr_dat_long) <- NULL

# Now for the density plotting:
library(ggplot2)
dp <- ggplot(corr_dat_long, aes(x = corr, color = method, fill = method)) + 
	geom_density(alpha = 0.5) + 
	labs(title = "Distribution of correlations coefficients\nfor PU-wise comparisons of 3 methods of estimating\nmean cortical thickness in 62 parcellation units")
print(dp)
ggsave(file = "pair_plots/corr_density_plot.pdf", height = 5, width = 6)

# maybe small boxplot below graph to show that info?
bp <- ggplot(corr_dat_long, aes(x = method, y = corr, color = method, fill = method)) + 
	geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
	coord_flip() + 
	theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
	
dev.new(height = 1)
print(bp)
# Nah ...

# summarise the corrs in a table?
aggregate(corr ~ method, data = corr_dat_long, FUN = mean)
aggregate(corr ~ method, data = corr_dat_long, FUN = sd)
aggregate(corr ~ method, data = corr_dat_long, FUN = median)
aggregate(corr ~ method, data = corr_dat_long, FUN = t.test)

summary_table <- lapply(unique(corr_dat_long$method), function(x) summary(corr_dat_long$corr[corr_dat_long$method %in% x]))
names(summary_table) <- unique(corr_dat_long$method)
summary_table
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
