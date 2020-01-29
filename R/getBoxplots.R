getBoxplots <-
function(corrs.list, ...) {
  # bind all tables in the list into one:
  corrs.list.df <- Reduce(cbind, corrs.list)
  # remove "p-value" columns to keep only "corr":
  corrs.list.df <- corrs.list.df[ , seq(1, ncol(corrs.list.df), 2)]
  # give each column the name of the group:
  colnames(corrs.list.df) <- names(corrs.list)
  # plot the corrs:
  par(mar = c(7, 4, 4, 2))
  boxplot(corrs.list.df, ...)
  abline(h = 0.7, lty = 2, col = "darkgrey")
}
