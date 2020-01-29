schemeCorrs <-
function(dataset, submats, submats.baseline, region.column, group.column, first.subsampl.col) {
  seqnevents <- sequential.Nevents(dataset = dataset, submats = submats, region.column = region.column, first.subsampl.col = first.subsampl.col)
  seqnevents.baseline <- sequential.Nevents(dataset = dataset, submats = submats.baseline, region.column = region.column, first.subsampl.col = first.subsampl.col)
  groups <- unique(dataset[ , group.column])
  corrs.tables <- vector("list", length = length(groups))
  names(corrs.tables) <- groups
  for (grp in groups) {
    scenarios.ind <- grep(grp, names(seqnevents))
    baseline.ind <- grep(grp, names(seqnevents.baseline))
    n.events.baseline <- seqnevents.baseline[[baseline.ind]]$total.events
    #n.events.scenarios <- matrix(nrow = nrow(base.seqnevents[[1]]), ncol = length(scenarios.ind))
    corrs.tables[[grp]] <- matrix(nrow = length(scenarios.ind), ncol = 2)
    colnames(corrs.tables[[grp]]) <- c("corr", "p")
    row.names(corrs.tables[[grp]]) <- scenarios.ind
    for (s in scenarios.ind) {
      corr <- cor.test(seqnevents[[s]]$total.events, n.events.baseline, method = "spearman")
      corrs.tables[[grp]][as.character(s), "corr"] <- corr$estimate
      corrs.tables[[grp]][as.character(s), "p"] <- corr$p.value
    }; rm(s)
  }; rm(grp)
  cat("Results saved as list. Ranges of values:\n\n")
  for (i in 1:length(corrs.tables))  cat(names(corrs.tables)[i], "| Rho:", round(range(corrs.tables[[i]][,1]), 2), "| p-value:", round(range(corrs.tables[[i]][,2]), 2), "\n")
  return(invisible(corrs.tables))
}
