plotEventCorrs <-
function(event.corrs, sep.plots = FALSE, ...) {

  if (length(event.corrs) == 0)  return (cat("No event.corrs available to plot."))

  groups <- rownames(event.corrs)
  sampl.intervals <- as.integer(colnames(event.corrs))

  if (sep.plots)  par(mfrow = c(1, 1))

  else {
    n.plots <- length(groups)
    root <- sqrt(n.plots)
    plot.rowcol <- c(round(root), ceiling(root))
    par(mfrow = plot.rowcol)
  }

  for(g in groups) {
    corr <- event.corrs[g, ]
    plot(corr, main = gsub("_", " ", g), ...)

  }
}
