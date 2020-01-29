repl.hs.comp <- function(seqsubmats.hs, hs.baseline, method = "Phi", stats = TRUE, plot = TRUE, plot.mean = TRUE, ylim = NULL, horiz.line = NA) {
  
  groups <- sapply(strsplit(names(hs.baseline[[1]]), split = "\\."), `[`, 1)
  replicate.corrs.bygroup <- vector("list", length(groups))
  names(replicate.corrs.bygroup) <- groups
  
  for (gr in 1:length(groups)) {
    replicate.corrs.list <- vector("list", length(seqsubmats.hs))
    names(replicate.corrs.list) <- paste0(names(seqsubmats.hs), ".corrs")
    for (g in 1:length(seqsubmats.hs)) {
      df.name.split <- strsplit(names(seqsubmats.hs[[g]][[1]]), split = "\\.")
      replicates.ind <- grep(groups[gr], df.name.split)
      baseline.ind <- grep(groups[gr], names(hs.baseline$hotspots.maps))
      #replicate.corrs <- vector(numeric, length(replicates.ind))
      replicate.corrs <- NULL
      for (r in replicates.ind) {
        replicate.corrs[r] <- binary.comparison(hs.baseline$hotspots.maps[baseline.ind][[1]][ , "hotspot"], seqsubmats.hs[[g]][[2]][[r]][["hotspot"]], method = method)
        replicate.corrs <- replicate.corrs[!is.na(replicate.corrs)]  # tirei os NAs, que eram feios
      }; rm(r)
      replicate.corrs.list[[g]] <- replicate.corrs
    }; rm(g)
    replicate.corrs.bygroup[[gr]] <- replicate.corrs.list
  }; rm(gr)
  
  result <- replicate.corrs.bygroup
  
  if (stats) {
    comp.mean <- comp.min <- comp.max <- comp.sd <- vector("list", length(groups))
    names(comp.mean) <- names(comp.min) <- names(comp.max) <- names(comp.sd) <- groups
    
    for (gr in groups) {
      rep.corr <- replicate.corrs.bygroup[[gr]]
      comp.mean[[gr]] <- sapply(rep.corr, mean, na.rm = TRUE)
      comp.min[[gr]] <- sapply(rep.corr, min, na.rm = TRUE)
      comp.max[[gr]] <- sapply(rep.corr, max, na.rm = TRUE)
      comp.sd[[gr]] <- sapply(rep.corr, sd, na.rm = TRUE)
      names(comp.mean[[gr]]) <- names(comp.min[[gr]]) <- names(comp.max[[gr]]) <- names(comp.sd[[gr]]) <- paste0("g", 1:30)
      
      if (plot) {
        yy <- range(unlist(rep.corr)[is.finite(unlist(rep.corr))])
        if (!is.finite(yy)) yy <- c(0, 0)
        xx <- c(1, length(rep.corr))
        plot(x = xx, y = yy, type = "n", main = gr, xlab = "gap", ylab = method, ylim = ylim)
        for (g in 1:length(rep.corr)) {
          y <- rep.corr[[g]]
          x <- rep(g, length(y))
          points(x, y, pch = 20, cex = 0.5, col = "grey")
        }; rm(g)
      if (plot.mean) points(comp.mean[[gr]], pch = 1)
      if (is.numeric(horiz.line)) abline(h = horiz.line, col = "grey", lty = 5)
      }  # end if plot
    }; rm(gr)
    
    result <- c(repl.corrs = result, repl.corr.stats = list(mean = data.frame(comp.mean), min = data.frame(comp.min), max = data.frame(comp.max), sd = data.frame(comp.sd)))
    
  }  # end if stats
  
  return (result)
}
