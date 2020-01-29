sequential.corr <-
function(hotspots.list, hotspots.thresholds, comp.method = "Phi", baseline.interval = 1, baseline.gap = 0, messages = "TRUE") {
  
  # NOTE: works only for hotspots of submats made with sampl.interval (not window & gap)!
  
  #if(!(comp.method %in% binary.comp.methods))
    #stop("Invalid 'comp.method'; type 'binary.comp.methods' for available options.")

  hotspots.exclude <- hotspots.thresholds[rowSums(is.na(hotspots.thresholds)) == ncol(hotspots.thresholds), , drop = FALSE]
  excluded.groups <- paste(rownames(hotspots.exclude), " ")
  if (messages) message(length(excluded.groups), " group(s) excluded for not fulfilling thresholds: ", excluded.groups)

  hotspots.thresholds <- hotspots.thresholds[rowSums(is.na(hotspots.thresholds)) != ncol(hotspots.thresholds), ]  # excludes groups with all NA in thresholds (= groups without min.total.events)
  groups <- rownames(hotspots.thresholds)
  sampl.intervals <- colnames(hotspots.thresholds)

  event.corrs <- matrix(data = NA, nrow = length(groups), ncol = length(sampl.intervals))
  rownames(event.corrs) <- groups
  colnames(event.corrs) <- sampl.intervals

  for (grp in groups)  for (intv in sampl.intervals) {
    #submat.name <- paste(grp, ".w", i, ".g", i, sep = "")
    #baseline.name <- paste(grp, ".w", baseline.gap, ".g", baseline.gap, sep = "")
    submat.name <- paste(grp, intv, sep = ".intv")
    if (!(submat.name) %in% names(hotspots.list$hotspots.maps))  stop ("Currently, this function works only for hotspots resulting from submats created with 'sampl.interval', not 'window.size' and 'gap.size' - see Note in help(hotspot.numbers)")
    baseline.name <- paste(grp, baseline.interval, sep = ".intv")
    submat.hs <- hotspots.list$hotspots.maps[[submat.name]]$hotspot
    baseline.hs <- hotspots.list$hotspots.maps[[baseline.name]]$hotspot
    event.corrs[grp, intv] <- binary.comparison(baseline.hs, submat.hs, method = comp.method)
    event.corrs[is.na(hotspots.thresholds)] <- NA
  }  # end for g for i

  return(event.corrs)

}
