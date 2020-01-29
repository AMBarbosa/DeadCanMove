hotspot.numbers <-
function(hotspots.list, sampl.intervals, groups, include.all.together = TRUE, min.total.events = 0, min.hotspot.threshold = 2) {
  
  # NOTE: works only for hotspots of submats created with sampl.interval (not window & gap)!

  #if (!is.integer(sampl.intervals) | sampl.intervals[1] != 1 | rle(diff(sampl.intervals))$values != 1) stop("Under the current implementation, sampl.intervals must be a vector of consecutive integers starting with 1.")

  if (!is.null(groups))  groups <- as.character(groups)
  if (include.all.together)  groups <- c(groups, "ALL")
  N.events <- HS.threshold <- N.hotspots <- events.in.HS <- matrix(data = NA, nrow = length(groups), ncol = length(sampl.intervals))
  rownames(N.events) <- rownames(HS.threshold) <- rownames(N.hotspots) <- rownames(events.in.HS) <- groups
  colnames(N.events) <- colnames(HS.threshold) <- colnames(N.hotspots) <- colnames(events.in.HS) <- sampl.intervals  #paste0("intv", sampl.intervals)

  #submat.name.split <- strsplit(names(submats), split = "\\.")
  #if (length(submat.name.split[[1]]) == 2)  submat.name <- paste(g, i, sep = ".intv")
  #else if (length(submat.name.split[[1]]) == 3) submat.name <- paste0(g, ".w", i, ".g", i)
  #else if (length(submat.name.split[[1]]) == 4) submat.name <- paste0(g, ".w", i, ".g", i, ".s", i)
  
  for (grp in groups)  for (intv in sampl.intervals) {
    #submat.name <- paste(g, ".w", i, ".g", i, sep = "")
    submat.name <- paste(grp, intv, sep = ".intv")
    if (!(submat.name) %in% names(hotspots.list$hotspots.maps))  stop ("Currently, this function works only for hotspots resulting from submats created with 'sampl.interval', not 'window.size' and 'gap.size' - see Note in help(hotspot.numbers)")
    N.events[grp, intv] <- sum(hotspots.list$hotspots.maps[[submat.name]]$total.events)
    HS.threshold[grp, intv] <- hotspots.list$hotspots.thresholds[submat.name]
    N.hotspots[grp, intv] <- sum(hotspots.list$hotspots.maps[[submat.name]]$hotspot)
    events.in.HS[grp, intv] <- sum(hotspots.list$hotspots.maps[[submat.name]]$total.events [hotspots.list$hotspots.maps[[submat.name]]$hotspot == 1])
  }

  n.below.min <- N.events[ , 1] < min.total.events
  thresh.below.min <- HS.threshold < min.hotspot.threshold
  N.hotspots[n.below.min | thresh.below.min] <- NA
  HS.threshold[n.below.min | thresh.below.min] <- NA
  events.in.HS[n.below.min | thresh.below.min] <- NA

  return(list(N.events = N.events, HS.threshold = HS.threshold, N.hotspots = N.hotspots, events.in.HS = events.in.HS))
}
