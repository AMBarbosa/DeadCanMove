hotspots.comparison <- function(dataset, 
                                sampl.columns, 
                                sampl.intervals, 
                                region.column, 
                                group.column, 
                                include.all.together = TRUE, 
                                confidence = 0.95, 
                                min.total.events = 80, 
                                min.hotspot.threshold = 2, 
                                comp.method = "Phi", 
                                plot = TRUE, 
                                sep.plots = FALSE, 
                                omit.baseline.interval = TRUE, 
                                ...) {
  
  if (!is.integer(sampl.intervals) | sampl.intervals[1] != 1 | rle(diff(sampl.intervals))$values != 1) stop("Under the current implementation, sampl.intervals must be a vector of consecutive integers starting with 1.")
  
#  stopifnot(
#    sampl.columns %in% 1:ncol(dataset),
#    sampl.intervals %in% 1:length(sampl.columns),
#    region.column %in% 1:ncol(dataset) | region.column %in% names(dataset),
#    group.column %in% 1:ncol(dataset) | group.column %in% names(dataset) | is.null(group.column),
#    confidence > 0 & confidence < 1,
#    comp.method %in% binary.comp.methods(),
#  )
  
  submatrices <- sequential.submatrix(dataset = dataset, sampl.columns = sampl.columns, sampl.intervals = sampl.intervals, group.column = group.column, include.all.together = include.all.together, remove.zeros = FALSE, keep.nonsampl.columns = TRUE)
  
  hotspots.list <- sequential.hotspots(dataset = dataset, submats = submatrices, region.column = region.column, first.subsampl.col = sampl.columns[1], confidence = confidence)
  
  hs.numbers <- hotspot.numbers(hotspots.list = hotspots.list, sampl.intervals = sampl.intervals, groups = unique(dataset[ , group.column]), include.all.together = include.all.together, min.total.events = min.total.events, min.hotspot.threshold = min.hotspot.threshold)
  
  event.corrs <- sequential.corr(hotspots.list = hotspots.list, hotspots.thresholds = hs.numbers$HS.threshold, comp.method = comp.method, baseline.interval = 1)
  
  event.loss <- sequential.corr(hotspots.list = hotspots.list, hotspots.thresholds = hs.numbers$HS.threshold, comp.method = "loss", baseline.interval = 1, messages = "FALSE")

  event.gain <- sequential.corr(hotspots.list = hotspots.list, hotspots.thresholds = hs.numbers$HS.threshold, comp.method = "gain", baseline.interval = 1, messages = "FALSE")

  event.balance <- sequential.corr(hotspots.list = hotspots.list, hotspots.thresholds = hs.numbers$HS.threshold, comp.method = "balance", baseline.interval = 1, messages = "FALSE")
  
  if (omit.baseline.interval) {
    event.corrs <- event.corrs[ , -1]
    event.loss <- event.loss[ , -1]
    event.gain <- event.gain[ , -1]
    event.balance <- event.balance[ , -1]
  }
  
  if (plot)  plotEventCorrs(event.corrs, sep.plots = sep.plots, ...)
  
  return(list(hotspots.list = hotspots.list$hotspots.maps, N.events = hs.numbers$N.events, HS.threshold = hs.numbers$HS.threshold, N.hotspots = hs.numbers$N.hotspots, events.in.HS = hs.numbers$events.in.HS, event.corrs = event.corrs, event.loss = event.loss, event.gain = event.gain, event.balance = event.balance))
  
}
