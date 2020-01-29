sequential.Nevents <-
function(dataset, submats, region.column, first.subsampl.col, estimate = FALSE) {
  
  n.submats <- length(submats)
  events <- vector("list", n.submats)
  
  for (s in 1:n.submats) {
    subsampl.columns <- first.subsampl.col : ncol(submats[[s]])
    events[[s]] <- hotspots(dataset = dataset, submat = submats[[s]], region.column = region.column, subsampl.columns = subsampl.columns, hotspots = FALSE)
    names(events)[s] <- names(submats)[s]
  }
  
  return(events.maps = events)
  
}
