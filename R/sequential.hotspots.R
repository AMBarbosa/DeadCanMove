sequential.hotspots <-
function(dataset, submats, region.column, first.subsampl.col, confidence = 0.95) {

  n.submats <- length(submats)
  hots <- hotspots.maps <- vector("list", n.submats)
  hotspots.thresholds <- vector("numeric", n.submats)

  for (s in 1:n.submats) {
    subsampl.columns <- first.subsampl.col : ncol(submats[[s]])
    hots[[s]] <- hotspots(dataset = dataset, submat = submats[[s]], region.column = region.column, subsampl.columns = subsampl.columns, confidence = confidence)
    hotspots.thresholds[s] <- hots[[s]]$threshold
    hotspots.maps[[s]] <- hots[[s]]$hotspots
    names(hotspots.maps)[s] <- names(hotspots.thresholds)[s] <- names(submats)[s]
  }

  return(list(hotspots.thresholds = hotspots.thresholds, hotspots.maps = hotspots.maps))

}
