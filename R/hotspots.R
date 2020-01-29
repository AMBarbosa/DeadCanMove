hotspots <- function(dataset, submat = NULL, region.column, subsampl.columns = NULL, n.events.column = NULL, hotspots = TRUE, confidence = 0.95, min.total.events = 0, min.hotspot.threshold = 2) {

  if (is.null(submat))  submat <- dataset

  if (!is.null(n.events.column))  total.events.by.row <- submat[ , n.events.column]
  else if (!is.null(subsampl.columns))  total.events.by.row <- rowSums(submat[ , subsampl.columns])
  else stop ("You must provide either 'subsampl.columns' or 'n.events.column'.")

  if (!is.null(n.events.column))  {
    total.events.by.region <- data.frame(total.events = dataset[,n.events.column], region=dataset[,region.column] )

    } else {
    count <- function(x) { sum(x > 0, na.rm = TRUE) }
    total.events.by.region <- as.data.frame(tapply(total.events.by.row, submat[ , region.column], FUN = count))
    total.events.by.region$region <- as.integer(row.names(total.events.by.region))
    colnames(total.events.by.region) <- c("total.events", "region")
  }

  dataset.regions <- unique(dataset[ , region.column])
  hotspots.map <- data.frame(dataset.regions)
  colnames(hotspots.map) <- "region"
  hotspots.map <- merge(hotspots.map, total.events.by.region, by = "region", all = TRUE)
  hotspots.map[is.na(hotspots.map)] <- 0
  results <- hotspots.map

  if (hotspots == TRUE) {
    numbers.of.events <- 0 : max(hotspots.map$total.events)
    mean.events <- mean(hotspots.map$total.events)

    #numbers.of.events.factorial <- ifelse(numbers.of.events < 170, factorial(numbers.of.events), factorial(170))  # still emitted warnings about the 170

    fact <- Vectorize(function (n) {
      if(n < 170)  f <- factorial(n)
      else  f <- factorial(170)
      return(f)
    })  # 'factorial' can't handle n > 170 but, from there on, the difference in ppoisson (next step) is negligible

    numbers.of.events.factorial <- fact(numbers.of.events)
    ppoisson <- ( (mean.events ^ numbers.of.events) * exp(-mean.events) ) / numbers.of.events.factorial  # Malo et al. (2004) JApplEcol; see also ?ppois
    cumulative.ppoisson <- NULL
    for (p in 1:length(numbers.of.events)) {
      cumulative.ppoisson[p] <- sum(ppoisson[1:p])
    }  # end for p

    hotspot.threshold <- min(numbers.of.events[cumulative.ppoisson > confidence])

    hotspots.map$hotspot <- ifelse(hotspots.map$total.events >= hotspot.threshold, 1, 0)

    if (hotspot.threshold < min.hotspot.threshold
        # | sum(hotspots.map$total.events) < min.total.events
    ) {
      hotspot.threshold <- NA
      hotspots.map$hotspot <- NA
    }  # end if

    results <- list(threshold = hotspot.threshold, hotspots = hotspots.map)
  }  # end if hotspots = TRUE

  return(results)
}
