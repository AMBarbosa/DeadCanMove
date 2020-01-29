submatrix <-
function(dataset, sampl.columns, sampl.interval = NULL, window.size = NULL, gap.size = NULL, start.column = 1, group.column = NULL, group.names = NULL, remove.zeros = TRUE, keep.nonsampl.columns = TRUE) {

  if (any(rle(diff(sampl.columns))$values != 1)) stop("Under the current implementation, 'sampl.columns' must be consecutive columns.")

  if (!is.null(sampl.interval)) {
    if (!is.null(window.size) | !is.null(gap.size))  message("NOTE: 'window.size' and 'gap.size' ignored in favour of 'sampl.interval'.")
    subsampl.columns <- seq(from = sampl.columns[1], to = sampl.columns[length(sampl.columns)], by = sampl.interval)
  } else {
    if (is.null(window.size) & is.null(gap.size))  stop ("You must provide either a sampl.interval, or both window.size and gap.size.")
    subsampl.columns <- jumping.window(sampl.columns = sampl.columns, window.size = window.size, gap.size = gap.size, start.column = start.column)
    #subsampl.columns <- subsampl.columns[subsampl.columns <= max(sampl.columns)]
  }

  data.column.names <- names(dataset)
  subsampl.column.names <- data.column.names[subsampl.columns]
  nonsampl.columns <- dataset[ , !(data.column.names %in% data.column.names[sampl.columns])]
  nonsampl.column.names <- names(nonsampl.columns)

  if (!is.null(group.names)) {
    if (is.null(group.column)) stop("if you want those particular group.names to be extracted, you must specify the group.column which contains them")
    dataset <- dataset[dataset[ , group.column] %in% group.names, ]
  }  # end if !null group column

  if (!is.null(group.column) & is.null(group.names))
    message("group.names not specified; extracting subsampling matrices
            for all rows")

  if (remove.zeros & is.numeric(dataset[ , subsampl.columns])) {
    dataset <- dataset[which(rowSums(dataset[ , subsampl.columns]) > 0), ]
  }  # end if remove zeros

  ifelse(keep.nonsampl.columns,
         submat <- subset(dataset, select = c(nonsampl.column.names,
                                              subsampl.column.names)),
         submat <- subset(dataset, select = c(subsampl.column.names))
  )

  return(invisible(submat))
}
