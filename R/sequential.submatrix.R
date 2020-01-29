sequential.submatrix <-
function(dataset, sampl.columns, sampl.intervals = NULL, window.sizes = NULL, gap.sizes = NULL, start.columns = 1, all.combinations = TRUE, group.column = NULL, include.all.together = TRUE, remove.zeros = TRUE, keep.nonsampl.columns = TRUE, n.subsampl.columns = NULL) {

  # to FIX: length(sampl.columns) too much: automatize n.subsampl.columns: = length(sampl.columns) - max(gap.sizes)?  - sum(gap.sizes)? ...

  if(is.null(group.column)) {
    dataset$one.group = "ALL"
    group.column = "one.group"
  }

  groups <- unique(dataset[ , group.column])
  submats <- NULL
  
  if (!is.null(sampl.intervals)) {
    if (!is.null(window.sizes) | !is.null(gap.sizes))  message("NOTE: 'window.sizes' and 'gap.sizes' ignored in favour of 'sampl.intervals'.")
    
    for (intv in sampl.intervals)  for (grp in groups) {
      submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns, sampl.interval = intv, group.column = group.column, group.names = grp, remove.zeros = remove.zeros, keep.nonsampl.columns = keep.nonsampl.columns)
      submat.name <- paste(grp, intv, sep = ".intv")
      submats[[submat.name]] <- submat
    }; rm(intv, grp)

    if (include.all.together) {
      for (intv in sampl.intervals) {
        submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns, sampl.interval = intv, keep.nonsampl.columns = keep.nonsampl.columns)
        submat.name <- paste("ALL", intv, sep = ".intv")
        submats[[submat.name]] <- submat
      }; rm(intv)
    }  # end if include.all.together

    return(invisible(submats))
  }  # end if !null sampl.intervals return

  # now if NULL sampl.intervals:
  if (is.null(window.sizes) | is.null(gap.sizes))  stop ("You must provide either sampl.intervals, or both window.sizes and gap.sizes.")

  if (is.null(n.subsampl.columns))
    n.subsampl.columns <- length(sampl.columns)  # FIX!!
  
  if (all.combinations) {
    for (w in window.sizes) for (gap in gap.sizes) for (s in start.columns) for (grp in groups) {
      submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns, window.size = w, gap.size = gap, start.column = s, group.column = group.column, group.names = grp, remove.zeros = remove.zeros, keep.nonsampl.columns = keep.nonsampl.columns)
      submat <- submat[ , 1:(min(sampl.columns) + n.subsampl.columns - 1)]
      submat.name <- paste0(grp, ".w", w, ".g", gap, ".s", s)
      submats[[submat.name]] <- submat
    }; rm(w, gap, s, grp)

    if (include.all.together) {
      for (w in window.sizes) for (gap in gap.sizes) for (s in start.columns) {
        submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns, window.size = w, gap.size = gap, start.column = s, keep.nonsampl.columns = keep.nonsampl.columns)
        submat <- submat[ , 1:(min(sampl.columns) + n.subsampl.columns - 1)]
        submat.name <- paste0("ALL.w", w, ".g", gap, ".s", s)
        submats[[submat.name]] <- submat
      }; rm(w, gap, s)
    }  # end if include.all.together
  }  # end if all combinations

  else {  # if !all.combinations
    if (length(window.sizes) != length(gap.sizes))
      stop("'all combinations = FALSE' requires that
           length(window.sizes) == length(gap.sizes)")

    sampl.schemes <- data.frame(window.sizes, gap.sizes)
    for (i in 1:nrow(sampl.schemes)) for (grp in groups) {
      submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns,
                          window.size = sampl.schemes[i, 1], gap.size = sampl.schemes[i, 2],
                          group.column = group.column, group.names = grp,
                          remove.zeros = remove.zeros,
                          keep.nonsampl.columns = keep.nonsampl.columns)
      submat <- submat[ , 1:(min(sampl.columns) + n.subsampl.columns - 1)]
      submat.name <- paste0(grp, ".w", sampl.schemes[i, 1], ".g", sampl.schemes[i, 2])
      submats[[submat.name]] <- submat
    }; rm(i, grp)  # VER: e aqui nao tem 's'??

    if (include.all.together) {
      for (i in 1:nrow(sampl.schemes)) {
        submat <- submatrix(dataset = dataset, sampl.columns = sampl.columns,
                            window.size = sampl.schemes[i, 1],
                            gap.size = sampl.schemes[i, 2],
                            keep.nonsampl.columns = keep.nonsampl.columns)
        submat <- submat[ , 1:(min(sampl.columns) + n.subsampl.columns - 1)]
        submat.name <- paste0("ALL.w", sampl.schemes[i, 1], ".g", sampl.schemes[i, 2])
        submats[[submat.name]] <- submat
      }; rm(i)
    }  # end if include.all.together
  }  # end else

  return(invisible(submats))

}
