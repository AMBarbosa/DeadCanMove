sequential.seqsubmat <- function(dataset, sampl.columns, group.column, gap.sizes, n.replicates.limit) {
  # window.size is always 1
  stopifnot(all(gap.sizes >= 0))
  if (0 %in% gap.sizes & length(gap.sizes) > 1) stop("Sorry, gap.size 0 must be processed separately from positive gap.sizes.")
  
  n.subsampl.columns <- length(sampl.columns)
  n.replicates <- vector("integer", length(gap.sizes))
  window.sum <- vector("integer", length(gap.sizes))
  seqsubmats <- vector("list", length(gap.sizes))
  for (g in gap.sizes) {
    if (g == 0)  seqsubmats[[1]] <- sequential.submatrix(dataset = dataset, sampl.columns = sampl.columns, group.column = group.column, window.sizes = 1, gap.sizes = 0, start.columns = 1, n.subsampl.columns = n.subsampl.columns)
    else {
      message("calculating for gap ", g, "...")
      window.sum[g] <- ceiling(n.subsampl.columns / (g + 1))
      scheme.length <- window.sum[g] + ((window.sum[g] - 1) * g)  # sum windows + gaps
      max.possible.replicates <- n.subsampl.columns - scheme.length + 1
      n.replicates[g] <- min(max.possible.replicates, n.replicates.limit)
      seqsubmats[[g]] <- sequential.submatrix(dataset = dataset, sampl.columns = sampl.columns, group.column = group.column, window.sizes = 1, gap.sizes = g, start.columns = 1:n.replicates[g], n.subsampl.columns = window.sum[g])
    }  # end else
  }  # end for g
  if (g == 0)  seqsubmats <- seqsubmats[[1]]
  else names(seqsubmats) <- paste0("seqsub.g", gap.sizes)
  message("Finished!")
  return(seqsubmats)
}
