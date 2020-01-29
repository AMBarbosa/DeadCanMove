sequential.posteriorN <-
function(submats, submats.N, first.subsampl.col, region.column, persist, effic, estimators = c("korner", "huso", "erickson", "etterson"), ...) {
  # v1.1 (21 Apr 2016)
  # persist, effic: named vectors of persistence probability and detection efficiency per group; group names must match those in the data
  # ...: additional arguments for carcass::posteriorN

  if (!all(estimators %in% c("korner", "huso", "erickson", "etterson"))) stop ("Invalid estimator. Possible choices are (case-sensitive) 'korner', 'huso', 'erickson' and 'etterson')")

  requireNamespace("carcass")
  results <- vector(mode = "list", length = length(submats.N))
  names(results) <- names(submats.N)

  for (i in 1:length(submats.N)) {
    df.name <- names(submats.N)[i]
    message("\nEstimating matrix ", i, " of ", length(submats.N), " (", df.name, ")...")
    df.name.split <- strsplit(df.name, split = "\\.")

    group <- df.name.split[[1]][1]

    window.size <- as.integer(substr(df.name.split[[1]][2], start = 2, stop = nchar(df.name.split[[1]][2])))

    gap.size <- as.integer(substr(df.name.split[[1]][3], start = 2, stop = nchar(df.name.split[[1]][3])))

    submat.N <- submats.N[[i]]
    submat.N.estim <- data.frame(submat.N)
    rownames(submat.N.estim) <- NULL  # carcass created annoying rownames

    getEstimates <- function(p) {
      estimate.list <- vector("list", nrow(submat.N))
      for (n in 1:nrow(submat.N)) {
        estimate.list[[n]] <- carcass::posteriorN(p = p, nf = submat.N[n, "total.events"], ...)
      }  # end for n
      estimate.df <- data.frame(estim = unlist(sapply(estimate.list, "[", "expected")), upper = unlist(sapply(estimate.list, "[", "interval"))[2], lower = unlist(sapply(estimate.list, "[", "interval"))[1], HT = unlist(sapply(estimate.list, "[", "HT.estimate")), row.names = NULL)
    }  # end getEstimates function

    if ("korner" %in% estimators) {
      korner <- carcass::pkorner(s = persist[group], f = effic[group], d = gap.size + 1, n = length(first.subsampl.col : ncol((submats.N)[[i]])))  # interval = gap + 1
      korner.df <- getEstimates(p = korner)
      colnames(korner.df) <- paste("korner", colnames(korner.df), sep = ".")
      submat.N.estim <- data.frame(submat.N.estim, korner.df)
    }  # end if korner

    if ("huso" %in% estimators) {
      huso <- carcass::phuso(s = persist[group], f = effic[group], d = gap.size + 1)
      huso.df <- getEstimates(p = huso)
      colnames(huso.df) <- paste("huso", colnames(huso.df), sep = ".")
      submat.N.estim <- data.frame(submat.N.estim, huso.df)
    }  # end if huso

    if ("erickson" %in% estimators) {
      erickson <- carcass::perickson(s = persist[group], f = effic[group], d = gap.size + 1)
      erickson.df <- getEstimates(p = erickson)
      colnames(erickson.df) <- paste("erickson", colnames(erickson.df), sep = ".")
      submat.N.estim <- data.frame(submat.N.estim, erickson.df)
    }  # end if erickson

    if ("etterson" %in% estimators) {
      J <- jumping.window(first.subsampl.col : ncol(submats[[i]]), window.size = window.size, gap.size = gap.size, J = TRUE)
      etterson <- carcass::ettersonEq14(s = persist[group], f = effic[group], J = J)
      etterson.df <- getEstimates(p = etterson)
      colnames(etterson.df) <- paste("etterson", colnames(etterson.df), sep = ".")
      submat.N.estim <- data.frame(submat.N.estim, etterson.df)
    }  # end if etterson

    cat("\n")
    print(submat.N.estim)
    results[[i]] <- submat.N.estim

  }  # end for i

  return(results)

}
