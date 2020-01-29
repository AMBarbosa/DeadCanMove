sequential.estimateN <-
function(submats, 
         submats.N, 
         first.subsampl.col, 
         region.column, 
         persist, 
         effic, 
         estimators = c("korner", "huso", "erickson", "etterson"), 
         margin = 0.05, 
         ...) {
  # v1.1 (18 Mar 2015)
  
  # NOTE: works only for submats created with window and gap size (not sampl.interval)!

  if (!requireNamespace("carcass", quietly = TRUE))  return (message("Package 'carcass' (required by this function) not available - please install it first."))
  else requireNamespace("carcass")
  if(!all(estimators %in% c("korner", "huso", "erickson", "etterson")))  stop ("'estimators' must be one or more of (case-sensitive) 'korner', 'huso', 'erickson' and 'etterson'.")

  results <- vector(mode = "list", length = length(submats.N))
  names(results) <- names(submats.N)

  for (i in 1:length(submats.N)) {
    df.name <- names(submats.N)[i]
    message("\nEstimating matrix ", i, " of ", length(submats.N), " (", df.name, ")...")
    df.name.split <- strsplit(df.name, split = "\\.")
    if (length(df.name.split[[1]]) < 3) stop ("Currently, this function works only for 'submats' created with 'window.size' and 'gap.size', not 'sampl.interval' - see Note in help(sequential.estimateN)")
    group <- df.name.split[[1]][1]
    window.size <- as.integer(substr(df.name.split[[1]][2], start = 2, stop = nchar(df.name.split[[1]][2])))
    gap.size <- as.integer(substr(df.name.split[[1]][3], start = 2, stop = nchar(df.name.split[[1]][3])))
    J <- jumping.window(first.subsampl.col : ncol(submats[[i]]), window.size = window.size, gap.size = gap.size, J = TRUE)
    submat.N <- submats.N[[i]]
    submat.N.estim <- data.frame(submat.N)
    rownames(submat.N.estim) <- NULL  # carcass created annoying rownames
    #estimateN.vectorized <- Vectorize(FUN = carcass::estimateN)  # didn't seem to improve speed & changed results structure

    if ("korner" %in% estimators) {
      korner <- carcass::pkorner(s = persist[group], f = effic[group], d = gap.size + 1, n = length(first.subsampl.col : ncol((submats.N)[[i]])))  # interval = gap + 1
      margin <- margin * korner
      korner.estimate.list <- lapply(submat.N[ , "total.events"], FUN = carcass::estimateN, p = korner, p.lower = korner - margin, p.upper = korner + margin)
      #korner.estimate.list <- korner.estimate.list[-length(korner.estimate.list)]
      # TRY MAPPLY?
      korner.df <- data.frame(korner.estim = unlist(sapply(korner.estimate.list, "[", "estimate")), korner.upper = unlist(sapply(korner.estimate.list, "[", "upper")), korner.lower = unlist(sapply(korner.estimate.list, "[", "lower")), korner.HT = unlist(sapply(korner.estimate.list, "[", "HT.estimate")))
      #korner.df <- t(sapply(korner.estimate.list, "[", 1:4))
      submat.N.estim <- data.frame(submat.N.estim, korner.df)
    }  # end if korner

    if ("huso" %in% estimators) {
      huso <- carcass::phuso(s = persist[group], f = effic[group], d = gap.size + 1)
      margin <- margin * huso
      huso.estimate.list <- lapply(submat.N[ , "total.events"], FUN = carcass::estimateN, p = huso, p.lower = huso  - margin, p.upper = huso + margin)
      huso.df <- data.frame(huso.estim = unlist(sapply(huso.estimate.list, "[", "estimate")), huso.upper = unlist(sapply(huso.estimate.list, "[", "upper")), huso.lower = unlist(sapply(huso.estimate.list, "[", "lower")), huso.HT = unlist(sapply(huso.estimate.list, "[", "HT.estimate")))
      submat.N.estim <- data.frame(submat.N.estim, huso.df)
    }  # end if huso

    if ("erickson" %in% estimators) {
      erickson <- carcass::perickson(s = persist[group], f = effic[group], d = gap.size + 1)
      margin <- margin * erickson
      erickson.estimate.list <- lapply(submat.N[ , "total.events"], FUN = carcass::estimateN, p = erickson, p.lower = erickson - margin, p.upper = erickson + margin)
      erickson.df <- data.frame(erickson.estim = unlist(sapply(erickson.estimate.list, "[", "estimate")), erickson.upper = unlist(sapply(erickson.estimate.list, "[", "upper")), erickson.lower = unlist(sapply(erickson.estimate.list, "[", "lower")), erickson.HT = unlist(sapply(erickson.estimate.list, "[", "HT.estimate")))
      submat.N.estim <- data.frame(submat.N.estim, erickson.df)
    }  # end if erickson

    if ("etterson" %in% estimators) {
      etterson <- carcass::ettersonEq14(s = persist[group], f = effic[group], J = J)
      margin <- margin * etterson
      etterson.estimate.list <- lapply(submat.N[ , "total.events"], FUN = carcass::estimateN, p = etterson, p.lower = etterson - margin, p.upper = etterson + margin)
      #names(etterson.estimate.list) <- submat.N[ , "region"]
      etterson.df <- data.frame(etterson.estim = unlist(sapply(etterson.estimate.list, "[", "estimate")), etterson.upper = unlist(sapply(etterson.estimate.list, "[", "upper")), etterson.lower = unlist(sapply(etterson.estimate.list, "[", "lower")), etterson.HT = unlist(sapply(etterson.estimate.list, "[", "HT.estimate")))
      submat.N.estim <- data.frame(submat.N.estim, etterson.df)
    }  # end if etterson

    cat("\n")
    print(submat.N.estim)
    results[[i]] <- submat.N.estim
  }  # end for i in submats.N
  
  return(results)
}
