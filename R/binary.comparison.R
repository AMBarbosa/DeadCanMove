binary.comparison <-
function(x, y, method) {

  # x and y are 2 binary (0-1) vectors
  # 'method': association coefficient to use; type binary.comp.methods for available options
  
  binary.comp.methods <- c("Phi", "Mathews", "Yule", "Jaccard", "Baroni", "kappa", "CCR", "TSS", "gain", "loss", "balance")
  
  if(!(method %in% binary.comp.methods)) 
    stop ("'method' must be one of (case-sensitive) ", binary.comp.methods)
  
  x0 <- x == 0
  x1 <- x == 1
  y0 <- y == 0
  y1 <- y == 1
  
  a <- sum(x1 & y1)
  b <- sum(x0 & y1)
  c <- sum(x1 & y0)
  d <- sum(x0 & y0)
  N <- sum(a, b, c, d)
  
  if (method == "Phi") {
    A <- a/N
    AB <- (a + b) / N
    AC <- (a + c) / N
    CD <- (c + d) / N
    BD <- (b + d) / N
    return((A -(AB) * (AC)) / sqrt(prod(AB, CD, AC, BD)))
  }  # end if Phi
  
  else if (method == "Mathews") {
    S <- (a + b) / N
    P <- (a + c) / N
    MCC <- (a / N - S * P) / sqrt(prod(P, S, (1 - S), (1 - P)))
    # same as (((a * d) - (b * c)) / sqrt((a + c) * (a + b) * (c + d) * (b + d)))
    return(MCC)
  }  # end if Mathews
  
  else if (method == "Yule") return((a * d - b * c)/(a * d + b * c))
  
  else if (method == "Jaccard") {
    shared <- a
    total  <- sum(x1 | y1)
    return(shared / total)
  }  # end if Jaccard
  
  else if (method == "Baroni") {
    A <- sum(x1)
    B <- sum(y1)
    C <- a
    D <- d
    return((sqrt(C * D) + C) / ((sqrt(C * D)) + A + B - C))
  }  # end if Baroni
  
  else if (method == "kappa") return(((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))/
                                       (N-(((a+c)*(a+b)+(b+d)*(c+d))/N)))
  
  else if (method == "CCR")  return((a + d) / N)
  
  else if (method == "TSS") return((a * d - b * c) / ((a + c) * (b + d)))
  
  else if (method %in% c("gain", "loss", "balance")) {
    diff <- y - x
    if (method == "gain") return(sum(diff == 1))
    else if (method == "loss") return(sum(diff == -1))
    else if (method == "balance") return(sum(diff))
  }  # end if gain | loss | balance
  
}
