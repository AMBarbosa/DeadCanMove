jumping.window <-
function(sampl.columns, window.size, gap.size, start.column = 1, J = FALSE) {
  window.indices <- integer(0)
  for (i in 1 : window.size) {
    window.indices <- c(window.indices, seq(from = i, to = length(sampl.columns), by = window.size + gap.size))
  }
  window.indices <- sort(window.indices)
  sampl.windows <- sampl.columns[window.indices]
  sampl.windows <- sampl.windows + start.column - 1
  sampl.windows <- sampl.windows[sampl.windows <= max(sampl.columns)]
  if (J == FALSE) return(sampl.windows)
  else {
    J <- vector("integer", length(sampl.windows) - 1)
    for (i in 1:length(J)) J[i] <- sampl.windows[i+1] - sampl.windows[i]
    return(J)
  }
}
