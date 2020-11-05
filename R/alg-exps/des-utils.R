bounceBackBoundary2 <- function(x, lower = rep(-100, 10), upper = rep(100, 10)) {
    if (all(x >= cbind(lower)) && all(x <= cbind(upper))) {
      return(x)
    } else if (any(x < cbind(lower))) {
      for (i in which(x < cbind(lower))) {
        x[i] <- lower[i] + abs(lower[i] - x[i]) %% (upper[i] - lower[i])
      }
    } else if (any(x > cbind(upper))) {
      for (i in which(x > cbind(upper))) {
        x[i] <- upper[i] - abs(upper[i] - x[i]) %% (upper[i] - lower[i])
      }
    }
    x <- deleteInfsNaNs(x)
    return(bounceBackBoundary2(x))
  }
deleteInfsNaNs <- function(x) {
    x[is.na(x)] <- .Machine$double.xmax
    x[is.infinite(x)] <- .Machine$double.xmax
    return(x)
  }

