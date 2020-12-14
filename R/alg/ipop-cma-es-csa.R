library(stringr)
ipop_cma_es_csa <- function(par, fn, ..., lower, upper, if_CMA = TRUE, control=list()) {

  norm <- function(x)
    drop(sqrt(crossprod(x)))
  
  controlParam <- function(name, default) {
    v <- control[[name]]
    if (is.null(v))
      return (default)
    else
      return (v)
  }
  
  ## Inital solution:
  xmean <- par
  N <- length(xmean)
  ## Box constraints:
  if (missing(lower))
    lower <- rep(-Inf, N)
  else if (length(lower) == 1)  
    lower <- rep(lower, N)
  
  if (missing(upper))
    upper <- rep(Inf, N)
  else if (length(upper) == 1)  
    upper <- rep(upper, N)
  
  ## Parameters:
  trace       <- controlParam("trace", FALSE)
  init_sigma       <- controlParam("init_sigma", 1)
  fnscale     <- controlParam("fnscale", 1)
  stopfitness <- controlParam("stopfitness", -Inf)
  budget      <- controlParam("budget", 10000*N )                     ## The maximum number of fitness function calls
  sc_tolx     <- controlParam("stop.tolx", 1e-12 * init_sigma) ## Undocumented stop criterion
  keep.best   <- controlParam("keep.best", TRUE)
  vectorized  <- controlParam("vectorized", FALSE)
  
  ## Logging options:
  log.all    <- controlParam("diag", TRUE)
  log.sigma  <- controlParam("diag.sigma", 1)
  log.eigen  <- controlParam("diag.eigen", 0)
  log.value  <- controlParam("diag.value", 0)
  log.pop    <- controlParam("diag.pop", 0)
  log.bestVal<- controlParam("diag.bestVal", 1)
  
  ## Strategy parameter setting (defaults as recommended by Nicolas Hansen):
  lambda       <- controlParam("lambda", 4 * N)
  maxiter     <- controlParam("maxit", round(budget/lambda))
  max_restarts <- controlParam("max_restarts", 0)
  restart_multiplier <- controlParam("restart_multiplier", 2)

  iter <- 0L      ## Number of iterations
  counteval <- 0L ## Number of function evaluations
  cviol <- 0L     ## Number of constraint violations
  msg <- ""     ## Reason for terminating
  nm <- names(par) ## Names of parameters
  restart_num = 0

  chiN <- sqrt(N) * (1-1/(4*N)+1/(21*N^2))

  if (log.sigma)
    sigma.log <- numeric(maxiter)
  if (log.eigen)
    eigen.log <- matrix(0, nrow=maxiter, ncol=N)
  if (log.value)
    value.log <- matrix(0, nrow=maxiter, ncol=lambda)
  if (log.pop)
    pop.log <- array(0, c(N, lambda, maxiter))
  if(log.bestVal)
    bestVal.log <-  matrix(0, nrow=0, ncol=1)


  stopifnot(length(upper) == N)  
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(sigma) == 1)

  best.fit <- Inf
  best.par <- NULL

  terminate = FALSE

  while ((restart_num <= max_restarts) && (counteval < budget)) {
    restart_num = restart_num + 1

     # increase population size (IPOP-CMA-ES)
     if ((restart_num - 1) > 0) {
      lambda = ceiling(restart_multiplier^(restart_num - 1) * lambda)
    }

    sigma       <- controlParam("sigma", init_sigma)
    mu          <- controlParam("mu", floor(lambda/2))
    weights     <- controlParam("weights", log(mu+1) - log(1:mu))
    weights     <- weights/sum(weights)
    mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2))
    cc          <- controlParam("ccum", 4/(N+4))
    cs          <- controlParam("cs", (mueff+2)/(N+mueff+3))
    mucov       <- controlParam("ccov.mu", mueff)
    ccov        <- controlParam("ccov.1",
                                (1/mucov) * 2/(N+1.4)^2
                                + (1-1/mucov) * ((2*mucov-1)/((N+2)^2+2*mucov)))
    damps       <- controlParam("damps",
                                1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + cs)

    pc <- rep(0.0, N)
    ps <- rep(0.0, N)
    B <- diag(N)
    D <- diag(N)
    BD <- B %*% D
    C <- BD %*% t(BD)
    
    while (counteval < budget) {

      iter <- iter + 1L
      
      if (!keep.best) {
        best.fit <- Inf
        best.par <- NULL
      }
      if (log.sigma)
        sigma.log[iter] <- sigma
      
      ## Generate new population:
      arz <- matrix(rnorm(N*lambda), ncol=lambda)
      arx <- xmean + sigma * (BD %*% arz)
      vx <- ifelse(arx > lower, ifelse(arx < upper, arx, upper), lower)
      if (!is.null(nm))
        rownames(vx) <- nm
      pen <- 1 + colSums((arx - vx)^2)
      pen[!is.finite(pen)] <- .Machine$double.xmax / 2
      cviol <- cviol + sum(pen > 1)
      
      if (vectorized) {
        y <- fn(vx, ...) * fnscale
      } else {
        y <- apply(vx, 2, function(x) fn(x, ...) * fnscale)
      }
      counteval <- counteval + lambda
      
      arfitness <- y * pen
      valid <- pen <= 1

      if (any(valid)) {
        wb <- which.min(y[valid])
        if (y[valid][wb] < best.fit) {
          best.fit <- y[valid][wb]
          best.par <- arx[,valid,drop=FALSE][,wb]
        }
      }
      
      ## Order fitness:
      arindex <- order(arfitness)
      arfitness <- arfitness[arindex]

      if (log.bestVal) 
        bestVal.log <- rbind(bestVal.log,min(suppressWarnings(min(bestVal.log)), min(arfitness)))

      
      aripop <- arindex[1:mu]
      selx <- arx[,aripop]
      xmean <- drop(selx %*% weights)
      selz <- arz[,aripop]
      zmean <- drop(selz %*% weights)
      
      ## Save selected x value:
      if (log.pop) pop.log[,,iter] <- selx
      if (log.value) value.log[iter,] <- arfitness[aripop]
      
      ## Cumulation: Update evolutionary paths
      ps <- (1-cs)*ps + sqrt(cs*(2-cs)*mueff) * (B %*% zmean)
      hsig <- drop((norm(ps)/sqrt(1-(1-cs)^(2*counteval/lambda))/chiN) < (1.4 + 2/(N+1)))
      pc <- (1-cc)*pc + hsig * sqrt(cc*(2-cc)*mueff) * drop(BD %*% zmean)
      
      ## Adapt Covariance Matrix:
      BDz <- BD %*% selz

      if (if_CMA) 
        C <- (1-ccov) * C + ccov * (1/mucov) *
          (pc %o% pc + (1-hsig) * cc*(2-cc) * C) +
          ccov * (1-1/mucov) * BDz %*% diag(weights) %*% t(BDz)
      else
        C = C
      
      ## Adapt step size sigma:
      sigma <- sigma * exp((norm(ps)/chiN - 1)*cs/damps)
      
      e <- eigen(C, symmetric=TRUE)
      eE <- eigen(cov(t(arx)))

      if (log.eigen)
        eigen.log[iter,] <- rev(sort(eE$values))
      
      if (!all(e$values >= sqrt(.Machine$double.eps) * abs(e$values[1]))) {      
        msg <- c(msg, stringr::str_glue("Restart iteration = {} :: Covariance matrix 'C' is numerically not positive definite."))
        break
      }
      
      B <- e$vectors
      D <- diag(sqrt(e$values), length(e$values))
      BD <- B %*% D
      
      ## break if fit:
      if (arfitness[1] <= stopfitness * fnscale) {
        msg <- c(msg, "Stop fitness reached.")
        terminate = TRUE 
        break
      }

      ## Check stop conditions:
      
      ## Condition 1 (sd < tolx in all directions):
      if (all(D < sc_tolx) && all(sigma * pc < sc_tolx)) {
        msg <- c(msg, stringr::str_glue("Restart iteration = {iter} :: All standard deviations smaller than tolerance."))
        break
      }

      ## Condition 2 no effect axis 
      eigen_index = iter %% N + 1L
      if (sum((xmean - (xmean + 0.1 * sigma * e$values[eigen_index] * e$vectors[, eigen_index]))^2) < .Machine$double.eps) {
        msg <- c(msg, stringr::str_glue("Restart iteration = {iter} :: Addition of 0.1 times sigma does not change mean value"))
        break
      }

      ## Condition 3 no effect   

      if (sum((xmean - (xmean + 0.2 * sigma))^2) < .Machine$double.eps) {
        msg <- c(msg, stringr::str_glue("Restart iteration = {iter} :: Addition of 0.2 times sigma in any coordinate does not change mean value."))
        break
      }

      ## Escape from flat-land:
      if (arfitness[1] == arfitness[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
        sigma <- sigma * exp(0.2+cs/damps);
      if (trace)
        message("Flat fitness function. Increasing sigma.")
      }

      if (trace) {
        message(sprintf("Iteration %i of %i: current fitness %f",
                        iter, maxiter, arfitness[1] * fnscale))
      }
    }
    if (terminate) {
      break
    }
  }
  cnt <- c(`function`=as.integer(counteval), gradient=NA)

  log <- list()
  if (log.value) log$value <- value.log[1:iter,]
  if (log.sigma) log$sigma <- sigma.log[1:iter]
  if (log.eigen) log$eigen <- eigen.log[1:iter,]
  if (log.pop)   log$pop   <- pop.log[,,1:iter]
  if (log.bestVal) log$bestVal <- bestVal.log
  names(best.fit) <- NULL

  res <- list(par=best.par,
              value=best.fit / fnscale,
              counts=cnt,
              init_sigma = init_sigma,
              restarts = restart_num - 1,
              iter = iter,
              convergence=ifelse(iter >= maxiter, 1L, 0L),
              message=msg,
              label = "ipop-cma-es-csa",
              constr.violations=cviol,
              diagnostic=log
  )

  class(res) <- "cma_es_csa.result"
  return(res)
}
