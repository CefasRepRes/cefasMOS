#' Ensemble Kalman Filter (EnKF)
#'
#' This function implements the Ensemble Kalman Filter (EnKF) as described
#' by Gillijns and colleagues in their 2006 paper "What Is the Ensemble Kalman #' Filter and How Well Does it Work?".
#' Note that this EnKF assumes (time invariant) additive noise.
#'
#' @param y data. The data can be a vector (one-dimensional measurements / # univariate time series)
#'  or a matrix (multidimensional measurements /
#' multivariate time series). In the matrix case, each of the columns contains
#' the measurements on a single dimension (or, similarly, a single [univariate]
#' time series).
#' Missing values (NA's) are allowed.
#' @param mod a list with the components m0 (initial state estimates),
#'  # C0 (error covariances of the initial state estimates), V (measurement noise), and W (process noise).
#' @param GGfunction a function with arguments x and k. The GGfunction specifies the
#' state transition function. The transition function is usually denoted as
#' f(x[k], u[k]), where x are the states estimates and u the control input at
#' time step k.
#' @param FFfunction a function with arguments x and k. The FFfunction specifies the
#' observation/measurement function. The measurement function is usually denoted as h(x[k]).
#' @param size size is a integer specifying the ensemble size.
#' @param logLik if TRUE, then return the negative loglikelihood of the specified model.
#' @param simplify if TRUE, then do not include the data (=y) in the output.
#' @author Stefan Gelissen (info@datall-analyse.nl)
#'
#' @return
#' @export
#'
EnKF <- function (y, mod, GGfunction, FFfunction, size=50,
                  logLik=FALSE, simplify=FALSE) {

  mod1 <- mod
  y <- as.matrix(y)
  ym <- ncol(y)
  yAttr <- attributes(y)
  p <- length(mod$m0)

  m <- rbind(mod$m0, matrix(0, nrow = nrow(y), ncol = length(mod$m0)))
  a <- matrix(0, nrow = nrow(y), ncol = length(mod$m0))
  f <- matrix(0, nrow = nrow(y), ncol = ncol(y))
  C <- vector(1 + nrow(y), mode = "list")
  ll <- 0

  #extract variances of a posteriori state estimates at t=0
  C[[1]] <- diag(mod$C0)

  #generate ensemble at t=0
  xa <- rmvnorm(n=size, mean=m[1,], sigma=as.matrix(mod$C0))

  for (i in seq(length = nrow(y))) {

    if (!any(whereNA <- is.na(y[i, ]))) {

      ##time update (forecast step)
      wks <- rmvnorm(n=size, sigma=as.matrix(mod$W))
      #a priori state estimate
      xf <- t(matrix(apply(xa, 1, GGfunction, k=i), nrow=p)) + wks
      a[i, ] <- apply(xf, 2, mean)
      #covariance of a priori state estimate:
      #1/(size-1) * tcrossprod(t(xf)-a[i, ])

      ##measurement update
      vks <- rmvnorm(n=size, sigma=as.matrix(mod$V))
      #predicted measurement
      yp <- t(matrix(apply(xf, 1, FFfunction, k=i), nrow=ym))
      f[i, ] <- apply(yp, 2, mean)
      #covariance of predicted measurement
      Qy <- 1/(size-1) * tcrossprod(t(yp)-f[i, ]) + mod$V

      #cross covariance between a priori state estimate and predicted measurement
      Qxy <- 1/(size-1) * tcrossprod(t(xf)-a[i, ], t(yp)-f[i, ])

      ##a posteriori estimates (analysis step)

      #Kalman gain
      Kk <- crossprod(t(Qxy), solve(Qy, tol=1e-30))
      #a posteriori state estimate
      yk <- y[i, ] + t(vks)
      xa <- t(t(xf) + crossprod(t(Kk), as.matrix(yk-t(yp))))
      m[i + 1, ] <- apply(xa, 2, mean)
      #extract a posteriori error variance
      C[[i + 1]] <- diag(1/(size-1) * tcrossprod(t(xa)-m[i + 1, ]))

      #compute log-likelihood
      if (logLik) {
        e <- as.matrix(y[i, ] - f[i,])
        ll <- ll + ym*log(2*pi) + sum(log(eigen(Qy)$values)) +
          crossprod(e, tcrossprod(solve(Qy, tol=1e-30), t(e)))}
    }

    else {
      if (all(whereNA)) {

        ##time update (forecast step)
        wks <- rmvnorm(n=size, sigma=as.matrix(mod$W))
        #a priori state estimate
        xf <- t(matrix(apply(xa, 1, GGfunction, k=i), nrow=p)) + wks
        a[i, ] <- apply(xf, 2, mean)
        #covariance of a priori state estimate
        Qx <- 1/(size-1) * tcrossprod(t(xf)-a[i, ])

        ##measurement update
        vks <- rmvnorm(n=size, sigma=as.matrix(mod$V))
        #predicted measurement
        yp <- t(matrix(apply(xf, 1, FFfunction, k=i), nrow=ym))
        f[i, ] <- apply(yp, 2, mean)

        ##a posteriori estimates

        xa <- xf

        #a posteriori state estimate
        m[i + 1, ] <- a[i, ]
        #extract a posteriori error variance
        C[[i + 1]] <- diag(Qx)
      }

      else {
        good <- !whereNA

        ##time update (forecast step)
        wks <- rmvnorm(n=size, sigma=as.matrix(mod$W))
        #a priori state estimate
        xf <- t(matrix(apply(xa, 1, GGfunction, k=i), nrow=p)) + wks
        a[i, ] <- apply(xf, 2, mean)
        #covariance of a priori state estimate:
        #1/(size-1) * tcrossprod(t(xf)-a[i, ])

        ##measurement update
        vks <- rmvnorm(n=size, sigma=as.matrix(mod$V))
        #predicted measurement
        yp <- t(matrix(apply(xf, 1, FFfunction, k=i), nrow=ym))
        f[i, ] <- apply(yp, 2, mean)
        #covariance of predicted measurement
        Qy <- 1/(size-1) * tcrossprod(t(yp[, good])-f[i, good]) + mod$V[good, good]

        #cross covariance between a priori state estimate and predicted measurement
        Qxy <- 1/(size-1) * tcrossprod(t(xf)-a[i, ], t(yp[, good])-f[i, good])

        ##a posteriori estimates (analysis step)

        #Kalman gain
        Kk <- crossprod(t(Qxy), solve(Qy, tol=1e-30))
        #a posteriori state estimate
        yk <- y[i, ] + t(vks)
        xa <- t(t(xf) + crossprod(t(Kk), as.matrix(yk[good, ]-t(yp)[good, ])))
        m[i + 1, ] <- apply(xa, 2, mean)
        #extract a posteriori error variance
        C[[i + 1]] <- diag(1/(size-1) * tcrossprod(t(xa)-m[i + 1, ]))

        #compute log-likelihood
        if (logLik) {
          e <- as.matrix(y[i, good] - f[i, good])
          ll <- ll + sum(good)*log(2*pi) + sum(log(eigen(Qy)$values)) +
            crossprod(e, tcrossprod(solve(Qy, tol=1e-30), t(e)))}
      }
    }
  }
  ans <- list(m = m, C = C, a = a, f = f)

  attributes(ans$f) <- yAttr

  if (logLik)
    ans <- c(ans, logLik = 0.5*ll)

  if (simplify)
    ans <- c(mod = list(mod1), size = list(size),
             GGfunction = list(GGfunction), FFfunction = list(FFfunction),
             xa = list(xa), ans)
  else {
    attributes(y) <- yAttr
    ans <- c(y = list(y), mod = list(mod1), size = list(size),
             GGfunction = list(GGfunction), FFfunction = list(FFfunction),
             xa = list(xa), ans)
  }
  return(ans)
}



#' EnKF forecast
#'
#' predicts future system states and observations for the Ensemble Kalman Filter (EnKF).
#'
#' @param filterData
#' @param nAhead
#' @author Stefan Gelissen (info@datall-analyse.nl)
#'
#' @return
#' @export
#'
EnKFforecast <- function (filterData, nAhead=1) {

  mod <- filterData

  GGfunction <- mod$GGfunction
  FFfunction <- mod$FFfunction
  size <- mod$size
  xa <- mod$xa

  modFuture <- mod$mod
  nobs <- nrow(as.matrix(mod$a))
  ym <- ncol(mod$f)
  lastObsIndex <- NROW(mod$m)
  modFuture$C0 <- mod$C[[lastObsIndex]]
  modFuture$m0 <- mod$m[lastObsIndex,]

  mod <- modFuture
  p <- length(mod$m0)
  a <- rbind(mod$m0, matrix(0, nAhead, p))
  R <- vector("list", nAhead + 1)
  R[[1]] <- mod$C0
  f <- matrix(0, nAhead, ym)
  Q <- vector("list", nAhead)

  for (it in 1:nAhead) {

    ##future states

    ##time update (forecast step)
    wks <- rmvnorm(n=size, sigma=as.matrix(mod$W))
    #a priori state estimate
    xf <- t(matrix(apply(xa, 1, GGfunction, k=nobs + it), nrow=p)) + wks
    a[it + 1, ] <- apply(xf, 2, mean)
    #variance of a priori state estimate
    R[[it + 1]] <- diag(1/(size-1) * tcrossprod(t(xf)-a[it, ]))

    ##future observations

    ##measurement update
    vks <- rmvnorm(n=size, sigma=as.matrix(mod$V))
    #predicted measurement
    yp <- t(matrix(apply(xf, 1, FFfunction, k=nobs + it), nrow=ym))
    f[it, ] <- apply(yp, 2, mean)
    #variance of predicted measurement
    Q[[it]] <- diag(1/(size-1) * tcrossprod(t(yp)-f[it, ]) + mod$V)

    xa <- xf
  }
  a <- a[-1, , drop = FALSE]
  R <- R[-1]
  ans <- list(a = a, R = R, f = f, Q = Q)

  return(ans)
}
