#' Unscented Kalman filter (UKF)
#'
#' This function implements the Unscented Kalman Filter (UKF) as described
#' by Julier and Uhlmann in their 1997 paper "A New Extension of the Kalman Filter to Nonlinear Systems".
#'
#' Note that this UKF assumes (time invariant) additive noise.
#' @param y data. The data can be a vector (one-dimensional measurements / # univariate time series)
#'  or a matrix (multidimensional measurements /
#' multivariate time series). In the matrix case, each of the columns contains
#' the measurements on a single dimension (or, similarly, a single [univariate]
#' time series).
#' Missing values (NA's) are allowed.
#' @param mod a list with the components m0 (initial state estimates),
#' @param GGfunction a function with arguments x and k. The GGfunction specifies the
#' state transition function. The transition function is usually denoted as
#' f(x[k], u[k]), where x are the states estimates and u the control input at
#' time step k.
#' See details below for how to handle control input in the GGfunction.
#' @param FFfunction a function with arguments x and k. The FFfunction specifies the
#' observation/measurement function. The measurement function is usually denoted as h(x[k]).
#' @param kappa a tuning parameter.
#' @param sqrtMethod specifies the method that will be used for computing the matrix,
#' square root. The method is either "Cholesky" (=Cholesky decomposition, the default) or svd" (=singular value decomposition).
#' @param logLik if TRUE, then return the negative loglikelihood of the specified model.
#' @param simplify if TRUE, then do not include the data (=y) in the output.
#' @references Julier & Uhlmann, 1997, A New Extension of the Kalman Filter to Nonlinear Systems
#' @author Stefan Gelissen
#'
#' @return list containing,
#' m = the filtered state estimates
#' C = the filtered state covariance
#'
#' @export
#'
UKF <- function (y, mod, GGfunction, FFfunction, kappa=0,
                 sqrtMethod="Cholesky", logLik=FALSE, simplify=FALSE) {

  #Details:
  #It is possible to include control input in the GGfunction.
  #As an example, let us assume that there are two states (x1 and x2) and that
  #we have 70 time steps at which we want to obtain state estimates. At each of
  #these 70 time steps there will be (external) control input.
  #
  #We may specify an arbitrary GGfunction with control input as follows:
  #GGfunction <- function (x, k){
  #x1 <- x[1]; x2 <- x[2]
  #c(2*x1 + 1*x2 - 4,
  #  3*x2 - 1*x2 - 2)}
  #where, at each time step, the control input for the first state equation
  #is -4, and the control input for the second state equation -2.
  #
  #Alternatively, we may store the control input for the two state equations
  #at each of the 70 time steps in a 70x2 matrix.
  #Subsequently, we specify the GGfunction as:
  #GGfunction <- function (x, k){
  #x1 <- x[1]; x2 <- x[2]
  #c(2*x1 + 1*x2 - u[k,1],
  #  3*x2 - 1*x2 - u[k,2])}
  #where we have stored the control input at each of the 70 time steps
  #in a matrix called u.
  #
  #Note that the latter specification of the GGfunction is useful when the control
  #input is known to vary over time.

  mod1 <- mod
  y <- as.matrix(y)
  ym <- ncol(y)
  yAttr <- attributes(y)
  p <- length(mod$m0)

  if (!is.null(mod$FF) | !is.null(mod$GG))
    warning ("FF or GG matrix will not be used in the UKF")

  if (!is.null(mod$JFF) | !is.null(mod$JGG))
    warning ("Time varying FF or GG matrix will not be used in the UKF")

  if (!is.null(mod$JW) | !is.null(mod$JV))
    warning ("Time varying V or W matrix will not be used in the UKF")

  if (!(sqrtMethod=="Cholesky" | sqrtMethod=="svd"))
    stop ("Name of sqrtMethod is incorrect")

  m <- rbind(mod$m0, matrix(0, nrow = nrow(y), ncol = length(mod$m0)))
  a <- matrix(0, nrow = nrow(y), ncol = length(mod$m0))
  f <- matrix(0, nrow = nrow(y), ncol = ncol(y))
  C <- vector(1 + nrow(y), mode = "list")
  R <- vector(nrow(y), mode = "list")
  ll <- 0
  w <- as.vector(c(kappa/(p+kappa), rep(1/(2*(p+kappa)), 2*p)))

  C[[1]] <- mod$C0

  for (i in seq(length = nrow(y))) {

    if (!any(whereNA <- is.na(y[i, ]))) {

      ##time update

      #compute sigma points
      if (sqrtMethod=="Cholesky") {
        sigmaPlus <- t(chol((p+kappa)*C[[i]]))}
      else {
        tmpxs <- La.svd((p+kappa)*C[[i]], nu=0)
        sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
      sigmax <- t(m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
      #a priori state estimate
      tmpx <- matrix(sapply(1:nrow(sigmax),
                            function (x) GGfunction(x=sigmax[x,], k=i)), nrow=p)
      a[i, ] <- tcrossprod(w, tmpx)
      #a priori error covariance
      R[[i]] <- tcrossprod(crossprod(t(tmpx-a[i, ]), diag(w)), tmpx-a[i, ]) + mod$W

      ##measurement update

      #compute sigma points
      if (sqrtMethod=="Cholesky") {
        sigmaPlus <- t(chol((p+kappa)*R[[i]]))}
      else {
        tmpys <- La.svd((p+kappa)*R[[i]], nu=0)
        sigmaPlus <- t(sqrt(tmpys$d)*tmpys$vt)}
      sigmay <- t(a[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
      #predicted measurement
      tmpy <- matrix(sapply(1:nrow(sigmay),
                            function (x) FFfunction(x=sigmay[x,], k=i)), nrow=ym)
      f[i, ] <- tcrossprod(w, tmpy)
      #covariance of predicted measurement
      Qy <- tcrossprod(crossprod(t(tmpy-f[i, ]), diag(w)), tmpy-f[i, ]) + mod$V
      #cross covariance between a priori state estimate and predicted measurement
      Qxy <- tcrossprod(crossprod(t(t(sigmay)-a[i, ]), diag(w)), tmpy-f[i, ])

      ##a posteriori estimates

      #Kalman gain
      Kk <- crossprod(t(Qxy), solve(Qy, tol=1e-30))
      #a posteriori state estimate
      m[i + 1, ] <- a[i, ] + crossprod(t(Kk), as.matrix(y[i, ]-f[i, ]))
      #a posteriori error covariance
      C[[i + 1]] <- R[[i]] - crossprod(t(Kk), tcrossprod(Qy, Kk))

      #compute log-likelihood
      if (logLik) {
        e <- as.matrix(y[i, ] - f[i,])
        ll <- ll + ym*log(2*pi) + sum(log(eigen(Qy)$values)) +
          crossprod(e, tcrossprod(solve(Qy, tol=1e-30), t(e)))}
    }

    else {
      if (all(whereNA)) {

        ##time update

        #compute sigma points
        if (sqrtMethod=="Cholesky") {
          sigmaPlus <- t(chol((p+kappa)*C[[i]]))}
        else {
          tmpxs <- La.svd((p+kappa)*C[[i]], nu=0)
          sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
        sigmax <- t(m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
        #a priori state estimate
        tmpx <- matrix(sapply(1:nrow(sigmax),
                              function (x) GGfunction(x=sigmax[x,], k=i)), nrow=p)
        a[i, ] <- tcrossprod(w, tmpx)
        #a priori error covariance
        R[[i]] <-  tcrossprod(crossprod(t(tmpx-a[i, ]), diag(w)), tmpx-a[i, ]) + mod$W

        ##measurement update

        #compute sigma points
        if (sqrtMethod=="Cholesky") {
          sigmaPlus <- t(chol((p+kappa)*R[[i]]))}
        else{
          tmpys <- La.svd((p+kappa)*R[[i]], nu=0)
          sigmaPlus <- t(sqrt(tmpys$d)*tmpys$vt)}
        sigmay <- t(a[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
        #predicted measurement
        tmpy <- matrix(sapply(1:nrow(sigmay),
                              function (x) FFfunction(x=sigmay[x,], k=i)), nrow=ym)
        f[i, ] <- tcrossprod(w, tmpy)

        ##a posteriori estimates

        #a posteriori state estimate
        m[i + 1, ] <- a[i, ]
        #a posteriori error covariance
        C[[i + 1]] <- R[[i]]
      }

      else {
        good <- !whereNA

        ##time update

        #compute sigma points
        if (sqrtMethod=="Cholesky") {
          sigmaPlus <- t(chol((p+kappa)*C[[i]]))}
        else{
          tmpxs <- La.svd((p+kappa)*C[[i]], nu=0)
          sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
        sigmax <- t(m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
        #a priori state estimate
        tmpx <- matrix(sapply(1:nrow(sigmax),
                              function (x) GGfunction(x=sigmax[x,], k=i)), nrow=p)
        a[i, ] <- tcrossprod(w, tmpx)
        #a priori error covariance
        R[[i]] <- tcrossprod(crossprod(t(tmpx-a[i, ]), diag(w)), tmpx-a[i, ]) + mod$W

        ##measurement update

        #compute sigma points
        if (sqrtMethod=="Cholesky") {
          sigmaPlus <- t(chol((p+kappa)*R[[i]]))}
        else {
          tmpys <- La.svd((p+kappa)*R[[i]], nu=0)
          sigmaPlus <- t(sqrt(tmpys$d)*tmpys$vt)}
        sigmay <- t(a[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
        #predicted measurement
        tmpy <- matrix(sapply(1:nrow(sigmay),
                              function (x) FFfunction(x=sigmay[x,], k=i)), nrow=ym)
        f[i, ] <- tcrossprod(w, tmpy)
        #covariance of predicted measurement
        Qy <- tcrossprod(crossprod(t(matrix(tmpy[good, ], ncol=2*p+1) -
                                       f[i, good]), diag(w)),
                         matrix(tmpy[good, ], ncol=2*p+1) - f[i, good]) +
          mod$V[good, good]
        #cross covariance between a priori state estimate and predicted measurement
        Qxy <- tcrossprod(crossprod(t(t(sigmay)-a[i, ]), diag(w)),
                          matrix(tmpy[good, ], ncol=2*p+1) - f[i, good])

        ##a posteriori estimates

        #Kalman gain
        Kk <- crossprod(t(Qxy), solve(Qy, tol=1e-30))
        #a posteriori state estimate
        m[i + 1, ] <- a[i, ] + crossprod(t(Kk), as.matrix(y[i, good]-f[i, good]))
        #a posteriori error covariance
        C[[i + 1]] <- R[[i]] - crossprod(t(Kk), tcrossprod(Qy, Kk))

        #compute log-likelihood
        if (logLik) {
          e <- as.matrix(y[i, good] - f[i, good])
          ll <- ll + sum(good)*log(2*pi) + sum(log(eigen(Qy)$values)) +
            crossprod(e, tcrossprod(solve(Qy, tol=1e-30), t(e)))}
      }
    }
  }
  ans <- list(m = m, C = C, a = a, R = R, f = f)

  attributes(ans$f) <- yAttr

  if (logLik)
    ans <- c(ans, logLik = 0.5*ll)

  if (simplify)
    ans <- c(mod = list(mod1), kappa = list(kappa),
             GGfunction = list(GGfunction), FFfunction = list(FFfunction),
             sqrtMethod=list(sqrtMethod), ans)
  else {
    attributes(y) <- yAttr
    ans <- c(y = list(y), mod = list(mod1), kappa = list(kappa),
             GGfunction = list(GGfunction), FFfunction = list(FFfunction),
             sqrtMethod=list(sqrtMethod), ans)
  }
  return(ans)
}


#' RTS smoother for the Unscented Kalman filter (UKF)
#'
#' Implements fixed-interval smoothing for the
#' Unscented Kalman filter (UKF). The function uses the Rauch
#' Tung, and Striebel (RTS) smoother for computing the smoothed estimates.
#'
#' @param filterData output from UKF
#'
#' @return list containing the smoothed state estimates "s" and the covariances "S"
#' @references Sarkka, 2008 "Unscented Rauch-Tung-Striebel Smoother"
#' @author Stefan Gelissen
#' @export
#'
UKFsmooth <- function (filterData) {

  mod <- filterData
  mAttr <- attributes(mod$m)
  mod$m <- as.matrix(mod$m)
  mod$a <- as.matrix(mod$a)
  mod$W <- as.matrix(mod$mod$W)
  kappa <- mod$kappa
  sqrtMethod <- mod$sqrtMethod

  n <- length(mod$R)
  p <- ncol(mod$m)
  w <- as.vector(c(kappa/(p+kappa), rep(1/(2*(p+kappa)), 2*p)))
  s <- rbind(matrix(0, n, p), mod$m[n + 1, ])
  S <- vector("list", length = n + 1)

  S[[n + 1]] <- mod$C[[n + 1]]

  if (n > 0)
    for (i in n:1) {

      #compute sigma points
      if (sqrtMethod=="Cholesky") {
        sigmaPlus <- t(chol((p+kappa)*mod$C[[i]]))}
      else {
        tmpxs <- La.svd((p+kappa)*mod$C[[i]], nu=0)
        sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
      sigmax <- t(mod$m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
      tmpx <- matrix(sapply(1:nrow(sigmax),
                            function (x) GGfunction(x=sigmax[x,], k=i)), nrow=p)
      #cross covariance between a priori state estimate (at k+1) and
      #posterior state estimate (at k)
      Qxy <- tcrossprod(crossprod(t(t(sigmax)-mod$m[i, ]), diag(w)),
                        tmpx-mod$a[i, ])

      #smoother Kalman gain
      Kk <- crossprod(t(Qxy), solve(mod$R[[i]], tol=1e-30))
      #smoothed state estimate
      s[i, ] <- mod$m[i, ] + crossprod(t(Kk), s[i + 1, ] - mod$a[i,])
      #smoothed error covariance
      S[[i]] <- mod$C[[i]] + tcrossprod(crossprod(t(Kk), S[[i + 1]] - mod$R[[i]]),
                                        Kk)

    }
  ans <- list(s = s, S = S)

  attributes(ans$s) <- mAttr
  return(ans)
}


#' UKF forecast
#'
#' predicts future system states and observations for the Unscented Kalman Filter (UKF).
#'
#' @param filterData an object as returned by UKF.
#' @param nAhead  the number of steps ahead for which to predict system states
#'
#' @return
#' @export
#' @author Stefan Gelissen
#'
UKFforecast <- function (filterData, nAhead=1) {

  mod <- filterData

  GGfunction <- mod$GGfunction
  FFfunction <- mod$FFfunction
  kappa <- mod$kappa
  sqrtMethod <- mod$sqrtMethod

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
  w <- as.vector(c(kappa/(p+kappa), rep(1/(2*(p+kappa)), 2*p)))

  for (it in 1:nAhead) {

    ##future states

    #compute sigma points
    if (sqrtMethod=="Cholesky") {
      sigmaPlus <- t(chol((p+kappa)*R[[it]]))}
    else {
      tmpxs <- La.svd((p+kappa)*R[[it]], nu=0)
      sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
    sigmax <- t(a[it, ] + cbind(0, sigmaPlus, -sigmaPlus))
    #a priori state estimate
    tmpx <- matrix(sapply(1:nrow(sigmax),
                          function (x) GGfunction(x=sigmax[x,], k=nobs+it)), nrow=p)
    a[it + 1, ] <- tcrossprod(w, tmpx)
    #a priori error covariance
    R[[it + 1]] <- tcrossprod(crossprod(t(tmpx-a[it, ]), diag(w)), tmpx-a[it, ]) + mod$W

    ##future observations

    #compute sigma points
    if (sqrtMethod=="Cholesky") {
      sigmaPlus <- t(chol((p+kappa)*R[[it + 1]]))}
    else {
      tmpys <- La.svd((p+kappa)*R[[it + 1]], nu=0)
      sigmaPlus <- t(sqrt(tmpys$d)*tmpys$vt)}
    sigmay <- t(a[it + 1, ] + cbind(0, sigmaPlus, -sigmaPlus))
    #predicted measurement
    tmpy <- matrix(sapply(1:nrow(sigmay),
                          function (x) FFfunction(x=sigmay[x,], k=nobs+it)), nrow=ym)
    f[it, ] <- tcrossprod(w, tmpy)
    #covariance of predicted measurement
    Q[[it]] <- tcrossprod(crossprod(t(tmpy-f[it, ]), diag(w)), tmpy-f[it, ]) + mod$V

  }
  a <- a[-1, , drop = FALSE]
  R <- R[-1]
  ans <- list(a = a, R = R, f = f, Q = Q)

  return(ans)
}
