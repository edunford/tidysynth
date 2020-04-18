#' implement_synth
#'
#' [AUX Function] Original synthetic control method proposed by (Abadie et al.
#' 2003, 2010, 2015) and implemented in `synth` package.
#'
#' Synth works as the main engine of the `tidysynth` package. More on the method
#' and estimation procedures can be found in (Abadie et al. 2010).
#'
#' @param data.prep.obj
#' @param X1
#' @param X0
#' @param Z0
#' @param Z1
#' @param custom.v
#' @param optimxmethod
#' @param genoud
#' @param quadopt
#' @param Margin.ipop
#' @param Sigf.ipop
#' @param Bound.ipop
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
synth_method <- function (data.prep.obj = NULL,
                          X1 = NULL,
                          X0 = NULL,
                          Z0 = NULL,
                          Z1 = NULL,
                          custom.v = NULL,
                          optimxmethod = c("Nelder-Mead", "BFGS"),
                          genoud = FALSE,
                          quadopt = "ipop",
                          Margin.ipop = 5e-04,
                          Sigf.ipop = 5,
                          Bound.ipop = 10,
                          verbose = FALSE, ...)
{
  if (is.null(data.prep.obj) == FALSE) {
    cat("\nX1, X0, Z1, Z0 all come directly from dataprep object.\n\n")
    X1 <- data.prep.obj$X1
    Z1 <- data.prep.obj$Z1
    X0 <- data.prep.obj$X0
    Z0 <- data.prep.obj$Z0
  }
  else {
    cat("X1,X0,Z1,Z0 were individually input (not dataprep object.)\n\n")
  }
  store <- list(X1 = X1, X0 = X0, Z1 = Z1, Z0 = Z0)
  for (i in 1:4) {
    if (is.null(store[[i]])) {
      stop(paste("\n", names(store)[i], "is missing \n"))
    }
    if (sum(is.na(store[[i]])) > 0) {
      stop(paste("\n NAs in", names(store)[i], "\n"))
    }
    if (is.matrix(store[[i]]) == FALSE) {
      stop(paste("\n", names(store)[i], "is not a matrix object\n"))
    }
  }
  if (ncol(X1) != 1) {
    stop("\n Please specify only one treated unit: X1 has to have ncol= 1")
  }
  if (ncol(Z1) != 1) {
    stop("\n Please specify only one treated unit: Z1 has to have ncol= 1")
  }
  if (ncol(X0) < 2) {
    stop("\n Please specify at least two control units: X0 has to have ncol >= 2 ")
  }
  if (ncol(Z0) < 2) {
    stop("\n Please specify only one treated unit: Z0 has to have ncol >= 2")
  }
  if (nrow(Z0) != nrow(Z1)) {
    stop("\n Different number of periods for treated and controls: nrow(Z0) unequal nrow(Z1)")
  }
  if (nrow(X0) != nrow(X1)) {
    stop("\n Different number of predictors for treated and controls: nrow(X0) unequal nrow(X1)")
  }
  if (nrow(X0) == 0) {
    stop("No predictors specified. Please specify at least on predictor")
  }
  if (nrow(Z0) == 0) {
    stop("No periods specified for Z1 and Z0. Please specify at least on period")
  }
  if (0 %in% apply(X0, 1, sd)) {
    stop("\n At least one predictor in X0 has no variation across control units. Please remove this predictor.")
  }
  nvarsV <- dim(X0)[1]
  big.dataframe <- cbind(X0, X1)
  divisor <- sqrt(apply(big.dataframe, 1, var))
  scaled.matrix <- t(t(big.dataframe) %*% (1/(divisor) * diag(rep(dim(big.dataframe)[1],
                                                                  1))))
  X0.scaled <- scaled.matrix[, c(1:(dim(X0)[2]))]
  if (is.vector(X0.scaled) == TRUE) {
    X0.scaled <- t(as.matrix(X0.scaled))
  }
  X1.scaled <- scaled.matrix[, dim(scaled.matrix)[2]]
  if (is.null(custom.v) & nrow(X0) != 1) {
    cat("\n****************", "\n searching for synthetic control unit  \n",
        "\n")
    if (genoud == TRUE) {
      require(rgenoud)
      cat("\n****************", "\n genoud() requested for optimization\n",
          "\n")
      rgV.genoud <- genoud(fn.V, nvarsV, X0.scaled = X0.scaled,
                           X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt,
                           margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop,
                           bound.ipop = Bound.ipop)
      SV1 <- rgV.genoud$par
      cat("\n****************", "\n genoud() finished, now running local optimization using optim()\n",
          "\n")
    }
    else {
      SV1 <- rep(1/nvarsV, nvarsV)
    }
    all.methods <- FALSE
    if (sum(optimxmethod %in% c("All")) == 1) {
      all.methods <- TRUE
    }
    rgV.optim.1 <- optimx(par = SV1, fn = fn.V, gr = NULL,
                          hess = NULL, method = optimxmethod, itnmax = NULL,
                          hessian = FALSE, control = list(kkt = FALSE, starttests = FALSE,
                                                          dowarn = FALSE, all.methods = all.methods), X0.scaled = X0.scaled,
                          X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt,
                          margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop,
                          bound.ipop = Bound.ipop)
    if (verbose == TRUE) {
      print(rgV.optim.1)
    }
    rgV.optim.1 <- collect.optimx(rgV.optim.1, "min")
    Xall <- cbind(X1.scaled, X0.scaled)
    Xall <- cbind(rep(1, ncol(Xall)), t(Xall))
    Zall <- cbind(Z1, Z0)
    Beta <- try(solve(t(Xall) %*% Xall) %*% t(Xall) %*% t(Zall),
                silent = TRUE)
    if (inherits(Beta, "try-error")) {
      rgV.optim <- rgV.optim.1
    }
    else {
      Beta <- Beta[-1, ]
      V <- Beta %*% t(Beta)
      SV2 <- diag(V)
      SV2 <- SV2/sum(SV2)
      rgV.optim.2 <- optimx(par = SV2, fn = fn.V, gr = NULL,
                            hess = NULL, method = optimxmethod, itnmax = NULL,
                            hessian = FALSE, control = list(kkt = FALSE,
                                                            starttests = FALSE, dowarn = FALSE, all.methods = all.methods),
                            X0.scaled = X0.scaled, X1.scaled = X1.scaled,
                            Z0 = Z0, Z1 = Z1, quadopt = quadopt, margin.ipop = Margin.ipop,
                            sigf.ipop = Sigf.ipop, bound.ipop = Bound.ipop)
      if (verbose == TRUE) {
        print(rgV.optim.2)
      }
      rgV.optim.2 <- collect.optimx(rgV.optim.2, "min")
      if (verbose == TRUE) {
        cat("\n Equal weight loss is:", rgV.optim.1$value,
            "\n")
        cat("\n Regression Loss is:", rgV.optim.2$value,
            "\n")
      }
      if (rgV.optim.1$value < rgV.optim.2$value) {
        rgV.optim <- rgV.optim.1
      }
      else {
        rgV.optim <- rgV.optim.2
      }
    }
    solution.v <- abs(rgV.optim$par)/sum(abs(rgV.optim$par))
  }
  else {
    cat("\n****************", "\n optimization over w weights: computing synthtic control unit \n",
        "\n\n")
    if (nrow(X0) == 1) {
      custom.v <- 1
    }
    else {
      cat("\n****************", "\n v weights supplied manually: computing synthtic control unit \n",
          "\n\n")
      if (length(custom.v) != nvarsV) {
        stop("custom.V misspecified: length(custom.V) != nrow(X1)")
      }
      if (mode(custom.v) != "numeric") {
        stop("custom.V must be numeric")
      }
    }
    rgV.optim <- NULL
    solution.v <- abs(custom.v)/sum(custom.v)
  }
  V <- diag(x = as.numeric(solution.v), nrow = nvarsV, ncol = nvarsV)
  H <- t(X0.scaled) %*% V %*% (X0.scaled)
  a <- X1.scaled
  c <- -1 * c(t(a) %*% V %*% (X0.scaled))
  A <- t(rep(1, length(c)))
  b <- 1
  l <- rep(0, length(c))
  u <- rep(1, length(c))
  r <- 0
  if (quadopt == "ipop") {
    res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u,
                r = r, margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop,
                bound = Bound.ipop)
    solution.w <- as.matrix(primal(res))
  }
  else {
    if (quadopt == "LowRankQP") {
      res <- LowRankQP(Vmat = H, dvec = c, Amat = A, bvec = 1,
                       uvec = rep(1, length(c)), method = "LU")
      solution.w <- as.matrix(res$alpha)
    }
  }
  rownames(solution.w) <- colnames(X0)
  colnames(solution.w) <- "w.weight"
  names(solution.v) <- rownames(X0)
  loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*% V %*%
    (X1.scaled - X0.scaled %*% solution.w)
  loss.v <- t(Z1 - Z0 %*% as.matrix(solution.w)) %*% (Z1 -
                                                        Z0 %*% as.matrix(solution.w))
  loss.v <- loss.v/nrow(Z0)
  cat("\n****************", "\n****************", "\n****************",
      "\n\nMSPE (LOSS V):", loss.v, "\n\nsolution.v:\n", round(as.numeric(solution.v),
                                                               10), "\n\nsolution.w:\n", round(as.numeric(solution.w),
                                                                                               10), "\n\n")
  optimize.out <- list(solution.v = solution.v, solution.w = solution.w,
                       loss.v = loss.v, loss.w = loss.w, custom.v = custom.v,
                       rgV.optim = rgV.optim)
  return(invisible(optimize.out))
}
