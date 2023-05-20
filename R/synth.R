#' synth_method
#'
#' AUX Function: Original synthetic control method proposed by (Abadie et al.
#' 2003, 2010, 2015) and implemented in `synth` package. Method has been
#' commandeered for internal use here.
#'
#' Synth works as the main engine of the `tidysynth` package. More on the method
#' and estimation procedures can be found in (Abadie et al. 2010).
#'
#' @param treatment_unit_covariates matrix of treated predictor data
#' @param control_units_covariates matrix of controls' predictor data.
#' @param control_units_outcome matrix of controls' outcome data for the
#'   pre-treatment periods over which MSPE is to be minimized.
#' @param treatment_unit_outcome matrix of treated outcome data for the
#'   pre-treatment periods over which MSPE is to be minimized.
#' @param custom.v vector of weights for predictors supplied by the user. uses
#'   synth to bypass optimization for solution.V. See details.
#' @param optimxmethod string vector that specifies the optimization algorithms
#'   to be used. Permissible values are all optimization algorithms that are
#'   currently implemented in the optimx function (see this function for
#'   details). This list currently includes c("Nelder-Mead', 'BFGS', 'CG',
#'   'L-BFGS-B', 'nlm', 'nlminb', 'spg', and 'ucminf"). If multiple algorithms
#'   are specified, synth will run the optimization with all chosen algorithms
#'   and then return the result for the best performing method. Default is
#'   c("Nelder-Mead", "BFGS"). As an additional possibility, the user can also
#'   specify 'All' which means that synth will run the results over all
#'   algorithms in optimx.
#' @param genoud Logical flag. If true, synth embarks on a two step
#'   optimization. In the first step, genoud, an optimization function that
#'   combines evolutionary algorithm methods with a derivative-based
#'   (quasi-Newton) method to solve difficult optimization problems, is used to
#'   obtain a solution. See genoud for details. In the second step, the genoud
#'   results are passed to the optimization algorithm(s) chosen in optimxmethod
#'   for a local optimization within the neighborhood of the genoud solution.
#'   This two step optimization procedure will require much more computing time,
#'   but may yield lower loss in cases where the search space is highly
#'   irregular.
#' @param Margin.ipop setting for ipop optimization routine: how close we get to
#'   the constrains (see ipop for details)
#' @param Sigf.ipop setting for ipop optimization routine: Precision (default: 7
#'   significant figures (see ipop for details)
#' @param Bound.ipop setting for ipop optimization routine: Clipping bound for
#'   the variables (see ipop for details)
#' @param verbose Logical flag. If TRUE then intermediate results will be shown.
#' @param ... Additional arguments to be passed to optimx and or genoud to
#'   adjust optimization.
#'
#' @details As proposed in Abadie and Gardeazabal (2003) and Abadie, Diamond,
#'   Hainmueller (2010), the synth function routinely searches for the set of
#'   weights that generate the best fitting convex combination of the control
#'   units. In other words, the predictor weight matrix V is chosen among all
#'   positive definite diagonal matrices such that MSPE is minimized for the
#'   pre-intervention period. Instead of using this data-driven procedures to
#'   search for the best fitting synthetic control group, the user may supply
#'   his own vector of V weights, based on his subjective assessment of the
#'   predictive power of the variables in treatment_unit_covariates and
#'   control_units_covariates. In this case, the vector of V weights for each
#'   variable should be supplied via the custom.V option in synth and the
#'   optimization over the V matrices is bypassed.
#'
#' @return solution.v =	vector of predictor weights; solution.w = vector of
#'   weights across the controls; loss.v	= MSPE from optimization over v and w
#'   weights; loss.w	= Loss from optimization over w weights; custom.v	=if this
#'   argument was specified in the call to synth, this outputs the weight vector
#'   specified; rgV.optim = Results from optimx() minimization. Could be used
#'   for diagnostics.
#'
synth_method <- function (treatment_unit_covariates = NULL,
                          control_units_covariates = NULL,
                          control_units_outcome = NULL,
                          treatment_unit_outcome = NULL,
                          custom.v = NULL,
                          optimxmethod = c("Nelder-Mead", "BFGS"),
                          genoud = FALSE,
                          Margin.ipop = 5e-04,
                          Sigf.ipop = 5,
                          Bound.ipop = 10,
                          verbose = FALSE,
                          ...)
{
  # Allocate the relevant data to list
  X1 = treatment_unit_covariates
  X0 = control_units_covariates
  Z1 = treatment_unit_outcome
  Z0 = control_units_outcome
  store <- list(X1 = treatment_unit_covariates,
                X0 = control_units_covariates,
                Z1 = treatment_unit_outcome,
                Z0 = control_units_outcome)
  # Optimization option
  quadopt = "ipop"

  # Checks
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
  if (ncol(treatment_unit_covariates) != 1) {
    stop("\n Please specify only one treated unit.")
  }
  if (ncol(treatment_unit_outcome) != 1) {
    stop("\n Please specify only one treated unit.")
  }
  if (ncol(control_units_covariates) < 2) {
    stop("\n Please specify at least two control units.")
  }
  if (ncol(control_units_outcome) < 2) {
    stop("\n Please specify only one treated unit.")
  }
  if (nrow(control_units_outcome) != nrow(Z1)) {
    stop("\n Different number of periods for treated and controls.")
  }
  if (nrow(control_units_covariates) != nrow(X1)) {
    stop("\n Different number of predictors for treated and controls.")
  }
  if (nrow(control_units_covariates) == 0) {
    stop("No predictors specified. Please specify at least on predictor")
  }
  if (nrow(control_units_outcome) == 0) {
    stop("No periods specified for Z1 and Z0. Please specify at least one period")
  }
  if (0 %in% apply(control_units_covariates, 1, sd)) {
    stop("\n At least one predictor in X0 has no variation across control units. Please remove this predictor.")
  }

  # AUX. functions
  collect.optimx <-
    function (res, opt = "min") {
      if (opt == "min") {
        res <- res[order(res$value, decreasing = FALSE), ]
      }
      if (opt == "max") {
        res <- res[order(res$value, decreasing = TRUE), ]
      }
      val <- which(colnames(res) == "value")
      pars <- 1:(val - 1)
      out <- list(out.list = res, par = res[1, pars], value = res[1,
                                                                  val])
      return(invisible(out))
    }


  fn.V <- function (variables.v = stop("variables.v missing"),
                    X0.scaled = stop("X0.scaled missing"),
                    X1.scaled = stop("X1.scaled missing"),
                    Z0 = stop("Z0 missing"),
                    Z1 = stop("Z1 missing"),
                    margin.ipop = 5e-04,
                    sigf.ipop = 5,
                    bound.ipop = 10,
                    quadopt = "ipop")
  {
    Check <- sum(quadopt %in% c("ipop", "LowRankQP"))
    if (Check != 1) {
      stop("option quadopt must be one of ipop or LowRankQP")
    }
    V <- diag(x = as.numeric(abs(variables.v)/sum(abs(variables.v))),
              nrow = length(variables.v), ncol = length(variables.v))
    H <- t(X0.scaled) %*% V %*% (X0.scaled)
    a <- X1.scaled
    c <- -1 * c(t(a) %*% V %*% (X0.scaled))
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0
    res <- kernlab::ipop(c = c, H = H, A = A, b = b, l = l, u = u,
                         r = r, bound = bound.ipop, margin = margin.ipop,
                         maxiter = 1000, sigf = sigf.ipop)
    solution.w <- as.matrix(kernlab::primal(res))
    loss.w <- as.numeric(t(X1.scaled - X0.scaled %*% solution.w) %*%
                           (V) %*% (X1.scaled - X0.scaled %*% solution.w))
    loss.v <- as.numeric(t(Z1 - Z0 %*% solution.w) %*% (Z1 -
                                                          Z0 %*% solution.w))
    loss.v <- loss.v/nrow(Z0)
    return(invisible(loss.v))
  }


  # Main algorithm
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

    if (genoud == TRUE) {

      rgV.genoud <- rgenoud::genoud(fn.V, nvarsV, X0.scaled = X0.scaled,
                                    X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt,
                                    margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop,
                                    bound.ipop = Bound.ipop)
      SV1 <- rgV.genoud$par

    }
    else {

      SV1 <- rep(1/nvarsV, nvarsV)

    }

    all.methods <- FALSE
    if (sum(optimxmethod %in% c("All")) == 1) {
      all.methods <- TRUE
    }

    rgV.optim.1 <- optimx::optimx(par = SV1, fn = fn.V, gr = NULL,
                                  hess = NULL, method = optimxmethod, itnmax = NULL,
                                  hessian = FALSE, control = list(kkt = FALSE, starttests = FALSE,
                                                                  dowarn = FALSE, all.methods = all.methods), X0.scaled = X0.scaled,
                                  X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt,
                                  margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop,
                                  bound.ipop = Bound.ipop)

    if (verbose == TRUE) {
      print(rgV.optim.1)
    }

    # Gather optimization output
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
      rgV.optim.2 <- optimx::optimx(par = SV2, fn = fn.V, gr = NULL,
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
    if (nrow(X0) == 1) {
      custom.v <- 1
    }
    else {

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
  if(quadopt == "ipop"){
    res <- kernlab::ipop(c = c, H = H, A = A, b = b, l = l, u = u,
                         r = r, margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop,
                         bound = Bound.ipop)
    solution.w <- as.matrix(kernlab::primal(res))
  }
  rownames(solution.w) <- colnames(X0)
  colnames(solution.w) <- "w.weight"
  names(solution.v) <- rownames(X0)
  loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*% V %*%
    (X1.scaled - X0.scaled %*% solution.w)
  loss.v <- t(Z1 - Z0 %*% as.matrix(solution.w)) %*% (Z1 -
                                                        Z0 %*% as.matrix(solution.w))
  loss.v <- loss.v/nrow(Z0)
  optimize.out <- list(solution.v = solution.v, solution.w = solution.w,
                       loss.v = loss.v, loss.w = loss.w, custom.v = custom.v,
                       rgV.optim = rgV.optim)
  return(invisible(optimize.out))
}
