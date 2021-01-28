#' synthetic_control
#'
#' `synthetic_control()` declares the input data frame for use in the synthetic
#' control method. Allows for the specification of the panel units along with
#' the intervention unit and time (`treated`). All units that are not the
#' designated treated units are entered into the donor pool from which the
#' synthetic control is generated. All time points prior and equal to the
#' intervention time are designated as the pre-intervention period; and all time
#' periods after are the post-intervention period.
#'
#' Note that `synthetic_control()` also allows for the simultaneous generation
#' of placebo units (i.e. units where the treated unit is one of the controls).
#' The addition of the placebo units increases computation time (as a synthetic
#' control needs to be generated for each placebo unit) but it allows for
#' inference as outlined in Abadie et al. 2010.
#'
#' @param data panel data frame in long format (i.e. unit of analysis is
#'   unit-time period, such as country-year) containing both treated and control
#'   donor pool units. All units/time periods that are not desired to be in the
#'   donor should be excluded prior to passing to `synthetic_control()`.
#' @param outcome Name of the outcome variable. Outcome variable should be a
#'   continuous measure that is observed across multiple time points.
#' @param unit Name of the case unit variable in the panel data.
#' @param time Name of the time unit variable in the panel data.
#' @param i_unit Name of the treated case unit where the intervention occurred.
#' @param i_time Name of the treated time period when the intervention occurred.
#' @param generate_placebos logical flag requesting that placebo versions of the
#'   data be generated for downstream inferential methods. Generates a version
#'   of the nested data where each control unit is the intervention unit.
#'   Default is TRUE.
#'
#' @return `tbl_df` with nested fields containing the following:
#'
#'   - `.id`: unit id for the intervention case (this will differ when a placebo
#'   unit).
#'
#'   - `.placebo`: indicator field taking on the value of 1 if a unit is a
#'   placebo unit, 0 if it's the specified treated unit.
#'
#'   - `.type`: type of the nested data construct: `treated` or `controls`.
#'   Keeps tract of which data construct is located in `.outcome` field.
#'
#'   - `.outcome`: nested data construct containing the outcome variable
#'   configured for the sythnetic control method. Data is configured into a wide
#'   formate for the optimization task.
#'
#'   - `.original_data`: original impute data filtered by treated or control
#'   units. This allows for easy processing down stream when generating
#'   predictors.
#'
#'   - `.meta`: stores information regarding the unit and time index, the
#'   treated unit and time and the name of the outcome variable. Used downstream
#'   in subsequent functions.
#'
#' @export
#'
#' @examples
#'
#' ############################
#' ###### Basic Example #######
#' ############################
#'
#' \donttest{
#' # Smoking example data
#' data(smoking)
#'
#' # initial the synthetic control object
#' smoking_out <-
#' smoking %>%
#' synthetic_control(outcome = cigsale,
#'                   unit = state,
#'                   time = year,
#'                   i_unit = "California",
#'                   i_time = 1988,
#'                   generate_placebos= FALSE)
#'
#' # data configuration
#' dplyr::glimpse(smoking_out)
#'
#' # Grap the organized outcome variables
#' smoking_out %>% grab_outcome(type = "treated")
#' smoking_out %>% grab_outcome(type = "controls")
#'
#'
#' ###################################
#' ####### Full implementation #######
#' ###################################
#'
#'
#' # Smoking example data
#' data(smoking)
#'
#' smoking_out <-
#' smoking %>%
#'
#' # initial the synthetic control object
#' synthetic_control(outcome = cigsale,
#'                   unit = state,
#'                   time = year,
#'                   i_unit = "California",
#'                   i_time = 1988,
#'                   generate_placebos= FALSE) %>%
#'
#' # Generate the aggregate predictors used to generate the weights
#'   generate_predictor(time_window=1980:1988,
#'                      lnincome = mean(lnincome, na.rm = TRUE),
#'                      retprice = mean(retprice, na.rm = TRUE),
#'                      age15to24 = mean(age15to24, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1984:1988,
#'                      beer = mean(beer, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1975,
#'                      cigsale_1975 = cigsale) %>%
#'
#'   generate_predictor(time_window=1980,
#'                      cigsale_1980 = cigsale) %>%
#'
#'   generate_predictor(time_window=1988,
#'                      cigsale_1988 = cigsale) %>%
#'
#'
#'   # Generate the fitted weights for the synthetic control
#'   generate_weights(optimization_window =1970:1988,
#'                    Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>%
#'
#'   # Generate the synthetic control
#'   generate_control()
#'
#' # Plot the observed and synthetic trend
#' smoking_out %>% plot_trends(time_window = 1970:2000)
#'
#' }
#'
#'
synthetic_control <- function(data = NULL,
                              outcome = NULL,
                              unit = NULL,
                              time = NULL,
                              i_unit=NULL,
                              i_time =NULL,
                              generate_placebos = TRUE){
  UseMethod("synthetic_control")
}


#' @export
synthetic_control.data.frame <- function(data = NULL,
                                         outcome = NULL,
                                         unit = NULL,
                                         time = NULL,
                                         i_unit=NULL,
                                         i_time =NULL,
                                         generate_placebos = TRUE){


  # AUX FUNCTION ------------------------------------------------------------

  # **generate_nested_data**:  Auxiliary function to build out the treated and
  # control units. Takes each data scenario one at a time (e.g. the observed
  # treated and the controls serving as placebo treatment units) inside
  # `synthetic_control`
  generate_nested_data <- function(data=NULL,
                                   i_unit = NULL,
                                   placebo = NULL,
                                   id = 1,...){


    # configure the outcome for the treated unit
    trt_outcome <-
      data %>%
      dplyr::filter(!!time <= i_time & !!unit == i_unit) %>% # Pre-intervention period
      dplyr::select(!!time,!!unit,!!outcome) %>%
      tidyr::pivot_wider(names_from=!!unit,values_from=!!outcome) %>%
      dplyr::rename(time_unit = !!time)

    # configure the outcome for the control units
    cnt_outcome <-
      data %>%
      dplyr::filter(!!time <= i_time & !!unit != i_unit) %>% # Pre-intervention period
      dplyr::select(!!time,!!unit,!!outcome) %>%
      tidyr::pivot_wider(names_from=!!unit,values_from=!!outcome) %>%
      dplyr::rename(time_unit = !!time)

    # initialize the nested output
    nested_output <-
      tibble::tibble(.id = id,
                     .placebo = placebo,
                     .type = c("treated","controls"),
                     .outcome = list(trt_outcome,cnt_outcome),
                     .original_data = list(data %>% dplyr::filter(!!unit == i_unit),
                                           data %>% dplyr::filter(!!unit != i_unit)),
                     .meta = list(tibble::tibble(unit_index=rlang::quo_text(unit),
                                                 time_index=rlang::quo_text(time),
                                                 treatment_unit = i_unit,
                                                 treatment_time = i_time,
                                                 outcome = rlang::quo_text(outcome))))
    return(nested_output)
  }



  # Main  -----------------------------------------------------

  # Quasi-quosures
  outcome <- rlang::enquo(outcome)
  unit <- rlang::enquo(unit)
  time <- rlang::enquo(time)

  # specify all relevant units to generate treated and placebo data frames
  all_units <-
    data %>%
    dplyr::distinct(!!unit) %>%
    dplyr::mutate(placebo = ifelse(!!unit != i_unit,1,0)) %>%
    dplyr::rename(unit_name = !!unit) %>%
    dplyr::arrange(placebo)


  # Build up dataset with treated and placebo data types
  master_nest <- NULL
  for ( i in 1:nrow(all_units) ){

    # unit ids
    unit_name = all_units$unit_name[i]
    placebo = all_units$placebo[i]

    # Build up master nested data frame with treated and placebo frames.
    master_nest <-
      generate_nested_data(data=data, i_unit = unit_name,
                           placebo = placebo,id=unit_name) %>%
      dplyr::bind_rows(master_nest,.)

    # Break cycle if now placebo units are requested.
    if(i==1 & !generate_placebos){break()}
  }

  # return the entire nested object.
  return(master_nest)
}



#' generate_predictor
#'
#' Create one or more scalar variables summarizing covariate data across a
#' specified time window. These predictor variables are used to fit the
#' synthetic control.
#'
#' matrices of aggregate-level covariates to be used in the following
#' minimization task.
#'
#' \deqn{W^*(V) =  min \sum^M_{m=1} v_m (X_{1m} - \sum^{J+1}_{j=2}w_j X_{jm})^2}
#'
#' The importance of the generate predictors are determine by vector \eqn{V},
#' and the weights that determine unit-level importance are determined by vector
#' \eqn{W}. The nested optimation task seeks to find optimal values of \eqn{V}
#' and \eqn{W}. Note also that \eqn{V} can be provided by the user. See
#' `?generate_weights()`.
#'
#'
#' @param data nested data of type `tbl_df` generated from
#'   `sythetic_control()`. See `synthetic_control()` documentation for more
#'   information.
#' @param time_window set time window from the pre-intervention period that the
#'   data should be aggregated across to generate the specific predictor.
#'   Default is to use the entire pre-intervention period.
#' @param ... Name-value pairs of summary functions. The name will be the name
#'   of the variable in the result. The value should be an expression that
#'   returns a single value like min(x), n(), or sum(is.na(y)). Note that for
#'   all summary functions `na.rm = TRUE` argument should be specified as
#'   aggregating across units with missing values is a common occurrence.
#'
#' @return `tbl_df` with nested fields containing the following:
#'
#'   - `.id`: unit id for the intervention case (this will differ when a placebo
#'   unit).
#'
#'   - `.placebo`: indicator field taking on the value of 1 if a unit is a
#'   placebo unit, 0 if it's the specified treated unit.
#'
#'   - `.type`: type of the nested data construct: `treated` or `controls`.
#'   Keeps tract of which data construct is located in `.outcome` field.
#'
#'   - `.outcome`: nested data construct containing the outcome variable
#'   configured for the sythnetic control method. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.predictors`: nested data construct containing the covariate matrices
#'   for the treated and control (donor) units. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.original_data`: original impute data filtered by treated or control
#'   units. This allows for easy processing down stream when generating
#'   predictors.
#'
#'   - `.meta`: stores information regarding the unit and time index, the
#'   treated unit and time and the name of the outcome variable. Used downstream
#'   in subsequent functions.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # Smoking example data
#' data(smoking)
#'
#' smoking_out <-
#' smoking %>%
#'
#' # initial the synthetic control object
#' synthetic_control(outcome = cigsale,
#'                   unit = state,
#'                   time = year,
#'                   i_unit = "California",
#'                   i_time = 1988,
#'                   generate_placebos= FALSE) %>%
#'
#' # Generate the aggregate predictors used to generate the weights
#'   generate_predictor(time_window=1980:1988,
#'                      lnincome = mean(lnincome, na.rm = TRUE),
#'                      retprice = mean(retprice, na.rm = TRUE),
#'                      age15to24 = mean(age15to24, na.rm = TRUE))
#'
#' # Extract respective predictor matrices
#' smoking_out %>% grab_predictors(type = "treated")
#' smoking_out %>% grab_predictors(type = "controls")
#'
#' }
#'
generate_predictor <- function(data,time_window=NULL,...){
  UseMethod("generate_predictor")
}

#' @export
generate_predictor <- function(data,time_window=NULL,...){


  # Auxiliary function for generating a predictor for a single configuration of
  # the data (e.g. the treated and placebo sets)
  aux_gen_pred <- function(data,time_window=NULL,...){

    # Grab meta data re: relevant indices
    time_index <- data$.meta[[1]]$time_index[1]
    unit_index <- data$.meta[[1]]$unit_index[1]

    # INTERNAL FUNCTIONS
    extract_predictor <- function(data,type = "treated",...){
      # Aux. function for processing predictors. Takes in quosure statement,
      # passes it internally to summarize, and organizes data to behave
      # accordingly as a synth_method input.

      data %>%
        dplyr::filter(.type==type) %>%
        dplyr::select(.original_data) %>%
        tibble::as_tibble() %>%
        tidyr::unnest(cols=c(.original_data)) %>%

        # Only relevant time units (as specified by the window)
        dplyr::filter(.data[[time_index]] %in% time_window) %>%

        # Group by the unit axis
        dplyr::group_by(.data[[unit_index]]) %>%
        dplyr::summarize(...,.groups='drop') %>%

        # Convert to the relevant format
        tidyr::gather(variable,value,-.data[[unit_index]]) %>%
        tidyr::spread(.data[[unit_index]],value)
    }

    assign_to_predictors <- function(data=NULL,candidate_data=NULL,type = "treated"){
      # Aux function for assigning the newly generated predictor to the predictor
      # frame.

      data %>%

        # Select the relevant data type
        dplyr::filter(.type == type) %>%

        # extract the existing predictors set and unnest.
        dplyr::select(.predictors) %>%
        tibble::as_tibble() %>%
        tidyr::unnest(cols=.predictors) %>%

        # Drop any prior generations of the same variable
        dplyr::filter(!any(candidate_data$variable %in% variable)) %>%

        # Bind on the new candidate data
        dplyr::bind_rows(candidate_data)
    }


    # If a specific aggregation period is NOT specified, use the whole
    # pre-treatment period (don't use any unit in the post-treatment environment)
    if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit}


    # Extract the relevant predictors for both unit types
    pred_trt = extract_predictor(data,type="treated",...)
    pred_cnt = extract_predictor(data,type="controls",...)


    # Check if there are NAs in the generated frame. If so, return an error.
    if(any(is.na(pred_trt)) | any(is.na(pred_cnt))){
      stop("NA generated in specified predictors. Consider using `rm.na= TRUE` in aggregation function or specifying a larger/different time window")
    }


    # If a .predictors column has yet to be generated, generate it.
    if(!".predictors" %in% colnames(data)){

      data <- data %>%
        dplyr::mutate(.predictors = list(pred_trt,pred_cnt)) %>%
        dplyr::select(.id:.outcome,.predictors,dplyr::everything())

    } else{  # overwrite existing set up

      data <- data %>%
        dplyr::mutate(.predictors = list(assign_to_predictors(data,pred_trt,type="treated"),
                                         assign_to_predictors(data,pred_cnt,type="controls")))

    }

    return(data)
  }


  # Iterate through the versions of the data, and generate the predictors for
  # each version of the data (e.g. the treated and placebo sets)
  data_versions <- unique(data$.id)
  master_nest <- NULL
  for (v in data_versions){
    master_nest <-
      data %>%
      dplyr::filter(.id == v) %>%
      aux_gen_pred(data=.,time_window = time_window,...) %>%
      dplyr::bind_rows(master_nest,.)
  }


  return(master_nest)
}



#' generate_weights
#'
#' Generates weights from the the aggregate-level predictors to generate the
#' synthetic control. These weights determine which variable and which unit from
#' the donor pool is important in generating the synthetic control.
#'
#' **Optimization**
#'
#' The method completes the following nested minimization task:
#'
#' \deqn{W^*(V) =  min \sum^M_{m=1} v_m (X_{1m} - \sum^{J+1}_{j=2}w_j X_{jm})^2}
#'
#' Where \eqn{X_1} and \eqn{X_0}, which are matrices of aggregate-level
#' covariates, are generated using the `generate_predictor()` function. \eqn{V}
#' denotes the variable weights with \eqn{M} reflecting the total number of
#' predictor variables. Thus, the optimal weights are a function of \eqn{V}.
#'
#' The weights themselves are optimized via the following:
#'
#' \deqn{\sum^{T_0}_{t=1}(Y_{1t} - \sum^{J=1}_{j=2}w^*_j(V)Y_{jt})^2}
#'
#' where \eqn{T_0} denotes the pre-intervention period (or a specific
#' optimization window supplied by the argument `time_window`); \eqn{J} denotes
#' the number of control units from the donor pool, where \eqn{j=1} reflects the
#' treated unit.
#'
#' Thus, the weights are selected in a manner that produces a synthetic
#' \eqn{\hat{Y}} that approximates the observed \eqn{Y} as closely as possible.
#'
#' **Variable Weights**
#'
#' As proposed in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller
#' (2010), the synth function routinely searches for the set of weights that
#' generate the best fitting convex combination of the control units. In other
#' words, the predictor weight matrix V (`custom_variable_weights`) is chosen
#' among all positive definite diagonal matrices such that MSPE is minimized for
#' the pre-intervention period. Instead of using this data-driven procedures to
#' search for the best fitting synthetic control group, the user may supply
#' their own weights using the `custom_variable_weights` argument. These weights
#' reflect the user's subjective assessment of the predictive power of the
#' variables generated by `generate_predictor()`.
#'
#' When generating weights for the placebo cases, the variable weights used for
#' the fit of the treated unit optimization. This ensures comparability between
#' the placebo and treated fits. In addition, it greatly decreases processing
#' time as the variable weights do not be learned for every placebo entry.
#'
#'
#' @param data nested data of type `tbl_df` generated from `sythetic_control()`.
#'   See `synthetic_control()` documentation for more information. In addition,
#'   a matrix of predictors must be prespecified using the
#'   `generate_predictor()` function. See documentation for more information on
#'   how to generate a predictor function.
#' @param optimization_window the temporal window of the pre-intervention
#'   outcome time series to be used in the optimization task. Default behavior
#'   uses the entire pre-intervention time period.
#' @param custom_variable_weights a vector of provided weights that define a
#'   variable's importance in the optimization task. The weights are intended to
#'   reflect the users prior regarding the relative significance of each
#'   variable. Vector must sum to one. Note that the method is significantly
#'   faster when a custom variable weights are provided. Default behavior
#'   assumes no wieghts are provided and thus must be learned from the data.
#' @param include_fit Boolean flag, if TRUE, then the optimization output is
#'   included in the outputted `tbl_df`.
#' @param optimization_method string vector that specifies the optimization
#'   algorithms to be used. Permissable values are all optimization algorithms
#'   that are currently implemented in the optimx function (see this function
#'   for details). This list currently includes c('Nelder-Mead', 'BFGS', 'CG',
#'   'L-BFGS-B', 'nlm', 'nlminb', 'spg', and 'ucminf"). If multiple algorithms
#'   are specified, synth will run the optimization with all chosen algorithms
#'   and then return the result for the best performing method. Default is
#'   c('Nelder-Mead','BFGS'). As an additional possibility, the user can also
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
#' @param quadopt string vector that specifies the routine for quadratic
#'   optimization over w weights. possible values are "ipop" and "LowRankQP"
#'   (see ipop and LowRankQP for details). default is 'ipop'
#' @param margin_ipop setting for ipop optimization routine: how close we get to
#'   the constrains (see ipop for details)
#' @param sigf_ipop setting for ipop optimization routine: Precision (default: 7
#'   significant figures (see ipop for details)
#' @param bound_ipop setting for ipop optimization routine: Clipping bound for
#'   the variables (see ipop for details)
#' @param verbose Logical flag. If TRUE then intermediate results will be shown.
#' @param ... Additional arguments to be passed to optimx and or genoud to
#'   adjust optimization.
#'
#' @return `tbl_df` with nested fields containing the following:
#'
#'   - `.id`: unit id for the intervention case (this will differ when a placebo
#'   unit).
#'
#'   - `.placebo`: indicator field taking on the value of 1 if a unit is a
#'   placebo unit, 0 if it's the specified treated unit.
#'
#'   - `.type`: type of the nested data construct: `treated` or `controls`.
#'   Keeps tract of which data construct is located in `.outcome` field.
#'
#'   - `.outcome`: nested data construct containing the outcome variable
#'   configured for the sythnetic control method. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.predictors`: nested data construct containing the covariate matrices
#'   for the treated and control (donor) units. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.unit_weights`: Nested column of unit weights (i.e. how each unit from
#'   the donor pool contributes to the synthetic control). Weights should sum to
#'   1.
#'
#'   - `.predictor_weights`: Nested column of predictor variable weights (i.e.
#'   the significance of each predictor in optimizing the weights that generate
#'   the synthetic control). Weights should sum to 1. If variable weights are
#'   provided, those variable weights are provided.
#'
#'   - `.original_data`: original impute data filtered by treated or control
#'   units. This allows for easy processing down stream when generating
#'   predictors.
#'
#'   - `.meta`: stores information regarding the unit and time index, the
#'   treated unit and time and the name of the outcome variable. Used downstream
#'   in subsequent functions.
#'
#'   - `.loss`: the RMPE loss for both sets of weights.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # Smoking example data
#' data(smoking)
#'
#' smoking_out <-
#' smoking %>%
#'
#' # initial the synthetic control object
#' synthetic_control(outcome = cigsale,
#'                   unit = state,
#'                   time = year,
#'                   i_unit = "California",
#'                   i_time = 1988,
#'                   generate_placebos= TRUE) %>%
#'
#' # Generate the aggregate predictors used to generate the weights
#'   generate_predictor(time_window=1980:1988,
#'                      lnincome = mean(lnincome, na.rm = TRUE),
#'                      retprice = mean(retprice, na.rm = TRUE),
#'                      age15to24 = mean(age15to24, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1984:1988,
#'                      beer = mean(beer, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1975,
#'                      cigsale_1975 = cigsale) %>%
#'
#'   generate_predictor(time_window=1980,
#'                      cigsale_1980 = cigsale) %>%
#'
#'   generate_predictor(time_window=1988,
#'                      cigsale_1988 = cigsale) %>%
#'
#'
#'   # Generate the fitted weights for the synthetic control
#'   generate_weights(optimization_window =1970:1988,
#'                    Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6)
#'
#' # Retrieve weights
#' smoking_out %>% grab_predictor_weights()
#' smoking_out %>% grab_unit_weights()
#'
#' # Retrieve the placebo weights as well.
#' smoking_out %>% grab_predictor_weights(placebo= TRUE)
#' smoking_out %>% grab_unit_weights(placebo= TRUE)
#'
#' # Plot the unit weights
#' smoking_out %>% plot_weights()
#'
#' }
#'
generate_weights <-function(data,
                            optimization_window = NULL,
                            custom_variable_weights = NULL,
                            include_fit = FALSE,
                            optimization_method = c('Nelder-Mead','BFGS'),
                            genoud = FALSE,
                            quadopt = "ipop",
                            margin_ipop = 5e-04,
                            sigf_ipop = 5,
                            bound_ipop = 10,
                            verbose = FALSE,
                            ...){
  UseMethod("generate_weights")
}

#' @export
generate_weights <-function(data,
                                      optimization_window = NULL,
                                      custom_variable_weights = NULL,
                                      include_fit = FALSE,
                                      optimization_method = c('Nelder-Mead','BFGS'),
                                      genoud = FALSE,
                                      quadopt = "ipop",
                                      margin_ipop = 5e-04,
                                      sigf_ipop = 5,
                                      bound_ipop = 10,
                                      verbose = FALSE,
                                      ...){


  # Iterate through the versions of the data, and generate the predictors for
  # each version of the data (e.g. the treated and placebo sets) if user wants
  # wieghts for each placebo dataset (a necessary condition for the inferential
  # strategy)

  # Grab data versions
  data_versions <- unique(data$.id)

  # Iterate through the data versions
  master_nest <- NULL; first_pass <- TRUE; second_pass <- FALSE
  for (v in data_versions){

    # Progress reports
    if(verbose){
      if(second_pass){cat("Generating weights for the placebo units.\n");second_pass <- F}
      if(first_pass){cat("Generating weights for the intervention unit.\n");second_pass <- T}
    }

    # Generate weight for the data version
    master_nest <-
      data %>%
      dplyr::filter(.id == v) %>%
      synth_weights(data=.,
                    time_window = optimization_window,
                    custom_variable_weights = custom_variable_weights,
                    include_fit = include_fit,
                    optimization_method = optimization_method,
                    genoud = genoud,
                    quadopt = quadopt,
                    Margin.ipop = margin_ipop,
                    Sigf.ipop = sigf_ipop,
                    Bound.ipop = bound_ipop,
                    verbose = verbose) %>%
      dplyr::bind_rows(master_nest,.)


    # Use the same weights on the placebos that was used on the treated unit.
    # This makes the output of the different versions comparible and greatly
    # reduces computation time.
    if (first_pass & is.null(custom_variable_weights)){
      custom_variable_weights <- master_nest$.predictor_weights[[2]]$weight
      first_pass <- FALSE
    }

  }

  return(master_nest)
}


#' synth_weights
#'
#' Auxiliary Function for generating individual weights for each unit-specific
#' data entry. The method allows of opimtizing weights for all placebo and
#' treated data configurations (assuming there are placebo configurations to
#' generate)
#'
#' @param data nested data of type `synth_tbl` generated from
#'   `sythetic_control()`. See `synthetic_control()` documentation for more
#'   information. In addition, a matrix of predictors must be pre-specified
#'   using the `generate_predictor()` function. See documentation for more
#'   information on how to generate a predictor function.
#' @param time_window the temporal window of the pre-intervention outcome time
#'   series to be used in the optimization task. Default behavior uses the
#'   entire pre-intervention time period.
#' @param custom_variable_weights a vector of provided weights that define a
#'   variable's importance in the optimization task. The weights are intended to
#'   reflect the users prior regarding the relative significance of each
#'   variable. Vector must sum to one. Note that the method is significantly
#'   faster when a custom variable weights are provided. Default behavior
#'   assumes no wieghts are provided and thus must be learned from the data.
#' @param include_fit Boolean flag, if TRUE, then the optimization output is
#'   included in the outputted `tbl_df`.
#' @param optimization_method string vector that specifies the optimization
#'   algorithms to be used. Permissable values are all optimization algorithms
#'   that are currently implemented in the optimx function (see this function
#'   for details). This list currently includes c("Nelder-Mead', 'BFGS', 'CG',
#'   'L-BFGS-B', 'nlm', 'nlminb', 'spg', and 'ucminf"). If multiple algorithms
#'   are specified, synth will run the optimization with all chosen algorithms
#'   and then return the result for the best performing method. Default is
#'   "BFGS". As an additional possibility, the user can also specify 'All' which
#'   means that synth will run the results over all algorithms in optimx.
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
#' @param quadopt string vector that specifies the routine for quadratic
#'   optimization over w weights. possible values are "ipop" and "LowRankQP"
#'   (see ipop and LowRankQP for details). default is 'ipop'
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
#' @return tibble data frame with optimized weights attached.
synth_weights <- function(data,
                          time_window = NULL,
                          custom_variable_weights = NULL,
                          include_fit = FALSE,
                          optimization_method = c("Nelder-Mead", "BFGS"),
                          genoud = FALSE,
                          quadopt = "ipop",
                          Margin.ipop = 5e-04,
                          Sigf.ipop = 5,
                          Bound.ipop = 10,
                          verbose = verbose,
                          ...){

  # Checks (make sure data is being passed from synthetic_control with predictors)
  if(!all(c(".type",".outcome",".original_data",".meta") %in% colnames(data))){stop("Data must be passed from the `sythetic_control()` function.")}
  if(!".predictors" %in% colnames(data)){ stop("Predictors must be generated prior to running using `generate_predictors()`.")}

  # If no temporal window is set to use in the optimization task, use the entire
  # pretreatment period
  if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit }

  # If placebo out version of the grab_ function, clear unnecessary fields
  is_placebo = ifelse(any(data$.placebo == 1),T,F)
  clear_placebo = function(data){
    if(".placebo" %in% colnames(data)){
      data %>% dplyr::select(-.id,-.placebo)
    }else{
      data
    }
  }

  # Unpack the predictors used in the weighting task
  treatment_unit_covariates <-
    data %>%
    grab_predictors("treated",placebo = is_placebo) %>%
    clear_placebo(.) %>%
    dplyr::select(-variable) %>%
    as.matrix()

  control_units_covariates <-
    data %>%
    grab_predictors("controls",placebo = is_placebo) %>%
    clear_placebo(.) %>%
    dplyr::select(-variable) %>%
    as.matrix()


  # Unpack the outcomes used in the weighting task
  treatment_unit_outcome <-
    data %>%
    grab_outcome("treated",placebo = is_placebo) %>%
    clear_placebo(.) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::select(-time_unit) %>%
    as.matrix()

  control_units_outcome <-
    data %>%
    grab_outcome("controls",placebo = is_placebo) %>%
    clear_placebo(.) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::select(-time_unit) %>%
    as.matrix()


  # Implement the main fit method (Abadie et. al 2010)
  fit <- synth_method(treatment_unit_covariates=treatment_unit_covariates,
                      control_units_covariates=control_units_covariates,
                      treatment_unit_outcome=treatment_unit_outcome,
                      control_units_outcome=control_units_outcome,
                      custom.v = custom_variable_weights,
                      optimxmethod = optimization_method,
                      genoud = genoud,
                      quadopt = quadopt,
                      Margin.ipop = Margin.ipop,
                      Sigf.ipop = Sigf.ipop,
                      Bound.ipop = Bound.ipop,
                      verbose = verbose)

  # **Simplify output to combine with main data nest**

  # gather variable weights.
  variable_weights <-
    data %>%
    grab_predictors(placebo = is_placebo) %>%
    dplyr::transmute(variable,
                     weight = as.numeric(fit$solution.v))

  # gather control case assignment weights.
  unit_weights <-
    fit$solution.w %>%
    as.data.frame(.) %>%
    tibble::rownames_to_column(var = "unit") %>%
    dplyr::rename(weight = w.weight) %>%
    tibble::as_tibble(.)


  # Return the input data with the control weights assigned.
  fit_out <-
    data %>%
    dplyr::mutate(.unit_weights = list(unit_weights,unit_weights),
                  .predictor_weights = list(variable_weights,variable_weights),
                  .loss = list(tibble::tibble(variable_mspe = as.numeric(fit$loss.v[1]),
                                              control_unit_mspe = as.numeric(fit$loss.w[1])),
                               tibble::tibble(variable_mspe = as.numeric(fit$loss.v[1]),
                                              control_unit_mspe = as.numeric(fit$loss.w[1])))
    ) %>%
    dplyr::select(.id:.predictors,
                  .unit_weights,
                  .predictor_weights,
                  dplyr::everything())

  # Return fit if requested
  if(include_fit){
    fit_out <- fit_out %>% dplyr::mutate(.fit=list(fit,fit))
  }

  return(fit_out)
}



#' generate_control
#'
#' Uses the weights generated from `generate_weights()` to weight control units
#' from the donor pool to denerate a synthetic version of the treated unit time
#' series.
#'
#' @param data nested data of type `tbl_df` generated from `sythetic_control()`.
#'   See `synthetic_control()` documentation for more information. In addition,
#'   `.unit_weights` must be generate using `generate_weights()`. See
#'   documentation for more information on how to generate weights.
#'
#' @return `tbl_df` with nested fields containing the following:
#'
#'   - `.id`: unit id for the intervention case (this will differ when a placebo
#'   unit).
#'
#'   - `.placebo`: indicator field taking on the value of 1 if a unit is a
#'   placebo unit, 0 if it's the specified treated unit.
#'
#'   - `.type`: type of the nested data construct: `treated` or `controls`.
#'   Keeps tract of which data construct is located in `.outcome` field.
#'
#'   - `.outcome`: nested data construct containing the outcome variable
#'   configured for the sythnetic control method. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.predictors`: nested data construct containing the covariate matrices
#'   for the treated and control (donor) units. Data is configured into a wide
#'   format for the optimization task.
#'
#'   - `.synthetic_control`: nested data construct containing the synthetic
#'   control version of the outcome variable generated from the unit weights.
#'
#'   - `.unit_weights`: Nested column of unit weights (i.e. how each unit from
#'   the donor pool contributes to the synthetic control). Weights should sum to
#'   1.
#'
#'   - `.predictor_weights`: Nested column of predictor variable weights (i.e.
#'   the significance of each predictor in optimizing the weights that generate
#'   the synthetic control). Weights should sum to 1. If variable weights are
#'   provided, those variable weights are provided.
#'
#'   - `.original_data`: original impute data filtered by treated or control
#'   units. This allows for easy processing down stream when generating
#'   predictors.
#'
#'   - `.meta`: stores information regarding the unit and time index, the
#'   treated unit and time and the name of the outcome variable. Used downstream
#'   in subsequent functions.
#'
#'   - `.loss`: the RMPE loss for both sets of weights.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # Smoking example data
#' data(smoking)
#'
#' smoking_out <-
#' smoking %>%
#'
#' # initial the synthetic control object
#' synthetic_control(outcome = cigsale,
#'                   unit = state,
#'                   time = year,
#'                   i_unit = "California",
#'                   i_time = 1988,
#'                   generate_placebos= FALSE) %>%
#'
#' # Generate the aggregate predictors used to generate the weights
#'   generate_predictor(time_window=1980:1988,
#'                      lnincome = mean(lnincome, na.rm = TRUE),
#'                      retprice = mean(retprice, na.rm = TRUE),
#'                      age15to24 = mean(age15to24, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1984:1988,
#'                      beer = mean(beer, na.rm = TRUE)) %>%
#'
#'   generate_predictor(time_window=1975,
#'                      cigsale_1975 = cigsale) %>%
#'
#'   generate_predictor(time_window=1980,
#'                      cigsale_1980 = cigsale) %>%
#'
#'   generate_predictor(time_window=1988,
#'                      cigsale_1988 = cigsale) %>%
#'
#'
#'   # Generate the fitted weights for the synthetic control
#'   generate_weights(optimization_window =1970:1988,
#'                    Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>%
#'
#'   # Generate the synthetic control
#'   generate_control()
#'
#' # Plot the observed and synthetic trend
#' smoking_out %>% plot_trends(time_window = 1970:2000)
#'
#' }
#'
generate_control <- function(data){
  UseMethod("generate_control")
}

#' @export
generate_control <- function(data){

  # Auxiliary function to generate the synthetic control for a single version of
  # the data.
  aux_gen_control <- function(data){

    # Checks
    if(!(".meta" %in% colnames(data))){ stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
    if(!(".unit_weights" %in% colnames(data))){ stop("`.unit_weights` column has been removed. Run `generate_weights()` prior to running this function.")}
    if(!(".predictor_weights" %in% colnames(data))){ stop("`.predictor_weights` column has been removed. Run `generate_weights()` prior to running this function.")}

    # Grab metadata
    trt_time <- data$.meta[[1]]$treatment_time[1]
    trt_unit <- data$.meta[[1]]$treatment_unit[1]
    outcome_name <- data$.meta[[1]]$outcome[1]
    unit_index <- data$.meta[[1]]$unit_index[1]
    time_index <- data$.meta[[1]]$time_index[1]

    # If placebo out version of the grab_ function, clear unnecessary fields
    is_placebo = ifelse(any(data$.placebo == 1),T,F)
    clear_placebo = function(data){
      if(".placebo" %in% colnames(data)){
        data %>% dplyr::select(-.id,-.placebo)
      }else{
        data
      }
    }

    # vector of unit weights
    W <-
      data %>%
      grab_unit_weights(placebo = is_placebo) %>%
      clear_placebo(.) %>%
      dplyr::select(-unit) %>%
      as.matrix()

    # coutcome time series for the control units
    outcome_controls <-
      data %>%
      dplyr::filter(.placebo == is_placebo,.type=="controls") %>%
      dplyr::select(.original_data)  %>%
      tibble::as_tibble() %>%
      tidyr::unnest(cols = c(.original_data)) %>%
      dplyr::select(.data[[unit_index]],.data[[time_index]],.data[[outcome_name]]) %>%
      tidyr::spread(.data[[unit_index]],.data[[outcome_name]])

    # outcome time series for the treated unit
    outcome_treatment <-
      data %>%
      dplyr::filter(.placebo == is_placebo,.type=="treated") %>%
      dplyr::select(.original_data)  %>%
      tibble::as_tibble() %>%
      tidyr::unnest(cols = c(.original_data)) %>%
      dplyr::select(y=.data[[outcome_name]])


    # Convert to matrix format
    Y_U <- outcome_controls %>%
      dplyr::select(-.data[[time_index]]) %>%
      as.matrix()


    # Dot with the weights vector and generate the synthetic Y
    synthetic_output <-
      (Y_U  %*%  W) %>%
      tibble::as_tibble() %>%
      dplyr::transmute(time_unit = outcome_controls[[time_index]],
                       real_y = outcome_treatment$y,
                       synth_y = weight)

    # Join the synetic control the nested frame and return
    out <-
      data %>%
      dplyr::mutate(.synthetic_control = list(synthetic_output,synthetic_output)) %>%
      dplyr::select(.id:.predictors,.synthetic_control,dplyr::everything())

    return(out)
  }


  # Iterate through the versions of the data, and generate the predictors for
  # each version of the data (e.g. the treated and placebo sets)
  data_versions <- unique(data$.id)
  master_nest <- NULL
  for (v in data_versions){
    master_nest <-
      data %>%
      dplyr::filter(.id == v) %>%
      aux_gen_control(data=.) %>%
      dplyr::bind_rows(master_nest,.)
  }

  return(master_nest)
}

