#' synthetic_control
#'
#' Main function for setting up the synthetic control method...
#'
#' [FILL IN!]
#'
#' @param .data
#' @param .formula
#' @param i_unit Treated unit (all other units are considered the control units)
#' @param i_time When the intervention (treatment) occurs
#' @param restrict_time
#' @param .aggregate
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
synthetic_control <- function(data = NULL,
                              outcome = NULL,
                              unit = NULL,
                              time = NULL,
                              i_unit=NULL,
                              i_time =NULL){

  # Quasi-quosures
  outcome <- rlang::enquo(outcome)
  unit <- rlang::enquo(unit)
  time <- rlang::enquo(time)

  # configure the outcome for the treated unit
  trt_outcome <-
    data %>%
    dplyr::filter(!!time < i_time & !!unit == i_unit) %>% # Pre-intervention period
    dplyr::select(!!time,!!unit,!!outcome) %>%
    tidyr::pivot_wider(names_from=!!unit,values_from=!!outcome) %>%
    dplyr::rename(time_unit = !!time)

  # configure the outcome for the control units
  cnt_outcome <-
    data %>%
    dplyr::filter(!!time < i_time & !!unit != i_unit) %>% # Pre-intervention period
    dplyr::select(!!time,!!unit,!!outcome) %>%
    tidyr::pivot_wider(names_from=!!unit,values_from=!!outcome) %>%
    dplyr::rename(time_unit = !!time)

  # initialize the nested output
  nested_output <-
    tibble::tibble(.type = c("treated","controls"),
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



#' generate_predictor
#'
#' @param data
#' @param window
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_predictors <- function(data,
                               time_window=NULL,
                               ...){

  # Grab meta data re: relevant indices
  time_index <- data$.meta[[1]]$time_index[1]
  unit_index <- data$.meta[[1]]$unit_index[1]

  # INTERNAL FUNCTIONS
  extract_predictor <- function(data,type = "treatment",...){
    # Aux. function for processing predictors. Takes in quosure statement,
    # passes it internally to summarize, and organizes data to behave
    # accordingly as a synth_method input.

    data %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.original_data) %>%
      tidyr::unnest(cols=c(.original_data)) %>%

      # Only relevant time units (as specified by the window)
      dplyr::filter(.data[[time_index]] %in% time_window) %>%

      # Group by the unit axis
      dplyr::group_by(.data[[unit_index]]) %>%
      dplyr::summarize(...) %>%

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
    stop("NA generated in specified predictors. Consider using `rm.na=T` in aggregation function or specifying a larger/different time window")
  }


  # If a .predictors column has yet to be generated, generate it.
  if(!".predictors" %in% colnames(data)){

    data <- data %>%
      dplyr::mutate(.predictors = list(pred_trt,pred_cnt)) %>%
      dplyr::select(.type,.outcome,.predictors,dplyr::everything())

  } else{  # overwrite existing set up

    data <- data %>%
      dplyr::mutate(.predictors = list(assign_to_predictors(data,pred_trt,type="treated"),
                                       assign_to_predictors(data,pred_cnt,type="controls")))

  }

  return(data)

}




#' generate_weights
#'
#' @param .data data passed from synthetic_control()
#' @param custom_variable_weights Set custom weights for the covariates
#' @param include_fit Return the synth fit in the nested output
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
generate_weights <- function(data,
                             time_window = NULL,
                             custom_variable_weights = NULL,
                             include_fit = FALSE,
                             optimization_method = c("Nelder-Mead", "BFGS"),
                             genoud = FALSE,
                             quadopt = "ipop",
                             Margin.ipop = 5e-04,
                             Sigf.ipop = 5,
                             Bound.ipop = 10,
                             verbose = F,
                             ...){

  # Checks (make sure data is being passed from synthetic_control with predictors)
  if(!all(c(".type",".outcome",".original_data",".meta") %in% colnames(data))){stop("Data must be passed from the `sythetic_control()` function.")}
  if(!".predictors" %in% colnames(data)){ stop("Predictors must be generated prior to running using `generate_predictors()`.")}


  # Internal functions to extract the relevant data elements.
  grab_predictors <- function(data,type="treated"){
    data %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.predictors) %>%
      tidyr::unnest(cols=c(.predictors)) %>%
      dplyr::select(-variable) %>%
      as.matrix()
  }
  grab_outcomes <- function(data,type="treated"){
    data %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.outcome) %>%
      tidyr::unnest(cols=c(.outcome)) %>%
      dplyr::filter(time_unit %in% time_window) %>%
      dplyr::select(-time_unit) %>%
      as.matrix()
  }

  # If no temporal window is set to use in the optimization task, use the entire
  # pretreatment period
  if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit }

  # Unpack the predictors used in the weighting task
  treatment_unit_covariates <- data %>% grab_predictors("treated")
  control_units_covariates <- data %>% grab_predictors("controls")


  # Unpack the outcomes used in the weighting task
  treatment_unit_outcome <- data %>% grab_outcomes("treated")
  control_units_outcome <- data %>% grab_outcomes("controls")


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
    dplyr::filter(.type=="treated") %>%
    dplyr::select(.predictors) %>%
    tidyr::unnest(cols = c(.predictors)) %>%
    dplyr::transmute(variable,
                     weight = as.numeric(fit$solution.v))

  # gather control case assignment weights.
  unit_weights <-
    fit$solution.w %>%
    as.data.frame(.) %>%
    tibble::rownames_to_column(var = "unit") %>%
    dplyr::rename(weight = w.weight)


  # Return the input data with the control weights assigned.
  fit_out <-
    data %>%
    dplyr::mutate(.unit_weights = list(NULL,unit_weights),
                  .predictor_weights = list(NULL,variable_weights),
                  .loss = list(NULL,tibble(variable_rmspe = as.numeric(fit$loss.v[1]),
                                           control_unit_rmspe = as.numeric(fit$loss.w[1])))
    ) %>%
    dplyr::select(.type:.predictors,
                  .unit_weights,
                  .predictor_weights,
                  dplyr::everything())

  # Return fit if requested
  if(include_fit){
    fit_out <- fit_out %>% dplyr::mutate(.fit=list(NULL,fit))
  }

  return(fit_out)
}



#' generate_control
#'
#' @param .data .data from fit_weights()
#' @param plot if True, plots the sythetic control; default to FALSE.
#'
#' @return
#' @export
#'
#' @examples
generate_control <- function(data,plot=F){

  # Checks
  if(!(".meta" %in% colnames(data))){ stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # grab metadata
  trt_time <- data$.meta[[1]]$treatment_time[1]
  trt_unit <- data$.meta[[1]]$treatment_unit[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  time_index <- data$.meta[[1]]$time_index[1]

  # vector of unit weights
  W <-
    data %>%
    grab_unit_weights() %>%
    select(-unit) %>%
    as.matrix()

  # coutcome time series for the control units
  outcome_controls <-
    data %>%
    dplyr::filter(.type=="controls") %>%
    dplyr::select(.original_data)  %>%
    tidyr::unnest(cols = c(.original_data)) %>%
    dplyr::select(.data[[unit_index]],.data[[time_index]],.data[[outcome_name]]) %>%
    tidyr::spread(.data[[unit_index]],.data[[outcome_name]])

  # outcome time series for the treated unit
  outcome_treatment <-
    data %>%
    dplyr::filter(.type=="treated") %>%
    dplyr::select(.original_data)  %>%
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
    dplyr::mutate(.synthetic_control = list(synthetic_output,NULL)) %>%
    dplyr::select(.type:.predictors,.synthetic_control,dplyr::everything())


  # Plot of the trends
  if(plot){ plot_trend(out) }

  return(out)
}



#' plot_trends
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
plot_trends <- function(data,time_window=NULL){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # Generate plot
  data %>%
    grab_synthetic_control() %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::rename(Synthetic= synth_y,
                  Observed= real_y) %>%
    tidyr::gather(var,val,-time_unit) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,val,color=var,linetype=var)) +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=2) +
    ggplot2::geom_line(size=1,alpha=.7) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values=c("grey50","#b41e7c")) +
    ggplot2::scale_linetype_manual(values=c(1,4)) +
    ggplot2::labs(color="",linetype="",y=outcome_name,x=time_index,
                  title=paste0("Time Series of the Synthetic and Observed ",outcome_name)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' plot_difference
#'
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
plot_differences <- function(data,time_window=NULL){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # Generate plot
  data %>%
    grab_synthetic_control() %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,diff)) +
    ggplot2::geom_hline(yintercept = 0,color="black") +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=2) +
    ggplot2::geom_line(size=1,alpha=.75,color="#b41e7c",linetype=4) +
    ggplot2::geom_point(color="#b41e7c") +
    ggplot2::labs(color="",linetype="",y=outcome_name,x=time_index,
                  title=paste0("Difference in the synthetic control and observed ",outcome_name)) +
    ggplot2::theme_minimal()
}





#' plot_weights
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
plot_weights <- function(.data){

  # Combine the different type of weight outputs
  dplyr::bind_rows(

    grab_unit_weights(.data) %>%
      dplyr::mutate(type="Control Unit Weights (W)"),

    grab_predictor_weights(.data) %>%
      dplyr::mutate(type="Variable Weights (V)") %>%
      dplyr::rename(unit = variable)

  ) %>%

    # Generate plot
    dplyr::arrange(weight) %>%
    dplyr::mutate(unit = forcats::fct_reorder(unit,weight)) %>%
    ggplot2::ggplot(ggplot2::aes(unit,weight,fill=type)) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::coord_flip() +
    # ggplot2::ylim(0,1) +
    ggplot2::labs(x="") +
    ggplot2::facet_wrap(~type,ncol = 2,scales="free") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("darkred","steelblue")) +
    ggplot2::theme(text = ggplot2::element_text(size = 14))

}



#' grab_unit_weights
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
grab_unit_weights <- function(data){

  # Check if .unit_weights is in data.
  if(!(".unit_weights" %in% colnames(data))){
    stop("`.unit_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  data %>%
    dplyr::filter(.type=="controls") %>%
    dplyr::select(.unit_weights) %>%
    tidyr::unnest(.unit_weights)
}


#' grab_predictor_weights
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
grab_predictor_weights <- function(data){

  # Check if .predictor_weights is in data.
  if(!(".predictor_weights" %in% colnames(data))){
    stop("`.predictor_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  data %>%
    dplyr::filter(.type=="controls") %>%
    dplyr::select(.predictor_weights) %>%
    tidyr::unnest(.predictor_weights)
}

grab_loss <- function(data){

  # Check if .loss is in data.
  if(!(".loss" %in% colnames(data))){
    stop("`.loss` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  data %>%
    dplyr::select(.loss) %>%
    tidyr::unnest(cols = c(.loss))
}


#' grab_synthetic_control
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
grab_synthetic_control <- function(data){

  # Check if .synthetic_control is in data.
  if(!(".synthetic_control" %in% colnames(data))){
    stop("`.synthetic_control` column has been removed. Please run `generate_control()` to generate this data field.")
  }

  data %>%
    dplyr::filter(.type=="treated") %>%
    dplyr::select(.synthetic_control) %>%
    tidyr::unnest(.synthetic_control)
}
