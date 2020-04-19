#' synthetic_control
#'
#' Main function for setting up the synthetic control method...
#'
#' [FILL IN!]
#'
#' @param .data
#' @param .formula
#' @param i_unit
#' @param i_time
#' @param restrict_time
#' @param .aggregate
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
synthetic_control <- function(.data = NULL,
                              .formula = NULL,
                              i_unit=NULL,
                              i_time =NULL,
                              .aggregate=NULL){

  # Extract specified data frome formula
  tmp <- get_all_vars(.formula, data = .data) %>% tibble::as_tibble()

  # Gather formula inputs (to draw from the data)
  inputs = colnames(tmp)

  # Isolate the panel unit indices (using the treatment specification as the key)
  unit_indices = tmp %>% select(inputs[(length(inputs)-1):length(inputs)])
  time_index = names(unit_indices)[apply(unit_indices == i_time,2,any)]
  unit_index = names(unit_indices)[apply(unit_indices == i_unit,2,any)]

  # Isolate the predictor and outcome variables
  outcome = inputs[1]
  predictors = inputs[2:(length(inputs)-2)]

  # Format Treatment unit (i.e. the unit for which the synthetic control is being generated)
  pre_period_tr = tmp[tmp[time_index] < i_time & tmp[unit_index] == i_unit,]

  y_pre_tr <-
    pre_period_tr[c(unit_index,time_index,outcome)] %>%
    tidyr::pivot_wider(names_from = name,values_from = Y) %>%
    dplyr::rename(time_unit = .data[[time_index]])

  X_pre_tr <-
    pre_period_tr[c(unit_index,predictors)] %>%
    dplyr::group_by(.data[[unit_index]]) %>%
    dplyr::summarize_all(function(x) mean(x,na.rm = T)) %>%
    tidyr::gather(variable,val,-.data[[unit_index]]) %>%
    tidyr::spread(.data[[unit_index]],val)


  # Format Control units (i.e. units from which the synthetic control is generated from)
  pre_period_cnt = tmp[tmp[time_index] < i_time & tmp[unit_index] != i_unit,]

  y_pre_cnt <-
    pre_period_cnt[c(unit_index,time_index,outcome)] %>%
    tidyr::pivot_wider(names_from = name,values_from = Y) %>%
    dplyr::rename(time_unit = .data[[time_index]])

  X_pre_cnt <-
    pre_period_cnt[c(unit_index,predictors)] %>%
    dplyr::group_by(.data[[unit_index]]) %>%
    dplyr::summarize_all(function(x) mean(x,na.rm = T)) %>%
    tidyr::gather(variable,val,-.data[[unit_index]]) %>%
    tidyr::spread(.data[[unit_index]],val)


  # Combine data as a nested data frame output
  nested_output <-
    tibble::tibble(type = c("treatment","control"),
                   .treatment = list(y_pre_tr,y_pre_cnt),
                   .predictors = list(X_pre_tr,X_pre_cnt),
                   .original_data = list(tmp),
                   .meta = list(tibble::tibble(unit_index=unit_index,
                                               time_index=time_index,
                                               treatment_unit = i_unit,
                                               treatment_time = i_time,
                                               outcome = outcome,
                                               predictors = predictors)))
  return(nested_output)
}







#' fit_weights
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
fit_weights <- function(.data,
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


  # Checks (make sure .data is being passed from synthetic_control)
  if(!all(colnames(.data) == c("type",".treatment",".predictors",
                               ".original_data",".meta"))){
    stop("Data must be passed from the `sythetic_control()` function.")
  }


  # Internal functions to extract the relevant data elements.
  grab_predictors <- function(.data,.type="treatment"){
    .data %>%
      dplyr::filter(type==.type) %>%
      dplyr::select(.predictors) %>%
      tidyr::unnest(cols=c(.predictors)) %>%
      dplyr::select(-variable) %>%
      as.matrix()
  }
  grab_outcomes <- function(.data,.type="treatment"){
    .data %>%
      dplyr::filter(type==.type) %>%
      dplyr::select(.treatment) %>%
      tidyr::unnest(cols=c(.treatment)) %>%
      dplyr::select(-time_unit) %>%
      as.matrix()
  }


  # Unpack the predictors used in the weighting task
  treatment_unit_covariates <- .data %>% grab_predictors("treatment")
  control_units_covariates <- .data %>% grab_predictors("control")

  # Unpack the outcomes used in the weighting task
  treatment_unit_outcome <- .data %>% grab_outcomes("treatment")
  control_units_outcome <- .data %>% grab_outcomes("control")

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
    .data %>%
    dplyr::filter(type=="treatment") %>%
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
    .data %>%
    dplyr::mutate(.unit_weights = list(unit_weights),
                  .predictor_weights = list(variable_weights),
                  .loss = list(variable_rmspe = fit$loss.v,
                               control_unit_rmspe = fit$loss.w)) %>%
    dplyr::select(type:.predictors,
                  .unit_weights,
                  .predictor_weights,
                  dplyr::everything())

  # Return fit if requested
  if(include_fit){
    fit_out <-
      fit_out %>%
      dplyr::mutate(.fit=list(fit))
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
generate_control <- function(.data,plot=F){

  if(!(".meta" %in% colnames(.data))){
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }

  # grab metadata
  trt_time <- .data$.meta[[1]]$treatment_time[1]
  trt_unit <- .data$.meta[[1]]$treatment_unit[1]
  outcome_name <- .data$.meta[[1]]$outcome[1]
  unit_index <- .data$.meta[[1]]$unit_index[1]
  time_index <- .data$.meta[[1]]$time_index[1]

  # vector of unit weights
  W <-
    .data %>%
    grab_unit_weights() %>%
    select(-unit) %>%
    as.matrix()

  # coutcome time series for the control units
  outcome_controls <-
    .data %>%
    dplyr::filter(type=="control") %>%
    dplyr::select(.original_data)  %>%
    tidyr::unnest(cols = c(.original_data)) %>%
    dplyr::filter(!(.data[[unit_index]] %in% trt_unit)) %>%
    dplyr::select(.data[[unit_index]],.data[[time_index]],.data[[outcome_name]]) %>%
    tidyr::spread(.data[[unit_index]],.data[[outcome_name]])

  # outcome time series for the treated unit
  outcome_treatment <-
    .data %>%
    dplyr::filter(type=="treatment") %>%
    dplyr::select(.original_data)  %>%
    tidyr::unnest(cols = c(.original_data)) %>%
    dplyr::filter(name == trt_unit) %>%
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
    .data %>%
    dplyr::mutate(.synthetic_control = list(synthetic_output)) %>%
    dplyr::select(type:.predictors,.synthetic_control,dplyr::everything())


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
plot_trends <- function(.data){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(.data))){
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- .data$.meta[[1]]$treatment_time[1]
  time_index <- .data$.meta[[1]]$time_index[1]

  # Generate plot
  .data %>%
    grab_synthetic_control() %>%
    tidyr::gather(var,val,-time_unit) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,val,color=var)) +
    ggplot2::geom_vline(xintercept = trt_time,color="darkred") +
    ggplot2::geom_line(size=.7,alpha=.7) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values=c("grey30","steelblue")) +
    ggplot2::labs(color="") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
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
grab_unit_weights <- function(.data){

  # Check if .unit_weights is in data.
  if(!(".unit_weights" %in% colnames(.data))){
    stop("`.unit_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  .data %>%
    dplyr::filter(type=="treatment") %>%
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
grab_predictor_weights <- function(.data){

  # Check if .unit_weights is in data.
  if(!(".predictor_weights" %in% colnames(.data))){
    stop("`.predictor_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  .data %>%
    dplyr::filter(type=="treatment") %>%
    dplyr::select(.predictor_weights) %>%
    tidyr::unnest(.predictor_weights)
}


#' grab_synthetic_control
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
grab_synthetic_control <- function(.data){

  # Check if .synthetic_control is in data.
  if(!(".synthetic_control" %in% colnames(.data))){
    stop("`.synthetic_control` column has been removed. Please run `generate_control()` to generate this data field.")
  }

  .data %>%
    dplyr::filter(type=="treatment") %>%
    dplyr::select(.synthetic_control) %>%
    tidyr::unnest(.synthetic_control)
}
