#' synthetic_control
#'
#' @param data
#' @param outcome
#' @param unit
#' @param time
#' @param i_unit
#' @param i_time
#'
#' @return
#' @export
#'
#' @examples
synthetic_control <- function(data = NULL,
                              outcome = NULL,
                              unit = NULL,
                              time = NULL,
                              i_unit=NULL,
                              i_time =NULL)
{
  # Aux. functions
  generate_nested_data <-function(data=NULL,i_unit = NULL,placebo = NULL,id = 1){
    # Auxiliary function to build out the treated and control units

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

    # Placebo == unit that did not receive the treatment
    master_nest <-
      generate_nested_data(data=data, i_unit = unit_name,
                           placebo = placebo,id=unit_name) %>%
      dplyr::bind_rows(master_nest,.)
  }

  # return the entire nested object.
  return(master_nest)
}


#' generate_predictor
#'
#' @param data
#' @param time_window
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_predictor <- function(data,
                               time_window=NULL,
                               ...){


  # Auxiliary function for generating a predictor for a single configuration of
  # the data (e.g. the treated and placebo sets)
  aux_gen_pred <- function(data,time_window=NULL,...){

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




#' synth_weights
#'
#' Auxiliary Function for generating individual weights for each data entry.
#'
#' @param custom_variable_weights Set custom weights for the covariates
#' @param include_fit Return the synth fit in the nested output
#' @param genoud
#' @param quadopt
#' @param Margin.ipop
#' @param Sigf.ipop
#' @param Bound.ipop
#' @param verbose
#' @param ...
#' @param data
#' @param time_window
#' @param optimization_method
#'
#' @return
#' @export
#'
#' @examples
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
                          verbose = F,
                          ...){

  # Checks (make sure data is being passed from synthetic_control with predictors)
  if(!all(c(".type",".outcome",".original_data",".meta") %in% colnames(data))){stop("Data must be passed from the `sythetic_control()` function.")}
  if(!".predictors" %in% colnames(data)){ stop("Predictors must be generated prior to running using `generate_predictors()`.")}


  # If no temporal window is set to use in the optimization task, use the entire
  # pretreatment period
  if(is.null(time_window)){ time_window <- data$.outcome[[1]]$time_unit }

  # Unpack the predictors used in the weighting task
  treatment_unit_covariates <-
    data %>%
    grab_predictors("treated") %>%
    dplyr::select(-variable) %>%
    as.matrix()

  control_units_covariates <-
    data %>%
    grab_predictors("controls") %>%
    dplyr::select(-variable) %>%
    as.matrix()


  # Unpack the outcomes used in the weighting task
  treatment_unit_outcome <- data %>%
    grab_outcomes("treated") %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::select(-time_unit) %>%
    as.matrix()

  control_units_outcome <- data %>%
    grab_outcomes("controls") %>%
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
    grab_predictors() %>%
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
    dplyr::select(.id:.predictors,
                  .unit_weights,
                  .predictor_weights,
                  dplyr::everything())

  # Return fit if requested
  if(include_fit){
    fit_out <- fit_out %>% dplyr::mutate(.fit=list(NULL,fit))
  }

  return(fit_out)
}



#' generate_weights
#'
#' @param data
#' @param optimization_window
#' @param custom_variable_weights
#' @param include_fit
#' @param optimization_method
#' @param genoud
#' @param quadopt
#' @param Margin.ipop
#' @param Sigf.ipop
#' @param Bound.ipop
#' @param verbose
#' @param track_progress
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_weights <-function(data,
                            optimization_window = NULL,
                            custom_variable_weights = NULL,
                            include_fit = FALSE,
                            optimization_method = c("Nelder-Mead", "BFGS"),
                            genoud = FALSE,
                            quadopt = "ipop",
                            Margin.ipop = 5e-04,
                            Sigf.ipop = 5,
                            Bound.ipop = 10,
                            verbose = FALSE,
                            track_progress = TRUE,
                            ...){


  # Iterate through the versions of the data, and generate the predictors for
  # each version of the data (e.g. the treated and placebo sets) if user wants
  # wieghts for each placebo dataset (a necessary condition for the inferential
  # strategy)

  # Grab data versions
  data_versions <- unique(data$.id)

  # Track progress
  pb <- dplyr::progress_estimated(length(data_versions),min_time = 10)

  # Iterate through the data versions
  master_nest <- NULL; first_pass <- TRUE
  for (v in data_versions){

    # Track progress
    if(track_progress){pb$tick()$print()}

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
                    Margin.ipop = Margin.ipop,
                    Sigf.ipop = Sigf.ipop,
                    Bound.ipop = Bound.ipop,
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



#' generate_control
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
generate_control <- function(data){


  # Auxiliary function to generate the synthetic control for a single version of
  # the data.
  aux_gen_control <- function(data){

    # Checks
    if(!(".meta" %in% colnames(data))){ stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
    if(!(".unit_weights" %in% colnames(data))){ stop("`.unit_weights` column has been removed. Run `generate_weights()` prior to running this function.")}
    if(!(".predictor_weights" %in% colnames(data))){ stop("`.predictor_weights` column has been removed. Run `generate_weights()` prior to running this function.")}

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



#' plot_trends
#'
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
plot_trends <- function(data,time_window=NULL){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # Generate plot
  data %>%
    grab_synthetic_control(placebos = F) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::rename(Synthetic= synth_y,
                  Observed= real_y) %>%
    tidyr::pivot_longer(cols = c(Observed,Synthetic)) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,value,color=name,linetype=name)) +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=2) +
    ggplot2::geom_line(size=1,alpha=.7) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values=c("grey50","#b41e7c")) +
    ggplot2::scale_linetype_manual(values=c(1,4)) +
    ggplot2::labs(color="",linetype="",y=outcome_name,x=time_index,
                  title=paste0("Time Series of the synthetic and observed ",outcome_name)) +
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
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # Generate plot
  data %>%
    grab_synthetic_control(placebos = F) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,diff)) +
    ggplot2::geom_hline(yintercept = 0,color="black",linetype=2) +
    ggplot2::geom_vline(xintercept = trt_time,color="black") +
    ggplot2::geom_line(size=1,alpha=.75,color="#b41e7c") +
    ggplot2::geom_point(color="#b41e7c") +
    ggplot2::labs(color="",linetype="",y=outcome_name,x=time_index,
                  title=paste0("Difference in the synthetic control and observed ",treatment_unit)) +
    ggplot2::theme_minimal()
}


#' plot_placebos
#'
#' @param data
#' @param time_window
#' @param prune
#'
#' @return
#' @export
#'
#' @examples
plot_placebos <- function(data,time_window=NULL,prune=T){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Generate plot data
  plot_data <-
    data %>%
    grab_synthetic_control(placebos = T) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::mutate(type_text = ifelse(.placebo==0,treatment_unit,"control units"),
                  type_text = factor(type_text,level=c(treatment_unit,"control units")))


  # Pruning implementation-- if one of the donors falls outside two standard
  # deviations of the rest of the pool, it's dropped.
  caption <- ""
  if (prune){

    # Gather significance field
    sig_data = data %>% grab_signficance()

    # Treated units Pre-Period RMSPE
    thres <-
      sig_data %>%
      dplyr::filter(type=="Treated") %>%
      dplyr::pull(pre_rmspe)

    # Only retain units that are 2 times the treated unit RMSPE.
    retain_ <-
      sig_data %>%
      dplyr::select(unit_name,pre_rmspe) %>%
      dplyr::filter(pre_rmspe <= thres*2) %>%
      dplyr::pull(unit_name)

    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- "Pruned cases with pre-period RMSPE exceeding two times the treated units pre-period RMSPE."
  }

  # Generate plot
  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,diff,group=.id,
                                 color=type_text,
                                 alpha=type_text,
                                 size=type_text)) +
    ggplot2::geom_hline(yintercept = 0,color="black",linetype=2) +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=3) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values=c("#b41e7c","grey60")) +
    ggplot2::scale_alpha_manual(values=c(1,.4)) +
    ggplot2::scale_size_manual(values=c(1,.5)) +
    ggplot2::labs(color="",alpha="",y=outcome_name,x=time_index,
                  title=paste0("Difference of each '",unit_index,"' in the donor pool"),
                  caption = caption) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom")
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

    grab_unit_weights(.data,placebo = F) %>%
      dplyr::mutate(type="Control Unit Weights (W)"),

    grab_predictor_weights(.data,placebo = F) %>%
      dplyr::mutate(type="Variable Weights (V)") %>%
      dplyr::rename(unit = variable)

  ) %>%

    # Generate plot
    dplyr::arrange(weight) %>%
    dplyr::mutate(unit = forcats::fct_reorder(unit,weight)) %>%
    ggplot2::ggplot(ggplot2::aes(unit,weight,fill=type,color=type)) +
    ggplot2::geom_col(show.legend = F,alpha=.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(x="") +
    ggplot2::facet_wrap(~type,ncol = 2,scales="free") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#b41e7c","grey60")) +
    ggplot2::scale_color_manual(values=c("#b41e7c","grey60")) +
    ggplot2::theme(text = ggplot2::element_text(size = 14))
}


#' plot_rmspe_ratio
#'
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
plot_rmspe_ratio <- function(data,time_window=NULL){
  data %>%
    grab_signficance(time_window=time_window) %>%
    mutate(unit_name = forcats::fct_reorder(as.character(unit_name),rmspe_ratio)) %>%
    ggplot2::ggplot(ggplot2::aes(unit_name,
                                 rmspe_ratio,
                                 fill=type,color=type)) +
    ggplot2::geom_col(alpha=.65) +
    ggplot2::coord_flip() +
    ggplot2::geom_point() +
    ggplot2::labs(y = "Post-Period RMSPE / Pre-Period RMSPE",x="",fill="",color="") +
    ggplot2::scale_fill_manual(values=c("grey50","#b41e7c")) +
    ggplot2::scale_color_manual(values=c("grey50","#b41e7c")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}



#' grab_unit_weights
#'
#' @param data
#' @param placebo
#'
#' @return
#' @export
#'
#' @examples
grab_unit_weights <- function(data,placebo=F){

  # Check if .unit_weights is in data.
  if(!(".unit_weights" %in% colnames(data))){
    stop("`.unit_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  # Check if there is a placebo option exists
  option_exist <- length(unique(data$.placebo))>1

  if(placebo | (!option_exist)){

    data %>%
      dplyr::filter(.type=="controls") %>%
      dplyr::select(.unit_weights) %>%
      tidyr::unnest(.unit_weights)

  }else{

    data %>%
      dplyr::filter(.placebo==0) %>%
      dplyr::filter(.type=="controls") %>%
      dplyr::select(.unit_weights) %>%
      tidyr::unnest(.unit_weights)

  }
}



#' grab_predictors
#'
#' @param data
#' @param type
#' @param placebo
#'
#' @return
#' @export
#'
#' @examples
grab_predictors <- function(data,type="treated",placebo=F){

  # Checks
  if(!".predictors" %in% colnames(data)){ stop("Predictors must be generated prior to running using `generate_predictors()`.")}

  # Check if there is a placebo option exists
  option_exist <- length(unique(data$.placebo))>1

  if(placebo | (!option_exist)){

    data %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.predictors) %>%
      tidyr::unnest(cols=c(.predictors))

  }else{

    data %>%
      dplyr::filter(.placebo==0) %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.predictors) %>%
      tidyr::unnest(cols=c(.predictors))

  }
}


#' grab_outcomes
#'
#' @param data
#' @param type
#' @param placebo
#'
#' @return
#' @export
#'
#' @examples
grab_outcomes <- function(data,type="treated",placebo=F){

  # Check if there is a placebo option exists
  option_exist <- length(unique(data$.placebo))>1

  if(placebo | (!option_exist)){

    data %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.outcome) %>%
      tidyr::unnest(cols=c(.outcome))

  }else{

    data %>%
      dplyr::filter(.placebo==0) %>%
      dplyr::filter(.type==type) %>%
      dplyr::select(.outcome) %>%
      tidyr::unnest(cols=c(.outcome))

  }

}


#' grab_predictor_weights
#'
#' @param data
#' @param placebo
#'
#' @return
#' @export
#'
#' @examples
grab_predictor_weights <- function(data,placebo=F){

  # Check if .predictor_weights is in data.
  if(!(".predictor_weights" %in% colnames(data))){
    stop("`.predictor_weights` column has been removed. Please run `fit_weights()` to generate this data field.")
  }

  if(placebo){

    data %>%
      dplyr::filter(.type=="controls") %>%
      dplyr::select(.predictor_weights) %>%
      tidyr::unnest(.predictor_weights)

  } else {

    data %>%
      dplyr::filter(.placebo==0) %>%
      dplyr::filter(.type=="controls") %>%
      dplyr::select(.predictor_weights) %>%
      tidyr::unnest(.predictor_weights)

  }

}

#' grab_loss
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
#' @param data
#' @param placebos
#'
#' @return
#' @export
#'
#' @examples
grab_synthetic_control <- function(data,placebos=T){

  # Check if .synthetic_control is in data.
  if(!(".synthetic_control" %in% colnames(data))){
    stop("`.synthetic_control` column has been removed. Please run `generate_control()` to generate this data field.")
  }

  if(placebos){
    data %>%
      dplyr::filter(.type=="treated") %>%
      dplyr::select(.id,.placebo,.synthetic_control) %>%
      tidyr::unnest(.synthetic_control)
  }else{
    data %>%
      dplyr::filter(.placebo==0,.type=="treated") %>%
      dplyr::select(.synthetic_control) %>%
      tidyr::unnest(.synthetic_control)
  }

}



#' grab_signficance
#'
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
grab_signficance <- function(data,time_window = NULL){

  # Check if .synthetic_control is in data.
  if(!(".synthetic_control" %in% colnames(data))){stop("`.synthetic_control` column has been removed. Please run `generate_control()` to generate this data field.")}
  if(!(".meta" %in% colnames(data))){ stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}


  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time

  # If no time window is specified for the table, calculate the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Grad unit identifiers
  unit_ids <-
    data %>%
    dplyr::filter(.type=="treated") %>%
    dplyr::select(.id,.meta) %>%
    tidyr::unnest(cols=.meta) %>%
    dplyr::select(.id,unit_name = treatment_unit)

  # Formulate the output data using the donor and treated synthetic controls
  data %>%
    grab_synthetic_control() %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::group_by(.id, period = ifelse(time_unit < trt_time,"pre_rmspe","post_rmspe"))  %>%
    dplyr::summarize(.placebo = mean(.placebo),
                     rmspe = sqrt(sum((real_y - synth_y)^2)/n()) ) %>%
    tidyr::pivot_wider(names_from = period,values_from = rmspe) %>%
    dplyr::mutate(rmspe_ratio = post_rmspe/pre_rmspe) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(rmspe_ratio)) %>%
    dplyr::mutate(rank = dplyr::row_number(),
                  fishers_exact_pvalue = rank / max(rank),
                  z_score = (rmspe_ratio-mean(rmspe_ratio))/sd(rmspe_ratio),
                  type = ifelse(.placebo==0,"Treated","Donor")) %>%
    dplyr::left_join(.,unit_ids,by=".id") %>%
    dplyr::select(unit_name,type,pre_rmspe,post_rmspe,dplyr::everything(),-.id,-.placebo)
}



#' grab_comparison_table
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
grab_comparison_table <- function(data){

  # Checks
  if(!(".meta" %in% colnames(data))){ stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
  if(!(".unit_weights" %in% colnames(data))){ stop("`.unit_weights` column has been removed. Run `generate_weights()` prior to running this function.")}
  if(!(".predictor_weights" %in% colnames(data))){ stop("`.predictor_weights` column has been removed. Run `generate_weights()` prior to running this function.")}


  # Treated mean values
  treated_values <-
    data %>%
    grab_predictors(type="treated",placebo = F)

  # Synthetic Control Weighted Values
  control_values =
    data %>%
    grab_predictors(type="controls") %>%
    tidyr::gather(unit,value,-variable) %>%
    dplyr::left_join(grab_unit_weights(data),by="unit") %>%
    dplyr::mutate(value_adjusted = value*weight) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(synthetic_values = sum(value_adjusted))
  colnames(control_values)[2] = paste0("synthetic_",colnames(treated_values)[2])

  # Donor mean values
  donor_values <-
    data %>%
    grab_predictors(type="controls") %>%
    tidyr::gather(unit,value,-variable) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(donor_sample = mean(value))

  # Combine as a table
  table_ <-
    dplyr::left_join(treated_values,control_values,by='variable') %>%
    dplyr::left_join(donor_values,by='variable')

  return(table_)
}
