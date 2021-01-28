# All plotting methods used in the syntehtic_control package.


#' plot_trends
#'
#' Plot the observed and synthetic trends for the treated units.
#'
#' Synthetic control is a visual-based method, like Regression Discontinuity, so
#' inspection of the pre-intervention period fits is key assessing the sythetic
#' control's fit. A poor fit in the pre-period reduces confidence in the
#' post-period trend capturing the counterfactual.
#'
#' See `?generate_control()` for information on how to generate a synthetic
#' control unit.
#'
#' @param data nested data of type `tbl_df`.
#' @param time_window time window of the trend plot.
#'
#' @return `ggplot` object of the observed and synthetic trends.
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
#'                   generate_placebos=TRUE) %>%
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
plot_trends <- function(data,time_window=NULL){
  UseMethod("plot_trends")
}

#' @export
plot_trends <- function(data,time_window=NULL){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Generate plot
  data %>%
    grab_synthetic_control(placebo = FALSE) %>%
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
                  title=paste0("Time Series of the synthetic and observed ",outcome_name),
                  caption = "Dashed line denotes the time of the intervention.") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' plot_difference
#'
#' Plot the difference between the observed and sythetic control unit. The
#' difference captures the causal quantity (i.e. the magnitude of the difference
#' between the observed and counterfactual case).
#'
#' @param data nested data of type `tbl_df`.
#' @param time_window time window of the trend plot.
#'
#' @return `ggplot` object of the difference between the observed and synthetic
#'   trends.
#'
#' @return
#' @export
#'
#' @examples
#'
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
#'                   generate_placebos=TRUE) %>%
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
#' smoking_out %>% plot_differences(time_window = 1970:2000)
#'
#' }
#'
plot_differences <- function(data,time_window=NULL){
  UseMethod("plot_differences")
}

#' @export
plot_differences <- function(data,time_window=NULL){

  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}

  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  outcome_name <- data$.meta[[1]]$outcome[1]

  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}

  # Generate plot
  data %>%
    grab_synthetic_control(placebo = FALSE) %>%
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
#' Plot the difference between the observed and sythetic control unit for the
#' treated and the placebo units. The difference captures the causal quantity
#' (i.e. the magnitude of the difference between the observed and counterfactual
#' case). Plotting the actual treated observation against the placebos captures
#' the likelihood (or rarity) of the observed differenced trend.
#'
#'
#' The function provides a pruning rule where all placebo cases with a
#' pre-period root mean squared predictive error (RMSPE) exceeding two times the
#' treated unit pre-period RMSPE are pruned. This helps overcome scale issues
#' when a particular placebo case has poor fit in the pre-period.
#'
#' See documentation on `?synthetic_control` on how to generate placebo cases.
#' When initializing a synth pipeline, set the `generate_placebos` argument to
#' `TRUE`. The processing pipeline remains the same.
#'
#' @param data nested data of type `tbl_df`.
#' @param time_window time window of the tbl_df plot.
#' @param prune boolean flag; if TRUE, then all placebo cases with a pre-period
#'   RMSPE exceeding two times the treated unit pre-period RMSPE are pruned;
#'   Default is TRUE.
#'
#' @return `ggplot` object of the difference between the observed and synthetic
#'   trends for the treated and placebo units.
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
#'                   generate_placebos=TRUE) %>%
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
#' smoking_out %>% plot_placebos(time_window = 1970:2000)
#'
#' }
#'
plot_placebos <- function(data,time_window=NULL,prune=TRUE){
  UseMethod("plot_placebos")
}

#' @export
plot_placebos <- function(data,time_window=NULL,prune=TRUE){

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
    grab_synthetic_control(placebo = TRUE) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::mutate(type_text = ifelse(.placebo==0,treatment_unit,"control units"),
                  type_text = factor(type_text,levels=c(treatment_unit,"control units")))


  # Pruning implementation-- if one of the donors falls outside two standard
  # deviations of the rest of the pool, it's dropped.
  caption <- ""
  if (prune){

    # Gather significance field
    sig_data = data %>% grab_signficance(time_window = time_window)

    # Treated units Pre-Period RMSPE
    thres <-
      sig_data %>%
      dplyr::filter(type=="Treated") %>%
      dplyr::pull(pre_mspe) %>%
      sqrt(.)

    # Only retain units that are 2 times the treated unit RMSPE.
    retain_ <-
      sig_data %>%
      dplyr::select(unit_name,pre_mspe) %>%
      dplyr::filter(sqrt(pre_mspe) <= thres*2) %>%
      dplyr::pull(unit_name)

    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- "Pruned all placebo cases with a pre-period RMSPE exceeding two times the treated unit's pre-period RMSPE."
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
    ggplot2::labs(color="",alpha="",size="",y=outcome_name,x=time_index,
                  title=paste0("Difference of each '",unit_index,"' in the donor pool"),
                  caption = caption) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom")
}



#' plot_weights
#'
#' Plot the unit and predictor variable weights generated using `generate_weights()`
#'
#' See `grab_unit_weights()` and `grab_predictor_weights()`
#'
#' @param data nested data of type `tbl_df`.
#'
#' @return a `ggplot` object that plots the unit and variable weights.
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
#'                   generate_placebos=TRUE) %>%
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
#' smoking_out %>% plot_weights()
#'
#' }
#'
plot_weights <- function(data){
  UseMethod("plot_weights")
}

#' @export
plot_weights <- function(data){

  # Combine the different type of weight outputs
  dplyr::bind_rows(

    grab_unit_weights(data,placebo = FALSE) %>%
      dplyr::mutate(type="Control Unit Weights (W)"),

    grab_predictor_weights(data,placebo = FALSE) %>%
      dplyr::mutate(type="Variable Weights (V)") %>%
      dplyr::rename(unit = variable)

  ) %>%

    # Generate plot
    dplyr::arrange(weight) %>%
    dplyr::mutate(unit = forcats::fct_reorder(unit,weight)) %>%
    ggplot2::ggplot(ggplot2::aes(unit,weight,fill=type,color=type)) +
    ggplot2::geom_col(show.legend = FALSE,alpha=.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(x="") +
    ggplot2::facet_wrap(~type,ncol = 2,scales="free") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#b41e7c","grey60")) +
    ggplot2::scale_color_manual(values=c("#b41e7c","grey60")) +
    ggplot2::theme(text = ggplot2::element_text(size = 14))
}


#' plot_mspe_ratio
#'
#' Plot the MSPE ratios for each case (observed and placebos). The ratio is used
#' for inference in the synthetic control setup. The following plot ranks the
#' RMSE ratio's in descending order.
#'
#' Inferential statitics are generated by comparing the observed difference
#' between the actual treated unit and its synthetic control to each placebo
#' unit and its synthetic control. The rarity of the actual to the placebo is
#' used to infer the likelihood of observing the effect.
#'
#' Inference in this framework leverages the mean squared predictive error
#' (MSPE) of the fit in the pre-period to the fit in the post-period as a ratio.
#'
#' \deqn{\frac{RMSE_{Post}}{RMSE_{Pre}}}
#'
#' The ratio captures the differences between the pre-intervention fit and the
#' post-intervention divergence of the trend (i.e. the causal quantity). A good
#' fit in the pre-period denotes that the observed and synthetic case tracked
#' well together. Divergence in the post-period captures the difference brought
#' about by the intervention in the two trends. Thus, when the ratio is high, we
#' observe more of a difference between the two trends. If, however, the
#' pre-period fit is poor, or there is not substantial divergence in the
#' post-period, then this ratio amount will be smaller. A more detailed outline
#' of inference within the synthetic control framework can be found in Adabie et
#' al. 2010.
#'
#'
#' @param data nested data of type `tbl_df`.
#' @param time_window time window that the pre- and post-period values should be
#'   used to compute the MSPE ratio.
#'
#' @return `ggplot` object plotting the MSPE ratios by case.
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
#'                   generate_placebos=TRUE) %>%
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
#' smoking_out %>% plot_mspe_ratio(time_window = 1970:2000)
#'
#' }
#'
plot_mspe_ratio <- function(data,time_window=NULL){
  UseMethod("plot_mspe_ratio")
}

#' @export
plot_mspe_ratio <- function(data,time_window=NULL){
  data %>%
    grab_signficance(time_window=time_window) %>%
    dplyr::mutate(unit_name = forcats::fct_reorder(as.character(unit_name),mspe_ratio)) %>%
    ggplot2::ggplot(ggplot2::aes(unit_name,
                                 mspe_ratio,
                                 fill=type)) +
    ggplot2::geom_col(alpha=.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "Post-Period MSPE / Pre-Period MSPE",x="",fill="",color="",
                  title="Ratio of the pre and post intervention period mean squared predictive error") +
    ggplot2::scale_fill_manual(values=c("grey50","#b41e7c")) +
    ggplot2::scale_color_manual(values=c("grey50","#b41e7c")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
