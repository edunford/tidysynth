# All plotting methods used in the syntehtic_control package.


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
  UseMethod("plot_trends")
}

#' @export
plot_trends.synth_tbl <- function(data,time_window=NULL){

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
    grab_synthetic_control(placebo = F) %>%
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
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
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
    grab_synthetic_control(placebo = F) %>%
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
  UseMethod("plot_placebos")
}

#' @export
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
    grab_synthetic_control(placebo = T) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::mutate(type_text = ifelse(.placebo==0,treatment_unit,"control units"),
                  type_text = factor(type_text,level=c(treatment_unit,"control units")))


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
  UseMethod("plot_weights")
}

#' @export
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


#' plot_mspe_ratio
#'
#' @param data
#' @param time_window
#'
#' @return
#' @export
#'
#' @examples
plot_mspe_ratio <- function(data,time_window=NULL){
  UseMethod("plot_mspe_ratio")
}

#' @export
plot_mspe_ratio <- function(data,time_window=NULL){
  data %>%
    grab_signficance(time_window=time_window) %>%
    mutate(unit_name = forcats::fct_reorder(as.character(unit_name),mspe_ratio)) %>%
    ggplot2::ggplot(ggplot2::aes(unit_name,
                                 mspe_ratio,
                                 fill=type,color=type)) +
    ggplot2::geom_col(alpha=.65) +
    ggplot2::coord_flip() +
    ggplot2::geom_point() +
    ggplot2::labs(y = "Post-Period MSPE / Pre-Period MSPE",x="",fill="",color="") +
    ggplot2::scale_fill_manual(values=c("grey50","#b41e7c")) +
    ggplot2::scale_color_manual(values=c("grey50","#b41e7c")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
