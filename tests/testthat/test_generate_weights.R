context("Test generate_weights()")

#'
#' NOTE: if all the grab_ functions are operating correctly, than all the plot
#' functions will.
#'

# Generate Fake Data for Test ---------------------------------------------

set.seed(123)
time_span <- 1980:2000
unit_span <- c("a","b","c","d","e")
unit <- rep(unit_span,length(time_span)); unit <- unit[order(unit)]
time <- rep(time_span,length(unit_span))
N = length(time)
x1 <- rnorm(N,0,.1)
tr <- rep(0,N); tr[12:length(time_span)] <- 1
e  <- rnorm(N,0,.1)
y  <- rep(NA,N)
y <- x1 + tr + e
# Data generated
dat <- tibble::tibble(unit,time,outcome=y,x1)

# Expected Weights
unit_weights <-
  tibble::tibble(
    unit = c("b", "c", "d", "e"),
    weight = c(0.14593576401803,0.205551760722976,0.175872688538295,
               0.472639786639596)
  ) %>% dplyr::mutate(weight=round(weight,3))
predictor_weights <-
  tibble::tibble(
    variable = c("x1"),
    weight = c(1)
  )
fit_loss =
  tibble::tibble(
    variable_mspe = c(0.03356176),
    control_unit_mspe = c(1.334333e-10)
)


# Tests -------------------------------------------------------------------

test_that("Test generate_weights() with placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1980:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.)


  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Check output weights class
  expect_is(synth_out$.unit_weights[[1]],"tbl_df")
  expect_is(synth_out$.predictor_weights[[1]],"tbl_df")

  # Weights were generated for the placebo units (redundant)
  expect_true(length(synth_out$.unit_weights)==10)

  # Check output weights structure
  expect_equivalent(synth_out$.unit_weights[[1]] %>%
                      dplyr::mutate(weight=round(weight,3)),
                    unit_weights)
  expect_equivalent(synth_out$.predictor_weights[[1]],predictor_weights)

  # Check for loss output
  expect_equivalent(synth_out$.loss[[1]] %>%
                      dplyr::mutate_all(function(x) round(x,3)),
                    fit_loss %>%
                      dplyr::mutate_all(function(x) round(x,3)))

})



test_that("Test grab_ functions relevant to retrieving weights",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1980:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.)

  # Grab unit weights
  expect_equivalent(synth_out %>% grab_unit_weights() %>%
                      dplyr::mutate(weight=round(weight,3)),
                    unit_weights)

  expect_equivalent(
    synth_out %>%
      grab_unit_weights(placebo = T) %>%
      dplyr::filter(.id=="b") %>%
      dplyr::select(unit,weight),
    synth_out$.unit_weights[[3]]
  )

  # Grab predictor weights
  expect_equivalent(synth_out %>% grab_predictor_weights(),
                    predictor_weights)

  expect_equivalent(
    synth_out %>%
      grab_predictor_weights(placebo = T) %>%
      dplyr::filter(.id=="b") %>%
      dplyr::select(variable,weight),
    synth_out$.predictor_weights[[3]]
  )

  # Grab loss
  expect_equivalent(synth_out %>% grab_loss() %>% .[5,4] %>% round(.,3),
                    tibble::tibble(control_unit_mspe = 0.998))

  # Grab balance
  expect_equivalent(synth_out %>%
                      grab_balance_table() %>%
                      dplyr::mutate_if(is.numeric,function(x) round(x,3)),
                    tibble::tribble(
                      ~variable,                 ~a,       ~synthetic_a,       ~donor_sample,
                      "x1", 0.018, 0.018, 0.005
                    ))
})





# New weights for different fit
unit_weights <-
  tibble::tibble(
    unit = c("b", "c", "d", "e"),
    weight = c(0.199470353213706,
               0.388936735331363,
               0.285579899661123,
               0.126013011762585)
  ) %>% dplyr::mutate(weight=round(weight,3))

fit_loss <-
  tibble::tibble(
    variable_mspe = c(0.015129143920358),
    control_unit_mspe = c(1.34040760212062e-10)
  )

test_that("Test constraining the optimization window",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1985:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.)


  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Check output weights class
  expect_is(synth_out$.unit_weights[[1]],"tbl_df")
  expect_is(synth_out$.predictor_weights[[1]],"tbl_df")

  # Weights were generated for the placebo units (redundant)
  expect_true(length(synth_out$.unit_weights)==10)

  # Check output weights structure
  expect_equivalent(synth_out$.unit_weights[[1]] %>%
                      dplyr::mutate(weight=round(weight,3)),
                    unit_weights)
  expect_equivalent(synth_out$.predictor_weights[[1]],predictor_weights)

  # Check for loss output
  # expect_equivalent(synth_out$.loss[[1]] %>%
  #                     dplyr::mutate_all(function(x) round(x,3)),
  #                   fit_loss %>%
  #                     dplyr::mutate_all(function(x) round(x,3)))



})






