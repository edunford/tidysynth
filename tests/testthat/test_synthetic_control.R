context("Test synthetic_control() initialization of the synth pipeline")

#'
#' NOTE: if all the grab_ functions are operating correctly, than all the plot
#' functions will.
#'

# Generate FAKE DATA for the TESTs ----------------------------------------
set.seed(123)

# Treatment unit
treated <-
  tibble::tibble(unit= "a",
                 time= 1990:2000,
                 outcome = runif(11,-1,1),
                 treatment_value = c(rep(0,6),1,2,3,4,5),
                 intervention_outcome = outcome + treatment_value,
                 x1 = rnorm(11,0,1),
                 x2 = rnorm(11,0,1),
                 x3 = rnorm(11,0,1))

# Control units
controls <-
  dplyr::bind_rows(
    tidyr::crossing(tibble::tibble(unit=c("b","c","d","e")),
                    tibble::tibble(time=1990:2000))) %>%
  dplyr::mutate(outcome = runif(44,-1,1),
                x1 = rnorm(44,0,1),
                x2 = rnorm(44,0,1),
                x3 = rnorm(44,0,1))

dat <- dplyr::bind_rows(treated,controls) %>% tidyr::replace_na(list(treatment_value=0))



# Tests -------------------------------------------------------------------

test_that("Test initialization of a synth pipeline using sythetic_control() works as expected with placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = T)

  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Correct columns
  expect_equal(colnames(synth_out),expected = c(".id",".placebo",".type", ".outcome", ".original_data", ".meta"))

  # There is a treated and controls unit
  expect_equal(unique(synth_out$.type),expected = c("treated","controls"))

  # Original data is stored correctly
  expect_identical(synth_out$.original_data[[1]], expected = treated)

  # all placebo units are contained in the data.
  expect_equal(unique(synth_out$.id), expected = c("a","b","c","d","e"))

  # .placebo indicator reflects the actual number of placebo entries
  expect_true(sum(synth_out$.placebo)==8)

  # check meta
  expect_equal(synth_out$.meta[[1]],
               expected=tibble::tibble(unit_index="unit",
                                       time_index = "time",
                                       treatment_unit = "a",
                                       treatment_time = 1995,
                                       outcome = "outcome"))
  expect_equal(synth_out$.meta[[9]],
               expected=tibble::tibble(unit_index="unit",
                                       time_index = "time",
                                       treatment_unit = "e",
                                       treatment_time = 1995,
                                       outcome = "outcome"))

  # Outcome initialization is correct (for the treated unit)
  expect_is(synth_out$.outcome[[1]],class="tbl_df")
  expect_equal(colnames(synth_out$.outcome[[1]]),expected = c("time_unit","a"))
  expect_equivalent(synth_out$.outcome[[1]],
                    expected = treated %>%
                      dplyr::select(time_unit=time,a=outcome) %>%
                      dplyr::filter(time_unit <= 1995))

  # Outcome initialization is correct (for a placebo unit)
  expect_is(synth_out$.outcome[[9]],class="tbl_df")
  expect_equal(colnames(synth_out$.outcome[[9]]),expected = c("time_unit","e"))
  expect_equivalent(synth_out$.outcome[[9]],
                    expected = controls %>%
                      dplyr::filter(unit == "e") %>%
                      dplyr::select(time_unit=time,e=outcome) %>%
                      dplyr::filter(time_unit <= 1995))

  # Check control unit (for treated unit)
  expect_is(synth_out$.outcome[[2]],class="tbl_df")
  expect_equal(colnames(synth_out$.outcome[[2]]),expected = c("time_unit","b","c","d","e"))
  expect_equivalent(synth_out$.outcome[[2]],
                    expected = controls %>%
                      dplyr::select(time_unit = time,outcome,unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from = outcome) %>%
                      dplyr::filter(time_unit <= 1995))

  # Check control unit (for a placebo unit)
  expect_is(synth_out$.outcome[[10]],class="tbl_df")
  expect_equal(colnames(synth_out$.outcome[[10]]),expected = c("time_unit","a","b","c","d"))
  expect_equivalent(synth_out$.outcome[[10]],
                    expected = dat %>%
                      dplyr::filter(unit!="e") %>%
                      dplyr::select(time_unit = time,outcome,unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from = outcome) %>%
                      dplyr::filter(time_unit <= 1995))
})


# Generate test for the relevant grab function
test_that("Test grab_outcomes works",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = T)

  # Grabs the outcome
  expect_equivalent(synth_out %>% grab_outcome(),
                    synth_out$.outcome[[1]])

  # Grab function works for placebos
  expect_equivalent(synth_out %>% grab_outcome(placebo=T) %>% .[7,5] %>% round(.,3),
                    tibble::tibble(b=0.226))

})


test_that("Test initialization of a synth pipeline using sythetic_control() works as expected without placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = F)

  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Correct columns
  expect_equal(colnames(synth_out),expected = c(".id",".placebo",".type", ".outcome", ".original_data", ".meta"))

  # There is a treated and controls unit
  expect_equal(unique(synth_out$.type),expected = c("treated","controls"))

  # Original data is stored correctly
  expect_identical(synth_out$.original_data[[1]], expected = treated)

  # all placebo units are contained in the data.
  expect_error(expect_equal(unique(synth_out$.id), expected = c("a","b","c","d","e")))
  expect_equal(unique(synth_out$.id), expected = c("a"))
  expect_true(sum(synth_out$.placebo)==0)

  # Outcome initialization is correct (for the treated unit)
  expect_equal(colnames(synth_out$.outcome[[1]]),expected = c("time_unit","a"))
  expect_is(synth_out$.outcome[[1]],class="tbl_df")
  expect_equivalent(synth_out$.outcome[[1]],
                    expected = treated %>%
                      dplyr::select(time_unit=time,a=outcome) %>%
                      dplyr::filter(time_unit <= 1995))
  expect_equal(synth_out$.meta[[1]],
               expected=tibble::tibble(unit_index="unit",
                                       time_index = "time",
                                       treatment_unit = "a",
                                       treatment_time = 1995,
                                       outcome = "outcome"))

  # There shouldn't be a plcebo unit
  expect_error(synth_out$.outcome[[9]])


  # Check control unit (for treated unit)
  expect_is(synth_out$.outcome[[2]],class="tbl_df")
  expect_equal(colnames(synth_out$.outcome[[2]]),expected = c("time_unit","b","c","d","e"))
  expect_equivalent(synth_out$.outcome[[2]],
                    expected = controls %>%
                      dplyr::select(time_unit = time,outcome,unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from = outcome) %>%
                      dplyr::filter(time_unit <= 1995))
})

test_that("synthetic_control works when no placebos and when the treated country doesn't come first in the data", {

  smoking2 <- smoking %>%
    dplyr::filter(state %in% c("Rhode Island", "Tennessee", "Connecticut", "California",
                        "Nevada", "Indiana", "Arkansas"))
  no_placebo <- smoking2 %>%
    synthetic_control(
      outcome = cigsale,
      unit = state,
      time = year,
      i_unit = "California",
      i_time = 1988,
      generate_placebos = FALSE
    ) %>%
    generate_predictor(
      time_window = 1980:1988,
      ln_income = mean(lnincome, na.rm = T)
    ) %>%
    generate_weights(optimization_window = 1970:1988) %>%
    generate_control()

  with_placebo <- smoking2 %>%
    synthetic_control(
      outcome = cigsale,
      unit = state,
      time = year,
      i_unit = "California",
      i_time = 1988,
      generate_placebos = TRUE
    ) %>%
    generate_predictor(
      time_window = 1980:1988,
      ln_income = mean(lnincome, na.rm = T)
    ) %>%
    generate_weights(optimization_window = 1970:1988) %>%
    generate_control()

  expect_identical(
    with_placebo %>% grab_outcome(),
    no_placebo %>% grab_outcome()
  )
  expect_identical(
    with_placebo %>% grab_predictors(),
    no_placebo %>% grab_predictors()
  )
  expect_identical(
    with_placebo %>% grab_unit_weights(),
    no_placebo %>% grab_unit_weights()
  )
  expect_identical(
    with_placebo %>% grab_predictor_weights(),
    no_placebo %>% grab_predictor_weights()
  )
  expect_identical(
    with_placebo %>% grab_balance_table(),
    no_placebo %>% grab_balance_table()
  )
  expect_identical(
    with_placebo %>% grab_synthetic_control(),
    no_placebo %>% grab_synthetic_control()
  )

  # grab_significance and grab_loss show the results even for the placebos
  # so not tested here

})
