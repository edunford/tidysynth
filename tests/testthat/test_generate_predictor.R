context("Test generate_predictor()")

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


test_that("Test generate_predictor() with placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1990:1995,
                       x1 = mean(x1),
                       x2 = mean(x2),
                       x3 = mean(x3))


  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Check for scenarios where there is an NA in a predictor field
  expect_error(dat %>%
                 mutate(x1 = NA) %>%
                 synthetic_control(data=.,outcome = outcome,time = time,
                                   unit = unit,i_unit = "a",i_time=1995,
                                   generate_placebos = T) %>%
                 generate_predictor(time_window = 1990:1995,
                                    x1 = mean(x1),
                                    x2 = mean(x2),
                                    x3 = mean(x3))
  )

  # Check that overwriting a field doesn't result in redundancies.
  expect_equivalent(
      synthetic_control(data=dat,outcome = outcome,time = time,
                        unit = unit,i_unit = "a",i_time=1995,
                        generate_placebos = T) %>%
        # Create variable
        generate_predictor(time_window = 1990:1995,
                           x1 = mean(x1)) %>%
        # Overwrite variable
        generate_predictor(time_window = 1990:1995,
                           x1 = mean(x2)) %>%
        .$.predictors %>% .[[1]],

      # Compared to just creating the variable with no overwriting
      synthetic_control(data=dat,outcome = outcome,time = time,
                        unit = unit,i_unit = "a",i_time=1995,
                        generate_placebos = T) %>%
        generate_predictor(time_window = 1990:1995,
                           x1 = mean(x2)) %>%
        .$.predictors %>% .[[1]]
  )


  # check data construct is correct for the predictors for treated unit
  expect_is(synth_out$.predictors[[1]],"tbl_df")
  expect_equal(colnames(synth_out$.predictors[[1]]),c("variable","a"))
  expect_equal(colnames(synth_out$.predictors[[2]]),c("variable","b","c","d","e"))
  # treated
  expect_equivalent(synth_out$.predictors[[1]],
                    expected =
                      dplyr::filter(dat,unit=="a",time <= 1995) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,a))
  # controls
  expect_equivalent(synth_out$.predictors[[2]],
                    expected =
                      dplyr::filter(dat,unit!="a",time <= 1995) %>%
                      dplyr::group_by(unit) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,value,-unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from=value))



  # check data construct is correct for the predictors for a placebo unit
  expect_is(synth_out$.predictors[[9]],"tbl_df")
  expect_equal(colnames(synth_out$.predictors[[9]]),c("variable","e"))
  expect_equal(colnames(synth_out$.predictors[[10]]),c("variable","a","b","c","d"))
  # treated
  expect_equivalent(synth_out$.predictors[[9]],
                    expected =
                      dplyr::filter(dat,unit=="e",time <= 1995) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,e))
  # controls
  expect_equivalent(synth_out$.predictors[[10]],
                    expected =
                      dplyr::filter(dat,unit!="e",time <= 1995) %>%
                      dplyr::group_by(unit) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,value,-unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from=value))
})



test_that("Test generate_predictor() without placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = F) %>%
    generate_predictor(time_window = 1990:1995,
                       x1 = mean(x1),
                       x2 = mean(x2),
                       x3 = mean(x3))


  # Correct class
  # expect_is(synth_out,class="synth_tbl")

  # Check for scenarios where there is an NA in a predictor field
  expect_error(dat %>%
                 mutate(x1 = NA) %>%
                 synthetic_control(data=.,outcome = outcome,time = time,
                                   unit = unit,i_unit = "a",i_time=1995,
                                   generate_placebos = T) %>%
                 generate_predictor(time_window = 1990:1995,
                                    x1 = mean(x1),
                                    x2 = mean(x2),
                                    x3 = mean(x3))
  )

  # Check that overwriting a field doesn't result in redundancies.
  expect_equivalent(
    synthetic_control(data=dat,outcome = outcome,time = time,
                      unit = unit,i_unit = "a",i_time=1995,
                      generate_placebos = T) %>%
      # Create variable
      generate_predictor(time_window = 1990:1995,
                         x1 = mean(x1)) %>%
      # Overwrite variable
      generate_predictor(time_window = 1990:1995,
                         x1 = mean(x2)) %>%
      .$.predictors %>% .[[1]],

    # Compared to just creating the variable with no overwriting
    synthetic_control(data=dat,outcome = outcome,time = time,
                      unit = unit,i_unit = "a",i_time=1995,
                      generate_placebos = T) %>%
      generate_predictor(time_window = 1990:1995,
                         x1 = mean(x2)) %>%
      .$.predictors %>% .[[1]]
  )


  # check data construct is correct for the predictors for treated unit
  expect_is(synth_out$.predictors[[1]],"tbl_df")
  expect_equal(colnames(synth_out$.predictors[[1]]),c("variable","a"))
  expect_equal(colnames(synth_out$.predictors[[2]]),c("variable","b","c","d","e"))
  # treated
  expect_equivalent(synth_out$.predictors[[1]],
                    expected =
                      dplyr::filter(dat,unit=="a",time <= 1995) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,a))
  # controls
  expect_equivalent(synth_out$.predictors[[2]],
                    expected =
                      dplyr::filter(dat,unit!="a",time <= 1995) %>%
                      dplyr::group_by(unit) %>%
                      dplyr::summarize(x1 = mean(x1),
                                       x2 = mean(x2),
                                       x3 = mean(x3)) %>%
                      tidyr::gather(variable,value,-unit) %>%
                      tidyr::pivot_wider(names_from = unit,values_from=value))


  # Check there aren't any placebo entries
  expect_error(synth_out$.predictors[[9]])

})


test_that("Test grab_predictors()",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1995,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1990:1995,
                       x1 = mean(x1),
                       x2 = mean(x2),
                       x3 = mean(x3))

  # grab should be the same as directly tapping into the data.
  expect_equivalent(synth_out %>% grab_predictors(),
                    synth_out$.predictors[[1]])

  expect_equivalent(synth_out %>% grab_predictors(placebo=T) %>% .[5,5] %>% round(.,3),
                    tibble::tibble(b = 0.089))

})

