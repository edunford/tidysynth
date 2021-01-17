context("Test generate_control()")

#'
#' NOTE: if all the grab_ functions are operating correctly, than all the plot
#' functions will.
#'


# Fake Data ---------------------------------------------------------------

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


# Expected synthetic control
sc_dat <-
  tibble::tribble(
    ~time_unit,             ~real_y,              ~synth_y,
    1980L, -0.0605503371361133,   -0.0544594628373882,
    1981L,  -0.101508195894036,    0.0873721061992959,
    1982L, -0.0109233622439013,    0.0117459880470076,
    1983L, -0.0309718128863186,    0.0170141322902309,
    1984L,   0.104828434422171,   -0.0533975029507728,
    1985L,   0.113971802427489,    0.0620490124465434,
    1986L,   0.106888052821424,     0.162515468875739,
    1987L,   -0.28829439428957, -0.000544430276171917,
    1988L, -0.0742414817418066,    -0.100027175594688,
    1989L, 0.00737452338435042,  -0.00169531032760945,
    1990L,   0.152523515960618,   -0.0306751705342468,
    1991L,    1.04654900212063,     0.127467568403872,
    1992L,   0.976006544228868,     0.193083334730229,
    1993L,   0.926097836991154,     0.120841294435654,
    1994L,   0.842003007464101,   -0.0461947475045125,
    1995L,    1.19045597339032,   -0.0897580982329787,
    1996L,   0.955037586404444,    -0.017364922641622,
    1997L,   0.754282539966969,   -0.0591969425250982,
    1998L,    1.04452637093654,   -0.0284580170478066,
    1999L,    1.13710705975043,    0.0414469796578313,
    2000L,    0.82802263923177,      -0.0640961244327
  )


p_sc_dat<-
  tibble::tribble(
    ~time_unit,              ~real_y,             ~synth_y,
    1980L,   -0.050171333879348,  -0.0249695296150878,
    1981L,  -0.0702826699090492,  0.00790796747907694,
    1982L,  0.00881481337625646,  -0.0937075865319851,
    1983L,   0.0513502812154741,  -0.0456272782539003,
    1984L,  0.00189369729920402, -0.00421129228939562,
    1985L,    0.305071936689592,   0.0881146601831639,
    1986L,    0.130874858935432,   0.0528394437863349,
    1987L,    -0.15870274331312,   -0.125958706790801,
    1988L,   -0.227138047646973,  -0.0680154645496344,
    1989L,    0.131621927390539,   0.0886148346044455,
    1990L,   -0.027267728367221,   -0.084693666256595,
    1991L,   -0.114637394917846,    0.307133146609838,
    1992L, -0.00377547642744921,    0.332079935701486,
    1993L,   0.0978412169038481,    0.158411708255173,
    1994L,   -0.157036810020809,    0.165314851290359,
    1995L,  -0.0684209382904224,    0.169024125202278,
    1996L,  -0.0375170931380141,   0.0932937662230701,
    1997L,  -0.0191411708448665, -0.00191310541810609,
    1998L,    0.149520069083997,     0.04398525836081,
    1999L,  -0.0285922739595213,    0.113651524102581,
    2000L,    0.139843033370335,  0.00270109298633095
  )

# Tests -------------------------------------------------------------------

test_that("Test generate_control() with placebos",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1980:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.) %>%

    generate_control()


  # Correct class
  # expect_is(synth_out,class="synth_tbl")
  expect_is(synth_out$.synthetic_control[[1]],"tbl_df")

  # Weights were generated for the placebo units (redundant)
  expect_true(length(synth_out$.synthetic_control)==10)

  # make sure that the synthetic control fits are the same
  expect_equivalent(synth_out$.synthetic_control[[1]] %>%
                      dplyr::mutate_all(function(x) round(x,3)),
                    sc_dat %>%
                      dplyr::mutate_all(function(x) round(x,3)))

  # Make sure this holds true for the placebo cases as well
  expect_equivalent(synth_out$.synthetic_control[[7]] %>%
                      dplyr::mutate_all(function(x) round(x,3)),
                    p_sc_dat %>%
                      dplyr::mutate_all(function(x) round(x,3)))

})


test_that("test grab_synthetic_control()",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1980:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.) %>%

    generate_control()

  # Grab function works
  expect_equivalent(synth_out$.synthetic_control[[1]],
                    synth_out %>% grab_synthetic_control())
  expect_equivalent(synth_out$.synthetic_control[[7]],
                    synth_out %>% grab_synthetic_control(placebo = T) %>%
                      dplyr::filter(.id=="d") %>%
                      dplyr::select(time_unit,real_y,synth_y))
})


test_that("Test grab_significance()",{

  synth_out <- synthetic_control(data=dat,outcome = outcome,time = time,
                                 unit = unit,i_unit = "a",i_time=1990,
                                 generate_placebos = T) %>%
    generate_predictor(time_window = 1980:1990,
                       x1 = mean(x1)) %>%
    generate_weights(.) %>%

    generate_control()

  # Test that the right significane table is produced
  expect_equivalent(synth_out %>% grab_signficance() %>%
                      dplyr::mutate_if(is.numeric,function(x) round(x,3)),
                    tibble::tribble(
                      ~unit_name,     ~type,~pre_mspe,~post_mspe,~mspe_ratio,~rank,~fishers_exact_pvalue,           ~z_score,
                      "a", "Treated", 0.0169512922594286,  0.928891875870006, 54.7977028331478,    1L,                   0.2,   1.56317811804723,
                      "e",   "Donor", 0.0312252450128028,  0.921076115953743, 29.4978026778041,    2L,                   0.4,  0.448733268451988,
                      "d",   "Donor", 0.0101273790666457, 0.0522235877045452, 5.15667354414948,    3L,                   0.6, -0.623478314588758,
                      "b",   "Donor", 0.0125409230160292, 0.0546244705940941, 4.35569778430788,    4L,                   0.8, -0.658760798105499,
                      "c",   "Donor", 0.0274592850257227, 0.0753998986945673, 2.74587989541373,    5L,                     1, -0.729672273804957
                    ) %>% dplyr::mutate_if(is.numeric,function(x) round(x,3))
  )
})






