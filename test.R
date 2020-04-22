# require(Synth)
require(tidyverse)
require(tidysynth)

# load data
data(synth.data,package = "Synth")
dat <- as_tibble(synth.data)


out =

  dat %>%

  # Initial specification of the method
  synthetic_control(outcome = Y,
                    unit = name,
                    time = year,
                    i_unit = "treated.region",
                    i_time = 1991) %>%

  # Generate the aggregate predictors used to generate the weights
  generate_predictors(time_window=1984:1989,
                      X1 = mean(X1, na.rm = T),
                      X2 = mean(X2, na.rm = T),
                      X3 = mean(X3, na.rm = T)) %>%
  generate_predictors(time_window=1980, y_1980 = Y) %>%
  generate_predictors(time_window=1985, y_1985 = Y) %>%
  generate_predictors(time_window=1991, y_1991 = Y) %>%

  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1984:1990) %>%

  # Generate the synthetic control
  generate_control()


# plots functions
out %>% plot_trends(time_window = 1984:1996) + ylim(80,160)
out %>% plot_differences(time_window = 1984:1996) + ylim(-25,25)
out %>% plot_weights()
out %>% plot_placebos(time_window = 1984:1996) + ylim(-25,25)
out %>% plot_rmspe_ratio(time_window = 1984:1996)

# grab functions
out %>% grab_signficance(time_window = 1984:1996)
out %>% grab_comparison_table()




# Germany Example ---------------------------------------------------------

germany <- haven::read_dta("/Users/edunford/Desktop/abadie_et_al_2015/repgermany.dta")


germany_out =

  germany %>%

  # Initial specification of the method
  synthetic_control(outcome = gdp,
                    unit = country,
                    time = year,
                    i_unit = "West Germany",
                    i_time = 1990) %>%

  # Generate the aggregate predictors used to generate the weights
  generate_predictors(time_window=1981:1990,
                      gdp_81_90 = mean(gdp, na.rm = T),
                      trade = mean(trade, na.rm = T),
                      infrate = mean(infrate, na.rm = T),
                      industry = mean(industry, na.rm = T)) %>%

  generate_predictors(time_window=c(1980,1985),
                      schooling = mean(schooling, na.rm = T)) %>%

  generate_predictors(time_window=1980,
                      invest80 = mean(invest80, na.rm = T)) %>%

  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1960:1989,include_fit = T,
                   custom_variable_weights = as.numeric(synth.out$solution.v[c(1,4,3,2,5,6)])) %>%

  # Generate the synthetic control
  generate_control()


# Assess output

# plots functions
rel_window = 1960:2003
germany_out %>%  plot_trends(time_window = rel_window) #+ ylim(0,35000)
germany_out %>% plot_differences(time_window = rel_window) #+ ylim(-25,25)
germany_out %>% plot_weights()
germany_out %>% plot_placebos(time_window = rel_window,prune = T) #+ ylim(-25,25)
germany_out %>% plot_rmspe_ratio(time_window = rel_window)

# grab functions
germany_out %>%
  grab_signficance(time_window = rel_window) %>%
  select(unit_name,fishers_exact_pvalue,z_score)
germany_out %>% grab_comparison_table()
germany_out %>% grab_unit_weights(placebo = F)





# Basque Example ----------------------------------------------------------

data(basque,package = "Synth")
basque = as_tibble(basque)

basque %>% filter(regionno==17)

basque_output <-

  basque %>%

  filter(regionno %in% c(2:16,18,17)) %>%

  # Initial specification of the method
  synthetic_control(outcome = gdpcap,
                    unit = regionname,
                    time = year,
                    i_unit = "Basque Country (Pais Vasco)",
                    i_time = 1970) %>%

  # Generate the aggregate predictors used to generate the weights
  generate_predictors(time_window=1964:1969,
                      school.illit = mean(school.illit, na.rm = T),
                      school.prim = mean(school.prim, na.rm = T),
                      school.med = mean(school.med, na.rm = T),
                      school.high = mean(school.high, na.rm = T),
                      school.post.high = mean(school.post.high, na.rm = T),
                      invest = mean(invest, na.rm = T)) %>%

  generate_predictors(time_window=1960:1969,
                      gdpcap_60_69 = mean(gdpcap, na.rm = T)) %>%

  generate_predictors(time_window=seq(1961,1969,2),
                      sec.agriculture = mean(sec.agriculture, na.rm = T),
                      sec.energy = mean(sec.energy, na.rm = T),
                      sec.industry = mean(sec.industry, na.rm = T),
                      sec.construction = mean(sec.construction, na.rm = T),
                      sec.services.venta = mean(sec.services.venta, na.rm = T),
                      sec.services.nonventa = mean(sec.services.nonventa, na.rm = T)) %>%

  generate_predictors(time_window=1969,
                      popdens = mean(popdens,na.rm = T)) %>%

  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1960:1969) %>%

  # Generate the synthetic control
  generate_control()



# plots functions
rel_window = 1955:1997
basque_output %>% plot_trends(time_window = rel_window) #+ ylim(80,160)
basque_output %>% plot_differences(time_window = rel_window) #+ ylim(-25,25)
basque_output %>% plot_weights()
basque_output %>% plot_placebos(time_window = rel_window) #+ ylim(-25,25)
basque_output %>% plot_rmspe_ratio(time_window = rel_window)

# grab functions
basque_output %>% grab_signficance(time_window = rel_window)
basque_output %>% grab_comparison_table()
basque_output %>% grab_outcomes()






data(basque,package = "Synth")

basque =  as_tibble(basque)
# dataprep: prepare data for synth
dataprep.out2 <-
 Synth::dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 17
    ,controls.identifier   = c(2:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997)
  )



# Smoking Example ---------------------------------------------------------

smoking <- read_csv("~/Desktop/smoking.csv")

smoking_out <-
  smoking %>%

  # Initial specification of the method
  synthetic_control(outcome = cigsale,
                    unit = state,
                    time = year,
                    i_unit = "California",
                    i_time = 1988) %>%

  # Generate the aggregate predictors used to generate the weights
  generate_predictor(time_window=1980:1988,
                      lnincome = mean(lnincome, na.rm = T),
                      retprice = mean(retprice, na.rm = T),
                      age15to24 = mean(age15to24, na.rm = T)) %>%

  generate_predictor(time_window=1984:1988,
                      beer = mean(beer, na.rm = T)) %>%

  generate_predictor(time_window=1975,
                      cigsale_1975 = cigsale) %>%

  generate_predictor(time_window=1980,
                      cigsale_1980 = cigsale) %>%

  generate_predictor(time_window=1988,
                      cigsale_1988 = cigsale) %>%


  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window =1970:1988,
                   quadopt = "LowRankQP",
                   Margin.ipop=.015,Sigf.ipop=7,Bound.ipop=6) %>%

  # Generate the synthetic control
  generate_control()



rel_window = 1970:2000
smoking_out %>% plot_trends(time_window = rel_window) #+ ylim(80,160)
smoking_out %>% plot_differences(time_window = rel_window) #+ ylim(-25,25)
smoking_out %>% plot_weights()
smoking_out %>% plot_placebos(time_window = rel_window) # ylim(-50,50)
smoking_out %>% plot_rmspe_ratio(time_window = rel_window)

# grab functions
smoking_out %>% grab_signficance(time_window = rel_window) %>% select(fishers_exact_pvalue,z_score)
smoking_out %>% grab_comparison_table()
smoking_out %>% grab_outcomes()



# Fake Example ------------------------------------------------------------



dataprep.out$Z1

# create matrices from panel data that provide inputs for synth()
dataprep.out<-
  Synth::dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )


str(dataprep.out)
dataprep.out$X0
## run the synth command to identify the weights
## that create the best possible synthetic
## control unit for the treated.
synth.out <- Synth::synth(dataprep.out)




## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
# contains the unit weights or
synth.out$solution.v
## contains the predictor weights.

## the output from synth opt
## can be flexibly combined with
## the output from dataprep to
## compute other quantities of interest
## for example, the period by period
## discrepancies between the
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps

## also there are three convenience functions to summarize results.
## to get summary tables for all information
## (V and W weights plus balance btw.
## treated and synthetic control) use the
## synth.tab() command
synth.tables <- Synth::synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories
## of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
Synth::path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
Synth::gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)



