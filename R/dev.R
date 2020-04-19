# require(Synth)
require(tidyverse)

# load data
data(synth.data,package = "Synth")





dat <- as_tibble(synth.data)

.formula = Y ~ X1 + X2 + X3 | year + name
.data = dat
i_unit = "treated.region" # Treated unit (all other units are considered the control units)
i_time = 1988 # When treatment is received
restrict_time = NA # Provide a restricted time period to aggregate observations.
agg_methods = NA # Provide as a list different aggregation methods, e.g. list(X1 = mean(X1), X2 = sum(X2)), default behavior is the mean

# synthetic_control() [X]
# fit_weights() [X]
# generate_control() [X]



# Main pipeline/implementation
out <-

  dat %>%

  filter(year >= 1984 & year <= 1996) %>%
  filter(unit.num %in% c(2, 7, 13, 17, 29, 32, 38)) %>%
  synthetic_control(Y ~ X1 + X2 + X3 | year + name,
                    i_unit = "treated.region",
                    i_time = 1990) %>%
  fit_weights() %>%
  generate_control()


# plots
out %>% plot_trends()
out %>% plot_weights()



# This
synthetic_control(Y ~ X1 + X2 + X3 | year + name,
                  i_unit = "treated.region",
                  i_time = 1990,
                  predictor_window = 1984:1989,
                  optimization_window = 1984:1990)
# Default behavior will be to use the entire pre-treatment time period in the
# data.

# and make the aggregate function work for real.




# plots
out %>% plot_trends() + ylim(80,160)
out %>% plot_weights()


dat %>%
  mutate(treated = ifelse(year >= 1990 & name == "treated.region",1,0)) %>%
  group_by(treated,name,year) %>%
  mutate(y_mean = mean(Y)) %>%
  ungroup %>%
  mutate(y_1980 = ifelse(year==1980,y_mean,NA),
         y_1985 = ifelse(year==1985,y_mean,NA),
         y_1991 = ifelse(year==1991,y_mean,NA)) %>%
  select(-y_mean,-treated)



dat %>%
  filter(year==1980) %>%
  mutate(case_when())


dat %>%
  filter(year<=1985) %>%
  filter(unit.num==2) %>%
  pluck("Y") %>%
  mean(.)








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
    # special.predictors = list(
    #   list("Y", 1991, "mean"),
    #   list("Y", 1985, "mean"),
    #   list("Y", 1980, "mean")
    # ),
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


dataprep.out2$X1

basque %>%
  filter(regionno==17) %>%
  filter(year %in% 1964:1969) %>%
  summarize_all(function(x) mean(x,,na.rm=T)) %>%
  glimpse()


