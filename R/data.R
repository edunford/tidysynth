#' @importFrom tibble tibble
NULL

#' smoking dataset
#'
#' A dataset on the implmentation of Proposition 99 in California in 1988. Data
#' contains information on California and 38 other (control/donor) states used
#' in Abadie et al. 2010's paper walking through the synthetic control method.
#' Covers the time range 1970 to 2000
#'
#' @docType data
#'
#' @usage data(smoking)
#'
#' @format A data frame with 1209 rows and 7 variables: \describe{
#'   \item{state}{name of U.S. state} \item{year}{year} \item{cigsale}{cigarette
#'   sales pack per 100,000 people} \item{lnincome}{log mean income}
#'   \item{beer}{beer sales per 100,000 people} \item{age15to24}{Proportion of
#'   the population between 15 and 24} \item{retprice}{Retail price of a box of
#'   cigarettes} }
#'
#' @keywords datasets
#'
#' @references Abadie, A., Diamond, A. and Hainmueller, J., 2010. Synthetic
#'   control methods for comparative case studies: Estimating the effect of
#'   California’s tobacco control program. Journal of the American statistical
#'   Association, 105(490), pp.493-505.
#'
#' @source https://economics.mit.edu/files/11859
#'
'smoking'




