#' Wave and tidal data for a site near Stonehaven, Scotland
#'
#' A time series of wave and tidal data for a station near Stonehaven, Scotland.
#' Depth of this site is 38 metres and the D50 is 0.2 mm
#' Reference: https://www.sciencedirect.com/science/article/pii/S0964569116302587
#'
#' @format A data frame with 35040 rows and 8 variables
#' \describe{
#'   \item{tidal_velocity}{depth averaged tidal velocity, in m/s}
#'   \item{tidal_direction}{tidal direction}
#'   \item{wave_height}{significant wave height, in m}
#'   \item{wave_period}{wave period}
#'   \item{wave_direction}{wave direction}
#' }
#' @source \url{https://www.sciencedirect.com/science/article/pii/S0964569116302587}
"stonehaven_ts"
