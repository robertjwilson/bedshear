

#' @title Orbital velocitycalculation
#' @description Calculates the orbital velocity
#' @param bathymetry Bathymetry of location
#' @param wave_height Significant wave height (m).
#' @param wave_period Wave period.
#' @param switch Switching term. Defaults to 0.
#'
#' @return numeric vector of wave orbital velocities
#' @export

#' @examples
#' 
#' # Calculating a time series of bed shear stress at Stonehaven
#' library(bedshear)
#' library(dplyr)
#' library(ggplot2)
#' 
#' 
#' # Calculating a time series of wave orbital velocities at Stonehaven
#' 
#' stonehaven_orbital <- orbital_velocity(bathymetry = 40, stonehaven_ts$wave_height, stonehaven_ts$wave_period, switch = 0)
#' 
#' stonehaven_ts %>%
#'   mutate(Velocity = stonehaven_orbital) %>%
#'   ggplot(aes(Date, Velocity)) +
#'   geom_line()
orbital_velocity <- function(bathymetry = NULL, wave_height = NULL, wave_period = NULL, switch = 0) {

  # step one. Check the inputs
  if (!is.numeric(bathymetry)) {
    stop("error: bathymetry is not numeric")
  }
  if (length(bathymetry) != 1) {
    stop("error: please supply a single value for bathymetry")
  }

  if (switch != 0 & switch != 1) {
    stop("error: switch can only be 0 or 1")
  }

  # check wave inputs are vectors
  if (!is.numeric(wave_height) + !is.numeric(wave_period)) {
    stop("error: make sure wave inputs are all vectors")
  }

  # check the inputs are all the same length

  if (length(wave_period) != length(wave_height)) {
    stop("error: wave inputs do not have the same length")
  }

  as.numeric(velocity(h = bathymetry, Hs_V = wave_height, Px_V = wave_period, switch1 = switch))
}
