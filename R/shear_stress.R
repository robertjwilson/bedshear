
# to do: give the name of flow type, not the number

#' @title Bed shear stress calculation
#' @description Calculates the combined bed shear stress from waves and tides
#' @param bathymetry Bathymetry of location
#' @param D50 Median grain size (mm).
#' @param tidal_velocity A numeric vector of tidal velocity (m/s). If not given, this is set to zero.
#' @param tidal_direction A numeric vector of tidal directions (degrees). If not given, this is set to zero.
#' @param wave_height Significant wave height (m). If not given, this is set to zero.
#' @param wave_period Wave period. If not given, this is set to zero.
#' @param wave_direction Wave direction. If not given, this is set to zero.
#' @param switch Switching term. Defaults to 0.
#'
#' @return Data frame with mean shear stress, maximum shear stress, root mean square shear stress, tidal current only shear stress (if tides supplied),
#' wave-only shear stress (if waves supplied), Shields number, Critical Shields,  Flow type.
#' @export


#' @examples
#'
#' # Calculating a time series of bed shear stress at Stonehaven
#' library(bedshear)
#' library(dplyr)
#' library(ggplot2)
#'
#' stonehaven_stress <- shear_stress(
#'   bathymetry = 40, 0.0002, tidal_velocity = stonehaven_ts$tidal_velocity, tidal_direction = stonehaven_ts$tidal_direction, wave_height = stonehaven_ts$wave_height,
#'   wave_period = stonehaven_ts$wave_period, wave_direction = stonehaven_ts$wave_direction, switch = 0
#' )
#'
#' stonehaven_ts %>%
#'   mutate(Stress = stonehaven_stress$shearmax) %>%
#'   ggplot(aes(Date, Stress)) +
#'   geom_line()
shear_stress <- function(bathymetry = NULL, D50 = NULL, tidal_velocity = NULL, tidal_direction = NULL,
                         wave_height = NULL, wave_period = NULL, wave_direction = NULL, switch = 0) {

  # step one. Check the inputs
  if (!is.numeric(bathymetry)) {
    stop("error: bathymetry is not numeric")
  }
  if (length(bathymetry) != 1) {
    stop("error: please supply a single value for bathymetry")
  }

  if (!is.numeric(D50)) {
    stop("error: D50 is not numeric")
  }
  if (length(D50) != 1) {
    stop("error: please supply a single value for D50")
  }

  if (switch != 0 & switch != 1) {
    stop("error: switch can only be 0 or 1")
  }


  # check the inputs are all the same length

  if (length(tidal_velocity) != length(tidal_direction)) {
    stop("error: wave and tidal inputs do not have the same length")
  }
  if (length(wave_period) != length(wave_height)) {
    stop("error: wave and tidal inputs do not have the same length")
  }
  if (length(wave_period) != length(wave_direction)) {
    stop("error: wave and tidal inputs do not have the same length")
  }

  # Now, if there is no wave or tide input, we need to set them to zero

  tide_output <- TRUE
  # First, tides
  if (length(tidal_velocity) == 0) {
    tide_output <- FALSE
    tidal_velocity <- rep(0, length(wave_height))
    tidal_direction <- rep(0, length(wave_height))
  }
  # Second, waves

  wave_output <- TRUE

  if (length(wave_height) == 0) {
    wave_output <- FALSE
    wave_height <- rep(0, length(tidal_direction))
    wave_period <- rep(0, length(tidal_direction))
    wave_direction <- rep(0, length(tidal_direction))
  }

  if (length(tidal_direction) != length(wave_height)) {
    stop("error: wave and tidal inputs do not have the same length")
  }

  # check wave and tidal inputs are vectors
  if (!is.numeric(tidal_velocity) + !is.numeric(tidal_direction) + !is.numeric(wave_height) + !is.numeric(wave_period) + !is.numeric(wave_direction)) {
    stop("error: make sure wave and tidal inputs are all vectors")
  }


  result <- stress(
    h = bathymetry, D50 = D50, mu_V = tidal_velocity, phic_V = tidal_direction,
    Hs_V = wave_height, Px_V = wave_period, switch1 = switch, phiw_V = wave_direction
  )

  result <- matrix(unlist(result), ncol = 8, byrow = TRUE)
  result <- as.data.frame(result)

names(result) <- c("shear_current_only", "shear_wave_only", "shear_mean", "shear_max", "shear_rms", "flow_type", "shields_number", "shields_critical")

# reorder them by importance
result <- dplyr::select(result, "shear_mean",
									 "shear_max",
									 "shear_rms",
										"shear_current_only",
									 "shear_wave_only",
									 "shields_number",
									 "shields_critical",
									 "flow_type")




  if (wave_output == FALSE) {
    result <- dplyr::select(result, -shear_wave_only)
  }


  if (tide_output == FALSE) {
    result <- dplyr::select(result, -shear_current_only)
  }

	# convert the flow type to a name

	result$flow_type <- ifelse(result$flow_type == 1, "laminar", result$flow_type)
	result$flow_type <- ifelse(result$flow_type == 2, "turbulent_smooth", result$flow_type)
	result$flow_type <- ifelse(result$flow_type == 3, "turbulent_rough", result$flow_type)


  result
}
