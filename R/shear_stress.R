

#' @title Bed shear stress calculation
#' @description Calculates the combined bed shear stress from waves and tides
#' @param bathymetry Bathymetry of location
#' @param D50 Median grain size (mm).
#' @param tidal_velocity A numeric vector of tidal velocity (m/s).
#' @param tidal_direction A numeric vector of tidal directions (degrees).
#' @param wave_height Significant wave height (m).
#' @param wave_period Wave period.
#' @param wave_direction Wave direction.
#' @param switch Switching term. Defaults to 0.
#'
#' @return data frame with bed shear stress etc.
#' @export


#' @examples

#' # Calculating cell areas for NOAA world ocean atlas data sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_cellareas(ff)
#'

#' # Calculating a time series of bed shear stress at Stonehaven
#' library(bedshear)
#' library(dplyr)
#' library(ggplot2)

#' stonehaven_stress <- shields_bedshear(Depth,0.0002, sample_data$tidal_velocity, sample_data$tidal_direction,sample_data$wave_height,
#'																			sample_data$wave_period , switch = 0, sample_data$wave_direction )

#' stonehaven_ts %>%
#' 	mutate(Stress = new_result$shearmax) %>%
#' 	ggplot(aes(Date, Stress))+
#' 	geom_line()


shields_bedshear <- function (bathymetry = NULL, D50 = NULL, tidal_velocity = NULL, tidal_direction = NULL,
															wave_height = NULL, wave_period = NULL, wave_direction = NULL, switch = 0){

	# step one. Check the inputs
	if(!is.numeric(bathymetry))
		stop("error: bathymetry is not numeric")
	if(length(bathymetry) != 1)
		stop("error: please supply a single value for bathymetry")

	if(!is.numeric(D50))
		stop("error: D50 is not numeric")
	if(length(D50) != 1)
		stop("error: please supply a single value for D50")

	if(switch != 0 & switch != 1)
		stop("error: switch can only be 0 or 1")

	# check wave and tidal inputs are vectors
	if(!is.numeric(tidal_velocity) + !is.numeric(tidal_direction) + !is.numeric(wave_height) + !is.numeric(wave_period) + !is.numeric(wave_direction))
		stop("error: make sure wave and tidal inputs are all vectors")

	# check the inputs are all the same length

	if(length(tidal_velocity) != length(tidal_direction))
		stop("error: wave and tidal inputs do not have the same length")
	if(length(tidal_direction) != length(wave_height))
		stop("error: wave and tidal inputs do not have the same length")
	if(length(wave_period) != length(wave_height))
		stop("error: wave and tidal inputs do not have the same length")
	if(length(wave_period) != length(wave_direction))
		stop("error: wave and tidal inputs do not have the same length")

	result <- stress(h = bathymetry, D50 = D50, mu_V = tidal_velocity, phic_V = tidal_direction,
												Hs_V = wave_height, Px_V = wave_period, switch1 = switch, phiw_V = wave_direction)

	result <- matrix(unlist(result), ncol = 8, byrow = TRUE)
	result <- as.data.frame(result)

	names(result) <- c("shearcurrent", "shearwave", "shearmean", "shearmax", "trms", "flowtype", "shields", "shieldscrit")
	result


}


