
library(bedshear)
library(dplyr)
library(ggplot2)

stonehaven_ts

stonehaven_stress <- shields_bedshear(Depth,0.0002, sample_data$tidal_velocity, sample_data$tidal_direction,sample_data$wave_height,
															 sample_data$wave_period , switch = 0, sample_data$wave_direction )

stonehaven_ts %>%
	mutate(Stress = new_result$shearmax) %>%
	ggplot(aes(Date, Stress))+
	geom_line()
