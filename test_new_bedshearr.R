#

# This only needs to be sourced once
datadir= "~/Dropbox/RCpp/"
setwd(datadir)

require(magrittr)
require(tidyverse)
library(bedshear)

## First get the times etc. for all files
times <- readr::read_table(paste(datadir,"mikesedout.dat",sep=""), col_types = readr::cols(`hh:mm:ss`  = "c")) %>%
  dplyr::rename(hh.mm.ss = `hh:mm:ss`, U.m.2.s. = `U(m^2/s)`,V.m.2.s. = `V(m^2/s)`,   u.cm.s. = `u(cm/s)`,  v.cm.s. =  `v(cm/s)`, Depth.m. = `Depth(m)`) %>%
  dplyr::mutate(Date = lubridate::mdy(mm.dd.yyyy))

times$minute<-as.numeric(substr(times$hh.mm.ss,4,5))
times$hour<-as.numeric(substr(times$hh.mm.ss,1,2))
times$day<-as.numeric(substr(times$mm.dd.yyyy,4,5))
times$month<-as.numeric(substr(times$mm.dd.yyyy,1,2))

# Get rid off this later once the things are being looped through
times = times %>%
  dplyr::mutate(daynum = lubridate::yday(Date), day = lubridate::day(Date), month = lubridate::month(Date)) %>%
  dplyr::select(-Date)

times$decimalday<-(times$daynum-1)+(times$hour/24)+( (times$minute/60) /24 )

time <- times %>%
  dplyr::select(daynum, decimalday)


# Source the disturbance stuff

##source("Disturbance_Functions.R")               ## Original version.
ptm <- proc.time()

datadir= "~/Dropbox/RCpp/"
# require(rbenchmark)
# benchmark(tidedata <- readr::read_table(paste(datadir,"mikesedout.dat",sep=""), col_types = cols(`hh:mm:ss`  = "c")))
#
#
#
tidedata <- data.table::fread(paste(datadir,"mikesedout.dat",sep=""), nrows = 35040,
                              select = c("U(m^2/s)", "V(m^2/s)", "u(cm/s)", "v(cm/s)", "Depth(m)") , header = TRUE) %>%
  dplyr::bind_cols(times)

Depth <- tidedata$Depth.m.[1]

tidedata <- tidedata %>%
  dplyr::select(-Depth.m.)
#
# # benchmark(tidedata <- readr::read_table(paste(datadir,"mikesedout.dat",sep=""), col_types = cols(`hh:mm:ss`  = "c")),tidedata <- readr::read_table(paste(datadir,"mikesedout.dat",sep=""), col_types = cols(Lat = "d", Lon = "d", `U(m^2/s)` = "d", `V(m^2/s)` = "d",  `u(cm/s)` = "d", `v(cm/s)` = "d", `Depth(m)` = "d", mm.dd.yyyy = "c", `hh:mm:ss`  = "c"), n_max = 35040))
# #
#
#
# tidedata <- readr::read_table(paste(datadir,"mikesedout.dat",sep=""), col_types = cols(`hh:mm:ss`  = "c")) %>%
#   rename(hh.mm.ss = `hh:mm:ss`, U.m.2.s. = `U(m^2/s)`,V.m.2.s. = `V(m^2/s)`,   u.cm.s. = `u(cm/s)`,  v.cm.s. =  `v(cm/s)`, Depth.m. = `Depth(m)`) %>%
#   mutate(Date = lubridate::mdy(mm.dd.yyyy))

# 15 min interval current data
# tidedata<-read.table(paste(datadir,"mikesedout.dat",sep=""),header=TRUE)

#6h interval wave data
wavedata<-read.table(paste(datadir,"wave.dat",sep=""),header=TRUE)

#
# tidedata$minute<-as.numeric(substr(tidedata$hh.mm.ss,4,5))
# tidedata$hour<-as.numeric(substr(tidedata$hh.mm.ss,1,2))
# tidedata$day<-as.numeric(substr(tidedata$mm.dd.yyyy,4,5))
# tidedata$month<-as.numeric(substr(tidedata$mm.dd.yyyy,1,2))
#
# # Get rid off this later once the things are being looped through
# tidedata = tidedata %>%
#             mutate(daynum = lubridate::yday(Date), day = lubridate::day(Date), month = lubridate::month(Date)) %>%
#             select(-Date)
#
# tidedata$decimalday<-(tidedata$daynum-1)+(tidedata$hour/24)+( (tidedata$minute/60) /24 )



#First job is to interpolate the wave data onto the current data timke intervals
#Assume the wave data are centred on the mid-point of each 6h interval
#so if j is the wave time index and i is the tide-time index,
# the tide-time index ij of each wave data value is given by:

wavedata$tide_time<-NA
for(j in 1:nrow(wavedata)){
wavedata$tide_time[j]<-(12+(j-1)*24)
}





waveheight <- approxfun(wavedata$tide_time, wavedata$Significant_Wave_Height, rule=2)
waveperiod <- approxfun(wavedata$tide_time, wavedata$Mean_Wave_Period, rule=2)
wavedirection <- approxfun(wavedata$tide_time, wavedata$Mean_Wave_Direction, rule=2)

tidedata$waveheight<-waveheight(seq(1,nrow(tidedata)))
tidedata$waveperiod<-waveperiod(seq(1,nrow(tidedata)))
tidedata$wavedirection<-wavedirection(seq(1,nrow(tidedata)))


tidedata$depthmeanspeed.cm.s<-sqrt( (tidedata$u.cm.s^2)+(tidedata$v.cm.s^2) )

zz<-(acos(tidedata$v.cm.s/tidedata$depthmeanspeed.cm.s))*180/3.1416

cc<-which(tidedata$u.cm.s<0)
zz[cc]<-(360-zz[cc])
tidedata$currentdirection<-zz

proc.time() - ptm





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ New version by RW


q1<- rep(1026.96, nrow(tidedata))                             # seawater density
q2<- rep(1.48e-3, nrow(tidedata))                               # dynamic viscosity
q4<- rep(0.0002, nrow(tidedata))                                # median grain size 200 microns
q5<- rep(2650, nrow(tidedata))                                  # sediment density
q10<- rep(0, nrow(tidedata))                                    # switch to set input as mean wave period

proc.time() - ptm



source("~/Dropbox/bedshear/R/shear_stress_original.R")

base_result <- shields_bedshear_original(q1,q2, Depth,q4,q5,tidedata$depthmeanspeed.cm.s/100,tidedata$currentdirection,tidedata$waveheight,
																						 tidedata$waveperiod ,q10,tidedata$wavedirection )
#
 base_result <- matrix(unlist(base_result), ncol = 8, byrow = TRUE)
 base_result <- as.data.frame(base_result)
 names(base_result) <- c("shearcurrent", "shearwave", "shearmean", "shearmax", "trms", "flowtype", "shields", "shieldscrit")

# names(result) <- c("shearcurrent", "shearwave", "shearmean", "shearmax", "missing", "flowtype", "shields", "shieldscrit")

# source("~/Dropbox/bedshearr/R/shear_stress_development.R")
# source("~/Dropbox/bedshearr/R/stress_rcpp.R")


 tidedata %>%
 	as_tibble() %>%
 	select(depthmeanspeed.cm.s, currentdirection, waveheight, waveperiod, wavedirection) %>%
 	mutate(depthmeanspeed.cm.s = depthmeanspeed.cm.s/100)

 sample_data <- tidedata %>%
 	as_tibble() %>%
 	select(depthmeanspeed.cm.s, currentdirection, waveheight, waveperiod, wavedirection) %>%
 	mutate(depthmeanspeed.cm.s = depthmeanspeed.cm.s/100) %>%
 	rename(tidal_velocity = depthmeanspeed.cm.s, tidal_direction = currentdirection) %>%
 	rename(wave_height = waveheight, wave_period = waveperiod) %>%
 	rename(wave_direction = wavedirection)


new_result <- shields_bedshear(Depth,0.0002, sample_data$tidal_velocity, sample_data$tidal_direction,sample_data$wave_height,
																						 sample_data$wave_period , switch = 0, sample_data$wave_direction )

all.equal(base_result, new_result)

