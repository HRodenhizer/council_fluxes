################################################################################
###                 Council Tower Flux Data Exploration                      ###
###                         Code by HGR 1/2023                               ###
################################################################################

### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(viridis)
library(tidyverse)
################################################################################

### Load Data ##################################################################
# Info about variables and units:
# https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/
council_hh <- fread('data/AMF_US-NGC_BASE_HH_2-5.csv',
                    na.strings = c(-9999, 'NA'))
council_hh[, ':=' (TIMESTAMP_START = ymd_hm(TIMESTAMP_START),
                   TIMESTAMP_END = ymd_hm(TIMESTAMP_END))]

# soil sensors
filenames <- list.files('/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/soil_temp_moisture/raw',
                        pattern = '^C',
                        full.names = TRUE)


### Figure out the issue with some of the csv files (something with quoted column names, I think)
import_soil_data <- function(filename) {
  short_name <- str_split(filename, '/')[[1]]
  short_name <- short_name[length(short_name)]
  print(paste('Reading in', short_name))
  
  # Get name of sensor_name
  sensor_name <- as.character(fread(filename, skip = 'area_code:', nrows = 1, header = FALSE)[, .(V2)])
  # remove trailing underscores from sensor_name
  if (str_sub(sensor_name, str_length(sensor_name)) == '_') {
    sensor_name <- str_sub(sensor_name, 1, str_length(sensor_name) - 1)
  }
  # remove quotes from sensor_name
  if (str_sub(sensor_name, 1, 1) == ' ') {
    sensor_name <- str_sub(sensor_name, 4, str_length(sensor_name) - 2)
  }
  print(sensor_name)
  data <- fread(filename, 
                skip = 'Date', 
                select = seq(1, 9))
  # data <- melt(data,
  #              id.vars = c('Date'),
  #              measure.vars = patterns(tsoil = '^Tsoil.+',
  #                                      vwc = '^VWC.+'),
  #              variable.name = 'depth')
  # data[, sensor_name := sensor_name]
  # data <- data[, .(sensor_name, date = Date, depth, tsoil, vwc)]
  return(data)
}

soil_sensor <- map(filenames,
                   ~ import_soil_data(.x))
################################################################################

### Plot Meteorological Data ###################################################
# Air Temp + Soil Temp + Precip
ggplot(council_d[NEE_VUT_REF_QC != 0],
       aes(x = TIMESTAMP)) +
  geom_line(aes(y = P_F, color = 'Precip'), 
            linetype = 'dashed',
            linewidth = 0.25) +
  geom_line(aes(y = TA_F_MDS, color = 'Air Temp')) +
  geom_line(aes(y = TS_F_MDS_1, color = 'Soil Temp')) +
  scale_y_continuous(name = 'Temperature (C)',
                     sec.axis = sec_axis(trans = ~ .x,
                                         name = 'Precip (mm)')) +
  scale_color_manual(name = '',
                     values = c('black', 'blue', 'brown'))

# Zoom in on each year
# 2017
ggplot(council_d[NEE_VUT_REF_QC != 0 & year(TIMESTAMP) == 2017],
       aes(x = TIMESTAMP)) +
  geom_line(aes(y = P_F, color = 'Precip'), 
            linetype = 'dashed',
            linewidth = 0.25) +
  geom_line(aes(y = TA_F_MDS, color = 'Air Temp')) +
  geom_line(aes(y = TS_F_MDS_1, color = 'Soil Temp')) +
  scale_y_continuous(name = 'Temperature (C)',
                     sec.axis = sec_axis(trans = ~ .x,
                                         name = 'Precip (mm)')) +
  scale_color_manual(name = '',
                     values = c('black', 'blue', 'brown'))

# 2018
ggplot(council_d[NEE_VUT_REF_QC != 0 & year(TIMESTAMP) == 2018],
       aes(x = TIMESTAMP)) +
  geom_line(aes(y = P_F, color = 'Precip'), 
            linetype = 'dashed',
            linewidth = 0.25) +
  geom_line(aes(y = TA_F_MDS, color = 'Air Temp')) +
  geom_line(aes(y = TS_F_MDS_1, color = 'Soil Temp')) +
  scale_y_continuous(name = 'Temperature (C)',
                     sec.axis = sec_axis(trans = ~ .x,
                                         name = 'Precip (mm)')) +
  scale_color_manual(name = '',
                     values = c('black', 'blue', 'brown'))

# 2019
ggplot(council_d[NEE_VUT_REF_QC != 0 & year(TIMESTAMP) == 2019],
       aes(x = TIMESTAMP)) +
  geom_line(aes(y = P_F, color = 'Precip'), 
            linetype = 'dashed',
            linewidth = 0.25) +
  geom_line(aes(y = TA_F_MDS, color = 'Air Temp')) +
  geom_line(aes(y = TS_F_MDS_1, color = 'Soil Temp')) +
  scale_y_continuous(name = 'Temperature (C)',
                     sec.axis = sec_axis(trans = ~ .x,
                                         name = 'Precip (mm)')) +
  scale_color_manual(name = '',
                     values = c('black', 'blue', 'brown'))


# Air Pressure
ggplot(council_d[NEE_VUT_REF_QC != 0],
       aes(x = TIMESTAMP, y = PA_F)) +
  geom_line()

# PAR
ggplot(council_d[NEE_VUT_REF_QC != 0],
       aes(x = TIMESTAMP, y = PPFD_IN)) +
  geom_line()

# VPD
ggplot(council_d[NEE_VUT_REF_QC != 0],
       aes(x = TIMESTAMP, y = VPD_F_MDS)) +
  geom_line()


################################################################################

### Plot Half Hourly Data ######################################################
# Wind speed by wind direction
ggplot(council_hh, 
       aes(x = WD, y = WS)) +
  geom_point()

### NEE - units umol C m-2 s-1
ggplot(council_hh, 
       aes(x = TIMESTAMP_START, y = FC)) +
  geom_point()

# Air temp
ggplot(council_hh, 
       aes(x = TA, y = FC)) +
  geom_point()

# Wind direction
ggplot(council_hh, 
       aes(x = WD, y = FC,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

ggplot(council_hh, 
       aes(x = WD, y = FC)) +
  geom_point() +
  scale_color_viridis() +
  facet_wrap(~ month(TIMESTAMP_START))

# Wind speed
ggplot(council_hh, 
       aes(x = WS, y = FC,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

ggplot(council_hh, 
       aes(x = WS, y = FC)) +
  geom_point() +
  scale_color_viridis() +
  facet_wrap(~ month(TIMESTAMP_START))

### Reco
## Daytime method (Lasslop et al. 2010)
# Variable u* threshold
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = RECO_DT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()

ggplot(council_hh[NEE_VUT_USTAR50_QC != 3], 
       aes(x = TIMESTAMP_START, y = RECO_DT_VUT_USTAR50,
           color = NEE_VUT_USTAR50_QC)) +
  geom_point()

# Wind direction
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WD, y = RECO_DT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Wind speed
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WS, y = RECO_DT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

## Nighttime method (Reichstein et al. 2005)
# Variable u* threshold
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = RECO_NT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()

ggplot(council_hh[NEE_VUT_USTAR50_QC != 3], 
       aes(x = TIMESTAMP_START, y = RECO_NT_VUT_USTAR50,
           color = NEE_VUT_USTAR50_QC)) +
  geom_point()

# Wind direction
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WD, y = RECO_NT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Wind speed
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WS, y = RECO_NT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Difference between DT and NT methods
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = RECO_NT_VUT_REF - RECO_DT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()

### GPP
## Daytime method (Lasslop et al. 2010)
# Variable u* threshold
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = GPP_DT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()

ggplot(council_hh[NEE_VUT_USTAR50_QC != 3], 
       aes(x = TIMESTAMP_START, y = GPP_DT_VUT_USTAR50,
           color = NEE_VUT_USTAR50_QC)) +
  geom_point()

# Wind direction
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WD, y = GPP_DT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Wind speed
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WS, y = GPP_DT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

## Nighttime method (Reichstein et al. 2005)
# Variable u* threshold
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()

ggplot(council_hh[NEE_VUT_USTAR50_QC != 3], 
       aes(x = TIMESTAMP_START, y = GPP_NT_VUT_USTAR50,
           color = NEE_VUT_USTAR50_QC)) +
  geom_point()

# Wind direction
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WD, y = GPP_NT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Wind speed
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = WS, y = GPP_NT_VUT_REF,
           color = month(TIMESTAMP_START))) +
  geom_point() +
  scale_color_viridis()

# Difference between DT and NT methods
ggplot(council_hh[NEE_VUT_REF_QC != 3], 
       aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF - GPP_DT_VUT_REF, 
           color = NEE_VUT_REF_QC)) +
  geom_point()


################################################################################

### Plot Daily Data ############################################################
# NEE - units gC m-2 d-1
ggplot(council_d[NEE_VUT_REF_QC != 0], 
       aes(x = TIMESTAMP,
           color = TA_F_MDS)) +
  geom_line(aes(y = NEE_VUT_REF, color = 'NEE')) +
  geom_line(aes(y = RECO_DT_VUT_REF, color = 'Reco')) +
  geom_line(aes(y = GPP_DT_VUT_REF*-1, color = 'GPP')) +
  scale_y_continuous(name = 'Flux (gC m-2 d-1)') +
  scale_color_manual(name = 'Flux',
                     values = c('green', 'black', 'brown'))
################################################################################

### Plot Annual Data ###########################################################
# these are missing considerable data - sums are not accurate!
ggplot(council_y, 
       aes(x = TIMESTAMP,
           color = TA_F_MDS)) +
  geom_point(aes(y = NEE_VUT_REF, color = 'NEE')) +
  geom_point(aes(y = RECO_DT_VUT_REF, color = 'Reco')) +
  geom_point(aes(y = GPP_DT_VUT_REF*-1, color = 'GPP')) +
  scale_y_continuous(name = 'Flux (gC m-2 yr-1)') +
  scale_color_manual(name = 'Flux',
                     values = c('green', 'black', 'brown'))

################################################################################