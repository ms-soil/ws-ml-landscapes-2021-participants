#######################################################
#
# Workshop: Machine Learning for (soil) parameter prediction in R - A practical introduction
# Date: Thursday, September 23, 2021
# Conference: Landscape 2021
# Repository: https://github.com/ms-soil/ws-ml-landscapes-2021 (private)
# Main code contributors: Marcus Schmidt (marcus.schmidt@zalf.de) and Masahiro Ryo (masahiro.ryo@zalf.de)
#
#######################################################


#### DATA FROM ISRIC (soilgrids.org) ON CC-BY license ####

#### NOTE ####
# see a similar, commented project at:
# https://github.com/ms-soil/ds-submission-soil/blob/main/analysis-report.pdf


#######################################################
# library and set-up
#######################################################

library(raster)
library(rgdal)
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest) 
library(vip)
library(pdp)
library(ggpubr)

# set up plot area
par(mfrow = c(2,2))
par(mar=c(2, 2, 2, 2))


#######################################################
# spatial data read and preprocessing
#######################################################

ocd <- raster("data/soilgrids-ocd-5-15-g-dm3.tif") # read in raster
range(values(ocd))

# correction of isric ocd:
values(ocd) <- values(ocd)/10

# plotting ocd
plot(ocd, main = "organic C density g/dm3") # simple plot
points(14.11, 52.52) # add ZALF as point
text(labels = "ZALF", x = 14.1, y = 52.4) # add label

crs(ocd)

ocd_vals <- values(ocd)

# shorter version - a function to get values and plot:
val_from_tif <- function(file_ending){
  plot(raster(paste0("data/soilgrids-", file_ending)), main = file_ending)
  points(14.11, 52.52) # add ZALF as point
  text(labels = "ZALF", x = 14.1, y = 52.4) # add label
  vals <- values(raster(paste0("data/soilgrids-", file_ending)))
  vals
}

a <- raster("data/soilgrids-bd-5-15-cg-cm3.tif")
b <- raster("data/soilgrids-soilgroup-wrb.tif")
a
b

unique(values(a))
unique(values(b))

c <- resample(b, a, method = "ngb")
unique(values(c))

bd_vals <- val_from_tif(file_ending = "bd-5-15-cg-cm3.tif")
cec_vals <- val_from_tif(file_ending = "cec-5-15-mmolc-kg.tif")
clay_vals <- val_from_tif(file_ending = "clay-5-15-g-kg.tif")
soil_types_nr <- values(c)

unique(soil_types_nr)

soil_types_name = case_when(
  soil_types_nr == 0 ~ "Acrisols",
  soil_types_nr == 1 ~ "Albeluvisols",
  soil_types_nr == 4 ~ "Arenosols",
  soil_types_nr == 5 ~ "Calcisols",
  soil_types_nr == 6 ~ "Cambisols",
  soil_types_nr == 7 ~ "Chernozems",
  soil_types_nr == 11 ~ "Fluvisols",
  soil_types_nr == 12 ~ "Gleysols",
  soil_types_nr == 14 ~ "Histosols",
  soil_types_nr == 15 ~ "Kastanozems",
  soil_types_nr == 16 ~ "Leptosols",
  soil_types_nr == 18 ~ "Luvisols",
  soil_types_nr == 20 ~ "Phaeozems",
  soil_types_nr == 23 ~ "Podzols",
  soil_types_nr == 24 ~ "Regosols",
  soil_types_nr == 27 ~ "Stagnosols",
  soil_types_nr == 29 ~ "Vertisols",
)

unique(sort(soil_types_name))




#######################################################
# Set up data frame
#######################################################

d <- data.frame(c = ocd_vals, bd = bd_vals, cec = cec_vals, clay = clay_vals,
                soil_nr = soil_types_nr, soil_name = soil_types_name)

as_tibble(d)

set.seed(1)
id_sample <- sample(1:nrow(d), 10000)
id_sample %>% head
d <- d[id_sample,]

plot(d)


# the zeros are kind of weird

# see where we have NAs
table(is.na(d$c)) # False means not NA
table(is.na(d$bd))
table(is.na(d$cec))
table(is.na(d$clay))

hist(d$c)
hist(d$bd)
hist(d$cec)
hist(d$clay)

# we need to use NA instead of the zeros
d$c[d$c == 0] <- NA
d$bd[d$bd == 0] <- NA
d$cec[d$cec == 0] <- NA
d$clay[d$clay == 0] <- NA

# KEEP IN MIND 0s:
# We will have to also set 0 to NA first for any set where we do the prediction

plot(d) # makes more sense now

# keep only observations where all parameters are given
d <- d %>% filter(!is.na(c) & !is.na(bd) & !is.na(cec) & !is.na(clay))
str(d)



save(d, file = "data/clean_data.rda")
rm(d)

#######################################################
# start here with clean data 
#######################################################


load("data/clean_data.rda")

