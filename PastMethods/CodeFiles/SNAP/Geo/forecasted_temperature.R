### Retrieving SNAP data for specific coordinates

## Author: AM Willson

## First, download SNAP data product to local machine
## Using terminal: wget -r http://data.snap.uaf.edu/data/Base/AK_CAN_10min/projected/Projected_Monthly_and_Derived_Temperature_Products_10min_CMIP5_AR5/
## You can optionally specify a different working directory for the data 
## to download to. I am personally putting it on a hard drive because of large data volume
## Total download size: ~ 2.5 GB

## Data are downloaded in a series of zip files
## Zip files are specific to both the climate model and the
## scenario
## There are also zip files for the multi-model ensemble average, which will not be used

## Files are unzipped manually
## Using terminal: unzip file.zip -d folder_name/
## Saved each zip file in a different subdirectory
## Naming convention: model_rcp
## Total size with unzipped files: 4.65 GB

rm(list = ls())

# Set working directory
setwd('/Volumes/FileBackup/data.snap.uaf.edu/data/Base/AK_CAN_10min/projected/Projected_Monthly_and_Derived_Temperature_Products_10min_CMIP5_AR5/')

# Coordinates of interest
coords <- matrix(, nrow = 2, ncol = 3)
coords <- as.data.frame(coords)
colnames(coords) <- c('site', 'x', 'y')
coords$site <- c('yak', 'cord')
coords$y <- c(59.5091, 60.5438)
coords$x <- c(-139.6599, -145.72593)

# Convert to sf
points <- sf::st_as_sf(coords, coords = c('x', 'y'),
                       crs = 'EPSG:4326')
#### GFDL RCP4.5 ####

# Directory name
direct_name <- 'gfdl_rcp45/'

# All .tif files
file_names <- list.files(direct_name)

# Storage
yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

# Loop over all files
for(i in 1:length(file_names)){
  # Progress
  if(i %% 100 == 0) print(i)
  
  # File name to use for defining variables
  fn <- file_names[i]
  
  # RCP scenario
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  # Model
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  # Month & year
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  # If within our domain of interest, load data
  if(year < 2020 | year > 2100) next
  
  # Load data
  dat <- terra::rast(paste0(direct_name, fn))
  
  # Extract values at airports
  temp <- terra::extract(x = dat, y = points)
  
  # Add information about scenario, model, time, and site
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  # Convert to data frame
  temp <- as.data.frame(temp)
  
  # Save in matrices
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

# Format as data frames
yak_aat <- as.data.frame(yak_temp)
cord_aat <- as.data.frame(cord_temp)

# Add column names
colnames(yak_aat) <- colnames(cord_aat) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

# Remove extract column and drop NAs
yak_aat <- yak_aat |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_aat <- cord_aat |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

#### GFDL RCP6.0 ####

## Repeat for next subdirectory

direct_name <- 'gfdl_rcp60/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### GFDL RCP8.5 ####

## Repeat for next subdirectory

direct_name <- 'gfdl_rcp85/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### GISS RCP4.5 ####

## Repeat for next subdirectory

direct_name <- 'giss_rcp45/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### GISS RCP6.0 ####

## Repeat for next subdirectory

direct_name <- 'giss_rcp60/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### GISS RCP8.5 ####

## Repeat for next subdirectory

direct_name <- 'giss_rcp85/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### IPSL RCP4.5 ####

## Repeat for next subdirectory

direct_name <- 'ipsl_rcp45/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### IPSL RCP6.0 ####

## Repeat for next subdirectory

direct_name <- 'ipsl_rcp60/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### IPSL RCP8.5 ####

## Repeat for next subdirectory

direct_name <- 'ipsl_rcp85/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### MRI RCP4.5 ####

## Repeat for next subdirectory

direct_name <- 'mri_rcp45/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### MRI RCP6.0 ####

## Repeat for next subdirectory

direct_name <- 'mri_rcp60/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### MRI RCP8.5 ####

## Repeat for next subdirectory

direct_name <- 'mri_rcp85/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### NCAR RCP4.5 ####

## Repeat for next subdirectory

direct_name <- 'ncar_rcp45/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### NCAR RCP6.0 ####

## Repeat for next subdirectory

direct_name <- 'ncar_rcp60/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### NCAR RCP8.5 ####

## Repeat for next subdirectory

direct_name <- 'ncar_rcp85/'

file_names <- list.files(direct_name)

yak_temp <- matrix(, nrow = length(file_names), ncol = 7)
cord_temp <- matrix(, nrow = length(file_names), ncol = 7)

for(i in 1:length(file_names)){
  if(i %% 100 == 0) print(i)
  
  fn <- file_names[i]
  
  rcp <- sub(pattern = '.*rcp', replacement = '', x = fn)
  rcp <- sub(pattern = '_.*', replacement = '', x = rcp)
  
  model <- sub(pattern = '.*ar5_', replacement = '', x = fn)
  model <- sub(pattern = '_.*', replacement = '', x = model)
  
  time <- sub(pattern = '.*rcp', replacement = '', x = fn)
  month <- substring(text = time, first = 4, last = 5)
  year <- substring(text = time, first = 7, last = 10)
  
  if(year < 2020 | year > 2100) next
  
  dat <- terra::rast(paste0(direct_name, fn))
  
  temp <- terra::extract(x = dat, y = points)
  
  temp$rcp <- rcp
  temp$model <- model
  temp$month <- month
  temp$year <- year
  temp$site <- points$site
  
  temp <- as.data.frame(temp)
  
  yak_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'yak'))
  cord_temp[i,] <- as.matrix(dplyr::filter(temp, site == 'cord'))
}

yak_temp <- as.data.frame(yak_temp)
cord_temp <- as.data.frame(cord_temp)

colnames(yak_temp) <- colnames(cord_temp) <-
  c('ind', 'temperature', 'rcp', 'model', 'month', 'year', 'site')

yak_temp <- yak_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()
cord_temp <- cord_temp |>
  dplyr::select(-ind) |>
  tidyr::drop_na()

yak_aat <- rbind(yak_aat, yak_temp)
cord_aat <- rbind(cord_aat, cord_temp)

#### Formatting and plotting #####

aat <- rbind(yak_aat, cord_aat)

aat <- aat |>
  dplyr::mutate(temperature = as.numeric(temperature),
                date = paste0(month, '-', year),
                date = zoo::as.yearmon(date, '%m-%Y'))

# Monthly temperature

aat |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = temperature, color = model)) +
  ggplot2::facet_wrap(site~rcp) +
  ggplot2::theme_minimal()

# Annual summaries
aat |>
  dplyr::group_by(rcp, model, year, site) |>
  dplyr::summarize(aat = mean(temperature)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = aat, color = model)) +
  ggplot2::facet_wrap(site~rcp) +
  ggplot2::theme_minimal()

# Annual across models
aat |>
  dplyr::group_by(rcp, model, year, site) |>
  dplyr::summarize(aat = mean(temperature)) |>
  dplyr::ungroup() |>
  dplyr::group_by(rcp, year, site) |>
  dplyr::summarize(mean = mean(aat),
                   sd = sd(aat)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = mean, color = rcp)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = as.numeric(year), ymin = mean - sd, ymax = mean + sd, fill = rcp), alpha = 0.5) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal()

# refining the graph (aa)
plotFutureEnsemble <- aat |>
  dplyr::filter(rcp != "60") |>
  dplyr::group_by(rcp, model, year, site) |>
  dplyr::summarize(aat = mean(temperature), .groups = 'drop') |>
  dplyr::group_by(rcp, year, site) |>
  dplyr::summarize(mean = mean(aat),
                   sd = sd(aat), .groups = 'drop') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = mean, color = rcp)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = as.numeric(year), ymin = mean - sd, ymax = mean + sd, fill = rcp), alpha = 0.5) +
  ggplot2::facet_wrap(~site, labeller = ggplot2::labeller(site = c("yak" = "YF", "cord" = "CRD"))) +
  ggplot2::labs(x = "Year", y = "Mean Temp (C)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggsave("FutureRCP_5Ensemble.png", plot = plotFutureEnsemble, width = 8, height = 6)

# Save
save(aat, file = 'DataFiles/GeoData/forecasted_temperature.RData')


#trying to make this match the forecasted h20 temp plot
library(dplyr)
library(ggplot2)

# Define color palette for RCP scenarios
color_palette <- c("45" = "#6baed6",  # Soft Blue
                   "85" = "#d73027")  # Dark Red

# Create a mapping for legend labels
legend_labels <- c("45" = "RCP 4.5", "85" = "RCP 8.5")

# Plot
plotFutureEnsemble <- aat %>%
  dplyr::filter(rcp != "60") %>%
  dplyr::group_by(rcp, model, year, site) %>%
  dplyr::summarize(aat = mean(temperature), .groups = 'drop') %>%
  dplyr::group_by(rcp, year, site) %>%
  dplyr::summarize(mean = mean(aat), sd = sd(aat), .groups = 'drop') %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = mean, color = rcp, fill = rcp)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5) +
  ggplot2::scale_color_manual(values = color_palette, labels = legend_labels) +
  ggplot2::scale_fill_manual(values = color_palette, labels = legend_labels) +
  ggplot2::facet_wrap(~site, labeller = ggplot2::labeller(site = c("yak" = "YF", "cord" = "CRD"))) +
  ggplot2::labs(x = "Year", y = "Mean Temp (C)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.title = ggplot2::element_blank())

# Print the plot
print(plotFutureEnsemble)

# Matching this plot to the forecast output for the model

plotFutureEnsemble <- aat %>%
  dplyr::filter(rcp != "60") %>%
  dplyr::group_by(rcp, model, year, site) %>%
  dplyr::summarize(aat = mean(temperature), .groups = 'drop') %>%
  dplyr::group_by(rcp, year, site) %>%
  dplyr::summarize(mean = mean(aat), sd = sd(aat), .groups = 'drop') %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = mean, color = rcp, fill = rcp)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5) +
  ggplot2::scale_color_manual(values = color_palette, labels = legend_labels) +
  ggplot2::scale_fill_manual(values = color_palette, labels = legend_labels) +
  ggplot2::xlim(2020, 2100) +  # Set x-axis limits
  ggplot2::scale_y_continuous(limits = c(min(aat$temperature), max(aat$temperature))) +  # Adjust y-axis limits as needed
  ggplot2::facet_wrap(~site, labeller = ggplot2::labeller(site = c("yak" = "YF", "cord" = "CRD"))) +
  ggplot2::labs(x = "Year", y = "Air Temperature (C)") +
  ylim(2,12)+
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = element_text(size = 18),  # Rotate x-axis labels and set size
    axis.text.y = element_text(size = 18),  # Set y-axis tick labels size
    axis.title.x = element_text(size = 24),  # Set x-axis title size
    axis.title.y = element_text(size = 22),  # Set y-axis title size
    legend.position = c(0.6, 0.84),  # Position of legend
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 14),  # Increase legend text size
    strip.text = element_text(size = 18)  # Set facet label text size
  )
plotFutureEnsemble

ggsave(filename = "CombinedForecastNAP_plots.png",
       plot = plotFutureEnsemble,
       width = 14,  # Width in inches
       height = 6)  # Height in inches
file.path(getwd(), "CombinedForecastH20Temp_plots.png")
