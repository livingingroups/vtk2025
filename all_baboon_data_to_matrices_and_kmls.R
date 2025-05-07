#This script reads in movebank data and converts the data to time-syncrhonized matrix format, then also output kmls for each month

#Important note:
#I connverted all timestamps to the same time line by using 5 min interavls. The way I did this is probably suboptimal but was trying to be simple.
#I just floored all data points to the nearest 5 min interval and then used the first value when duplicates existed.

library(cocomo)
library(lubridate)

#-----PARAMS-----

#path to file containing all gps data from movebank
gpsdat_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya.csv'

#path to file containing metadata from movebank
metadata_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya-reference-data.csv'

#path to output file where level0 data for all baboons will be stored
level0_file_path <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/mpala_baboons_all_5min.RData'

#path to output folder where kmls will be stored
kmls_folder_path <- '~/Dropbox/Teaching/VTK_2025/KMLs_5min/'

#resolution of gps for the time series (in seconds) - default 300
res <- 300

#cutoff mass above which to consider a female an adult (for icon colors in visualization only)
cutoff_mass_fem <- 11
cutoff_mass_male <- 22

#----MAIN----

#load GPS data and metadata
print('loading data from csvs')
alldat <- read.csv(gpsdat_file, header = T, stringsAsFactors = F)
metadata <- read.csv(metadata_file, header = T, stringsAsFactors = F, sep = ',')

#Change group names from old names to new color names
metadata$animal.group.id[which(metadata$animal.group.id=='Clifford')] <- 'Copper'
metadata$animal.group.id[which(metadata$animal.group.id=='Campsite')] <- 'Chartreuse'
metadata$animal.group.id[which(metadata$animal.group.id=='EagleScout')] <- 'Emerald'
metadata$animal.group.id[which(metadata$animal.group.id=='Leikiji')] <- 'Lilac'
metadata$animal.group.id[which(metadata$animal.group.id=='WestMukenya')] <- 'Magenta'
metadata$animal.group.id[which(metadata$animal.group.id=='Mlimafisi')] <- 'Maroon'

#Processing data
print('processing data and generating timestamps')

#exclude individuals not in metadata (undeployed collars)
dat <- alldat[which(alldat$tag.local.identifier %in% metadata$tag.id),]
rm('alldat')

#convert timestamps to POSIX
dat$timestamp <- as.POSIXct(dat$timestamp, tz = 'UTC')

#find start and end time across all data
start_time <- min(dat$timestamp)
end_time <- max(dat$timestamp)

#round start time to previous hour mark and end time to subsequent hour mark
start_time <- lubridate::floor_date(start_time, unit = 'hours')
end_time <- lubridate::ceiling_date(end_time, unit = 'hours')

# create a timeline going from start to end in segments of 5 minutes
timestamps <- seq.POSIXt(from = start_time, to = end_time, by = '5 min', tz = 'UTC')

#floor timestamps to the previous 5 min, keep first entry
dat$timestamp_floor <- lubridate::floor_date(dat$timestamp, unit = '5 mins')
dat$id_time <- paste0(dat$tag.local.identifier, '_', dat$timestamp_floor)
dat <- dat[!duplicated(dat$id_time),]

#convert data to level 0 matrix form
print('converting data to matrix form')
movebank_dat <- dat[,c('timestamp_floor','tag.local.identifier','location.long','location.lat')]

#note that we use the tag.local.identifier instead of the individual.local.identifier for id code
colnames(movebank_dat) <- c('timestamp','individual.local.identifier','location.long','location.lat')
data_chunks <- data.frame(start = start_time, end = end_time)
cocomo::reformat_movebank_to_matrix(movebank_data = movebank_dat, output_file_path = level0_file_path, utm_zone = 37, hemisphere = 'north', data_chunks = data_chunks, seconds_per_time_step = res)

#add metadata to ids saved with level0 data - resave
load(level0_file_path)
ids$group <- metadata$animal.group.id[match(ids$code, metadata$tag.id)]
ids$sex <- metadata$animal.sex[match(ids$code, metadata$tag.id)]
ids$mass <- metadata$animal.mass[match(ids$code, metadata$tag.id)]

#add colors to ids (for plotting)
#color_kml = weird kml hex format - OOBBGGRR (O = opacity, B = blue, G = green, R = red)
#color_hex = normal R hext format #RRGGBBOO

ids$color_kml <- ''
ids$color_hex <- ''
ids$color_kml[which(ids$group == 'Lilac')] <- 'ffffafd2'
ids$color_hex[which(ids$group == 'Lilac')] <- '#D2AFFF'
ids$color_kml[which(ids$group == 'Copper')] <- 'ff4f8cce'
ids$color_hex[which(ids$group == 'Copper')] <- '#ce8c4f'
ids$color_kml[which(ids$group == 'Chartreuse')] <- 'ff00ff7f'
ids$color_hex[which(ids$group == 'Chartreuse')] <- '#7FFF00'
ids$color_kml[which(ids$group == 'Emerald')] <- 'ff438705'
ids$color_hex[which(ids$group == 'Emerald')] <- '#058743'
ids$color_kml[which(ids$group == 'Magenta')] <- 'ffff00ff'
ids$color_hex[which(ids$group == 'Magenta')] <- '#FF00FF'
ids$color_kml[which(ids$group == 'Maroon')] <- 'ff000088'
ids$color_hex[which(ids$group == 'Maroon')] <- '#800000'
ids$color_hex[which(ids$group == 'Pylon')] <- '#FFE5B4' #peach
ids$color_kml[which(ids$group == 'Pylon')] <- 'ffb4e5ff'
ids$color_hex[which(ids$group == 'LizardRock')] <- '#26619c' #lazuli
ids$color_kml[which(ids$group == 'LizardRock')] <- 'ff9c6126'
ids$color_hex[which(ids$group == 'LizardRock2')] <- '#bad4ee' #lazuli (lighter)
ids$color_kml[which(ids$group == 'LizardRock2')] <- 'ffeed4ba'
ids$color_hex[which(ids$group == 'Cliff')] <- '#00FFFF' #cyan
ids$color_kml[which(ids$group == 'Cliff')] <- 'ffffff00'
save(file = level0_file_path, list = c('xs','ys','lats','lons','timestamps','ids'))

#Generating KMLs

print('generating kmls')
#create kmls - one kml per month
load(level0_file_path)

#get start and end times
start_date <- lubridate::ceiling_date(timestamps[1],'months')
end_date <- lubridate::floor_date(timestamps[length(timestamps)],'months')
start_time_kmls <- seq.POSIXt(start_date, end_date, by = 'month')
t0s <- match(start_time_kmls, timestamps)
t0s <- c(t0s, length(timestamps))

#get icons for males, females, subadults
icons <- rep('https://maps.google.com/mapfiles/kml/paddle/blu-circle-lv.png', nrow(ids)) #adult males are blue with a dot
icons[which(ids$sex == 'f' & ids$mass >= cutoff_mass_fem)] <- 'https://maps.google.com/mapfiles/kml/paddle/ylw-circle-lv.png' #adult females are yellow with a dot
icons[which(ids$sex == 'f' & ids$mass < cutoff_mass_fem)] <- 'https://maps.google.com/mapfiles/kml/paddle/ylw-blank-lv.png' #subadult females are yellow with no dot
icons[which(ids$sex == 'm' & ids$mass < cutoff_mass_male)] <- 'https://maps.google.com/mapfiles/kml/paddle/blu-blank-lv.png' #subadult males are blue with no dot


for(i in 1:(length(t0s)-1)){
  filename <- paste0('allbabs_',substr(as.character(timestamps[t0s[i]]),1,7),'.kml')
  cocomo::create_trajectories_kml(lons, lats, timestamps, ids$code, t0 = t0s[i], tf = t0s[i+1]-1, output_file_path = paste0(kmls_folder_path,filename), cols = ids$color_kml, icons = icons)
}




