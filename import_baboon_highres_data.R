#This script is for the 2025 VTK course.
#It reads in baboon data from a csv (in movebank format) as well as a metadata table,
#then extracts the high-res time window (3-5 UTC) for the groups Chartreuse and Lilac/Copper.
#For both groups, it then converts the data into matrix form, and finally outputs KML
#visualization files for each day, with individuals with high-res data (should be all males)
#colored in red and others colored in blue.

library(cocomo)
library(lubridate)

#set system time to UTC - just in case
Sys.setenv(TZ='UTC')

#----PARAMS----

#path to file containing all gps data from movebank
gpsdat_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya_recent.csv'

#path to file containing metadata from movebank
metadata_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya-reference-data.csv'

#path to folder where high res output files (matrix form) will be stored
highres_path <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/'

#path to file containing reference locations (bait piles, cages, etc.)
locs_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/reference_locations.csv'

#path to folder where KMLs will be stored
kmls_folder <- '~/Dropbox/Teaching/VTK_2025/KMLs_1s/'

#dates when we want highres data from target groups
highres_start_date <- '2025-04-27'
highres_end_date <- '2025-05-10'
highres_start_time <- '02:59:42' #in UTC not GPS time!
highres_end_time <- '04:59:41' #in UTC not GPS time!

utc_offset <- 18 #UTC offset from GPS time - needed to convert from GPS time to UTC because eObs tags record GPS time and not UTC

#----MAIN----

#load GPS data and metadata
print('loading data from csvs')
alldat <- read.csv(gpsdat_file, header = T, stringsAsFactors = F)
metadata <- read.csv(metadata_file, header = T, stringsAsFactors = F, sep = ',')

#get reference locations and convert to data frame
ref_locs <- read.csv(locs_file, stringsAsFactors = F, header = T, sep=';')
lat_degs <- as.numeric(unlist(sapply(ref_locs$Latitude, FUN = function(x){return(strsplit(x,'deg')[[1]][1])})))
lon_degs <- as.numeric(unlist(sapply(ref_locs$Longitude, FUN = function(x){return(strsplit(x,'deg')[[1]][1])})))
lat_mins <- unlist(sapply(ref_locs$Latitude, FUN = function(x){return(strsplit(x,'deg')[[1]][2])}))
lat_mins <- as.numeric(gsub('min','',lat_mins))
lon_mins <- unlist(sapply(ref_locs$Longitude, FUN = function(x){return(strsplit(x,'deg')[[1]][2])}))
lon_mins <- as.numeric(gsub('min','',lon_mins))
ref_locs$lon <- lon_degs + lon_mins / 60
ref_locs$lat <- lat_degs + lat_mins / 60
ref_locs$name <- ref_locs$Waypoint.name...Landmark.ID

#Processing data
print('processing data and generating timestamps')

#exclude individuals not in metadata (undeployed collars)
dat <- alldat[which(alldat$tag.local.identifier %in% metadata$tag.id),]
rm('alldat')

#convert timestamps to POSIX
dat$timestamp <- as.POSIXct(dat$timestamp, tz = 'UTC')

#convert to UTC
dat$timestamp <- dat$timestamp - utc_offset

#Change group names from old names to new color names
metadata$animal.group.id[which(metadata$animal.group.id=='Clifford')] <- 'Copper'
metadata$animal.group.id[which(metadata$animal.group.id=='Campsite')] <- 'Chartreuse'
metadata$animal.group.id[which(metadata$animal.group.id=='EagleScout')] <- 'Emerald'
metadata$animal.group.id[which(metadata$animal.group.id=='Leikiji')] <- 'Lilac'
metadata$animal.group.id[which(metadata$animal.group.id=='WestMukenya')] <- 'Magenta'
metadata$animal.group.id[which(metadata$animal.group.id=='Mlimafisi')] <- 'Maroon'

#GETTING HIGHRES 1 HZ DATA FOR STUDENT PROJECT

#Chartreuse
print('Generating data matrices')

#get data from Chartreuse
chart_ids <- metadata$tag.id[which(metadata$animal.group.id=='Chartreuse')]
dat_chart <- dat[which(dat$tag.local.identifier %in% chart_ids),]
dat_chart <- dat_chart[,c('timestamp','tag.local.identifier','location.long','location.lat')]
colnames(dat_chart) <- c('timestamp','individual.local.identifier','location.long','location.lat')

#Get data from Lilac / copper
lilcop_ids <- metadata$tag.id[which(metadata$animal.group.id %in% c('Lilac','Copper'))]
dat_lilcop <- dat[which(dat$tag.local.identifier %in% lilcop_ids),]
dat_lilcop <- dat_lilcop[,c('timestamp','tag.local.identifier','location.long','location.lat')]
colnames(dat_lilcop) <- c('timestamp','individual.local.identifier','location.long','location.lat')

#floor timestamps to nearest second
dat_chart$timestamp <- lubridate::floor_date(dat_chart$timestamp, unit = 'sec')
dat_lilcop$timestamp <- lubridate::floor_date(dat_lilcop$timestamp, unit = 'sec')

#create matrices and save output
cocomo::reformat_movebank_to_matrix(movebank_data = dat_chart, output_file_path = paste0(highres_path, 'chartreuse_xy_level0.RData'), seconds_per_time_step = 1,
                                    start_date = highres_start_date, end_date = highres_end_date,
                                    start_time = highres_start_time, end_time = highres_end_time, utm_zone = 37, hemisphere = 'north')

cocomo::reformat_movebank_to_matrix(movebank_data = dat_lilcop, output_file_path = paste0(highres_path, 'copperlilac_xy_level0.RData'), seconds_per_time_step = 1,
                                    start_date = highres_start_date, end_date = highres_end_date,
                                    start_time = highres_start_time, end_time = highres_end_time, utm_zone = 37, hemisphere = 'north')

#add additional metadata to ids  for the two groups

#chartreuse
load(paste0(highres_path, 'chartreuse_xy_level0.RData'))
ids$group <- metadata$animal.group.id[match(ids$code, metadata$tag.id)]
ids$sex <- metadata$animal.sex[match(ids$code, metadata$tag.id)]
ids$mass <- metadata$animal.mass[match(ids$code, metadata$tag.id)]
save(file= paste0(highres_path, 'chartreuse_xy_level0.RData'), list = c('timestamps','xs','ys','lats','lons','ids'))

#lilac and copper
load(paste0(highres_path, 'copperlilac_xy_level0.RData'))
ids$group <- metadata$animal.group.id[match(ids$code, metadata$tag.id)]
ids$sex <- metadata$animal.sex[match(ids$code, metadata$tag.id)]
ids$mass <- metadata$animal.mass[match(ids$code, metadata$tag.id)]
save(file= paste0(highres_path, 'copperlilac_xy_level0.RData'), list = c('timestamps','xs','ys','lats','lons','ids'))

#convert to level 1
load(paste0(highres_path, 'chartreuse_xy_level0.RData'))
cocomo::preprocess_gps_level0_to_level1(input_file_path = paste0(highres_path, 'chartreuse_xy_level0.RData'), output_file_path = paste0(highres_path, 'chartreuse_xy_level1.RData'))
cocomo::preprocess_gps_level0_to_level1(input_file_path = paste0(highres_path, 'copperlilac_xy_level0.RData'), output_file_path = paste0(highres_path, 'copperlilac_xy_level1.RData'))


#make kmls for each day
print('generating KMLs for each day')
group <- 'chartreuse'
dates <- seq.Date(as.Date(highres_start_date), as.Date(highres_end_date), by = 'day')
for(group in c('chartreuse','copperlilac')){
  for(i in 1:length(dates)){
    load(paste0(highres_path, group,'_xy_level0.RData'))
    t0 <- min(which(date(timestamps) == dates[i]))
    tf <- max(which(date(timestamps) == dates[i]))

    #color highres individuals a different color
    n_fixes_per_ind <- rowSums(!is.na(xs[,t0:tf]))
    highres_inds <- which(n_fixes_per_ind > 3000)

    cols <- rep('ff000000',nrow(xs))
    if(length(highres_inds)>0){
      cols[highres_inds] <- 'ff0000ff'
    }

    icons <- rep('https://maps.google.com/mapfiles/kml/paddle/wht-blank-lv.png', nrow(xs))
    if(length(highres_inds)>0){
      icons[highres_inds] <- 'https://maps.google.com/mapfiles/kml/paddle/blu-circle-lv.png'
    }

    cocomo::create_trajectories_kml(lons = lons, lats = lats, timestamps = timestamps, id_codes = ids$code, t0 = t0, tf = tf,
                                    output_file_path = paste0(kmls_folder, group,'_', dates[i],'.kml'),
                                    cols = cols, icons = icons, fixed_locs = ref_locs)

  }
}

