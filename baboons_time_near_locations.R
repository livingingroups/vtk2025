#Get a periods_of_proximity table of times periods where each tracked individual is within a certain 'radius' of each fixed location

library(cocomo)

#---PARAMS---

#path to directory where your GPS data (matrix format!) is stored
datadir <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/'

#path to reference locations file (csv)
locs_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/reference_locations.csv'

#path to metadata file
metadata_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya-reference-data.csv'

#path to output directory (where to save output data file)
out_dir <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/'

#group you want to get data for - either 'chartreuse' or 'copperlilac'
group <- 'chartreuse'

#max radius to consider the individual near the location (in meters)
radius <- 5

#----MAIN---

#load xy data
load(paste0(datadir,group,'_xy_level1.RData'))

#load metadata and rename groups
metadata <- read.csv(metadata_file)
metadata$animal.group.id[which(metadata$animal.group.id=='Leikiji')] <- "Lilac"
metadata$animal.group.id[which(metadata$animal.group.id=='Clifford')] <- "Copper"
metadata$animal.group.id[which(metadata$animal.group.id=='Campsite')] <- "Chartreuse"

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

#convert to UTC
xy <- cocomo::latlon_to_utm(cbind(ref_locs$lon, ref_locs$lat), utm_zone = 37, hemisphere = 'north')
ref_locs$easting <- xy[,1]
ref_locs$northing <- xy[,2]

periods_of_proximity <- data.frame()
for(i in 1:nrow(ref_locs)){
  for(j in 1:nrow(xs)){
    dist_to_loc <- sqrt((xs[j,] - ref_locs$easting[i])^2 + (ys[j,] - ref_locs$northing[i])^2)
    proximity <- dist_to_loc <= radius
    proximity[which(is.na(proximity))] <- F
    periods <- rle(proximity)
    prox_idxs <- which(periods$values == T)
    if(length(prox_idxs)>0){
      for(k in 1:length(prox_idxs)){
        proximity_row <- data.frame(ind_idx = j, tag.id = ids$code[j], location = ref_locs$name[i],
                                    t0 = sum(periods$lengths[1:prox_idxs[k]-1])+1,
                                    tf = sum(periods$lengths[1:(prox_idxs[k])])+1)
        periods_of_proximity <- rbind(periods_of_proximity, proximity_row)
      }
    }
  }
}

#get duration of bouts of proximity
periods_of_proximity$duration <- periods_of_proximity$tf - periods_of_proximity$t0

#get group id
periods_of_proximity$group <- metadata$animal.group.id[match(periods_of_proximity$tag.id, metadata$tag.id)]

#get timestamps in UTC for start and end
periods_of_proximity$t0_UTC <- timestamps[periods_of_proximity$t0]
periods_of_proximity$tf_UTC <- timestamps[periods_of_proximity$tf]

save(file = paste0(out_dir, group,'_periods_of_proximity.RData'), list = c('periods_of_proximity'))

