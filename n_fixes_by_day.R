#Generate data frame of number of fixes by individual and date for the high-res period

library(lubridate)

#Create a data frame of number of fixes for each date for each individual

#high res recording window
t0_utc <- '03:00'
tf_utc <- '05:00'

#movebank data file
movebank_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya_recent.csv'
metadata_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya-reference-data.csv'

#Load data
dat <- read.csv(movebank_file)
metadata <- read.csv(metadata_file)

#process metadata
metadata$animal.group.id[which(metadata$animal.group.id=='Clifford')] <- 'Copper'
metadata$animal.group.id[which(metadata$animal.group.id=='Campsite')] <- 'Chartreuse'
metadata$animal.group.id[which(metadata$animal.group.id=='Leikiji')] <- 'Lilac'

#Process data
dat$timestamp <- as.POSIXct(dat$timestamp, tz = 'UTC')
dat$group <- metadata$animal.group.id[match(dat$tag.local.identifier, metadata$tag.id)]

#Dates
dates <- unique(lubridate::date(dat$timestamp))
fixes_per_ind <- data.frame(id = unique(metadata$tag.id[which(metadata$animal.group.id %in% c('Copper','Lilac','Chartreuse'))]))
fixes_per_ind$group <- metadata$animal.group.id[match(fixes_per_ind$id, metadata$tag.id)]
for(d in 1:length(dates)){

  t0 <- as.POSIXct(paste(dates[d],t0_utc), tz = 'UTC')
  tf <- as.POSIXct(paste(dates[d],tf_utc), tz = 'UTC')

  currdat <- dat[which(dat$timestamp >= t0 & dat$timestamp <= tf & dat$group %in% c('Copper','Lilac','Chartreuse')),]

  n_fixes <- table(currdat$tag.local.identifier)

  fixes_per_ind[,paste0(dates[d])] <- NA
  for(i in 1:length(n_fixes)){
    idx <- which(fixes_per_ind$id == names(n_fixes)[i])
    fixes_per_ind[idx,paste0(dates[d])] <- n_fixes[i]
  }



}

fixes_by_ind <- fixes_per_ind[order(fixes_per_ind$group),]
