#check presence of a certain group's data for a certain date

#path to file containing all gps data from movebank
gpsdat_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya.csv'

#path to file containing metadata from movebank
metadata_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboons MBRP Mpala Kenya-reference-data.csv'

date_to_check <- '2025-05-07'

highres_hrs <- c(3,4)

#group to check
#Campsite = Chartreuse
#Clifford = Copper
#Leikiji = Lilac
group_to_check <- 'Campsite' #or Clifford or Leikiji

#load gps data from csv
alldat <- read.csv(gpsdat_file, header=T, stringsAsFactors=F)

#load metadata from csv
metadata <- read.csv(metadata_file, header=T)

#get tag ids for the relevant group
tag_ids <- metadata$tag.id[which(metadata$animal.group.id==group_to_check)]

#get data from that date and that group
dat <- alldat[which(alldat$tag.local.identifier %in% tag_ids & date(alldat$timestamp)==date_to_check),]

#how much data is there per hour?
if(nrow(dat)>0){
  plot(table(hour(dat$timestamp)), xlab = 'hour', ylab = 'Fixes', main = 'Data present per hour (all individuals in group)')
} else{
  print(paste0('No data for group ', group_to_check, ' on date ', date_to_check))
}
#which individuals have data during the highres period and how much?
dat_highres_period <- dat[which(hour(dat$timestamp) %in% highres_hrs),]

if(nrow(dat_highres_period)>0){
  plot(table(dat_highres_period$tag.local.identifier), xlab = 'tag id', ylab = 'Fixes', main = 'Data present during highres interval')
} else{
  print(paste0('No data for group ', group_to_check, ' on date ', date_to_check, ' during the high res hours ', paste(highres_hrs, collapse = ',')))
}
