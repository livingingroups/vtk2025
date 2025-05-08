#Script to synchronize Baboon Timestamped Videos data table to UTC

#-----PARAMS-----

#Full path to where you have stored the Baboon Timestamped Videos file - must be a csv
path_to_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/Baboon_Timestamped_Videos - Data.csv'

#where to store the output file (with synched times)
output_file_name <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/baboon_timestamped_videos_synched.RData'

#----FUNCS-----

#Function for converting HH:MM:SS and MM:SS formats to seconds
#If time cannot be parsed, returns an NA
parse_time_to_sec <- function(time_str){

  time_list <- strsplit(time_str, ':')[[1]]

  if(length(time_list)==0){
    return(NA)
  }

  if(length(time_list)==2){
    secs <- as.numeric(time_list[1])*60 + as.numeric(time_list[2])
  }
  if(length(time_list)==3){
    secs <- as.numeric(time_list[1])*60*60 + as.numeric(time_list[2])*60 + as.numeric(time_list[3])
  }

  return(secs)

}

#-----MAIN-----

#Read in data table
data <- read.csv(path_to_file, header = T)

#Get vector of all videos
videos <- unique(data$filename)

#replace local time column with datetime including date
data$local_time_start <- as.POSIXct(paste0(data$date, ' ', data$local_time_start), format = '%Y-%m-%d %H:%M:%S', tz = 'Africa/Nairobi')
data$local_time_end <- as.POSIXct(paste0(data$date, ' ', data$local_time_end), format = '%Y-%m-%d %H:%M:%S', tz = 'Africa/Nairobi')

#create a column with seconds into video for start and end of events
data$video_time_start_sec <- sapply(data$video_time_start, FUN = function(x){return(parse_time_to_sec(x))})
data$video_time_end_sec <- sapply(data$video_time_end, FUN = function(x){return(parse_time_to_sec(x))})

#Create a column for UTC time as well as local time - posix format
data$utc_time_start <- data$utc_time_end <- as.POSIXct(NA, tz = 'UTC')

#For each video, get synchs
#Then convert the time in the video to UTC and fill in the local time column as well as a UTC column
for(i in 1:length(videos)){

  #get the current video
  video <- videos[i]

  #get rows in the table associated with that video
  rows <- which(data$filename == video)
  data_current <- data[rows,]

  #get time synchs associated with that video
  synchs <- data_current[which(data_current$type == 'timesynch'),]

  if(nrow(synchs) == 0){
    warning(paste0('No synchs found for video: ', video, ' - this video will be skipped'))
    next
  }

  #get video time and local time of the synch (use the first synch only)
  time_video <- synchs$video_time_start_sec[1]
  time_local <- synchs$local_time_start[1]

  #find the time of video start in local time
  time_video_start <- time_local - time_video

  #convert all times in that video to local time
  start_local_times <- rep(time_video_start, length(rows)) + data_current$video_time_start_sec
  end_local_times <- rep(time_video_start, length(rows)) + data_current$video_time_end_sec

  #store in data frame
  data$local_time_start[rows] <- start_local_times
  data$local_time_end[rows] <- end_local_times

  #store UTC in data frame
  data$utc_time_start[rows] <- as.POSIXct(start_local_times, tz = 'UTC')
  data$utc_time_end[rows] <- as.POSIXct(end_local_times, tz = 'UTC')

}

obs_data <- data

save(file = output_file_name, list = c('obs_data'))


