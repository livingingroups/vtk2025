#Dominance displacements

#Generate a data frame of potential supplants
#TODO: maybe add chases

library(cocomo)
library(lubridate)

#From Strandburg-Peshkin et al. 2015:
#1. Within a 20-second interval (the approach), the distance between individuals i (the approacher) and j (the avoider) changed from being greater than or equal to 3 meters apart to less than or equal to 2 meters apart.
#2. In the 10 seconds preceding the approach, individual j moved less than 1.5 meters (i.e. remained stationary).
#3. During the approach, individual i moved more than 3 meters.
#4. In the subsequent 20-second interval following the approach (the avoid), individuals i and
#j went from being less than or equal to 2 meters apart to being greater than or equal to 3
#meters apart.
#5. During the avoid, individual i (the approacher) moved less than 1.5 meters (i.e. remained
#                                                                               stationary).
#6. During the avoid, individual j (the avoider) moved at least 3 meters.

#PARAMS
level0_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/copperlilac_xy_level0.RData'
level1_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/copperlilac_xy_level1.RData'

cocomo::preprocess_gps_level0_to_level1(input_file_path = level0_file, output_file_path = level1_file)

min_fixes_highres <- 3000

#supplant params
PARAMS <- list()

#parameters about the approach phase
PARAMS$APPROACH <- list()
PARAMS$APPROACH$time <- 20
PARAMS$APPROACH$init_dyad_dist_thresh <- 3
PARAMS$APPROACH$final_dyad_dist_thresh <- 2
PARAMS$APPROACH$avoider_stationary_time <- 10
PARAMS$APPROACH$avoider_stationary_max_displacement <- 1.5
PARAMS$APPROACH$min_approacher_displacement <- 3

#parameters about the avoid phase
PARAMS$AVOID$time <- 20
PARAMS$AVOID$init_dyad_dist_thresh <- 2
PARAMS$AVOID$final_dyad_dist_thresh <- 3
PARAMS$AVOID$approacher_max_displacement <- 1.5
PARAMS$AVOID$avoider_min_displacement <- 3

#Detect supplants from GPS data
detect_supplants <- function(xs, ys, dyad_dists, i, j, PARAMS){

  #number of times
  n_times <- ncol(xs)

  #data frame to hold events
  events <- data.frame()

  #loop over times
  for(t_mid in 1:n_times){

    #times of the event
    #t_mid is the midpoint (reference time)
    #between t_start and t_mid is the approach
    #between t_mid and t_end is the avoid
    #between t_before and t_start is the prior time (when the avoider must be stationary)
    t_start <- t_mid - PARAMS$APPROACH$time
    t_before <- t_mid - PARAMS$APPROACH$time - PARAMS$APPROACH$avoider_stationary_time
    t_end <- t_mid + PARAMS$AVOID$time

    #make sure we don't go before t=1 or after max time
    if(t_before < 1 | t_end > n_times){
      next
    }

    #make sure dyadic distances are present for the full period - otherwise skip
    if(sum(is.na(dyad_dists[i,j,t_before:t_end]))){
      next
    }

    #Check for a dominance event at that time
    #i = approacher
    #j = avoider
    #1. Within a 20-second interval (the approach), the distance between individuals i (the approacher) and j (the avoider) changed from being greater than or equal to 3 meters apart to less than or equal to 2 meters apart.
    if(dyad_dists[i,j,t_start] >= PARAMS$APPROACH$init_dyad_dist_thresh & dyad_dists[i,j,t_mid] < PARAMS$APPROACH$final_dyad_dist_thresh){

      print(paste('#1', i, j, t_mid))
      #2. In the 10 seconds preceding the approach, individual j moved less than 1.5 meters (i.e. remained stationary).
      disp_j_prior <- sqrt((xs[j,t_start] - xs[j,t_before])^2 + (ys[j,t_start] - ys[j,t_before])^2)
      if(disp_j_prior <= PARAMS$APPROACH$avoider_stationary_max_displacement){
        print(paste('#2', i, j, t_mid))
        #3. During the approach, individual i moved more than 3 meters.
        disp_i_approach <- sqrt((xs[i,t_start] - xs[i,t_mid])^2 + (ys[i,t_start] - ys[i,t_mid])^2)
        if(disp_i_approach >= PARAMS$APPROACH$min_approacher_displacement){
          print(paste('#3', i, j, t_mid))
          #4. In the subsequent 20-second interval following the approach (the avoid), individuals i and
          #j went from being less than or equal to 2 meters apart to being greater than or equal to 3
          #meters apart.
          if(dyad_dists[i,j,t_mid] <= PARAMS$AVOID$init_dyad_dist_thresh & dyad_dists[i,j,t_end] >= PARAMS$AVOID$final_dyad_dist_thresh){
            print(paste('#4', i, j, t_mid))
            #5. During the avoid, individual i (the approacher) moved less than 1.5 meters (i.e. remained stationary).
            disp_i_avoid <- sqrt((xs[i,t_mid] - xs[i,t_end])^2 + (ys[i,t_mid] - ys[i,t_end])^2)
            if(disp_i_avoid <= PARAMS$AVOID$approacher_max_displacement){
              print(paste('#5', i, j, t_mid))
              #6. During the avoid, individual j (the avoider) moved at least 3 meters.
              disp_j_avoid <- sqrt((xs[j,t_mid] - xs[j,t_end])^2 + (ys[j,t_mid] - ys[j,t_end])^2)
              if(disp_j_avoid >= PARAMS$AVOID$avoider_min_displacement){
                print(paste('#6', i, j, t_mid))
                event_dat <- data.frame(approacher = i, avoider = j, t = t_mid)
                events <- rbind(events, event_dat)
              }
            }
          }
        }
      }
    }
  }

  return(events)

}

#Load data
load(level1_file)

#which inds are high res on each day
dates <- unique(lubridate::date(timestamps))
high_res_inds <- list()
for(i in 1:length(dates)){
  times <- which(lubridate::date(timestamps)==dates[i])
  fixes_per_ind <- rowSums(!is.na(xs[,times]))
  high_res_inds[[i]] <- which(fixes_per_ind >= min_fixes_highres)
}

dyad_dists <- cocomo::get_group_dyadic_distances(xs, ys)

inds <- seq(1, nrow(ids))

supplants <- data.frame()
for(i in 1:length(inds)){
  for(j in 1:length(inds)){
    if(i!=j){
      supps <- detect_supplants(xs, ys, dyad_dists, inds[i],inds[j],PARAMS)
      supplants <- rbind(supplants, supps)
    }
  }
}













