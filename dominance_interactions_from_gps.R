#Script for identifying dominance interactions from GPS data

#Generate data frame for detected supplants ('supplants') and detected chase sequences ('chase_seqs')
#More information below in functions

library(cocomo)
library(lubridate)

#------PARAMS-------
#Directories
level1_file <- '~/Dropbox/Teaching/VTK_2025/mpala_data_2025/chartreuse_xy_level1.RData'

#Parameters for determining supplants (see description below)
SUPP_PARAMS <- list()

#parameters about the approach phase
SUPP_PARAMS$APPROACH <- list()
SUPP_PARAMS$APPROACH$time <- 20
SUPP_PARAMS$APPROACH$init_dyad_dist_thresh <- 3
SUPP_PARAMS$APPROACH$final_dyad_dist_thresh <- 2
SUPP_PARAMS$APPROACH$avoider_stationary_time <- 10
SUPP_PARAMS$APPROACH$avoider_stationary_max_displacement <- 1.5
SUPP_PARAMS$APPROACH$min_approacher_displacement <- 3

#parameters about the avoid phase
SUPP_PARAMS$AVOID$time <- 20
SUPP_PARAMS$AVOID$init_dyad_dist_thresh <- 2
SUPP_PARAMS$AVOID$final_dyad_dist_thresh <- 3
SUPP_PARAMS$AVOID$approacher_max_displacement <- 1.5
SUPP_PARAMS$AVOID$avoider_min_displacement <- 3

#Parameters for determining chases (see description below)
CHASE_PARAMS <- list()
CHASE_PARAMS$heading_max_angle <- pi/4 #radians
CHASE_PARAMS$max_dist_apart <- 10 #meters
CHASE_PARAMS$min_speed <- .3 #meters per sec
CHASE_PARAMS$max_dist_to_side <- 5 #meters
CHASE_PARAMS$min_time <- 3 #sec

#------FUNCTIONS-----

#-----Detect supplants-----
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
#Function to detect supplants from GPS data
detect_supplants <- function(xs, ys, dyad_dists, i, j, SUPP_PARAMS){

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
    t_start <- t_mid - SUPP_PARAMS$APPROACH$time
    t_before <- t_mid - SUPP_PARAMS$APPROACH$time - SUPP_PARAMS$APPROACH$avoider_stationary_time
    t_end <- t_mid + SUPP_PARAMS$AVOID$time

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
    if(dyad_dists[i,j,t_start] >= SUPP_PARAMS$APPROACH$init_dyad_dist_thresh & dyad_dists[i,j,t_mid] < SUPP_PARAMS$APPROACH$final_dyad_dist_thresh){

      print(paste('#1', i, j, t_mid))
      #2. In the 10 seconds preceding the approach, individual j moved less than 1.5 meters (i.e. remained stationary).
      disp_j_prior <- sqrt((xs[j,t_start] - xs[j,t_before])^2 + (ys[j,t_start] - ys[j,t_before])^2)
      if(disp_j_prior <= SUPP_PARAMS$APPROACH$avoider_stationary_max_displacement){
        print(paste('#2', i, j, t_mid))
        #3. During the approach, individual i moved more than 3 meters.
        disp_i_approach <- sqrt((xs[i,t_start] - xs[i,t_mid])^2 + (ys[i,t_start] - ys[i,t_mid])^2)
        if(disp_i_approach >= SUPP_PARAMS$APPROACH$min_approacher_displacement){
          print(paste('#3', i, j, t_mid))
          #4. In the subsequent 20-second interval following the approach (the avoid), individuals i and
          #j went from being less than or equal to 2 meters apart to being greater than or equal to 3
          #meters apart.
          if(dyad_dists[i,j,t_mid] <= SUPP_PARAMS$AVOID$init_dyad_dist_thresh & dyad_dists[i,j,t_end] >= SUPP_PARAMS$AVOID$final_dyad_dist_thresh){
            print(paste('#4', i, j, t_mid))
            #5. During the avoid, individual i (the approacher) moved less than 1.5 meters (i.e. remained stationary).
            disp_i_avoid <- sqrt((xs[i,t_mid] - xs[i,t_end])^2 + (ys[i,t_mid] - ys[i,t_end])^2)
            if(disp_i_avoid <= SUPP_PARAMS$AVOID$approacher_max_displacement){
              print(paste('#5', i, j, t_mid))
              #6. During the avoid, individual j (the avoider) moved at least 3 meters.
              disp_j_avoid <- sqrt((xs[j,t_mid] - xs[j,t_end])^2 + (ys[j,t_mid] - ys[j,t_end])^2)
              if(disp_j_avoid >= SUPP_PARAMS$AVOID$avoider_min_displacement){
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

#-----Detect chases-----
#Chases are where 2 individuals are moving at high speed in the same direction and are close together in space for at least a few seconds
#We first find time steps in which the headings of two individuals are aligned within an angle of CHASE_PARAMS$heading_max_angle and both are
#moving at a speed of at least CHASE_PARAMS$min_speed, and are at a distance of maximum CHASE_PARAMS$max_dist_apart.
#In addition, the individuals must be oriented such that they are less than CHASE_PARAMS$max_dist_to_side
#distance apart along the direction perpendicular to their average heading
#We then filter these to contiguous sequences of at least CHASE_PARAMS$min_time seconds to identify chases
#The winner is defined as the individual who is "behind" relative to the average heading of the two individuals
detect_chases <- function(xs, ys, CHASE_PARAMS){
  n_inds <- nrow(xs)

  #Get headings and speeds for each individual at each time point
  heads <- speeds <- matrix(NA, nrow = nrow(xs), ncol = ncol(xs))
  breaks <- c(1, which(diff(timestamps)>1)+1, length(timestamps)+1) #day breaks
  for(i in 1:n_inds){
    for(d in 1:(length(breaks)-1)){
      t_idxs <- breaks[d]:(breaks[d+1]-1)
      x_i <- xs[i, t_idxs]
      y_i <- ys[i, t_idxs]
      out <- cocomo::get_heading_and_speed_temporal(x_i, y_i, t_window = 1, forward = T, seconds_per_time_step = 1)
      heads[i,t_idxs] <- out$heads
      speeds[i,t_idxs] <- out$speeds
    }
  }

  #get dyadic distances
  dyad_dists <- cocomo::get_group_dyadic_distances(xs, ys)

  #get heading correlations
  head_cors <- array(NA, dim = dim(dyad_dists))
  for(i in 1:(n_inds-1)){
    for(j in (i+1):n_inds){

      #get dot product of the headings of i and j
      dotp <- cos(heads[i,])*cos(heads[j,]) + sin(heads[i,]) * sin(heads[j,])

      #compute the angle between vectors
      ang <- acos(dotp)

      #store in array
      head_cors[i,j,] <- ang
    }
  }

  winners <- dist_perp_to_movement <- array(NA, dim = dim(dyad_dists))
  for(i in 1:n_inds){
    for(j in 1:n_inds){
      out <- cocomo::get_position_relative_to_group(xs[c(i,j),], ys[c(i,j),], heading_type = 'temporal',t_window = 1)

      dist_perp_to_movement[i,j,] <- abs(out$ys_rel[1,]) + abs(out$ys_rel[2,])

      i_wins <- out$xs_rel[1,] < out$xs_rel[2,] #get whether i (1) is behind j (2), i.e. wins. xs_rel give relative position along direction of movement
      winners[i,j,which(i_wins)] <- i
      winners[i,j,which(!i_wins)] <- j
    }
  }

  chase <- array(F, dim = dim(dyad_dists))
  chases <- data.frame()
  for(i in 1:n_inds){
    for(j in 1:n_inds){
      #get whether a given time and dyad are having a chase
      chase[i,j,] <- dyad_dists[i,j,] <= CHASE_PARAMS$max_dist_apart &
        head_cors[i,j,] <= CHASE_PARAMS$heading_max_angle &
        speeds[i,] >= CHASE_PARAMS$min_speed &
        speeds[j,] >= CHASE_PARAMS$min_speed &
        dist_perp_to_movement[i,j,] <= CHASE_PARAMS$max_dist_to_side

      chase_idxs <- which(chase[i,j,])
      if(length(chase_idxs)>0){
        chases <- rbind(chases,
                        data.frame(i = rep(i, length(chase_idxs)),
                                   j = rep(j, length(chase_idxs)),
                                   t = chase_idxs,
                                   winner = winners[i,j,chase_idxs]))
      }

    }
  }

  #combine consecutive time steps into contiguous sequences
  chase_seqs <- data.frame()
  if(nrow(chases)>1){
    curr_i <- chases$i[1]
    curr_j <- chases$j[1]
    curr_t <- chases$t[1]
    first_t <- chases$t[1]
    for(r in 2:nrow(chases)){
      if(chases$i[r] == curr_i &
         chases$j[r] == curr_j &
         chases$t[r] == (curr_t + 1)){
        curr_t <- curr_t + 1
      } else{
        chase_seq <- data.frame(i = curr_i, j = curr_j, t0 = first_t, tf = chases$t[r-1])
        chase_seqs <- rbind(chase_seqs, chase_seq)
        curr_i <- chases$i[r]
        curr_j <- chases$j[r]
        curr_t <- chases$t[r]
        first_t <- chases$t[r]
      }
    }
  } else{
    chase_seqs <- data.frame(i = chases$i[1], j = chases$j[1], t0 = chases$t[1], tf = chases$tf[1])
  }

  return(chase_seqs)
}


#----MAIN----

#Load data
load(level1_file)

#DETECT ALL SUPPLATNTS
dyad_dists <- cocomo::get_group_dyadic_distances(xs, ys)

inds <- seq(1, nrow(ids))

supplants <- data.frame()
for(i in 1:length(inds)){
  for(j in 1:length(inds)){
    if(i!=j){
      supps <- detect_supplants(xs, ys, dyad_dists, inds[i],inds[j],SUPP_PARAMS)
      supplants <- rbind(supplants, supps)
    }
  }
}

chase_seqs <- detect_chases(xs, ys, CHASE_PARAMS)

















