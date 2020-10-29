# First function will call all of the values 


# Notes---
# 1) Need to see if using RQuantLib provides better holiday functionality 



moment_clean_all <- function(all_pairs, zero_tolerance, outlier_tolerance, weekend_strip, holidays, write_to_path, diagnose = TRUE){
  # Loop through all the fx pairs in the list
  # define all elements to which objects will be stored in the loop
  r_moments_list <- list()
  obs_removed <- as.data.frame(matrix(data = NA, ncol = 2, nrow = 5)) # Data frame that will store all the obs 
  rownames(obs_removed) <- c("Holidays","Zero tolerance","Weekends","winsorization","Total obs removed")
  colnames(obs_removed) <- c("Total obs","Approx. nr of days")
  
  obs_removed_list <- list()
  diagnose_list <- list()
  
  # nr_removed_outliers <- list()
  # nr_obs_removed_weekends <- list()
  # total_obs <- list()
  # total_obs_removed <- list()
  # zero_tol_obs_removed <- list()
  
  
  
  for(i in 1:length(all_pairs)){
    # Read in temporary dataset
    data_temp <- all_pairs[[i]]
    
    print(paste(as.character(i)," Cleaning pair: ",  names(all_pairs)[i], sep = ""))
    
    clean_logret <- data_temp$logret
    
    
 # Make sure first day in dataset starts at 09:05pm and last day ends at 09:00pm   
    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    index_first9pm <- as.numeric(which(.indexhour(clean_logret)==21 & .indexmin(clean_logret) == 5)[1])
    if(index_first9pm>1){
      clean_logret<- clean_logret[(index_first9pm):nrow(clean_logret),]}
    
    index_last9pm <- as.numeric(which(.indexhour(clean_logret)==21))
    index_last9pm <- index_last9pm[length(index_last9pm)] 
    clean_logret <- clean_logret[1:(index_last9pm-(60/period)+1),] 
    
# Holidays
#-------------------------------------------------------------------------------------------------------------------
    # Take out US, JAP, UK, SWiss, and GER
    remove_counter <- nrow(clean_logret)
    
    holidays_1 <- "UnitedStates/NYSE" 
    holidays_2 <- "SouthAfrica"
    
    holiday_str_1 <-  holidayList(calendar="UnitedStates/NYSE", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    holiday_str_2 <-  holidayList(calendar= "Japan", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    holiday_str_3 <-  holidayList(calendar= "UnitedKingdom", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    holiday_str_4 <-  holidayList(calendar= "Switzerland", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    holiday_str_5 <-  holidayList(calendar= "Germany/Eurex", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    
    
    
    holiday_all <- unique(c(holiday_str_1,holiday_str_2,holiday_str_3,holiday_str_4,holiday_str_5))
    
    
    cut <- vector()
    
    for(j in 1:length(holiday_all)){
      cut[j] <- paste(holiday_all[j]-1, " 21:05", "/", holiday_all[j], " 21:00", sep = "")
          }    
      rmrow <- clean_logret[cut, which.i = TRUE]
      clean_logret <- clean_logret[-rmrow, ]
    
      if (diagnose==TRUE){
        diag_holidays <- diagnostic(clean_logret)
      } 
      
      obs_removed[1,1] <- remove_counter - nrow(clean_logret)
      obs_removed[1,2] <- obs_removed[1,1]/288
      
    
# Remove zeros 
#-------------------------------------------------------------------------------------------------------------------
    remove_counter <- nrow(clean_logret)
      
    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    endpts <- seq(0, nrow(clean_logret),by=((24*60)/period))
    
    ep <- endpts
    sp <- (ep + 1)[-length(ep)]
    ep <- ep[-1]
    clean_logret_split <- lapply(1:length(ep), function(X) clean_logret[sp[X]:ep[X]])
    
    intervals_per_day <- (24*60)/unlist(periodicity(clean_logret)[1])
    # Apply the rule that will remove a trading day if there are more 0's than a certain threshold
    index_zeros <- which(lapply(clean_logret_split, function(x){length(which(x[,"logret"]==0))}) <= intervals_per_day*zero_tolerance)
    clean_logret <- do.call(rbind, clean_logret_split[index_zeros])
    
    if (diagnose==TRUE){
      diag_consec_zeros <- diagnostic(clean_logret)
    }
    
    obs_removed[2,1] <- remove_counter - nrow(clean_logret)
    obs_removed[2,2] <-  obs_removed[2,1]/288
    
# Remove weekends
#--------------------------------------------------------------------------------------------------------------------
    remove_counter <- nrow(clean_logret)
    
    if(weekend_strip==TRUE){
      # Exclude Friday 21:05 to Sunday 21:00, inclusive
      # (see https://stackoverflow.com/questions/28922337/excluding-hours-and-days-in-xts)
      wday <- .indexwday(clean_logret)             # Weekday for each observation
      hh <- .indexhour(clean_logret)               # Hour for each observation
      mm <- .indexmin(clean_logret)                # Minute for each observation
      week.subset <-
        !((wday == 5 & hh >= 22) |           # Remove Fridays from 22:00
            (wday == 6) |                      # Remove Saturdays all day
            (wday == 0 & hh < 21) |            # Remove Sundays until 20:55
            (wday == 5 & hh == 21 & mm > 00) | # Remove Fridays 21:05--21:55
            (wday == 0 & hh == 21 & mm == 00)) # Remove Sundays 21:00
      clean_logret <- clean_logret[week.subset,]
    }
    
# Number of observations removed due to weekends
    
    #nr_obs_removed_weekends[[i]] <- total_nr - length(clean_logret) 
   
    if (diagnose==TRUE){
      diag_weekends <- diagnostic(clean_logret)
    } 

    obs_removed[3,1] <- remove_counter - nrow(clean_logret)
    obs_removed[3,2] <- obs_removed[3,1]/288 

# Remove outliers
#-------------------------------------------------------------------------------------------------------------------
    # Perform 99% wins
    # Roughly remove 
    
    #remove_counter <- nrow(clean_logret)
    
    #per_99 <- quantile(clean_logret, probs = 0.9999, na.rm = TRUE)
    #clean_logret[abs(clean_logret$logret)> per_99] <- per_99
    
    #obs_removed[4,1] <-  remove_counter - nrow(clean_logret[abs(clean_logret$logret)> per_99])  
    #obs_removed[4,2] <- obs_removed[4,1]/288
    
# Calculate moments
#-------------------------------------------------------------------------------------------------------------------  

    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    endpts <- seq(0, nrow(clean_logret),by=((24*60)/period))
    N_id <- diff(endpts)
    
    # Calculate realized variance
    rmoment <- period.apply(clean_logret^2,INDEX = endpts,FUN=sum)  
    colnames(rmoment) <- "rvar"
    rmoment$rvar[rmoment$rvar==0] <- NA # get rid of 0 rvar with NA
    
    # Need to scale rvar and realized volatility
    rmoment$rvar_scaled <- 10000*rmoment$rvar*260 # Annualized variance units
    rmoment$rvol_scaled <- sqrt(rmoment$rvar_scaled) # Annualized percent
    
    # Compute realised skewness
    tmp1 <- period.apply(clean_logret$logret^3,INDEX=endpts,FUN=sum)
    tmp2 <- sqrt(N_id)/rmoment$rvar^(3/2)
    rmoment$rskew <- tmp1*tmp2
    rm(tmp1,tmp2)
    
    # Compute realised kurtosis
    tmp1 <- period.apply(clean_logret$logret^4,INDEX=endpts,FUN=sum)
    tmp2 <- N_id/rmoment$rvar^2
    rmoment$rkurt <- tmp1*tmp2
    rm(tmp1,tmp2)
    
    # Convert returns to daily
    daily_returns <- period.apply(clean_logret$logret,INDEX=endpts,FUN=mean)
    rmoment$logret <- daily_returns
    
    
# Write the moments to a list and store them
#--------------------------------------------------------------------------------------------------------------------

    diag_all <- cbind(diag_holidays, diag_consec_zeros, diag_weekends)
    colnames(diag_all) <- c("Holidays clean","Zero tol clean","Weekend clean")
    
    # Calculate total obs removed
    obs_removed[5,1] <- obs_removed[1,1]+obs_removed[2,1]+obs_removed[3,1]
    obs_removed[5,2] <- obs_removed[5,1]/288
    
    # Add calculated moments, diagnostics, and removed stats to a list
    r_moments_list[[i]] <- rmoment
    obs_removed_list[[i]] <- obs_removed
    diagnose_list[[i]] <- diag_all
    
        
    
# Assign names to all the objects inside the lists
#--------------------------------------------------------------------------------------------------------------------
    name_temp <- names(all_pairs)[i]
    
    names(obs_removed_list)[i] <- name_temp
    names(diagnose_list)[i] <- name_temp
    names(r_moments_list)[i] <-name_temp 
    
    # Remove objects that do not need to be stored
    rm(data_temp)
    
    
  }# end for loop all_pairs
  names(all_pairs)
  return(r_moments_list)
   
}

