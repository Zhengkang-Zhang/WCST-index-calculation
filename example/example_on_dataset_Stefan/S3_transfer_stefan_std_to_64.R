library("tidyverse")



transfer_stefan_to_std <- function(filename){
  
  
  wcst_long <- read.table(paste(filename,'.csv',sep=""),fileEncoding="GBK", sep=",", header=TRUE)
  
  # find subid that has less than 64 trials.
  subid_trials <- data.frame(subid=wcst_long$subid %>% unique(),trial_num=0)
  for(i in c(1:nrow(subid_trials))){
    subid_trials[i,c("trial_num")] <- nrow(wcst_long[wcst_long$subid==subid_trials$subid[i],])
  }
  subid_valid <- subid_trials$subid[subid_trials$trial_num>=64]
  # remove subid that has less than 64 trials.
  wcst_long <- wcst_long[wcst_long$subid %in% subid_valid,]
  
  remain_first64 <- function(data){
    data <- data[c(1:64),]
    return(data)
  }
  
  
  wcst_long <- wcst_long %>% by(
    wcst_long$subid,
    remain_first64
  ) %>% (function(frames){data.table::rbindlist(frames,use.names=TRUE)})()
  
  
  write.csv(wcst_long,paste(filename,'_64.csv',sep=""),sep=",")

}

transfer_stefan_to_std("stefan_wcst_long_exp_fixed")
transfer_stefan_to_std("stefan_wcst_long_health_fixed")