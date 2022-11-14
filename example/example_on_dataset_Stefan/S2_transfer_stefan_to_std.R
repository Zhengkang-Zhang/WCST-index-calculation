library("tidyverse")

transfer_stefan_to_std <- function(filename){
  
  
  wcst_long <- read.table(paste(filename,'.csv',sep=""),fileEncoding="GBK", sep=",", header=TRUE)
  
  calc_category_completed <- function(data){
    data <- data %>% mutate(
      category_completed = 0
    )
    category_completed <- 0
    correct_successive <- 0
    for(i in c(1:nrow(data))){
      row <- data[i,]
      
      # 计算连续正确的次数
      if(row$feedback==1){
        correct_successive = correct_successive + 1
      }else{
        correct_successive = 0
      }
      
      # 更新完成类别数
      data[i,c('category_completed')] <- category_completed
      
      # 10次连续正确后，完成的category加1
      if(correct_successive == 10){
        category_completed = category_completed + 1
        correct_successive = 0
      }
    }
    return(data)
  }
  
  
  wcst_long <- wcst_long %>% by(
    wcst_long$subid,
    calc_category_completed
  ) %>% (function(frames){data.table::rbindlist(frames,use.names=TRUE)})()
  
  wcst_long <- wcst_long %>% mutate(
    button_pressed = button_pressed-1,
    color_rule = color_rule-1,
    shape_rule = shape_rule-1,
    number_rule = number_rule-1
  )
  
  write.csv(wcst_long,paste(filename,'_fixed.csv',sep=""),sep=",")

}

transfer_stefan_to_std("stefan_wcst_long_exp")
transfer_stefan_to_std("stefan_wcst_long_health")