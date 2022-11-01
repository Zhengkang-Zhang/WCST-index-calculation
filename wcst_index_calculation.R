
### ==================================================== README
### the indexs' calculation for Wisconsin Card Sorting task.
### the indexs included here are:
### # TC  -Total Number Correct
### # PR  -Perseverative Responses
### # PE  -Perseverative Errors
### # NPE -Nonperseverative Errors
### # CLR -Conceptual Level Responses
### # CAT -Number of Categories Completed
### # FMS -Failure to Maintain Set
### ==================================================== README


if (!("splithalfr" %in% installed.packages())) install.packages("splithalfr")
if (!("tidyverse" %in% installed.packages())) install.packages("tidyverse")
if (!("parallel" %in% installed.packages())) install.packages("parallel")

library("splithalfr")
library("tidyverse")
library("parallel")

# persistant directory for saving results.
path_persistance <- file.path(getwd(),"persistance")
if(!dir.exists(path_persistance)){
  dir.create(path_persistance)
}


### ==================================================== STEP1: data preperation

### We need a .csv file or a dataframe.
### The init columns of the frame should contain these:
### # 'subid':              subject's id
### # 'number_of_rule':     the present correct dimension(1/2/3 namely color/shape/number) that should be obey
### # 'correct_card':       the correct card(0/1/2/3 namely the top four fixed cards from left to right) that should be chosen if obey the present correct dimension
### # 'color_rule':         one of the four cards(0/1/2/3 ~) that would be chosen if obey the color dimension
### # 'shape_rule':         one of the four cards(0/1/2/3 ~) that would be chosen if obey the shape dimension
### # 'number_rule':        one of the four cards(0/1/2/3 ~) that would be chosen if obey the number dimension
### # 'button_pressed':     one of the four cards(0/1/2/3 ~) actually be chosen
### # 'category_completed': total categories completed(0/1/2/3/4/5/6-)

# 
wcst_long <- read.table("./test_wcst_long.csv",fileEncoding="GBK", sep=",", header=TRUE)
wcst_short <- wcst_long[,c("subid")] %>% unique()



### ==================================================== STEP2: necessary mediate indices calculation


### Add new columns
### # 'wcst_correct_dim': all the dimensions(1/2/3), if obey, that could lead to the same result as when obey the correct dimension.
### # 'wcst_obey_dim':    all the probably obeyed dimensions(1/2/3) that could lead to the actually chosen card.
### # 'wcst_ambiguous':   ambiguous if could not directly infer which dimension was obeyed according to the chosen card. (0/1 namely unambiguous/ambiguous)
### Columns depend on:    'correct_card', 'shape_rule', 'number_rule', 'correct_card', 'button_pressed'
cal_obey_dim <- function(data){
  data <- data %>% mutate(
    wcst_correct_dim = "", 
    wcst_obey_dim = "", 
    wcst_ambiguous = -1 
  )
  for(i in c(1:nrow(data))){
    row <- data[i,]
    
    # wcst_correct_dim
    correct_dim <- which(c(row$color_rule,row$shape_rule,row$number_rule)==row$correct_card) %>% paste(collapse = "")
    data$wcst_correct_dim[i] <- correct_dim
    
    # wcst_obey_dim
    obey_dim <- which(c(row$color_rule,row$shape_rule,row$number_rule)==row$button_pressed) %>% paste(collapse = "")
    data$wcst_obey_dim[i] <- obey_dim
    
    # wcst_ambiguous
    if(nchar(obey_dim)>1){
      data$wcst_ambiguous[i] <- 1 # ambiguous
    }else if(nchar(obey_dim)==1){
      data$wcst_ambiguous[i] <- 0 # unambiguous
    }
  }
  return(data)
  
}

### Add new columns
### # 'wcst_category_weight': distribute the total categories completed to each trial that contribute to the this score. (According to Kopp,2021,doi.org/10.1177/1073191119866257)
calc_category_weight <- function(data){
  data <- data %>% mutate(
    wcst_category_weight = 0
  )
  
  max_category_completed = max(unique(data$category_completed)) # 最大类别数
  
  if(max_category_completed==0)return(data)
  for(i in c(1:max_category_completed)){
    end_i <- 0
    # 找到达该类别数的trial
    for(j in c(1:nrow(data))){
      trial <- data[j,]
      if(trial$category_completed==i){ # 到达指定类别数时停止
        end_i <- j-1
        break
      }
    }
    # 往前推10个就是所有贡献的trial
    for(j in c((end_i-9):end_i)){
      data$wcst_category_weight[j] <- 0.1
    }
  }
  
  return(data)
}

### Add new columns
### # 'wcst_perseverated_to':     (1/2/3)the perseverated-to principle
### # 'wcst_perseverated_to_tag': three type of string represent the methods used for this trial to be a "wcst_perseverated_to"
cal_perseverated_to <- function(data){
  data <- data %>% mutate(
    wcst_perseverated_to = "",
    wcst_perseverated_to_tag = ""
  )
  category_completed <- 0
  last_category_number_of_rule <- 0
  number_of_rule <- 1
  wcst_perseverated_to <- ""
  wcst_perseverated_to_tag <- ""
  for(i in c(1:nrow(data))){
    row <- data[i,]
    
    
    # every time entering a new category, the new "perseverated-to principle" would be the correct dimension in the last category
    if(row$category_completed!=category_completed){
      category_completed <- row$category_completed
      last_category_number_of_rule <- number_of_rule
      number_of_rule <- row$number_of_rule
      wcst_perseverated_to <- last_category_number_of_rule
      wcst_perseverated_to_tag <- "last category correct dim"
    }
    
    
    # if not found the "perseverated-to principle" yet, then the first correct unambiguous trial's dimension obeyed would be the new one from next trial on.
    if(i>1){
      row_1 <- data[i-1,]
      row_2 <- data[i,]
      if(wcst_perseverated_to == ""){
        if(row_1$category_completed==0){ # the first category
          if(row_1$correct==FALSE){ # error trial
            if(row_1$wcst_ambiguous==0){ # unambiguous trial
              wcst_perseverated_to <- row_1$wcst_actual_obey
              wcst_perseverated_to_tag <- "first category error"
            }
          }
        }
      }
    }
    
    # three sequential unambiguous error trial get the new "perseverated-to principle" from the second trial on.
    if(i>3){
      row_1 <- data[i-3,]
      row_2 <- data[i-2,]
      row_3 <- data[i-1,]
      if(row_1$correct==FALSE & row_2$correct==FALSE & row_3$correct==FALSE){ # three sequential errors
        
        dim <- unique(c(row_1$wcst_obey_dim, row_2$wcst_obey_dim, row_3$wcst_obey_dim))
        if(length(dim)==1){ # has the same dimension
          if(dim!=""){ # this dimension is not empty
            
            wcst_perseverated_to <- dim
            wcst_perseverated_to_tag <- "third same dim error"
            
            data$wcst_perseverated_to[i-2] <- wcst_perseverated_to
            data$wcst_perseverated_to_tag[i-2] <- wcst_perseverated_to_tag
            
            data$wcst_perseverated_to[i-1] <- wcst_perseverated_to
            data$wcst_perseverated_to_tag[i-1] <- wcst_perseverated_to_tag
          }
        }
      }
    }
    
    data$wcst_perseverated_to[i] <- wcst_perseverated_to
    data$wcst_perseverated_to_tag[i] <- wcst_perseverated_to_tag
  }
  return(data)
}

### Add new columns
### calculate index 'perseverative response' according to Flashman's description. (Laura A. Flashman, 1991, 10.1080/13854049108403303)
### # 'pr_unambiguous': all correct unambiguous trial that match the "perseverated-to principle" would be tagged as a 'perseverative response'
### # 'pr_sandwich':    the ambiguous trial that match the "sandwich rule" would alse be tagged as a 'perseverative response'
### # 'pr':             the perseverative response
### # 'npr':            the trial that is not perseverative response
calc_PR <- function(data){
  data <- data %>% mutate(
    pr_unambiguous = 0,
    pr_sandwich = 0,
    pr = 0,
    npr = 0
  )
  # all correct unambiguous trial that match the "perseverated-to principle" would be tagged as a 'perseverative response'
  for(i in c(1:nrow(data))){
    row <- data[i,]
    if(row$wcst_ambiguous==0){
      if(as.numeric(row$wcst_obey_dim)==row$wcst_perseverated_to){
        data$pr_unambiguous[i] <- 1
        data$pr[i] <- 1
      }
    }
    
  }
  
  # the ambiguous trial that match the "sandwich rule" would alse be tagged as a 'perseverative response'
  ambigous_start_i <- 0
  ambigous_end_i <- 0
  for(i in c(1:nrow(data))){
    if(i>1){
      row_1 <- data[i-1,]
      row_2 <- data[i,]
      if(row_1$pr_unambiguous == 1 & row_2$wcst_ambiguous == 1){ # start the "sandwich" from an ambiguous trial followed by an unambiguous 'perseverative response' trial
        ambigous_start_i <- i
        ambigous_end_i <- i
        next
      }
      if(ambigous_start_i!=0 & ambigous_end_i!=0){ # when we started, let's see if this trial could be added to the "sandwich"
        if(row_2$wcst_ambiguous==1){ # added if this trial is also ambiguous
          ambigous_end_i <- i
        }else{ # end the "sandwich" if unambiguous
          if(data$wcst_obey_dim[ambigous_start_i-1] == data$wcst_obey_dim[ambigous_end_i+1]){ # the "sandwich" will be valid if the up and bottom trial (not included in the sandwich) obey the same dimension
            
            # all the trials in the "sandwich" should obey the same dimension the could be tagged as a 'perseverative response'
            same <- TRUE
            for(index in c(ambigous_start_i:ambigous_end_i)){
              if(!(data$wcst_obey_dim[ambigous_start_i-1] %in% strsplit(data$wcst_obey_dim[index],"")[[1]])){
                same <- FALSE
                break
              }
            }
            if(same==TRUE){
              for(index in c(ambigous_start_i:ambigous_end_i)){
                data$pr_sandwich[index] <- 1
                data$pr[index] <- 1
              }
            }
          }
          ambigous_start_i <- 0
          ambigous_end_i <- 0
        }
      }
      
    }
  }
  
  for(i in c(1:nrow(data))){
    row <- data[i,]
    if(row$pr==0){
      data$npr[i] <- 1
    }
  }
  
  
  return(data)

}

### Add new columns
### # 'wcst_conceptual_level_response': Conceptual level responses, started from three or more consecutively correct trials. 
calc_conceptual_level_response <- function(data){
  data <- data %>% mutate(
    wcst_conceptual_level_response = 0
  )
  for(i in c(1:nrow(data))){
    if(i>2){
      row_1 <- data[i-2,]
      row_2 <- data[i-1,]
      row_3 <- data[i,]
      if(row_1$correct==TRUE & row_2$correct==TRUE & row_3$correct==TRUE){
        data$wcst_conceptual_level_response[i] <- 1
      }
    }
    
  }
  return(data)
}

### Add new columns
### # 'wcst_failure_to_maintain_set_response':  Failure to maintain set, error trial started after the fifth consecutively correct trials. 
calc_failure_to_maintain_set <- function(data){
  data <- data %>% mutate(
    wcst_failure_to_maintain_set_response = 0
  )
  concecutive_right_n <- 0
  for(i in c(1:nrow(data))){
    row <- data[i,]
    if(row$correct==TRUE){
      concecutive_right_n <- concecutive_right_n + 1
    }else{
      if(concecutive_right_n>=5 & concecutive_right_n<10){
        data$wcst_failure_to_maintain_set_response[i] <- 1
      }
      concecutive_right_n <- 0
    }
  }
  return(data)
}



### start calculating all the mediate indices here
trial_index_calc <- function(data){
  data %>% cal_obey_dim() %>% 
    calc_category_weight() %>%
    cal_perseverated_to() %>%
    calc_PR() %>% 
    calc_conceptual_level_response() %>%
    calc_failure_to_maintain_set()
  
}



# the new wcst_long after adding the mediate indices:
wcst_long <- wcst_long %>% by(
  wcst_long$subid,
  trial_index_calc
) %>% (function(frames){data.table::rbindlist(frames,use.names=TRUE)})()



### ==================================================== STEP3: indices calculation functions and descriptive statistics

calc_correct_n <- function(data){
  return(length(data$correct[data$correct==TRUE]))
}

calc_pr_n <- function(data){
  return(length(data$pr[data$pr==1]))
}

calc_pr_e_n <- function(data){
  return(length(data$pr[data$pr==1 & data$correct==FALSE]))
}

calc_npr_n <- function(data){
  return(length(data$npr[data$npr==1]))
}

calc_npr_e_n <- function(data){
  return(length(data$npr[data$npr==1 & data$correct==FALSE]))
}

calc_conceptual_level_response_n <- function(data){
  return(length(data$wcst_conceptual_level_response[data$wcst_conceptual_level_response==1]))
}

calc_failure_to_maintain_set_response_n <- function(data){
  return(length(data$wcst_failure_to_maintain_set_response[data$wcst_failure_to_maintain_set_response==1]))
}

calc_category_n <- function(data){
  return(sum(data$wcst_category_weight))
}



calc_correct_rate <- function(data){
  return(length(data$correct[data$correct==TRUE])/nrow(data))
}

calc_pr_rate <- function(data){
  return(length(data$pr[data$pr==1])/nrow(data))
}

calc_pr_e_rate <- function(data){
  return(length(data$pr[data$pr==1 & data$correct==FALSE])/nrow(data))
}

calc_npr_rate <- function(data){
  return(length(data$npr[data$npr==1])/nrow(data))
}

calc_npr_e_rate <- function(data){
  return(length(data$npr[data$npr==1 & data$correct==FALSE])/nrow(data))
}

calc_conceptual_level_response_rate <- function(data){
  return(length(data$wcst_conceptual_level_response[data$wcst_conceptual_level_response==1])/nrow(data))
}

calc_failure_to_maintain_set_response_rate <- function(data){
  return(length(data$wcst_failure_to_maintain_set_response[data$wcst_failure_to_maintain_set_response==1])/nrow(data))
}

calc_category_rate <- function(data){
  cat <- sum(data$wcst_category_weight)/nrow(data)
  return(cat)
}

### start calculating all the indices here
calc_indices <- function(data){
  
  return(data.frame(subid=data$subid[1],
                    Number_Correct=calc_correct_n(data),
                    Number_pr = calc_pr_n(data),
                    Number_pr_e = calc_pr_e_n(data),
                    Number_npr_e = calc_npr_e_n(data),
                    Number_Category=calc_category_n(data),
                    Number_ConceptualLevelResponse=calc_conceptual_level_response_n(data),
                    Number_FailureToMaintainSet=calc_failure_to_maintain_set_response_n(data),
                    
                    Percent_Correct=calc_correct_rate(data),
                    Percent_pr = calc_pr_rate(data),
                    Percent_pr_e = calc_pr_e_rate(data),
                    Percent_npr_e = calc_npr_e_rate(data),
                    Percent_Category=calc_category_rate(data),
                    Percent_ConceptualLevelResponse=calc_conceptual_level_response_rate(data),
                    Percent_FailureToMaintainSet=calc_failure_to_maintain_set_response_rate(data)
  ))
}


# the new wcst_short after adding the mediate indices:
wcst_short <- wcst_short %>% merge(
  by(
    wcst_long,
    wcst_long$subid,
    calc_indices
  ) %>% (function(frames){data.table::rbindlist(frames,use.names=TRUE)})()
)

# save to file
write.csv(wcst_short,file.path(path_persistance,'wcst_short.csv'),sep=",")


### ==================================================== STEP4: split-half functions and estimates

### Calculate the mean and sd score for all subjects, of each subject the score is calculated with fn_score.
### Parameters
### # data:      A long-format dataframe.
### # fn_score:  A function which handles a long-format dataframe of one subject and returns a score number.
### # ncores:    The maximum cpu cores that will be used for parallel calculating. (unused in this function)
### # 
### Returns:     A list of result with item 'Mean' and 'SD'.
general_statistic <- function(data,fn_score,ncores=NA,fn_name=NA,data_name=NA){
  result <- list(
    Mean = NA,
    SD = NA
  ) %>% data.frame()
  
  tryCatch(
    {
      scores <- by(
        data,
        data$subid,
        fn_score
      )
      result$Mean <- mean(scores,na.rm=TRUE)
      result$SD <- sd(scores,na.rm=TRUE)
      return(result)
    },error=function(e){
      print(e)
      return(result)
    }
  )
}



### Calculate the First/second split estimates of score for all subjects, of each subject the score is calculated with fn_score.
### Parameters
### # data:       A long-format dataframe.
### # fn_score:   A function which handles a long-format dataframe of one subject and returns a score number.
### # ncores:     The maximum cpu cores that will be used for parallel calculating.
### # fn_name:    The name of fn_score, used for generating a path for saving or read the splitted data.
### # data_name:  The name of data, also used for generating the path, since we may have more than one data sets.
### # 
### Returns:     A list of result with item 'first_second_median'.
split_first_second <- function(data,fn_score,ncores,fn_name=NA,data_name=NA){
  result <- list(
    first_second_median = NA
  ) %>% data.frame()
  tryCatch(
    {
      
      # get the file_path from where to read the splitted data or to where to save.
      dir_path <- file.path(path_persistance,data_name,fn_name)
      if(!dir.exists(dir_path)){dir.create(dir_path,recursive=TRUE)}
      file_path <- file.path(dir_path, "split_first_second.txt")
      
      # if the splitted data already exists, then read, otherwise calculate and save.
      firstSecond <- NA
      if(file.exists(file_path)){
        firstSecond <- read.table(file_path,header=TRUE,sep=",")
      }else{
        firstSecond <- splithalfr::by_split(
          data,
          data$subid,
          method = "first_second",
          fn_score = fn_score,
          ncores = ncores,
          verbose = F
        )
        write.table(firstSecond,file_path,sep=",")
      }
      
      # Spearman-Brown adjusted Pearson correlations per replication
      reliability <- split_coefs(firstSecond, spearman_brown)
      result$first_second_median <- median(reliability)
    },error=function(e){
      print(e)
    },finally={
      return(result)
    }
  )
}



### Calculate the Odd/even split estimates of score for all subjects, of each subject the score is calculated with fn_score.
### Parameters
### # data:       A long-format dataframe.
### # fn_score:   A function which handles a long-format dataframe of one subject and returns a score number.
### # ncores:     The maximum cpu cores that will be used for parallel calculating.
### # fn_name:    The name of fn_score, used for generating a path for saving or read the splitted data.
### # data_name:  The name of data, also used for generating the path, since we may have more than one data sets.
### # 
### Returns:     A list of result with item 'odd_even_median'.
split_odd_even <- function(data,fn_score,ncores,fn_name=NA,data_name=NA){
  result <- list(
    odd_even_median = NA
  ) %>% data.frame()
  tryCatch(
    {
      # get the file_path from where to read the splitted data or to where to save.
      dir_path <- file.path(path_persistance,data_name,fn_name)
      if(!dir.exists(dir_path)){dir.create(dir_path,recursive=TRUE)}
      file_path <- file.path(dir_path, "split_odd_even.txt")
      
      # if the splitted data already exists, then read, otherwise calculate and save.
      oddEven <- NA
      if(file.exists(file_path)){
        oddEven <- read.table(file_path,header=TRUE,sep=",")
      }else{
        
        oddEven <- splithalfr::by_split(
          data,
          data$subid,
          method = "odd_even",
          fn_score = fn_score,
          ncores = ncores,
          verbose = F
        )
        write.table(oddEven,file_path,sep=",")
      }
      
      # Spearman-Brown adjusted Pearson correlations per replication
      reliability <- split_coefs(oddEven, spearman_brown)
      result$odd_even_median <- median(reliability)
      
    },error=function(e){
      print(e)
    },finally={
      return(result)
    }
  )
}


### Calculate the Permutated split estimates of score for all subjects, of each subject the score is calculated with fn_score.
### Parameters
### # data:       A long-format dataframe.
### # fn_score:   A function which handles a long-format dataframe of one subject and returns a score number.
### # ncores:     The maximum cpu cores that will be used for parallel calculating.
### # fn_name:    The name of fn_score, used for generating a path for saving or read the splitted data.
### # data_name:  The name of data, also used for generating the path, since we may have more than one data sets.
### # 
### Returns:     A list of result with item 'permutated_median', 'permutated_lower' and 'permutated_upper'.
split_permutated <- function(data,fn_score,ncores,fn_name=NA,data_name=NA){
  result <- list(
    permutated_median = NA,
    permutated_lower = NA,
    permutated_upper = NA
  ) %>% data.frame()
  tryCatch(
    {
      # get the file_path from where to read the splitted data or to where to save.
      dir_path <- file.path(path_persistance,data_name,fn_name)
      if(!dir.exists(dir_path)){dir.create(dir_path,recursive=TRUE)}
      file_path <- file.path(dir_path, "split_permutated.txt")
      
      # if the splitted data already exists, then read, otherwise calculate and save.
      permutated <- NA
      if(file.exists(file_path)){
        permutated <- read.table(file_path,header=TRUE,sep=",")
      }else{
        permutated <- splithalfr::by_split(
          data,
          data$subid,
          method = "random",
          replace = FALSE,
          split_p = 0.5,
          fn_score = fn_score,
          replications = 5000,
          ncores = ncores,
          verbose = F,
        )
        write.table(permutated,file_path,sep=",")
      }
      
      # Spearman-Brown adjusted Pearson correlations per replication
      reliability <- split_coefs(permutated, spearman_brown)
      result$permutated_median <- median(reliability)
      hdi <- bayestestR::hdi(reliability,ci=0.95) %>% as.data.frame()
      result$permutated_lower <- hdi$CI_low
      result$permutated_upper <- hdi$CI_high
      
    },error=function(e){
      print(e)
    },finally={
      return(result)
    }
  )
}


### Calculate the Monte Carlo split estimates of score for all subjects, of each subject the score is calculated with fn_score.
### Parameters
### # data:       A long-format dataframe.
### # fn_score:   A function which handles a long-format dataframe of one subject and returns a score number.
### # ncores:     The maximum cpu cores that will be used for parallel calculating.
### # fn_name:    The name of fn_score, used for generating a path for saving or read the splitted data.
### # data_name:  The name of data, also used for generating the path, since we may have more than one data sets.
### # 
### Returns:     A list of result with item 'monte_carlo_median', 'monte_carlo_lower' and 'monte_carlo_upper'.
split_monte_carlo <- function(data,fn_score,ncores,fn_name=NA,data_name=NA){
  result <- list(
    monte_carlo_median = NA,
    monte_carlo_lower = NA,
    monte_carlo_upper = NA
  ) %>% data.frame()
  tryCatch(
    {
      dir_path <- file.path(path_persistance,data_name,fn_name)
      if(!dir.exists(dir_path)){dir.create(dir_path,recursive=TRUE)}
      file_path <- file.path(dir_path, "split_monte_carlo.txt")
      
      monteCarlo <- NA
      if(file.exists(file_path)){
        monteCarlo <- read.table(file_path,header=TRUE,sep=",")
      }else{        monteCarlo <- splithalfr::by_split(
        data,
        data$subid,
        method = "random",
        replace = TRUE,
        split_p = 1,
        fn_score = fn_score,
        replications = 5000,
        ncores = ncores,
        verbose = F,
      )
      write.table(monteCarlo,file_path,sep=",")
      }
      
      # Pearson correlations
      reliability <- split_coefs(monteCarlo, cor) 
      result$monte_carlo_median <- median(reliability)
      hdi <- bayestestR::hdi(reliability,ci=0.95) %>% as.data.frame()
      result$monte_carlo_lower <- hdi$CI_low
      result$monte_carlo_upper <- hdi$CI_high
    },error=function(e){
      print(e)
    },finally={
      return(result)
    }
  )
}



### multiple indices's calculation
multi_summary <- function(data,framed_params){
  ncores = detectCores()*3/4 %>% floor() # config the max cores used for split-half
  data_name <- substitute(data)
  print(data_name)
  results <- data.frame(matrix(nrow = 0,ncol = 0))
  for(rowname in names(framed_params)){
    row_params <- framed_params[[rowname]]
    FUN <- row_params$FUN
    FUNS <- row_params$FUNS
    result <- data.frame(matrix(nrow = 1,ncol = 0))
    for(fun in FUNS){
      result <- result %>% cbind(fun(data,FUN,ncores=ncores,fn_name=rowname,data_name=data_name))
    }
    results <- results %>% rbind(result)
  }
  row.names(results) <- names(framed_params)
  return(results)
}


### all the methods used for each index
FUNS = list(general_statistic,split_first_second,split_odd_even,split_permutated,split_monte_carlo)

### multiple index functions and the methods to be used
multi_cal_params <- list(
  Number_Correct = list(
    FUN = calc_correct_n,
    FUNS = FUNS
  ),
  
  Number_pr = list(
    FUN = calc_pr_n,
    FUNS = FUNS
  ),
  Number_pr_e = list(
    FUN = calc_pr_e_n,
    FUNS = FUNS
  ),
  Number_npr_e = list(
    FUN = calc_npr_e_n,
    FUNS = FUNS
  ),
  Number_ConceptualLevelResponse = list(
    FUN = calc_conceptual_level_response_n,
    FUNS = FUNS
  ),
  Number_Category = list(
    FUN = calc_category_n,
    FUNS = FUNS
  ),
  Number_FailureToMaintainSet = list(
    FUN = calc_failure_to_maintain_set_response_n,
    FUNS = FUNS
  ),
  
  
  Percent_Correct = list(
    FUN = calc_correct_rate,
    FUNS = FUNS
  ),
  Percent_pr = list(
    FUN = calc_pr_rate,
    FUNS = FUNS
  ),
  Percent_pr_e = list(
    FUN = calc_pr_e_rate,
    FUNS = FUNS
  ),
  Percent_npr_e = list(
    FUN = calc_npr_e_rate,
    FUNS = FUNS
  ),
  Percent_ConceptualLevelResponse = list(
    FUN = calc_conceptual_level_response_rate,
    FUNS = FUNS
  ),
  Percent_Category = list(
    FUN = calc_category_rate,
    FUNS = FUNS
  ),
  Percent_FailureToMaintainSet = list(
    FUN = calc_failure_to_maintain_set_response_rate,
    FUNS = FUNS
  )
)


multi_calc_frame <- multi_summary(data=wcst_long,framed_params=multi_cal_params)

# save to file, check to see what has been saved.
write.csv(multi_calc_frame %>% round(4),file.path(path_persistance,file="multi_calc_frame.csv"))


