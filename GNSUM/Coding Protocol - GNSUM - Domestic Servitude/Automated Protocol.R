#Automate Protocol 

cal_GNSUM <- function(file_name){
  
  data_a <- type.convert(file_name, as.is = TRUE)
  sapply(data_a,class)
  
   #1
  answer1a <- length(data_a$Q5)
  data_b <- subset(data_a,Q8_a == "Filipino" )
  answer1b <- length(data_b$Q5)
  
  #2
  data_a$Q13 <- as.numeric(as.character(data_a$Q13))
  summary(data_a$Q13)
  answer2a <- sum(data_a$Q13) 
  answer2b <- sum(data_b$Q13)
  
  #3
  types_of_exploitation <- data_a |>
    dplyr::filter(Q29 %in% c("0",
                             "1",
                             "2"),
                  Q51 == "0" |
                    Q45 == "0" |
                    Q32 %in% c("0",
                               "1",
                               "2")|
                    Q46 %in% c("2",
                               "3") |
                    Q65 == "1" |
                    Q44 == "0" |
                    Q44_b == "0" |
                    Q47 %in% c("0",
                               "1",
                               "2") |
                    Q70 %in% c("0",
                               "1",
                               "2") |
                    Q42 %in% c("0",
                               "1",
                               "2") |
                    Q39 == "0" |
                    Q48 %in% c("0",
                               "1",
                               "2")|
                    Q63 %in% c("0",
                               "1",
                               "2")|
                    Q72 %in% c("2",
                               "3")|
                    Q76 %in% c("1",
                               "2",
                               "3") |
                    Q16 %in% c("6",
                               "7")|
                    Q61 %in% c("2",
                               "3")|
                    Q62 %in% c("0",
                               "1",
                               "2"))
  answer3a <- sum(types_of_exploitation $Q13)
  #=492
  #the numerator y^F,H for sample a is 492.
  
  #Sample B
  types_of_exploitation_b <- data_b |>
    dplyr::filter(Q29 %in% c("0",
                             "1",
                             "2"),
                  Q51 == "0" |
                    Q45 == "0" |
                    Q32 %in% c("0",
                               "1",
                               "2")|
                    Q46 %in% c("2",
                               "3") |
                    Q65 == "1" |
                    Q44 == "0" |
                    Q44_b == "0" |
                    Q47 %in% c("0",
                               "1",
                               "2") |
                    Q70 %in% c("0",
                               "1",
                               "2") |
                    Q42 %in% c("0",
                               "1",
                               "2") |
                    Q39 == "0" |
                    Q48 %in% c("0",
                               "1",
                               "2")|
                    Q63 %in% c("0",
                               "1",
                               "2")|
                    Q72 %in% c("2",
                               "3")|
                    Q76 %in% c("1",
                               "2",
                               "3") |
                    Q16 %in% c("6",
                               "7")|
                    Q61 %in% c("2",
                               "3")|
                    Q62 %in% c("0",
                               "1",
                               "2"))
  answer3b <- sum(types_of_exploitation_b $Q13)
  
  #4
  sum(is.na(data_a$Q13))
  answer4a <- sum(data_a$Q13) / answer1a
  
  
  sum(is.na(data_b$Q13)) 
  answer4b <- sum(data_b$Q13) / answer1b
  
  #5 
  #Answer 3 divided by Answer 4 divided by Answer 1 
  answer5a <- answer3a / (answer4a / answer1a)
  answer5b <- answer3b / (answer4b / answer1b)
  
  #6
  # Official estimate of Overseas Domestic Workers (19,780/ 2) divided by Answer 2a (size of the alter sample) multiplied by Answer 5 (individual estimates/ index of the hidden population).
  answer6a <- (19780 / 2 )/ answer2a * answer5a
  # = 61902.5
  
  # Official estimate of Overseas Domestic Workers (10,186/ 2) divided by Answer 2a (size of the alter sample) multiplied by Answer 5 (individual estimates/ index of the hidden population).
  answer6b <- (10186 / 2 )/ answer2b * answer5b
  return (answer6a)
}



