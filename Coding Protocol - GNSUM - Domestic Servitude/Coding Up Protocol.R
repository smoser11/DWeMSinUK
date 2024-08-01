getwd()
View(A_Sample_Data)

# This shows the structure of our data set
str(A_Sample_Data)
min (A_Sample_Data$Q13)
max (A_Sample_Data$Q13)
range(A_Sample_Data$Q13)
summary(A_Sample_Data$Q13)

#Change the class of variables 
sapply(A_Sample_Data,class)
data_a <- type.convert(A_Sample_Data, as.is = TRUE)
sapply(data_a,class)


#1. number of sample in Q5 = 0
  length(data_a$Q5)
  # the number of respondents is 97 for sample a.

  #Subset of Filipino respondents
  data_b <- subset(data_a,Q8_a == "Filipino" )
  length(data_b$Q5)
  # the number of respondents is 68 for sample b.


#2. sum of Q13, the sum of network 
  data_a$Q13 <- as.numeric(as.character(data_a$Q13))
  summary(data_a$Q13)
  sum(data_a$Q13) 
  # the result is 860 for sample a
  sum(data_b$Q13)
  # the result is 698 for sample b


#3.(for data_a) total number of the frame reporting types of exploitation
#Below we first calculate the total number of awareness of exploitation by type, 
#Then calculate the total number of the frame sample reporting awareness of domestic workers experiencing all types of exploitation.
  
#Abuse of vulnerability 
  library(dplyr)
  abuse_vulnerability <- data_a |>
  dplyr::filter(Q29 %in% c("0",
                          "1",
                          "2"),
                Q51 == "0")
  length(abuse_vulnerability$Q5)
  #21 respondents of sample a reported abuse of vulnerability.

  abuse_vulnerability_b <- data_b |>
    dplyr::filter(Q29 %in% c("0",
                             "1",
                             "2"),
                  Q51 == "0")
  length(abuse_vulnerability_b$Q5) 
  #16 respondents of sample b reported abuse of vulnerability.
  
#Deception
  deception <- data_a |>
  dplyr::filter(Q45 == "0")
  length(deception$Q5)
  #34 respondents of sample a reported deception.
  
  deception_b <- data_b |>
    dplyr::filter(Q45 == "0")
  length(deception_b$Q5)
  #26 respondents of sample b reported abuse of vulnerability.
  
#Restriction of movement
  restriction_movement <- data_a |>
  dplyr::filter(Q32 %in% c("0",
                          "1",
                           "2")|
                 Q46 %in% c("2",
                            "3"))
  length(restriction_movement$Q5)
  #82 respondents of sample a reported restriction of movement.
  
  restriction_movement_b <- data_b |>
  dplyr::filter(Q32 %in% c("0",
                           "1",
                           "2")|
                  Q46 %in% c("2",
                             "3"))
  length(restriction_movement_b$Q5)
  #58 respondents of sample b reported restriction of movement.
  
#Isolation
  isolation <- data_a |> dplyr::filter(Q65 == "1")
  length(isolation$Q5)
  #12 respondents of sample a reported isolation.
  
  isolation_b <- data_b |> dplyr::filter(Q65 == "1")
  length(isolation$Q5)
  #12 respondents of sample b reported isolation.
  
#Physical and sexual abuse 
  physical_sexual_abuse <- data_a |> 
  dplyr::filter(Q44 == "0" |
                Q44_b == "0")
  length(physical_sexual_abuse$Q5)
  #34 respondents of sample a reported physical and sexual abuse.
  
  physical_sexual_abuse_b <- data_b |> 
    dplyr::filter(Q44 == "0" |
                    Q44_b == "0")
  length(physical_sexual_abuse_b$Q5)
  #25 respondents of sample b reported physical and sexual abuse.
  
#Intimidation 
  intimidation <- data_a |> 
  dplyr::filter(Q47 %in% c("0",
                           "1",
                           "2"))
  length(intimidation$Q5)
  #41 respondents of sample a reported intimidation.
  
  intimidation_b <- data_b |> 
    dplyr::filter(Q47 %in% c("0",
                             "1",
                             "2"))
  length(intimidation_b$Q5)
  #35 respondents of sample b reported intimidation.
  
#Retention of identity documents 
  retention_identity_documents <- data_a |> 
  dplyr::filter(Q70 %in% c("0",
                           "1",
                           "2"))
  length(retention_identity_documents$Q5)
  #35 respondents of sample a reported retention of identity documents.
  
  retention_identity_documents_b <- data_b |> 
    dplyr::filter(Q70 %in% c("0",
                             "1",
                             "2"))
  length(retention_identity_documents_b$Q5)
  #19 respondents of sample b reported retention of identity documents.
  
#Withholding of wages 
  withholding_wages <- data_a |> 
  dplyr::filter(Q42 %in% c("0",
                           "1",
                           "2"))
  length(withholding_wages$Q5)
  #40 respondents of sample a reported withholding of wages.
  
  withholding_wages_b <- data_b |> 
    dplyr::filter(Q42 %in% c("0",
                             "1",
                             "2"))
  length(withholding_wages_b$Q5)
  #29 respondents of sample b reported withholding of wages.
  
#Debt bondage
  debt_bondage <- data_a |> dplyr::filter(Q39 == "0")
  length(debt_bondage$Q5)
  #28 respondents of sample a reported debt bondage.
  
  debt_bondage_b <- data_b |> dplyr::filter(Q39 == "0")
  length(debt_bondage_b$Q5)
  #20 respondents of sample b reported debt bondage.
  
#Abusive working and living conditions 
  abusive_working_living_conditions <- data_a |> 
  dplyr::filter(Q48 %in% c("0",
                           "1",
                           "2")|
                Q63 %in% c("0",
                           "1",
                           "2")|
                  Q72 %in% c("2",
                             "3")|
                  Q76 %in% c("1",
                             "2",
                             "3"))
  length(abusive_working_living_conditions$Q5)
  #72 respondents of sample a reported abusive working and living conditions.
  
  abusive_working_living_conditions_b <- data_b |> 
    dplyr::filter(Q48 %in% c("0",
                             "1",
                             "2")|
                    Q63 %in% c("0",
                               "1",
                               "2")|
                    Q72 %in% c("2",
                               "3")|
                    Q76 %in% c("1",
                               "2",
                               "3"))
  length(abusive_working_living_conditions_b$Q5)
  #55 respondents of sample b reported abusive working and living conditions.
  
#Excessive overtime 
  excessive_overtime <- data_a |> 
  dplyr::filter(Q16 %in% c("6",
                           "7")|
                  Q61 %in% c("2",
                             "3")|
                  Q62 %in% c("0",
                             "1",
                             "2"))
  length(excessive_overtime$Q5)
  #75 respondents of sample a reported excessive overtime.
  
  excessive_overtime_b <- data_b |> 
    dplyr::filter(Q16 %in% c("6",
                             "7")|
                    Q61 %in% c("2",
                               "3")|
                    Q62 %in% c("0",
                               "1",
                               "2"))
  length(excessive_overtime_b$Q5)
  #55 respondents of sample b reported excessive overtime.
  
#The sum of all items in the frame sample number of out reports of links to exploited domestic workers from person I.
#This is calculated in the sum of networks of people who reported an awareness to exploitation.
#Sample A
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
sum(types_of_exploitation $Q13)
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
sum(types_of_exploitation_b $Q13)
#=396
#the numerator y^F,H for sample a is 396.

  #As a side note: The how many questions are not covered here, would the network size from the how many questions matter? 
  sum(types_of_exploitation$Q13,data_a$Q14,data_a$Q21,data_a$Q25,data_a$Q33,data_a$Q33,data_a$Q35,data_a$Q43,data_a$Q49,data_a$Q64,data_a$Q68,data_a$Q71,data_a$Q74,data_a$Q77,data_a$Q79) 
  # the network size from the how many questions are 3179. 
  #The "how many" questions on the network of respondents are:
  #Q13, Q14, Q21, Q25, Q33, Q35, Q43, Q49, Q64, Q68, Q71, Q74, Q77, Q79,

  
#4.Weighted average number of connections
  #Average number of reported connections to everyone in the sample a:
  mean(data_a$Q13)
  #Mean is 8.865979
  #To check, calculate the sum of number of connections each respondent knows divided by the number of respondents who gave an answer to this question.
  sum(is.na(data_a$Q13)) #so all 97 respondents have answered this question
  sum(data_a$Q13) / 97
  # the result is 8.865979
  
  #Average number of reported connections to everyone in the sample b:
  mean(data_b$Q13)
  #Mean is 10.26471 #Filipina network is stronger
  #To check, calculate the sum of number of connections each respondent knows divided by the number of respondents who gave an answer to this question.
  sum(is.na(data_b$Q13)) #so all 68 respondents have answered this question
  sum(data_b$Q13) / 68
  # the result is 10.26471

  
#5.GNSUM according to equation 23 (Feehan and Salganik, 2014)
  #Answer 3 divided by Answer 4 divided by Answer 1 
  
  #For sample a:
  492 / 8.865979 / 97
  #The result is 0.4604651 
  #This is the estimate of the hidden population in the sample according to equation 23

  #For sample b:
  396 / 10.26471 / 68
  #The result is 0.567335
  #Filipina exploitation estimate is lower than the general sample.
  
  
#6.To calculate the absolute number of those exploited in the population

  #a) Overall sample calculation: 
  # Official estimate of Overseas Domestic Workers (19,780/ 2) divided by Answer 2a (size of the alter sample) multiplied by Answer 5 (individual estimates/ index of the hidden population).
  19780 / 2 / 860 * 0.572093 
  # = 6.579069
  # The result the absolute number of those exploited in sample a is 6.579069??
  
  #b) Overall sample calculation: 
  # Official estimate of Overseas Domestic Workers (10,186/ 2) divided by Answer 2a (size of the alter sample) multiplied by Answer 5 (individual estimates/ index of the hidden population).
  10186 / 2 / 698 * 0.567335 
  # = 4.139595
  # The result the absolute number of those exploited in sample b is 4.139595.
  

#7.Degree ratio (equation 17)
# For sample a: 
# Each of those who have been exploited (Q80: 5f12 (0,1)), the average number of their connections to members of the sample F:
degree_ratio <- data_a |> dplyr::filter(Q80 %in% c("0",
                                                   "1"))

q4_values <- unique(data_a$Q4)
q4_frequency <- table(data_a$Q4)
q4_appear <- any(data_a$Q4 %in% unlist(data_a))
q4_frequency <- table(data_a$Q4)
q4_frequency
# No phone number has been mentioned in the rest of the Frame data, which means the degree could be 0.
# We could still make the argument that the hidden populations members have, on average, fewer connection to the frame population than frame population members’?


#9. & 10. Variance estimation and confidence limits
# Need support 


#11. NRM
data_a$`Q83/88` <- as.numeric(as.character(data_a$`Q83/88`))
sum(data_a$`Q83/88`,na.rm = TRUE) / 73019
#The result for sample a is 0.001670798.

sum(data_b$`Q83/88`,na.rm = TRUE) / 73019
#The result for sample b is 0.001657103.



