rm(list=ls())
library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
library(gtools)

# setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/RightsLab/GNSUM-CEandS")
# setwd("~/Dropbox/research/RightsLab/DWeMSinUK/survey")

### DATA WORK:


dir()
library(janitor)

names(data)
data <- read.csv("./survey/UpdateSelimRiskIndex-sum_cat.csv")

data <- data %>% select(-contains("column"))
data <- clean_names(data)
data$q13 <- as.numeric(data$q13)
names(data)
data$recruiter.id <- as.numeric(data$node_1_recruiter)
data<- data %>% select(recruiter.id, q13, starts_with("node_"),  everything())

data$rowNum <- rownames(data)
names(data)
data$id <- data$node_2_id_respondent_recruit
data$ridc <- data$node_1_recruiter
data$recruiter.id[is.na(data$recruiter.id)] <- -1
data<- data %>% select(rowNum, ridc, recruiter.id,  id, q13, everything())
data$ridc[is.na(data$ridc)] <- "seed"

sort(names(data))

library(RDS)
data$network.size.variable <- as.numeric(data$q13)
sort(names(data))
write.csv(data, file = "./survey/data.csv", row.names = FALSE)
as.rds.data.frame(data)


## deal with zeros: fix if incorrect; remove if 'true zero' -- TAKE TWO
### Fix if incorrect:
df <- data

df_processed <- df %>%
	rowwise() %>%
	mutate(NonEmptyCount = sum(!is.na(c_across(q105:q115))),
		   NonEmptyValues = list(na.omit(c_across(q105:q115)))) %>%
	ungroup()


library(dplyr)

# Assuming 'data' is your original data frame
# Step 1: Count occurrences of each id in 'node_1_recruiter'
count_df <- df_processed %>%
	group_by(node_1_recruiter) %>%
	summarise(referedFreq = n(), .groups = 'drop')

# Step 2: Join this information back to the original data frame
df_processed <- df_processed %>%
	left_join(count_df, by = c("id" = "node_1_recruiter"))

# If any id does not appear in 'node_2_id_respondent_recruit', it will have NA in 'referedFreq'
# Replace NA with 0 to indicate 0 occurrences
df_processed$referedFreq[is.na(df_processed$referedFreq)] <- 0



data <- df_processed %>%
	mutate(suspicious_variable = ifelse(NonEmptyCount > q13 | referedFreq > q13, 1, 0),
		   numRef = pmax(NonEmptyCount, q13, referedFreq)) %>% select(numRef, NonEmptyCount, referedFreq,  q13, suspicious_variable, everything())

# dd$network.size.variable <- dd$q13
data$network.size.variable <- as.numeric(data$numRef)
data$network.size <- data$numRef

### Remove if 'true zero'

dd <- data %>% filter(numRef >0)

as.rds.data.frame(dd)

table(dd$recruiter.id, dd$node_1_recruiter, useNA = 'always')
table(data$recruiter.id, data$node_1_recruiter, useNA = 'always')


## Make long, sure why not.

df_long <- data %>%
	mutate(id = row_number()) %>%
	unnest(NonEmptyValues) %>%
	select(id, NonEmptyCount, NonEmptyValues, everything())

write.csv(df_long, "./survey/long_format_data.csv", row.names = FALSE)




## This removes the D/K
library(car)
unique(dd$q36)
dd$zQ36 <- recode(dd$q36, "5=NA")
unique(dd$zQ36)
dd$zQ36 <- recode(dd$q36, "c(0,1,2,3) = 1; c(4,5)=0")
unique(dd$q80)
dd$zQ80 <- recode(dd$q80, "4=NA")
dd$zQ80 <- recode(dd$q80, "c(0,1) = 1; c(2,3,4,5)=0")

summary(dd$zQ36)
unique(dd$q80)
library(psych)
describe(as.numeric(dd$q80) )

summary(dd$zQ80)


summary(dd$zQ36)
unique(dd$q80)
library(psych)
describe(as.numeric(dd$q80) )

summary(dd$zQ80)



#####--
## Use updated sum_categories2 (!!!)
library(psych)
names(dd)
library(car)
dd$sum_categories_factor <- as.factor(dd$sum_categories)
dd$sum_categories_cut <- cut_interval(dd$sum_categories, n = 10)
table(dd$sum_categories_cut)
describe(dd$sum_categories)

sort(names(dd))
# dd <- dd %>% select(id, recruiter.id, wave, network.size.variable, degree, everything())

rd.dd <- as.rds.data.frame(dd, id="id", recruiter.id="recruiter.id", max.coupons = 5, check.valid = FALSE)

dd$wt.RDS1_zQ36 <- rds.I.weights(rd.dd, "zQ36")
dd$wt.RDS1_zQ80 <- rds.I.weights(rd.dd, "zQ80")
dd$wt.RDS1_sum_categories_cut <- rds.I.weights(rd.dd, "sum_categories_cut")
dd$wt.RDS1_sum_categories_factor <- rds.I.weights(rd.dd, "sum_categories_factor")

dd$wt.vh_980k <- vh.weights(dd$numRef, N= 980000)
dd$wt.vh_100k <- vh.weights(dd$numRef, N= 100000)
dd$wt.vh_050k <- vh.weights(dd$numRef, N= 50000)

dd$wt.SS_980k <- gile.ss.weights(dd$numRef, N= 980000)
dd$wt.SS_100k <- gile.ss.weights(dd$numRef, N= 100000)
dd$wt.SS_050k <- gile.ss.weights(dd$numRef, N= 50000)


# dd$wt.RDS1_sum_categories_cut <- rds.I.weights(rd.dd, "sum_categories_cut")
# dd$wt.RDS1_sum_categories_factor <- rds.I.weights(rd.dd, "sum_categories_factor")


df <- apply(dd,2,as.character)
write.csv(as.data.frame(df), file = "./survey/dd.csv", row.names = FALSE)

rd.dd <- as.rds.data.frame(dd, id="id", recruiter.id="recruiter.id", max.coupons = 5, check.valid = FALSE)
save(dd, rd.dd, file = "./survey/dd.RData")
