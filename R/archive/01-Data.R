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

## Create comparable RDS/NSUM indicators following CE's specifications (2024-11-05)
create_comparable_indicators <- function(data) {
	library(car)
	
	# CE's mappings from most confident to least confident:
	
	# 1. Document withholding (MOST CONFIDENT): Q70 5f2 = Q71 5f3
	# Q70: Has employer withheld travel/identity documents?
	data$zQ70 <- recode(data$q70, "4=NA")  # Set don't know to NA
	data$zQ70 <- recode(data$q70, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
	
	# Q71: Know others without access to documents? (already numeric count)
	data$document_withholding_rds <- data$zQ70
	data$document_withholding_nsum <- ifelse(data$q71 > 0, 1, 0)  # Any known others = 1
	data$document_withholding_nsum[is.na(data$q71)] <- NA
	
	# 2. Pay/debt issues (HIGH CONFIDENCE): Q39 5b4 and Q42 5b7 = Q43 5b8
	# Q39: Have to pay debt to someone who helped find work?
	data$zQ39 <- recode(data$q39, "2=NA")  # Don't know to NA
	data$zQ39 <- recode(data$q39, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0
	
	# Q42: Has pay ever been withheld?
	data$zQ42 <- recode(data$q42, "4=NA")  # Don't know to NA
	data$zQ42 <- recode(data$q42, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
	
	# Q43: Know others with debt/pay problems? (already numeric count)
	data$pay_issues_rds <- ifelse(data$zQ39 == 1 | data$zQ42 == 1, 1, 0)  # Logical OR
	data$pay_issues_rds[is.na(data$zQ39) & is.na(data$zQ42)] <- NA  # NA if both missing
	data$pay_issues_nsum <- ifelse(data$q43 > 0, 1, 0)  # Any known others = 1
	data$pay_issues_nsum[is.na(data$q43)] <- NA
	
	# 3. Threats/abuse/force (HIGH CONFIDENCE): Q45 5c2 and Q47 5c4 and Q48 5c5 = Q49 5c6
	# Q45: Forced, deceived or threatened into poor conditions?
	data$zQ45 <- recode(data$q45, "2=NA")  # Prefer not to say to NA
	data$zQ45 <- recode(data$q45, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0
	
	# Q47: Has employer threatened or intimidated you?
	data$zQ47 <- recode(data$q47, "4=NA")  # Don't know to NA
	data$zQ47 <- recode(data$q47, "c(0,1,2) = 1; c(3,4)=0")  # Yes/often/sometimes = 1, never = 0
	
	# Q48: Has employer verbally abused you?
	data$zQ48 <- recode(data$q48, "4=NA")  # Don't know to NA
	data$zQ48 <- recode(data$q48, "c(0,1,2) = 1; c(3,4)=0")  # Yes often/sometimes/maybe = 1, never = 0
	
	# Q49: Know others with threat/force experiences? (already numeric count)
	data$threats_abuse_rds <- ifelse(data$zQ45 == 1 | data$zQ47 == 1 | data$zQ48 == 1, 1, 0)  # Logical OR
	data$threats_abuse_rds[is.na(data$zQ45) & is.na(data$zQ47) & is.na(data$zQ48)] <- NA  # NA if all missing
	data$threats_abuse_nsum <- ifelse(data$q49 > 0, 1, 0)  # Any known others = 1
	data$threats_abuse_nsum[is.na(data$q49)] <- NA
	
	# 4. Excessive hours (LOWER CONFIDENCE): Q61 5d8 and Q62 5d9 = Q64 5d11
	# NOTE: Q64 includes annual leave which RDS questions don't cover
	# Q61: Weekly rest longer than 24 hours consecutively?
	data$zQ61 <- recode(data$q61, "4=NA")  # Don't know to NA  
	data$zQ61 <- recode(data$q61, "c(2,3) = 1; c(0,1,4)=0")  # Sometimes/never = 1 (inadequate rest), always/often = 0
	
	# Q62: Worked overtime that felt excessive?
	data$zQ62 <- recode(data$q62, "4=NA")  # Don't know to NA
	data$zQ62 <- recode(data$q62, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
	
	# Q64: Know others with labour rights issues? (includes excessive hours + annual leave)
	data$excessive_hours_rds <- ifelse(data$zQ61 == 1 | data$zQ62 == 1, 1, 0)  # Logical OR
	data$excessive_hours_rds[is.na(data$zQ61) & is.na(data$zQ62)] <- NA  # NA if both missing
	data$excessive_hours_nsum <- ifelse(data$q64 > 0, 1, 0)  # Any known others = 1
	data$excessive_hours_nsum[is.na(data$q64)] <- NA
	
	# 5. Access to help (LEAST CONFIDENT): Q78 5f10 (coded as No) = Q79 5f11
	# Q78: Do you know who might help if not properly paid/treated? (reverse coded)
	data$zQ78 <- recode(data$q78, "1 = 0; 0 = 1")  # No = 1 (vulnerable), Yes = 0
	data$Q78rev <- data$zQ78  # Reversed version to align with Q79
	
	# Q79: Know others who don't know where to go for help? (already numeric count)
	data$access_to_help_rds <- data$Q78rev  # Higher values = more vulnerable
	data$access_to_help_nsum <- ifelse(data$q79 > 0, 1, 0)  # Any known others = 1
	data$access_to_help_nsum[is.na(data$q79)] <- NA
	
	# Additional: Known network size variable (from CE's note)
	# Q13 2f: Number of domestic workers for whom you have contact details in phone
	data$known_network_size <- as.numeric(data$q13)
	
	return(data)
}

# Apply the function to create comparable indicators
dd <- create_comparable_indicators(dd)

save(dd, rd.dd, file = "./survey/dd.RData")
