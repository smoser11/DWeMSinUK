###############################################
   ####### log file for Khoury RDS #######
###############################################

library(RDS)
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(scales)

rds_data <- read_excel("Google Drive/Documents/R Directory/rds_replication_data.xlsx")

getwd()

rds_data <- read_excel("~/Users/Dropbox/research/DWeMSinUK2/Hard-to-Survey Populations and Respondent-Driven Sampling/rds_replication_data.xlsx")

rds_data <- read_excel("../../../DWeMSinUK2/Hard-to-Survey Populations and Respondent-Driven Sampling/rds_replication_data.xlsx")


####### Convert to RDS Data Frame #########
rds_data <- as.data.frame(rds_data)
#create recruiter ID  w/ recruit's non-identifying ID & 3 non-identifying IDs ("coupons") distributed to them for recruitment
rds_data$recruiter.id <- rid.from.coupons(rds_data, "recruit.id",  paste0("coupon.",1:3),"id")
#convert to RDS data frame
rds_df <- as.rds.data.frame(rds_data, "id", "recruiter.id", "degree", 1500, 3)
assert.valid.rds.data.frame(rds_df)

#### Pre-Analysis Data Manipulation ####
#change categorical variables 
rds_df$sex<- as.factor(rds_df$sex)
rds_df$education<- as.factor(rds_df$education)
rds_df$trust <- as.factor(rds_df$trust)
rds_df$cooperation<-as.factor(rds_df$cooperation)
rds_df$cooperation_location<-as.factor(rds_df$cooperation_location)
rds_df$formality<-as.factor(rds_df$formality)
rds_df$syria_pre2011<-as.factor(rds_df$syria_pre2011)
rds_df$syria_post2011<-as.factor(rds_df$syria_post2011)
rds_df$humanitarian<-as.factor(rds_df$humanitarian)
rds_df$advocacy<-as.factor(rds_df$advocacy)
rds_df$development<-as.factor(rds_df$development)
rds_df$media<-as.factor(rds_df$media)
rds_df$protest<-as.factor(rds_df$protest)
rds_df$fundraising<-as.factor(rds_df$fundraising)
rds_df$institutions<-as.factor(rds_df$institutions)
rds_df$parties<-as.factor(rds_df$parties)
rds_df$most_recent<-as.factor(rds_df$most_recent)

#Create Age Group Variable given respondent age 
rds_df$agecat<-findInterval(rds_df$age, c(18, 24, 30, 36, 100), left.open = F)
rds_df$agecat<-as.factor(rds_df$agecat)


####### Recruitment Tree #########
plot(rds_df)


##### Degree #####
#median degree
summary(rds_df$degree)

str(rds_df)
####### Population Estimates ##########
#Giles Successive Sampling Estimates on demographic characteristics as in Table 4
pop.sex<-RDS.SS.estimates(rds_df, "sex")
pop.edu<-RDS.SS.estimates(rds_df, "education")
pop.age<-RDS.SS.estimates(rds_df, "agecat")
#additional estimates
pop.trust <- RDS.SS.estimates(rds_df, "trust")
pop.cooperation<-RDS.SS.estimates(rds_df, "cooperation")
pop.uprisingactivism <- RDS.SS.estimates(rds_df, "syria_post2011")
pop.formal <- RDS.SS.estimates(rds_df, "formality")

pop.trust

#varieties of activism in which respondents "have" participated since arriving in Jordan
rds_df$humanitarian1<-ifelse(rds_df$humanitarian==1, 1,0)
humanitarian1.est<-RDS.SS.estimates(rds_df, "humanitarian1")

rds_df$advocacy1<-ifelse(rds_df$advocacy==1, 1,0)
advocacy1.est<-RDS.SS.estimates(rds_df, "advocacy1")

rds_df$development1<-ifelse(rds_df$development==1, 1,0)
development1.est<-RDS.SS.estimates(rds_df, "development1")

rds_df$media1<-ifelse(rds_df$media==1, 1,0)
media1.est<-RDS.SS.estimates(rds_df, "media1")

rds_df$protest1<-ifelse(rds_df$protest==1, 1,0)
protest1.est<-RDS.SS.estimates(rds_df, "protest1")

rds_df$fundraising1<-ifelse(rds_df$fundraising==1, 1,0)
fundraising1.est<-RDS.SS.estimates(rds_df, "fundraising1")

rds_df$institutions1<-ifelse(rds_df$institutions==1, 1,0)
institutions1.est<-RDS.SS.estimates(rds_df, "institutions1")

rds_df$party1<-ifelse(rds_df$parties==1, 1,0)
party1.est<-RDS.SS.estimates(rds_df, "party1")

#most recent activism 
mostrecent.est <- RDS.SS.estimates(rds_df, "most_recent")

###### Figure 2 Varieties of Activism #####

#Engagement in activism
have.participated.df <- data.frame(
  activism = c("Humanitarian Relief", "Advocacy", "Development", "Media", "Protest", "Fundraising", "Institutions", "Political Parties"),
  Engagement = "Have Participated",
  participated=c(63.52, 36.72, 54.39, 49.37, 15.89, 41.57, 13.93, 2.52))

recent.df<-data.frame(
  activism = c("Humanitarian Relief", "Advocacy", "Development", "Media", "Protest", "Fundraising", "Institutions", "Political Parties"),
  Engagement ="Most Recent Participation", 
  participated=c(33.75, 6.49, 22.98, 23.33, NA, 5.02, 1.47, 0.35))

#create combined dataframe and add row for standard error
df<-bind_rows(have.participated.df, recent.df)
df$sd<-c(7.31, 6.4, 7.2, 7.39, 4.97, 7.28, 4.94, 1.82, 6.26, 3.68, 5.32, 6.12, NA, 4.77, 0.25, 0.31)

#plot 
plot.activism<-df %>% 
  ggplot(aes(activism, participated)) + 
  geom_col(aes(group=Engagement, fill=Engagement), position="dodge", width = 0.8) + 
  geom_errorbar(aes(ymin=participated-sd, ymax=participated+sd, group=Engagement), width=0.1,
                position=position_dodge(.9)) + scale_y_continuous((limits=c(0,NA)))+
  coord_flip()

plot.activism +theme_bw(base_size=15)+ scale_fill_grey(start = .3, end = .7,  labels = c("Prior Forms of Engagement", "Most Recent Form of Engagement")) +
  labs(fill = " ")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


######### Diagnostics ############
homophily <- homophily.estimates(rds_df, "sex", recruitment = T)
print(homophily)

convergence.plot(rds_df, "sex")


######### Bias test ############

#Database for iterations
rds_iterations<-rds_df

#Values
sd_value<-sd(rds_iterations$degree) #sd of degree
n<-as.numeric(nrow(rds_iterations)) #Number of rows

#compute RDS weights
rds_iterations$weights <- compute.weights(rds_iterations, N=1500)

#Models to calculate MSE 
rds_iterations$syria_post2011_it <- as.numeric(rds_iterations$syria_post2011)
true_model<-lm(syria_post2011_it~age, weights=weights, data=rds_iterations)

#Models with Noise
#create empty matrix
noisy_model_coeffs<-as.data.frame(matrix(ncol=1000, nrow = 100)) 


#Loops: for all values from i 1:1000 (param values), it repeats 100 times (j). 
for (i in 1:1000){ #Varies the error term 
  for (j in 1:100){ #Repeats for each parameter value
    #Generates noisy variable
    rds_iterations$error<-0 #Creating vector for error. 
    rds_iterations$error<-rnorm(n, mean=0, sd=sd_value/i)
    rds_iterations$noisy<-rds_iterations$error+rds_iterations$weights
    rds_iterations$noisy<-ifelse(rds_iterations$noisy<0,0,rds_iterations$noisy) #replaces negative for 0 b/c weights cannot be negative 
    noisy_model<-lm(syria_post2011_it~age, weights=noisy, data=rds_iterations)
    noisy_model_coeffs[j,i] <- noisy_model$coefficients[2] 
  }
}
summary(noisy_model)

#Estimates Mean Squared Error
MSE<-NULL

#store age coefficient (intercept is 1st, relevant variable is 2nd)
true<-true_model$coefficients[2]
for (i in 1:1000){ #Varies the error term 
  MSE[i]<-mean((noisy_model_coeffs[,i]-true)^2) #Calculates the mean for all iterations.
}

#Data for making graph
data_forgraph<-NULL
data_forgraph$MSE<-MSE
data_forgraph$x<-c(1:1000)
data_forgraph$noise<-sd_value/data_forgraph$x
data_forgraph<-as.data.frame(data_forgraph)

#Plot
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)
data_forgraph %>% ggplot(aes(x=noise, y=MSE)) +  geom_point() +  theme_bw()+
  scale_y_continuous(labels = point)

