library(dplyr)
library(ggplot2)
library(survival)
library(ggfortify)
library(plotly)


## load and preprocess data
## The dataset used here is the COMPAS dataset provided by ProPublica 
#(https://github.com/propublica/compas-analysis)
f <- file.choose()
raw_data <- read.csv(f)
nrow(raw_data)

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
nrow(df)

# summary of race distribution

summary(df$race)
summary(df$score_text)
xtabs(~ score_text + race, data=df)

# Compute statistical parity: Statistical paritiy means E[d(X)|g(X)]=E[d(X)] 
#where d is a decision rule (here the assigned decile "category" 
#(low, medium high)) and g is the group membership (here race)

# Compute E[d(X)]

A <- c(mean(df$score_text=="Low"),mean(df$score_text=="Medium"),mean(df$score_text=="High"))

#Compute E[d(X)|g(X)]

AF <- c(mean(df$score_text[df$race=="African-American"]=="Low"),
        mean(df$score_text[df$race=="African-American"]=="Medium"),
        mean(df$score_text[df$race=="African-American"]=="High"))
C <- c(mean(df$score_text[df$race=="Caucasian"]=="Low"),
       mean(df$score_text[df$race=="Caucasian"]=="Medium"),
       mean(df$score_text[df$race=="Caucasian"]=="High"))
H <- c(mean(df$score_text[df$race=="Hispanic"]=="Low"),
       mean(df$score_text[df$race=="Hispanic"]=="Medium"),
       mean(df$score_text[df$race=="Hispanic"]=="High"))


Scores <- c("a.Low_Score","b.Medium_Score","c.High_Score")
data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Measure of Statistical Parity",yaxis = list(title = 'Percentage expected in scoring category',range=c(0,1)), barmode = 'group')



#Compute Conditional Statistical parity: Mathematically: E[d(X)|(l(X),g(X)] = E[d(X),l(X)] 
#where d is a decision roule (here the assigned decile "category" (low, medium high)) 
#and g is the group membership (here race), l is a projection of all the attributes to 
#the "legitimate" attributes. Legitimate attribute chosen in this case: prior convictions


#plot a histogram of the number of prior convictions, choose to split into three groups accordingly
hist(df$priors_count,freq=FALSE, xlab='Prior Convictions',ylab='Frequency', 
     main='Distribution of Prior Convictions', col='lightgreen')

# 3 groups: low number of prior convictions: LNP (priors_count in [0,2])
#           medium number of prior convictions: MNP (priors_count in [3,10])
#           high number of prior convictions: HNP (prior_count in [11,inf])

#split into three subdatasets according to number-of-prior-conviction-groups

Low_conv<- subset(df, priors_count <3)

Med_conv <- subset(df,priors_count > 2 & priors_count <11)

High_conv <- subset(df,priors_count >10)

# look at Low_conv: 

# E[d(X)|l(X)]
A <- c(mean(Low_conv$score_text=="Low"),
       mean(Low_conv$score_text=="Medium"),
       mean(Low_conv$score_text=="High"))

#E[d(X)|l(X),g(X)]
AF <- c(mean(Low_conv$score_text[Low_conv$race=="African-American"]=="Low"),
        mean(Low_conv$score_text[Low_conv$race=="African-American"]=="Medium"),
        mean(Low_conv$score_text[Low_conv$race=="African-American"]=="High"))

C <- c(mean(Low_conv$score_text[Low_conv$race=="Caucasian"]=="Low"),
       mean(Low_conv$score_text[Low_conv$race=="Caucasian"]=="Medium"),
       mean(Low_conv$score_text[Low_conv$race=="Caucasian"]=="High"))

H <- c(mean(Low_conv$score_text[Low_conv$race=="Hispanic"]=="Low"),
       mean(Low_conv$score_text[Low_conv$race=="Hispanic"]=="Medium"),
       mean(Low_conv$score_text[Low_conv$race=="Hispanic"]=="High"))

Scores <- c("a.Low_Score","b.Medium_Score","c.High_Score")

data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Conditional Statistical Parity: Rates with few prior arrests (0-2)",yaxis = list(title = 'Expected percentage in scoring category',range=c(0,1)), barmode = 'group')


# look at Med_conv: 

# E[d(X)|l(X)]

A <- c(mean(Med_conv$score_text=="Low"),
       mean(Med_conv$score_text=="Medium"),
       mean(Med_conv$score_text=="High"))

#E[d(X)|l(X),g(X)]

AF <- c(mean(Med_conv$score_text[Med_conv$race=="African-American"]=="Low"),
        mean(Med_conv$score_text[Med_conv$race=="African-American"]=="Medium"),
        mean(Med_conv$score_text[Med_conv$race=="African-American"]=="High"))

C <- c(mean(Med_conv$score_text[Med_conv$race=="Caucasian"]=="Low"),
       mean(Med_conv$score_text[Med_conv$race=="Caucasian"]=="Medium"),
       mean(Med_conv$score_text[Med_conv$race=="Caucasian"]=="High"))

H <- c(mean(Med_conv$score_text[Med_conv$race=="Hispanic"]=="Low"),
       mean(Med_conv$score_text[Med_conv$race=="Hispanic"]=="Medium"),
       mean(Med_conv$score_text[Med_conv$race=="Hispanic"]=="High"))

data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Conditional Statistical Parity: Rates with \n medium prior arrests (3-10)",yaxis = list(title = 'Expected percentage in scoring category',range=c(0,1)), barmode = 'group')

# look at High_conv: 

# E[d(X)|l(X)]
A <- c(mean(High_conv$score_text=="Low"),
       mean(High_conv$score_text=="Medium"),
       mean(High_conv$score_text=="High"))

#E[d(X)|l(X),g(X)]
AF <- c(mean(High_conv$score_text[High_conv$race=="African-American"]=="Low"),
        mean(High_conv$score_text[High_conv$race=="African-American"]=="Medium"),
        mean(High_conv$score_text[High_conv$race=="African-American"]=="High"))

C <- c(mean(High_conv$score_text[High_conv$race=="Caucasian"]=="Low"),
       mean(High_conv$score_text[High_conv$race=="Caucasian"]=="Medium"),
       mean(High_conv$score_text[High_conv$race=="Caucasian"]=="High"))

H <- c(mean(High_conv$score_text[High_conv$race=="Hispanic"]=="Low"),
       mean(High_conv$score_text[High_conv$race=="Hispanic"]=="Medium"),
       mean(High_conv$score_text[High_conv$race=="Hispanic"]=="High"))

data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Conditional Statistical Parity: Rates with high prior arrests (11+)",yaxis = list(title = 'Expected percentage in scoring category',range=c(0,1)), barmode = 'group')


# Compute predictive equality

#Consider two year recidivism rate. Split the dataset in two groups, recidivated or not recidivated.
#Predictive equality means mathematically: E[d(X)|Y=0,g(X)] = E[d(X)|Y=0] where d is a decision roule 
# (here the assigned decile "category" (low, medium high)) and g is the group membership (here race) 
# and Y is binary for recidivated/not recidivated. We compute false positivse and false negatives

# create subdatasets

No_rec<- subset(df, two_year_recid==0)

Rec<- subset(df,  two_year_recid==1)

# look at No_rec: 

# E[d(X)|Y=0]

A <- c(mean(No_rec$score_text=="Low"),
       mean(No_rec$score_text=="Medium"),
       mean(No_rec$score_text=="High"))


#E[d(X)|l(X),g(X)]

AF <- c(mean(No_rec$score_text[No_rec$race=="African-American"]=="Low"),
        mean(No_rec$score_text[No_rec$race=="African-American"]=="Medium"),
        mean(No_rec$score_text[No_rec$race=="African-American"]=="High"))

C <- c(mean(No_rec$score_text[No_rec$race=="Caucasian"]=="Low"),
       mean(No_rec$score_text[No_rec$race=="Caucasian"]=="Medium"),
       mean(No_rec$score_text[No_rec$race=="Caucasian"]=="High"))

H <- c(mean(No_rec$score_text[No_rec$race=="Hispanic"]=="Low"),
       mean(No_rec$score_text[No_rec$race=="Hispanic"]=="Medium"),
       mean(No_rec$score_text[No_rec$race=="Hispanic"]=="High"))

Scores <- c("a.Low_Score","b.Medium_Score","c.High_Score")
data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Predictive equality - scoring among NOT recidivated defendants",yaxis = list(title = 'Expected percentage in scoring category',range=c(0,1)), barmode = 'group')

# look at Rec: 

# E[d(X)|Y=1]
A <- c(mean(Rec$score_text=="Low"),
       mean(Rec$score_text=="Medium"),
       mean(Rec$score_text=="High"))


#E[d(X)|l(X),g(X)]
AF <- c(mean(Rec$score_text[Rec$race=="African-American"]=="Low"),
        mean(Rec$score_text[Rec$race=="African-American"]=="Medium"),
        mean(Rec$score_text[Rec$race=="African-American"]=="High"))

C <- c(mean(Rec$score_text[Rec$race=="Caucasian"]=="Low"),
       mean(Rec$score_text[Rec$race=="Caucasian"]=="Medium"),
       mean(Rec$score_text[Rec$race=="Caucasian"]=="High"))

H <- c(mean(Rec$score_text[Rec$race=="Hispanic"]=="Low"),
       mean(Rec$score_text[Rec$race=="Hispanic"]=="Medium"),
       mean(Rec$score_text[Rec$race=="Hispanic"]=="High"))

data <- data.frame(Scores,AF,C,H,A)

#plotting
plot_ly(data, x = ~Scores, y = ~AF, type = 'bar', name = 'Afican-American') %>%
  add_trace(y = ~C, name = 'Caucasian') %>% add_trace(y = ~H, name = 'Hispanic') %>% add_trace(y = ~A, name = 'All') %>%
  layout(title = "Predictive equality - scoring among recidivated defendants",yaxis = list(title = 'Expected percentage in scoring category',range=c(0,1)), barmode = 'group')


#calibration

# Mathematically: Proportion(Y=1|s(X),g(X)) = Proportion(Y=1|s(X)) where Y=1 means recidivated,
# g expresses the group membership (here race) and s is the given riskscore

# Split in datasets according to decile score

Low<- subset(df, score_text =="Low")

Med<- subset(df, score_text =="Medium")

High<- subset(df, score_text =="High")

#compute rate of recidivism for each race


Low_Score <- c(sum(Low$two_year_recid == 1 & Low$race == "African-American")/sum(Low$race == "African-American"), 
               sum(Low$two_year_recid == 1 & Low$race == "Caucasian")/sum(Low$race == "Caucasian"),
               sum(Low$two_year_recid == 1 & Low$race == "Hispanic")/sum(Low$race == "Hispanic"))
Medium_Score <- c(sum(Med$two_year_recid == 1 & Med$race == "African-American")/sum(Med$race == "African-American"),
                  sum(Med$two_year_recid == 1 & Med$race == "Caucasian")/sum(Med$race == "Caucasian"), 
                  sum(Med$two_year_recid == 1 & Med$race == "Hispanic")/sum(Med$race == "Hispanic"))
High_Score <- c(sum(High$two_year_recid == 1 & High$race == "African-American")/sum(High$race == "African-American"),
                sum(High$two_year_recid == 1 & High$race == "Caucasian")/sum(High$race == "Caucasian"),
                sum(High$two_year_recid == 1 & High$race == "Hispanic")/sum(High$race == "Hispanic"))

Races <- c("African-American","Caucasian","Hispanic")
data <- data.frame(Races,Low_Score,Medium_Score,High_Score)


#plotting
plot_ly(data, x = ~Races, y = ~Low_Score, type = 'bar', name = 'Low Decile Score') %>%
  add_trace(y = ~Medium_Score, name = 'Medium Decile Score') %>% add_trace(y = ~High_Score, name = 'High Decile Score') %>%
  layout(title = "Calibration Measure (recidivism after 12 month)",yaxis = list(title = 'percentage recidivated after 2 years',range=c(0,1)), barmode = 'group')


