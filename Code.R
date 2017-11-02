library(tidyverse)
library(VIM)
library(devtools)
library(woe)
library(data.table)

setwd("C:/Users/manje/Documents/R/StatisticalLearning")

#import dataset for modelling
raw_data <- read.csv("Data/PL_XSELL.csv")

View(raw_data)
summary(raw_data)
str(tib_raw_data)

tib_raw_data <- as.tibble(raw_data)

summary(tib_raw_data)

#Transform Categorical Variables


#Check if there are any missing values
aggr_plot <- aggr(raw_data, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

#Exploratory Data Analysis

#Percentile Distributions
#for all the fields
apply(raw_data[,sapply(raw_data, is.numeric)], 
      2, quantile, 
      probs=c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
      na.rm=T)

#nformation Value
iv.plot.summary(iv.mult(raw_data[,!names(raw_data) %in% c("CUST_ID")],
                        "TARGET",TRUE, verbose = TRUE))

iv <- iv.mult(raw_data[,!names(raw_data) %in% c("CUST_ID")],
              "TARGET",summary = TRUE,
              verbose = FALSE)

iv

#Box PLot
boxplot(raw_data$HOLDING_PERIOD , 
        main= "Holding Period Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$NO_OF_L_DR_TXNS , 
        main= "Number of Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$TOT_NO_OF_L_TXNS , 
        main= "Total Number of Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$NO_OF_ATM_DR_TXNS , 
        main= "Number of ATM Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$NO_OF_L_CR_TXNS , 
        main= "Number of Credit Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$NO_OF_CHQ_DR_TXNS , 
        main= "Number of Cheque Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$AMT_CHQ_DR , 
        main= "Amount Debited By Cheque Transactions Box Plot" ,
        xlab = "Overall Base"
)

boxplot(raw_data$AMT_L_DR , 
        main= "Total Amount Debited Box Plot" ,
        xlab = "Overall Base"
)

#Flooring and capping
capped_data <- raw_data

capped_data$NO_OF_L_DR_TXNS <- 
  ifelse(capped_data$NO_OF_L_DR_TXNS > 14, 14, capped_data$NO_OF_L_DR_TXNS)
boxplot(capped_data$NO_OF_L_DR_TXNS , 
        main= "Treated Number of Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$TOT_NO_OF_L_TXNS <- 
  ifelse(capped_data$TOT_NO_OF_L_TXNS > 39, 39, capped_data$TOT_NO_OF_L_TXNS)
boxplot(capped_data$TOT_NO_OF_L_TXNS , 
        main= "Treated Total Number of Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$NO_OF_ATM_DR_TXNS <- 
  ifelse(capped_data$NO_OF_ATM_DR_TXNS > 2, 2, capped_data$NO_OF_ATM_DR_TXNS)
boxplot(capped_data$NO_OF_ATM_DR_TXNS , 
        main= "Treated Number of ATM Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$NO_OF_L_CR_TXNS <- 
  ifelse(capped_data$NO_OF_L_CR_TXNS > 26, 26, capped_data$NO_OF_L_CR_TXNS)
boxplot(capped_data$NO_OF_L_CR_TXNS , 
        main= "Treated Number of Credit Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$NO_OF_CHQ_DR_TXNS <- 
  ifelse(capped_data$NO_OF_CHQ_DR_TXNS > 10, 10, capped_data$NO_OF_CHQ_DR_TXNS)
boxplot(capped_data$NO_OF_CHQ_DR_TXNS , 
        main= "Treated Number of Cheque Debit Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$AMT_CHQ_DR <- 
  ifelse(capped_data$AMT_CHQ_DR > 180580, 180580, capped_data$AMT_CHQ_DR)
boxplot(capped_data$AMT_CHQ_DR , 
        main= "Amount Debited By Cheque Transactions Box Plot" ,
        xlab = "Overall Base"
)

capped_data$AMT_L_DR <- 
  ifelse(capped_data$AMT_L_DR > 2340134, 2340134, capped_data$AMT_L_DR)
boxplot(capped_data$AMT_L_DR , 
        main= "Treated Total Amount Debited Box Plot" ,
        xlab = "Overall Base"
)

#Pattern Detection
source("Source/Visualization.R")
output_folder = "Output/"
Target_var_name = "TARGET"

#Find all the numeric variables
col_list = colnames(capped_data)[
  lapply(capped_data, class) %in% c("numeric", "integer")
  ]
col_list
#See patterns in data
for (i in 1 : length(col_list)) {
  fn_biz_viz(df = capped_data, target = Target_var_name, var = col_list[i])
}


LR_DF$DV_Age <- ifelse(capped_data$Age > 43, 43 - (LR_DF$Age - 43), LR_DF$Age)
