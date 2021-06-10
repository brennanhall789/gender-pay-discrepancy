library(readxl)
library(tidyverse)
library(lubridate)

rawdata <- read_excel("testData20210413_alt.xlsx")

## Data prep
str(rawdata)
rawdata %>% dplyr::filter(exempt=="Non-Exempt" & overtimeHours==0)  ## only one case so consider exempt as redundant variable
rawdata$yearsInPos <- year(as_date(now())) - year(rawdata$posStartDate)   ## consider time in position, not date
data <- dplyr::select(rawdata,-c(employeeID,jobTitle,birthdate,state,zipcode,
                                 businessLine,fullPart,exempt,posStartDate))  ## removing unnecessary or redundant variables

## declare variables as factors
factor_vars <- c('jobLevel','department','gender',
                 'EstNam','remote','eeo1','rehire',
                 'reclass','perform1','division')
data[,factor_vars] <- lapply(data[,factor_vars], factor)

data %>% select_if(~is.numeric(.x)) %>% cor()
## Notes:
# 1. overtimeHours has large maximum considering it is 
# about half of the max of regHours.
# 2. relatively high correlation b/w (baseRate,overtimeHours) and (regHours,overtimeHours)
