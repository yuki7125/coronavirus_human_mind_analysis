library(tidyr)
library(dplyr)

df <- read.csv("/User/apple/Desktop/coronavirus-dataset-update-0206/2019-nCoV-cases-JHU.csv")
df <- df[c("Date","Region", "Confirmed", "Deaths", "Recovered")]

date_format <- function(x){
  y <- unlist(strsplit(x,"/"))
  return( paste(y[1], '/', y[2], '/2020', sep='') )
}

update <- df$Date
update <- unlist(lapply(update, toString))
update <- unlist(lapply(update, date_format))
df$Date <- update

list_date = vector(mode = "list", length = length(df$Date))
list_reg = vector(mode = "list", length = length(df$Region))
  
for(i in 1:length(df$Date)){
  if(df$Date[i] %in% list_date[1:i]) 
  {
    if(df$Region[i] %in% list_reg[1:i]){
    next
  }
    }
  list_date[i] = df$Date[i]
  list_reg[i] = df$Region[i]
}

df <- df %>% group_by(Region, Date) %>%
  summarize(Confirmed = sum(Confirmed), 
            Recovered = sum(Recovered),
            Deaths = sum(Deaths))%>%ungroup()

df[is.na(df)] <- 0

``