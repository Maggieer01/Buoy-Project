library(tidyverse)
library(ggplot2)
library(tidyr)
library(ggpubr)


### make URLs

url1 <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1998:2018)

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("buoy", years, sep = "")

###  read the data from the website

N <- length(urls)

for (i in 1:N){
  if (i <= 7) {
    suppressMessages(  ###  this stops the annoying messages on your screen.
      assign(filenames[i], read_table(urls[i], col_names = c("YYYY","MM","DD","hh",
                                                             "WDIR","WSPD", "GST","WVHT","DPD","APD",
                                                             "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE"), skip = 1)
  ))}
  else {
  suppressMessages(  ###  this stops the annoying messages on your screen.  
    assign(filenames[i], read_table(urls[i], col_names = c("YYYY","MM","DD","hh","mm",
                                    "WDIR","WSPD", "GST","WVHT","DPD","APD",
                                    "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE"), skip = 2))
  
    )}
    
  file <- get(filenames[i])
  
  ### name the first column "YYYY"
  colnames(file)[1] <-"YYYY"  
  
  # put '19' in front of 98 since there are only 2-digits
  
  if (i == 1) {
    file[i] = file[i] + 1900
    
  }
  
  # no data under 'TIDE' column in 1998, 1999 and 2000, filling with 0.
  
  if (i <= 3) {
    file[,'TIDE'] = 0
  }
  
  # no 'mm' column before year2005, filling with 0.
  
  if (i <= 7) {
    file[,'mm'] = 0
  }
  
  # combine data from 1998 to 2018 into one data frame called "MR"
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  
  }
}

# get rid of values such as '99' and '999', which are meaningless

MR <- filter(MR, MR$ATMP < 50)

# turn MM from character into numeric

MR$MM <- as.numeric(MR$MM) 
 
# filter data frame and only use data at 2pm
two_pm <- MR[MR$hh %in% c("14"), ]

# find the average air temperature of each month in each year from
# 1998 to 2018. Put all average numbers of temperature into one 
# data frame called "data_frame"

for (i in 1998:2018){
  year <- two_pm[two_pm$YYYY %in% c(i),]
  for (j in 1:12) {
    month <- year[year$MM %in% c(j),]
    if (j == 1){
      ave = mean(month$ATMP)
    } else {
      ave <- rbind(ave, mean(month$ATMP))
    }
  }
  if (i == 1998) {
    data_frame <- rbind.data.frame("year" = i, "ave_temp" <- ave)
  } else {
    data_frame <- cbind.data.frame(data_frame, rbind.data.frame("year" = i, "ave_temp" <- ave))
  }
}

# switch the order of column and rows in data_frame, calling the new data frame
# "final.df"

final.df <- as.data.frame(t(data_frame))

# assign names to each month, one column stands for one month, the first
# column stands for "year"

colnames(final.df) = c("year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# plot for the average air temperature in each month from year 1998 to 2018

jan <-ggplot(data=final.df, aes(x=year, y=Jan)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "January") + geom_smooth(method = lm)


feb <- ggplot(data=final.df, aes(x=year, y=Feb)) +
  geom_point()+
  labs(x="Year", y="ATMP", title= "February") +geom_smooth(method = lm)


mar <-ggplot(data=final.df, aes(x=year, y=Mar)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "March") + geom_smooth(method = lm)


apr <-ggplot(data=final.df, aes(x=year, y=Apr)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "April") +geom_smooth(method = lm)


may <-ggplot(data=final.df, aes(x=year, y=May)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "May") +geom_smooth(method = lm)


jun <-ggplot(data=final.df, aes(x=year, y=Jun)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "June")+geom_smooth(method = lm)


jul <-ggplot(data=final.df, aes(x=year, y=Jul)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "July") +geom_smooth(method = lm)


aug <-ggplot(data=final.df, aes(x=year, y=Aug)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "August") +geom_smooth(method = lm)


sep <-ggplot(data=final.df, aes(x=year, y=Sep)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "September") +geom_smooth(method = lm)


oct <-ggplot(data=final.df, aes(x=year, y=Oct)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "October") +geom_smooth(method = lm)


nov <-ggplot(data=final.df, aes(x=year, y=Nov)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "November") +geom_smooth(method = lm)


dem <-ggplot(data=final.df, aes(x=year, y=Dec)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "December") +geom_smooth(method = lm)

# arrange 12 plots together in one page, making the result more intuitive.

figure <- ggarrange(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dem,
                    ncol = 3, nrow = 4)
print(figure)



