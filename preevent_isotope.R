#########Prcipitation vs discharge and sampling time Upper Snake Gauge#####
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(dataRetrieval)
#Calling the upper snake precipitation from disk
upper.snake.precip.month <- read.csv("~/Documents/Data/Chapter.3/Weather/upper.snake.river.precip.csv") |> 
    #Converting datetime to a date in r
    mutate(date = as.POSIXct(DATE),
         #Converting preciptation from inches to millimeter
         prcp.mm = PRCP * 25.4)
#The discharge data from USGS is on a 15 minute interval. This create a time series
#with a column of date and time. I plot this on the x-axis. To plot precipation with discharge
#I create a column of a time, noon, and combine it to the date to create a column of date and time.
#This isn't a reflection of when the precipitation fell just a visual tool.
upper.snake.precip.month$dateTime <- with(upper.snake.precip.month,ymd(upper.snake.precip.month$DATE) + 
                                            hms(upper.snake.precip.month$TIME))

#Creating a function for gathering USGS data
#id is the USGS code, measurement is usge discharge code, and start and end is time start and time end
usgsdis <- function(id, measurement, start, end){
  readNWISuv(id, measurement,start,end) %>% 
    mutate(X_00060_00000 = X_00060_00000 * 0.0283168)
}
#A function to clip precip data to the need time span
#df is the dataframe the data is at, start is the start time, and end is the end time
precip <- function(df, start, end){
  df %>% 
    filter(between(date, as.Date(start), as.Date(end)))  
}
#A function to plot discharge
#df is dataframe, x is datetime, y is discharge, colltime is the time I collected the sample
dischargeplot <- function(df, x,y, colltime){
  ggplot()+
    geom_line(data = (df), aes(x = x, y = y))+
    geom_vline(xintercept = as.POSIXct(as.Date(colltime)))}
#A function to plot precipitation and discharge
#disfdf is discharge dataframe, disX is time, disY is discharge, prcpdf is the precipitation dataframe,
#prcpX is date time, prcpY is precipitation, coeff is the multiplier for the two y-axis, 
#y1 label is discharge, y2label is precipitation, and sampletime is the time I collected my sample.
disprecipplot <- function(disdf, disX, disY, prcpdf, prcpX, prcpY, coeff, y1label, y2label, sampletime){
  ggplot() +
    geom_line(data = disdf, aes(x = disX, y=disY)) +
    #Geomsegment is used to plot precipatation. 
    geom_segment(data = prcpdf, aes(x= prcpX, yend = prcpY/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
    scale_y_continuous(
      # Features of the discharge (cfs)
      name = y1label,
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name=y2label)
    )+
    scale_x_datetime(date_labels = "%m/%d/%Y")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(size =24),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 28))+
    geom_vline(xintercept = as.numeric(as.POSIXct(sampletime)), color = "red") 
}
#Calling USGS data for each sampling event
event1 <- usgsdis("13010065", "00060","2022-04-30", "2022-05-14")
event2 <- usgsdis("13010065", "00060","2022-06-03", "2022-06-18") 
event3 <- usgsdis("13010065", "00060","2022-06-11", "2022-06-26") 
event4 <- usgsdis("13010065", "00060","2022-06-30", "2022-07-15") 
event5 <- usgsdis("13010065", "00060","2022-07-11", "2022-07-25") 
event6 <- usgsdis("13010065", "00060","2022-08-14", "2022-08-29") 
event7 <- usgsdis("13010065", "00060","2023-06-21", "2023-07-06")
event8 <- usgsdis("13010065", "00060","2023-08-07", "2023-08-22")
event9 <- usgsdis("13010065", "00060","2023-09-07", "2023-09-22") 
event10 <- usgsdis("13010065", "00060","2024-06-03", "2024-06-18")
event11 <- usgsdis("13010065", "00060","2024-06-30", "2024-07-14")
event12 <- usgsdis("13010065", "00060","2024-08-02", "2024-08-17") 
#Calling precipitation for each sampling event
e1p <- precip(upper.snake.precip.month, '2022-04-30', '2022-05-14')
e2p <- precip(upper.snake.precip.month, '2022-06-03', '2022-06-18')
e3p <- precip(upper.snake.precip.month,'2022-06-11', '2022-06-26')
e4p <- precip(upper.snake.precip.month,'2022-06-30', '2022-07-15')
e5p <- precip(upper.snake.precip.month,'2022-07-11', '2022-07-25')
e6p <- precip(upper.snake.precip.month,'2022-08-14', '2022-08-29')
e7p <- precip(upper.snake.precip.month,'2023-06-21', '2023-07-06')
e8p <- precip(upper.snake.precip.month,'2023-08-07', '2023-08-22')
e9p <- precip(upper.snake.precip.month,'2023-09-07', '2023-09-22')
e10p <- precip(upper.snake.precip.month,'2024-06-03', '2024-06-18')
e11p <- precip(upper.snake.precip.month,'2024-06-30', '2024-07-14')
e12p <- precip(upper.snake.precip.month,'2024-08-02', '2024-08-17')
#plotting discharge and sampling time for each event
d1 <- dischargeplot(event1, event1$dateTime, event1$X_00060_00000, "2022-05-13 12:00:00")
d2 <- dischargeplot(event2, event2$dateTime, event2$X_00060_00000, "2022-06-17 15:45:00")
d3 <- dischargeplot(event3, event3$dateTime, event3$X_00060_00000, "2022-06-25 11:00:00")
d4 <- dischargeplot(event4, event4$dateTime, event4$X_00060_00000, "2022-07-14 14:45:00")
d5 <- dischargeplot(event5, event5$dateTime, event5$X_00060_00000, "2022-07-24 15:30:00")
d6 <- dischargeplot(event6, event6$dateTime, event6$X_00060_00000, "2022-08-28 10:00:00")
d7 <- dischargeplot(event7, event7$dateTime, event7$X_00060_00000, "2023-07-05 20:00:00")
d8 <- dischargeplot(event8, event8$dateTime, event8$X_00060_00000, "2023-08-21 20:45:00")
d9 <- dischargeplot(event9, event9$dateTime, event9$X_00060_00000, "2023-09-21 17:45:00")
d10 <- dischargeplot(event10, event10$dateTime, event10$X_00060_00000, "2024-06-17 19:45:00")
d11 <- dischargeplot(event11, event11$dateTime, event11$X_00060_00000, "2024-07-13 18:45:00")
d12 <- dischargeplot(event12, event12$dateTime, event12$X_00060_00000, "2024-08-16 18:00:00")
ggarrange(d1, d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12, ncol = 3, nrow = 4, align = 'h')
#Plotting precipitation, discharge and sampling time for each event.
e1 <- disprecipplot(event1, event1$dateTime, event1$X_00060_00000, e1p, e1p$dateTime, e1p$prcp.mm, .3,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-05-13 12:00:00") + 
  annotate("text",x = as.POSIXct("2022-05-02 22:00:00"), y = 45, size = 10, label = "E/I = -0.004")
e2 <- disprecipplot(event2, event2$dateTime, event2$X_00060_00000, e2p, e2p$dateTime, e2p$prcp.mm, .2,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-06-17 15:45:00")+ 
  annotate("text",x = as.POSIXct("2022-06-05 22:00:00"), y = 190,size = 10,label = "E/I = 0.057")
e3 <- disprecipplot(event3, event3$dateTime, event3$X_00060_00000, e3p, e3p$dateTime, e3p$prcp.mm, .2,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-06-25 11:00:00")+ 
  annotate("text",x = as.POSIXct("2022-06-17 07:00:00"), y = 190, size = 10,label = "E/I = 0.018")
e4 <- disprecipplot(event4, event4$dateTime, event4$X_00060_00000, e4p, e4p$dateTime, e4p$prcp.mm, .05,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-07-14 14:45:00")+ 
  annotate("text",x = as.POSIXct("2022-07-04 12:00:00"), y = 40,size = 10,label = "E/I = 0.007")
e5 <- disprecipplot(event5, event5$dateTime, event5$X_00060_00000, e5p, e5p$dateTime, e5p$prcp.mm, .6,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-07-24 15:30:00")+ 
  annotate("text",x = as.POSIXct("2022-07-12 18:00:00"), y = 13,size = 10, label = "E/I = 0.015")
e6 <- disprecipplot(event6, event6$dateTime, event6$X_00060_00000, e6p, e6p$dateTime, e6p$prcp.mm, 1.25,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2022-08-28 10:00:00")+ 
  annotate("text",x = as.POSIXct("2022-08-18 06:00:00"), y = 13, size = 10, label = "E/I = -0.065")
e7 <- disprecipplot(event7, event7$dateTime, event7$X_00060_00000, e7p, e7p$dateTime, e7p$prcp.mm, .09,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-07-05 20:00:00")+ 
  annotate("text",x = as.POSIXct("2023-06-26 12:00:00"), y = 65,size = 10, label = "E/I = -0.002")
e8 <- disprecipplot(event8, event8$dateTime, event8$X_00060_00000, e8p, e8p$dateTime, e8p$prcp.mm, 1.1,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-08-21 20:45:00")+ 
  annotate("text",x = as.POSIXct("2023-08-08 23:00:00"), y = 17,size =10, label = "E/I = -0.049")
e9 <- disprecipplot(event9, event9$dateTime, event9$X_00060_00000, e9p, e9p$dateTime, e9p$prcp.mm, 1.1,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-09-21 17:45:00")+ 
  annotate("text",x = as.POSIXct("2023-09-08 23:00:00"), y = 17,size = 10,label = "E/I = -0.025")
e10 <- disprecipplot(event10, event10$dateTime, event10$X_00060_00000, e10p, e10p$dateTime, e10p$prcp.mm, .05,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2024-06-17 19:45:00")+ 
  annotate("text",x = as.POSIXct("2024-06-12 12:00:00"), y = 200,size =10,label = "E/I = 0.022")
e11 <- disprecipplot(event11, event11$dateTime, event11$X_00060_00000, e11p, e11p$dateTime, e11p$prcp.mm, .3,
                     bquote(Discharge (m^3/s)), "Precipitation (mm)", "2024-07-13 18:45:00")+ 
  annotate("text",x = as.POSIXct("2024-07-07 12:00:00"), y = 23,size = 10,label = "E/I = 0.013")
e12 <- disprecipplot(event12, event12$dateTime, event12$X_00060_00000, e12p, e12p$dateTime, e12p$prcp.mm, 1.3,
                     bquote(Discharge (m^3/s)), "Precipitation (mm)", "2024-08-16 18:00:00")+ 
  annotate("text",x = as.POSIXct("2024-08-04 12:00:00"), y = 13,size =10,label = "E/I = -0.009")

eall <- ggarrange(e1, e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12, ncol = 3, nrow = 4, align = 'h')
erain <- ggarrange(e1, e6, e7, e8,e9,e12, ncol = 3, nrow = 2, align = 'h')
eall
png(filename="~/Documents/Data/Chapter.3/Figures/rain_event_isotope_mass_balance/eall.png", width = 1500, height = 1111)
eall
dev.off()
png(filename="~/Documents/Data/Chapter.3/Figures/rain_event_isotope_mass_balance/erain.png", width = 1500, height = 1000)
erain
dev.off()



####Calculating isotope value pre-event rain####
#First I will calculate the slope pre-event rain
# I want the long term average so I will go a ways out
# I am calling data for event 7. 
#For event 7 going post-sampling provides a better long-term signal.
#Dates before June 30th are heavily influence by rain events
event7 <- usgsdis("13010065", "00060","2023-06-29", "2023-07-20") 
e7p <- precip(upper.snake.precip.month, '2023-06-29', '2023-07-20')
e7 <- disprecipplot(event7, event7$dateTime, event7$X_00060_00000, e7p, e7p$dateTime, e7p$prcp.mm, .5,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-07-05 20:00:00")
e7
#Adding a count column to more easily run my linear model.
event7 <- event7 %>% 
  mutate(count = seq(1:nrow(event7)))

#Fitting a linear model to pre-event rain. This allows me to estimate what the 
linear_model <- lm(X_00060_00000 ~ count, data=event7) 
#Extracting the coefficents
cf <- coef(linear_model)
#I have taken a long-term decline, but when I reduce the dataset to fit the day
#of interestet the intercept is not an appropriate match for the location before
#discharge begins to rise. I adjust that until there is an appropriate fit.
event7 <- event7 %>% 
  mutate(estimate_discharge = (cf[1] - 1.37) + (cf[2] * count))

#Plotting the discharge data and the linear model
ggplot()+
  geom_line(data = event7, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = event7, aes(x = dateTime, y = estimate_discharge))+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-07-05 20:00:00", tz="UTC")), color = "red")


#Removing data that is before and after the rise in discharge.
#Here I am trying to visually determine changes to the long-term decline in 
#discharge. I will fit the model assuming this is an estimate of discharge values
#that would occur if not precipitation occurred.
#Start of rise in discharge
rain_start <- as.POSIXct("2023-07-04 00:45:00",tz = "UTC")
#Rise over
rain_end <- as.POSIXct("2023-07-05 20:00:00",tz = "UTC")
revent7 <- event7 %>%  
  filter(between(dateTime, rain_start, rain_end))
#Plotting to ensure fit is good
ggplot()+
  geom_line(data = revent7, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = revent7, aes(x = dateTime, y = estimate_discharge), color = "blue")+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-07-05 20:00:00", tz="UTC")), color = "red")
#Values of isotopes to input into mixing model
d18O <- -18.315115464
d18Op <- -9.803027243
d18Or <- -18.25802
#Calculating isotopic value of river pre rain.
deltapre7 <- ((sum(revent7$X_00060_00000) * d18O) - ((sum(revent7$X_00060_00000) - sum(revent7$estimate_discharge)) * d18Op))/ 
  sum(revent7$estimate_discharge)


#I am calling data for event 8. 
#For event 8 going pre-sampling provides a better long-term signal.
#Dates before August 21st are heavily influence by rain events
event8 <- usgsdis("13010065", "00060","2023-08-11", "2023-08-18") 
e8p <- precip(upper.snake.precip.month, "2023-08-11", "2023-08-18")
e8 <- disprecipplot(event8, event8$dateTime, event8$X_00060_00000, e8p, e8p$dateTime, e8p$prcp.mm, 1,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-08-21 20:45:00")
e8

#Adding a count column to more easily run my linear model.
event8 <- event8 %>% 
  mutate(count = seq(1:nrow(event8)))

#Fitting a linear model to pre-event rain. This allows me to estimate what the 
linear_model <- lm(X_00060_00000 ~ count, data=event8) 
#Extracting the coefficents
cf <- coef(linear_model)
#I have taken a long-term decline, but when I reduce the dataset to fit the day
#of interestet the intercept is not an appropriate match for the location before
#discharge begins to rise. I adjust that until there is an appropriate fit.
event8 <- usgsdis("13010065", "00060","2023-08-11", "2023-08-22")
event8 <- event8 %>% 
  mutate(count = seq(1:nrow(event8))) %>% 
  mutate(estimate_discharge = (cf[1] + (cf[2] * count)))

#Plotting the discharge data and the linear model
ggplot()+
  geom_line(data = event8, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = event8, aes(x = dateTime, y = estimate_discharge))+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-08-21 20:45:00", tz="UTC")), color = "red")

#Removing data that is before and after the rise in discharge.
#Here I am trying to visually determine changes to the long-term decline in 
#discharge. I will fit the model assuming this is an estimate of discharge values
#that would occur if not precipitation occurred.
#Start of rise in discharge
rain_start <- as.POSIXct("2023-08-19 08:45:00",tz = "UTC")
#Rise over
rain_end <- as.POSIXct("2023-08-21 05:45:00",tz = "UTC")
revent8 <- event8 %>%  
  filter(between(dateTime, rain_start, rain_end))
#Plotting to ensure fit is good
ggplot()+
  geom_line(data = revent8, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = revent8, aes(x = dateTime, y = estimate_discharge), color = "blue")+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-08-21 20:45:00", tz="UTC")), color = "red")
#Values of isotopes to input into mixing model
d18O <- -18.315115464
d18Op <- -5.990206792
d18Or <- -16.77375
#Calculating isotopic value of river pre rain.
deltapre8 <- ((sum(revent8$X_00060_00000) * d18O) - ((sum(revent8$X_00060_00000) - sum(revent8$estimate_discharge)) * d18Op))/ 
  sum(revent8$estimate_discharge)

####Event 9####
#I am calling data for event 9. 
#For event 9 going pre-sampling provides a better long-term signal.
event9 <- usgsdis("13010065", "00060","2023-09-08", "2023-09-21") 
e9p <- precip(upper.snake.precip.month,'2023-09-08', '2023-09-21')
e9 <- disprecipplot(event9, event9$dateTime, event9$X_00060_00000, e9p, e9p$dateTime, e9p$prcp.mm, 1.1,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-09-21 17:45:00")+ 
  annotate("text",x = as.POSIXct("2023-09-08 23:00:00"), y = 17,size = 10,label = "E/I = -0.025")

e9
#Dates before August 21st are heavily influence by rain events
#Start of rise in discharge
rain_start <- as.POSIXct("2023-09-09 00:00:00",tz = "UTC")
rain_end <- as.POSIXct("2023-09-21 00:00:00",tz = "UTC")
event9 <- usgsdis("13010065", "00060","2023-09-08", "2023-09-21") %>%  
  filter(between(dateTime, rain_start, rain_end))
e9p <- precip(upper.snake.precip.month,'2023-09-08', '2023-09-21')
e9 <- disprecipplot(event9, event9$dateTime, event9$X_00060_00000, e9p, e9p$dateTime, e9p$prcp.mm, 1.1,
                    bquote(Discharge (m^3/s)), "Precipitation (mm)", "2023-09-21 17:45:00")+ 
  annotate("text",x = as.POSIXct("2023-09-08 23:00:00"), y = 17,size = 10,label = "E/I = -0.025")

e9
#Adding a count column to more easily run my linear model.
event9 <- event9 %>% 
  mutate(count = seq(1:nrow(event9)))

#Fitting a linear model to pre-event rain. This allows me to estimate what the 
linear_model <- lm(X_00060_00000 ~ count, data=event9) 
#Extracting the coefficents
cf <- coef(linear_model)
#I have taken a long-term decline, but when I reduce the dataset to fit the day
#of interest the intercept is not an appropriate match for the location before
#discharge begins to rise. I adjust that until there is an appropriate fit.
event9 <- usgsdis("13010065", "00060","2023-09-20", "2023-09-22")
event9 <- event9 %>% 
  mutate(count = seq(1:nrow(event9))) %>% 
  mutate(estimate_discharge = ((cf[1] - 2)+ (cf[2] * count)))

#Plotting the discharge data and the linear model
ggplot()+
  geom_line(data = event9, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = event9, aes(x = dateTime, y = estimate_discharge))+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-09-21 17:45:00", tz="UTC")), color = "red")

#Removing data that is before and after the rise in discharge.
#Here I am trying to visually determine changes to the long-term decline in 
#discharge. I will fit the model assuming this is an estimate of discharge values
#that would occur if not precipitation occurred.
#Start of rise in discharge
rain_start <- as.POSIXct("2023-09-20 23:45:00",tz = "UTC")
#Rise over
rain_end <- as.POSIXct("2023-09-21 17:45:00",tz = "UTC")
revent9 <- event9 %>%  
  filter(between(dateTime, rain_start, rain_end))
#Plotting to ensure fit is good
ggplot()+
  geom_line(data = revent9, aes(x = dateTime, y = X_00060_00000))+
  geom_line(data = revent9, aes(x = dateTime, y = estimate_discharge), color = "blue")+
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-09-21 17:45:00", tz="UTC")), color = "red")
#Values of isotopes to input into mixing model
d18O <- -17.262355910
d18Op <- -10.57452193
d18Or <- -17.92000
#Calculating isotopic value of river pre rain.
deltapre9 <- ((sum(revent9$X_00060_00000) * d18O) - ((sum(revent9$X_00060_00000) - sum(revent9$estimate_discharge)) * d18Op))/ 
  sum(revent9$estimate_discharge)