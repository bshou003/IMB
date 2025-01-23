library(tidyverse)
library(readxl)
library(dataRetrieval)
library(stats)

####Weather Data Collection and Lake Temperatures####
#Jackson Airport has additional measurements temp, min temp, max temp, EA, ES, 
#dewpoint but were dropped because they will not be used currently
jack.airport.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/GSOD.Jackson.Airport.csv") |> 
  subset(select = c(date, YEAR, MONTH, RH)) |> 
  reframe(date = date,
          year = YEAR,
          month = MONTH,
          RH = RH) |> 
  #Grouping by year and month
  group_by(year, month) |> 
  subset(select = -c(date)) |> 
  #Calculating the monthly average of RH because analysis is conducted on a monthly basis and percentage
  summarise(RH.jack.ap = (mean(RH)/100)) |>
  #Filtering the summer months
  filter(month >= 5 & month < 10) 

#BoR data has min temp, max temp, and SWE but removed. Using for precipitation
bor.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/BoR.2022.2024.txt") |> 
  subset(select = -c(Notes)) |>
  transmute(date = as.Date(DateTime),
            prcp.m = jck_pp * 0.0254, #m
            air.temp.c = (jck_mm-32)/(9/5),#celcius
            air.temp.k = (jck_mm-32)/(9/5) + 273.15,#temp in kelvin
            year = as.numeric(format(date, format = "%Y")),
            month = as.numeric(format(date, format = "%m"))) |> 
  subset(select = -c(date)) |>
  #Grouping by year and month
  group_by(year, month) |> 
  #Taking the monthly averages and sums
  summarise(prcp.bor.m = sum(prcp.m),
            air.temp.bor.c =  mean(air.temp.c),
            air.temp.bor.k = mean(air.temp.k)) |> 
  #Filtering the summer months
  filter(month >= 5 & month < 10)

#Calling upper snake precipitation which will allow me to apply a theisen approach to rainfall amounts of Jackson Lake
upper.snake.precip.month <- read.csv("~/Documents/Data/Chapter.3/Weather/upper.snake.river.precip.csv") |> 
  mutate(date = as.Date(DATE),
         prcp.m = PRCP * 0.0254,
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m"))) |> 
  #Grouping by year and month
  group_by(year, month) |> 
  #Filtering the summer months
  filter(month >= 5 & month < 10) |> 
  subset(select = c(year, month, prcp.m)) |> 
  #Taking the monthly sums
  mutate(prcp.m = sum(prcp.m)) |> 
  distinct()

#Releases from Jackson Lake, may use discharge from the USGS gauge
bor.releases <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/bor.dam.releases.csv") %>% 
  mutate(date = as.Date(DateTime),
         #converting volume from acre-foot to km3
         jck.km3 = jck_af * 1.23348e-6,
         #calculating the percentage full Jackson lake is 
         jck.per = jck_af / (847000 * 1.23348e-6),
         #Calculating lake surface area from the relationship developed from remote sensing
         #and a polynomial equation from excel
         jck.area.km2 = ((-38.589 * jck.km3^2) + (82.796 * jck.km3) + 64.25),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")))|> 
  #Grouping by year and month
  group_by(year, month) |> 
  #Filtering the summer months
  filter(month >= 5 & month < 10) |> 
  reframe(jck.km3 = mean(jck.km3),
          #Will be used to calculate more accurate on-lake precipitation amounts
          jck_area.km2 = mean(jck.area.km2)) %>% 
  ungroup() %>% 
  mutate(vol.change.km3 = jck.km3 - lag(jck.km3, default = jck.km3[1]))

#Calling the lake data
JL_YSI_sur <- read_csv('~/Documents/Data/Lake_YSI/2023_YSI.csv',show_col_types = FALSE) |>
  mutate(DATE = as.Date(DATE, "%m/%d/%Y")) |>   #Calling the YSI data for site JL1-JL16
  group_by(SITE, Event) |>
  filter(Depth.m == min(Depth.m)) |>
  mutate(lake.temp.C = (Temp.F - 32) * (5/9),
         lake.temp.k = lake.temp.C + 273.15,
         year = as.numeric(format(DATE, format = "%Y")),
         month = as.numeric(format(DATE, format = "%m"))) |>
  subset(select = c(Event, SITE, lake.temp.C,lake.temp.k, year, month)) |>
  filter(Event == 2 | Event == 4 | Event == 6)

#Calling stream data
stream_ysi <- read_csv("~/Documents/Data/Trib_YSI/YSI.Tribs.csv") |> 
  subset(select = -c(Time, mmHg, DOPC, Domg.l, SALPPT, ORPmV, Notes, C, SP.C, pH, Date.Adj, Sample.ID)) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         Temp.c = as.numeric(Temp),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")))
#Calling dam outlet 
lake.outlet.ysi <- stream_ysi %>% 
  filter(Setting == "Lake.Outlet") %>% 
  group_by(Event) %>% 
  mutate(Temp.c = mean(Temp.c),
         lake.temp.k = as.numeric(Temp.c) + 273.15) %>% 
  distinct()

#Calling the penman data calculated using Wetbud, may need to do this by hand to account for differing albedos
#Limited to 2023 due to lack of solar data
et <- read_csv("~/Documents/Data/Chapter.3/Weather/jackson_penman.csv") %>% 
  mutate(date = as.Date(date),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m"))) %>% 
  group_by(month, year) %>%
  #converting cm to m
  summarise(et = sum(et.cm) / 100)|> 
  #Filtering the summer months
  filter(month >= 5 & month < 10)


## Changing the event name from the YSI into the events that match my labeling scheme
JL_YSI_sur$Event <- NA
JL_YSI_sur$Event[JL_YSI_sur$Event == 2] <- 7
JL_YSI_sur$Event[JL_YSI_sur$Event == 4] <- 8
JL_YSI_sur$Event[JL_YSI_sur$Event == 6] <- 9

####Need to work on####
####Lake area and volume calculation####
#importing in BoR data
# bor.volume  <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/bor.elevtation.volume.2013.2024.txt",sep = "\t") %>% 
#   #mutating DateTime column to date
#   mutate(DateTime = ymd(DateTime),
#          #converting acre-feet to km3
#          jck_km3 = jck_af * 1.23348e-6)
# #Importing in the remote sensed area data from seintel
# sent.area  <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/GEE/sentienel2/lake.area.sentinel2.csv") %>% 
#   #converting date to date
#   summarise(DateTime = ymd(date),
#             #converting from chr to numeric
#          area.km2 = as.double(area.km2)) %>% 
# #removing Nas
#     na.omit()
# #Merging the two above dataframes
# all.df <- bor.volume %>% 
#   merge(sent.area)
# write_csv(all.df,"~/Documents/Data/Chapter.3/bor.dam.releases/GEE/sentienel2/volume.area.csv")
# #Reordering the data to ensure the model works well
# all.df <- all.df[order(all.df$jck_km3),]
# 
# #PLotting the raw data to estimate the nls parameters
# ggplot() +
#   geom_point(data=all.df, aes(x = jck_km3, y = area.km2))
# 
# #Setting the inital parameters
# Asym <- 100 ; resp0 <- 75 ; lrc <- 4; c0 <- 0.1
# #Constructing the asympotic model
# model <- nls(area.km2 ~ SSasympOff(jck_km3, Asym, lrc, c0), data = all.df)
# #Print the model
# summary(model)
# #Creating a data.frame with a sequence of values for volumer
# volume <- data.frame(seq(from =0.1, to = 1.40, length = 147)) %>% 
#   #renaming the column
#   rename(volume = "seq.from...0.1..to...1.4..length...147.")
# 
# #Creating a column and predicting lake areas
# volume$predict <- predict(model, newdata = volume$volume)
# plot(all.df$jck_km3,all.df$area.km2)
# points(volume$volume,volume$predict, col = "green")

####Calling Isotope Data####
#OPIC data, Gabe Bowen isoscapes https://wateriso.utah.edu/waterisotopes/index.html
OPIC <- read_csv("~/Documents/Data/Chapter.3/Isotope.Data/OPIC.Data.csv") |>
  rename(month = MONTH)

gw <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "AMK Tap") |> 
  subset(select = -c(seq_position, Setting.Type, dxs)) %>% 
  group_by(Event) %>% 
  mutate(d18Og = mean(d18O),
         d2Hg = mean(d2H)) %>% 
  subset(select = -c(d18O, d2H, SITE)) %>% 
  distinct()


stream.isotopes <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Stream" | Setting.Type == "River") |> 
  subset(select = -c(seq_position, Setting.Type)) |> 
  filter(SITE != "26W" & SITE != "26E")

lake.outlet <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Lake.outlet") |> 
  subset(select = -c(seq_position, Setting.Type, dxs)) |> 
  group_by(Event) |>
  #Several samples were analyzed multiple times, these are averaged to simplify
  mutate(d18O = mean(d18O),
         d2H = mean(d2H)) |> 
  distinct() |> 
  merge(lake.outlet.ysi) |> 
  merge(bor.releases) |>
  merge(bor.monthly) |> 
  merge(jack.airport.monthly) |> 
  merge(OPIC) |>
  # merge(gw) |>
  # ET doesn't contain 2024, temporarily removed
  #merge(et) |>
  select(subset = -c(Setting, Setting.Adj))

lake.outlet.2016 <- lake.outlet %>% 
  mutate(d18Olag = d18O - lag(d18O),
         d2Hlag = d2H - lag(d2H))

####Fractionation Factors for both oxygen and hydrogen following Horita and Wesolowski (1994) & Majoube (1971), a 
#good description is presented in Gibson (2016) ####
#Need to ensure excel/csv column name matches this script, temperature values need to be kelvin
#From Mercer(2022)
lake.outlet.2016$a2hplus <- exp(((24.844 * (10^6 / lake.outlet.2016$lake.temp.k^2)) - 
                                   (76.248 * (10^3 / lake.outlet.2016$lake.temp.k)) + 52.612)/1000)
lake.outlet.2016$a18oplus <- exp(((1.137 * (10^6 / lake.outlet.2016$lake.temp.k^2)) - 
                                    (0.4156 * (10^3 / lake.outlet.2016$lake.temp.k)) - 2.0667)/1000)

####Equilibrium Separation####
lake.outlet.2016$ehplus <- (lake.outlet.2016$a2hplus -1) * 1000 #Equilibrium Separation for hydrogen using Fractionation Factor from Horita & Wesolowski (1994)
lake.outlet.2016$eoplus <- (lake.outlet.2016$a18oplus -1) * 1000 #Equilibrium Separation for oxygen using Fractionation Factor from Horita & Wesolowski (1994)

####Humidity inputs####
#Saturation Pressure for the atmosphere
#From Ward and Elliot (1995)
lake.outlet.2016$Psatair <- exp(((16.78* lake.outlet.2016$air.temp.bor) - 116.9) / (lake.outlet.2016$air.temp.bor + 237.3)) #(kPa) For this I will need the temperature of the air collected from Jackson Lake Dam
#Saturation Pressure for the lake water
lake.outlet.2016$Psatwater <- exp(((16.78* lake.outlet.2016$Temp) - 116.9) / (lake.outlet.2016$Temp + 237.3)) #(kPa) For this I will need the water temperature from the YSI and the interpolation maps.
#Normalized Relative Humidity
lake.outlet.2016$Hn <- (lake.outlet.2016$RH.jack.ap * (lake.outlet.2016$Psatair / lake.outlet.2016$Psatwater)) /100 #Psatair & Psatwater indicate saturation vapor pressure with respect to air and water temperature. (Mook, 2000*)

####Kinetic Separation Factor####
theta = 1
ck18O <- 14.2
ck2H <- 12.5

#Kinetic Separation Factor
lake.outlet.2016$ekh <- theta * ck2H * (1-lake.outlet.2016$Hn)
lake.outlet.2016$eko <- theta * ck18O * (1-lake.outlet.2016$Hn)
####Atmospheric Isotope Values####
# k = 0.5 #Suggested for highly seasonal climates Gibson (2015)
# # #Hydrogen
# lake.outlet.2016$deltaatmoh <- (lake.outlet.2016$deltaprcph - (k * lake.outlet.2016$ehplus))/(1+(10^-3*k*lake.outlet.2016$ehplus))  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above#oxygen
# lake.outlet.2016$deltaatmoo <- (lake.outlet.2016$deltaprcpo - (k * lake.outlet.2016$eoplus))/(1+(10^-3*k*lake.outlet.2016$eoplus))  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above

lake.outlet.2016$deltaatmoh1 <- (lake.outlet.2016$deltaprcph - lake.outlet.2016$ehplus)  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above#oxygen    
lake.outlet.2016$deltaatmoo1 <- (lake.outlet.2016$deltaprcpo - lake.outlet.2016$eoplus)  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above

lake.outlet.2016$deltaatmoh2 <- (lake.outlet.2016$deltaprcph - (log10(lake.outlet.2016$a2hplus) * 1000))  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above#oxygen    
lake.outlet.2016$deltaatmoo2 <- (lake.outlet.2016$deltaprcpo - (log10(lake.outlet.2016$a18oplus) * 1000))

lake.outlet.2016$deltaatmoh3 <- (lake.outlet.2016$deltaprcph - lake.outlet.2016$ehplus)/(lake.outlet.2016$a2hplus)  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above#oxygen
lake.outlet.2016$deltaatmoo3 <- (lake.outlet.2016$deltaprcpo - lake.outlet.2016$eoplus)/(lake.outlet.2016$a18oplus)  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above

k = 0.5 #Suggested for highly seasonal climates Gibson (2015)
lake.outlet.2016$deltaatmoh4 <- (lake.outlet.2016$deltaprcph - (k * lake.outlet.2016$ehplus))/(1+(10^-3*k*lake.outlet.2016$ehplus))  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above#oxygen
lake.outlet.2016$deltaatmoo4 <- (lake.outlet.2016$deltaprcpo - (k * lake.outlet.2016$eoplus))/(1+(10^-3*k*lake.outlet.2016$eoplus))  #deltaprcp is the delta value of the precipitation this will most likely be collected from Gabe Bowens precipitation map, k is a valu that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015)), and estar is the equilibrium seperation caluclated from above

####Importing Lake Isotope Values####
####Gathering Inputs####
# Function for calling streamflow data from USGS #
strmcall <- function(siteid, parameter, start, end){
  readNWISdv(siteNumber = siteid, 
             parameterCd = parameter, # Discharge
             startDate = start,
             endDate = end) 
}

####Parameters to call data from the Upper Snake Location####
siteid ="13010065"
parameter = "00060"
start = "2022-05-01"
end = "2024-08-31"

#Formatin stream.isotopes dataframe#
stream.isotopes <- stream.isotopes %>% 
  group_by(Event, SITE) %>% 
  mutate(d18O = mean(d18O),
         d2H = mean(d2H),
         dxs = mean(dxs),
         SITE = as.numeric(SITE)) %>% 
  distinct()
#Creating a data frame for Upper Snake#
river <- stream.isotopes %>% 
  filter(SITE == 15) %>% 
  mutate(Event = as.numeric(Event))

#Calling USGS Data#
upper.snake.river.monthly <- strmcall(siteid, parameter, start, end) 
#Summing the USGS data on a monthly basis
upper.snake.river.monthly <- upper.snake.river.monthly %>% 
  mutate(year = as.numeric(format(Date, format = "%Y")),
         month = as.numeric(format(Date, format = "%m")),
         day = as.numeric(format(Date, format = "%d")),
         SITE = 15,
         daily.total = X_00060_00003 * 24 * 60*60) %>% 
  group_by(month, year) %>% 
  mutate(month.dis = sum(daily.total)) %>% 
  select(subset = -c(agency_cd,site_no,Date,X_00060_00003,X_00060_00003_cd, day, daily.total)) %>% 
  distinct()
#Adding event data based on year and month
# upper.snake.river.monthly$Event <- NA
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 7 & upper.snake.river.monthly$year == 2023] <- 7
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 8 & upper.snake.river.monthly$year == 2023] <- 8
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 9 & upper.snake.river.monthly$year == 2023] <- 9
#Merging the isotope data wtih discharge data.
upper.snake.river.monthly <- upper.snake.river.monthly%>% 
  merge(river) %>% 
  mutate(month.dis = month.dis * 0.0283168,
         month.dis.18o = month.dis * d18O,
         month.dis.2h = month.dis * d2H)
#Calling in the trib discharge from rTop.
trib.discharge <- read.csv("~/Documents/Data/Chapter.3/rTop/rtop/rtop.discharge.estimates.csv")%>% 
  mutate(Date = as.Date(Date),
         year = as.numeric(format(Date, format = "%Y")),
         month = as.numeric(format(Date, format = "%m"))) %>% 
  rename(SITE = id) 
#Adding event data based on year and month
# trib.discharge$Event <- NA
# trib.discharge$Event[trib.discharge$month == 7 & trib.discharge$year == 2023] <- 7
# trib.discharge$Event[trib.discharge$month == 8 & trib.discharge$year == 2023] <- 8
# trib.discharge$Event[trib.discharge$month == 9 & trib.discharge$year == 2023] <- 9

trib.dis.iso.sum  <- trib.discharge %>% 
  merge(stream.isotopes) %>% 
  mutate(dis.18O = dis * d18O,
         dis.2H = dis * d2H) %>% 
  group_by(Event) %>% 
  reframe(Event = Event,
          sum.dis = sum(dis) * 60 *60 *24,
          sum.disv = sum(disv),
          sumd18o = sum(dis.18O) * 60 * 60 *24,
          sumd2h = sum(dis.2H)*60*60*24) %>% 
  distinct()

jack.lake.area <- 1.03366e+8 #meters
lake.outlet.2016 <- lake.outlet.2016 %>% 
  mutate(prcp.bor.dam.area = prcp.bor * jack.lake.area,
         prcp.bor.area.18o = prcp.bor.dam.area * deltaprcpo,
         prcp.bor.area.2h = prcp.bor.dam.area * deltaprcph)
#2016#
lake.outlet.2016$deltaHinput <- (trib.dis.iso.sum$sumd2h + lake.outlet.2016$prcp.bor.area.2h + upper.snake.river.monthly$month.dis.2h)  / 
  (lake.outlet.2016$prcp.bor.dam.area + trib.dis.iso.sum$sum.dis + upper.snake.river.monthly$month.dis) #U refers to the streams, P is precipitation and is the overlake precipitaiton amount, R is snake river inflow
lake.outlet.2016$deltaOinput <- (trib.dis.iso.sum$sumd18o + lake.outlet.2016$prcp.bor.area.18o+ upper.snake.river.monthly$month.dis.18o)  / 
  (lake.outlet.2016$prcp.bor.dam.area + trib.dis.iso.sum$sum.dis+ upper.snake.river.monthly$month.dis)

#calculation of m and delta star 2016#
lake.outlet.2016$m2 <- (lake.outlet.2016$Hn - 10^-3 *((lake.outlet.2016$ekh + lake.outlet.2016$ehplus)/lake.outlet.2016$a2hplus)) / 
  (1-lake.outlet.2016$Hn+10^-3*lake.outlet.2016$ekh)
lake.outlet.2016$m18 <- (lake.outlet.2016$Hn - 10^-3 *((lake.outlet.2016$eko + lake.outlet.2016$eoplus)/lake.outlet.2016$a18oplus)) / 
  (1-lake.outlet.2016$Hn+10^-3*lake.outlet.2016$eko)

lake.outlet.2016$deltastarh <- (((lake.outlet.2016$Hn * lake.outlet.2016$deltaatmoh3) + lake.outlet.2016$ekh + lake.outlet.2016$ehplus)/lake.outlet.2016$a2hplus)/
  (lake.outlet.2016$Hn - 10^-3 *((lake.outlet.2016$ekh + lake.outlet.2016$ehplus)/lake.outlet.2016$a2hplus))
lake.outlet.2016$deltastaro <- (((lake.outlet.2016$Hn * lake.outlet.2016$deltaatmoo3) + lake.outlet.2016$eko + lake.outlet.2016$eoplus)/lake.outlet.2016$a18oplus)/
  (lake.outlet.2016$Hn - 10^-3 *((lake.outlet.2016$eko + lake.outlet.2016$eoplus)/lake.outlet.2016$a18oplus))

#E/I#
lake.outlet.2016$ei2 <- (lake.outlet.2016$d2H-lake.outlet.2016$deltaHinput)/((lake.outlet.2016$m2 *(lake.outlet.2016$deltastarh-lake.outlet.2016$d2H)))
lake.outlet.2016$ei18 <- (lake.outlet.2016$d18O-lake.outlet.2016$deltaOinput)/((lake.outlet.2016$m18 *(lake.outlet.2016$deltastaro-lake.outlet.2016$d18O)))
lake.outlet.2016$ei2 <- (lake.outlet.2016$deltaHinput- lake.outlet.2016$d2H)/((lake.outlet.2016$m2 *(lake.outlet.2016$deltastarh-lake.outlet.2016$d2H)))
lake.outlet.2016$ei18 <- (lake.outlet.2016$deltaOinput-lake.outlet.2016$d18O)/((lake.outlet.2016$m18 *(lake.outlet.2016$deltastaro-lake.outlet.2016$d18O)))
####Gathering Outputs####
#Parameters to call USGS data below the dam
siteid =c("13011000")
parameter = "00060"
start = "2022-05-01"
end = "2024-08-31"
#Calling the USGS data
snake.river.dam <- strmcall(siteid, parameter, start, end)

snake.river.dam.monthly <- snake.river.dam %>% 
  mutate(year = as.numeric(format(Date, format = "%Y")),
         month = as.numeric(format(Date, format = "%m")),
         day = as.numeric(format(Date, format = "%d")),
         SITE = 1) %>% 
  group_by(month, year) %>% 
  mutate(month.dis = sum(X_00060_00003) *  0.0283168 * 60 *60 *24) %>% 
  select(subset = -c(agency_cd,site_no,Date,X_00060_00003,X_00060_00003_cd, day)) %>% 
  distinct()
snake.river.dam.monthly$Event <- NA
snake.river.dam.monthly$Event[snake.river.dam.monthly$month == 7 & snake.river.dam.monthly$year == 2023] <- 7
snake.river.dam.monthly$Event[snake.river.dam.monthly$month == 8 & snake.river.dam.monthly$year == 2023] <- 8
snake.river.dam.monthly$Event[snake.river.dam.monthly$month == 9 & snake.river.dam.monthly$year == 2023] <- 9
lake.outlet.2016 <- lake.outlet.2016 %>% 
  merge(snake.river.dam.monthly)

bor.monthly.dam.discharge.month  <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/bor.dam.releases.csv")%>% 
  mutate(Date = as.Date(DateTime),
         jck_qu = jck_qu * 0.0283168,
         jck_fb = jck_fb * 0.3048,
         jck_af = jck_af * 1233.48,
         jck_qd = jck_qd * 0.0283168,
         year = as.numeric(format(Date, format = "%Y")),
         month = as.numeric(format(Date, format = "%m")),
         day = as.numeric(format(Date, format = "%d"))) %>% 
  filter(year == 2023 & month == 7 | year == 2023 & month == 8 | year == 2023 & month == 9) %>% 
  group_by(year, month) %>% 
  subset(select = -c(day, Date)) %>% 
  reframe(month = month,
          year = year,
          jck_qu = sum(jck_qu),
          jck_fb = sum(jck_fb),
          jck_af = sum(jck_af),
          jck_qd = sum(jck_qd)) %>% 
  distinct()


# lake.volume.change.july.2023 <- bor.monthly.dam.discharge.month %>% 
#   filter(year == 2023 & month ==7) %>%
#   subset(select = c(year, month, day, jck_af)) %>% 
#   summarise(year = year,
#             month = month,
#             jck_af = last(jck_af - first(jck_af))) %>% 
#   distinct()
# 
# lake.volume.change.august.2023 <- bor.monthly.dam.discharge.month %>% 
#   filter(year == 2023 & month ==8) %>%
#   subset(select = c(year, month, day, jck_af)) %>% 
#   summarise(year = year,
#             month = month,
#             jck_af = last(jck_af - first(jck_af))) %>% 
#   distinct()




#Evaporation#
lake.outlet.2016$deltaevaph <- (((lake.outlet.2016$d2H*lake.outlet.2016$a2hplus)-(lake.outlet.2016$Hn*lake.outlet.2016$deltaatmoh3)
                                 -lake.outlet.2016$ehplus-lake.outlet.2016$ekh)/((1-lake.outlet.2016$Hn) + (lake.outlet.2016$ekh/10^3)))
#oxygen
lake.outlet.2016$deltaevapo <- (((lake.outlet.2016$d18O*lake.outlet.2016$a18oplus)-(lake.outlet.2016$Hn*lake.outlet.2016$deltaatmoo3)
                                 -lake.outlet.2016$eoplus-lake.outlet.2016$eko)/((1-lake.outlet.2016$Hn) + (lake.outlet.2016$eko/10^3)))


gio <- ((lake.outlet.2016$prcp.bor.area.2h + lake.outlet.2016$deltaHinput - (lake.outlet.2016$deltaevaph * jack.lake.area * lake.outlet.2016$et) - 
           (((lake.outlet.2016$prcp.bor * jack.lake.area) + trib.dis.iso.sum$sum.dis + upper.snake.river.monthly$month.dis - 
               (jack.lake.area * lake.outlet.2016$et)- lake.outlet.2016$vol.change) * lake.outlet.2016$d2H) - 
           (((lake.outlet.2016$vol.change * lake.outlet.2016$d2H)/30) - ((bor.releases$jck.af * lake.outlet.2016$d2Hlag)/30))) / 
          (lake.outlet.2016$d2H-lake.outlet.2016$d2Hg))

gih <- (((lake.outlet.2016$d2H - lake.outlet.2016$deltaprcph)/(lake.outlet.2016$d2Hg - lake.outlet.2016$d2H)) * (lake.outlet.2016$prcp.bor * jack.lake.area)) +
  (((lake.outlet.2016$deltaevaph - lake.outlet.2016$d2H) / (lake.outlet.2016$d2Hg - lake.outlet.2016$d2H)) * (jack.lake.area * lake.outlet.2016$et))


evaporation <- snake.river.dam.monthly$month.dis *((lake.outlet.2016$deltaHinput - lake.outlet.2016$d2H) / 
                                                     (lake.outlet.2016$deltaevaph- lake.outlet.2016$deltaHinput))
evaporationO <- snake.river.dam.monthly$month.dis *((lake.outlet.2016$deltaOinput - lake.outlet.2016$d18O) / 
                                                      (lake.outlet.2016$deltaevapo- lake.outlet.2016$deltaOinput))
einum <- (lake.outlet.2016$deltaHinput - lake.outlet.2016$d2H) 
eidem <-(lake.outlet.2016$deltaevaph- lake.outlet.2016$d2H)
evaporation.mm.month <- (evaporation/jack.lake.area) * 1000

evaporation <- snake.river.dam.monthly$month.dis *((lake.outlet.2016$d2H-lake.outlet.2016$deltaHinput ) / 
                                                     (lake.outlet.2016$deltaevaph- lake.outlet.2016$deltaHinput))

####Gibson 2002 ####

estar18 <- -7.685 + (6.7123 *(10^3/lake.outlet$lake.temp.k)) - (1.6664 * (10^6/lake.outlet$lake.temp.k^2)) + (0.35041* (10^9/lake.outlet$lake.temp.k^3))
estar2 <- (1158.8 * (lake.outlet$lake.temp.k^3 / 10^9)) - (1620.1 * (lake.outlet$lake.temp.k^2/10^6)) + (794.84*(lake.outlet$lake.temp.k/10^3))-
  161.04 + (2.9992 * (10^9/lake.outlet$lake.temp.k^3))
astar18 <- 1 + estar18
astar2 <- 1 + estar2
ek18 <- 14.2 * (1-lake.outlet$Hn)
ek2 <- 12.5 * (1 - lake.outlet$Hn)
e18 <- estar18 + ek18
e2 <- estar2 + ek2
m18 <- (lake.outlet$Hn - (10^-3 * e18)) / (1 - lake.outlet$Hn + (10^-3 * ek18))
m2 <- (lake.outlet$Hn - (10^-3 * e2)) / (1 - lake.outlet$Hn + (10^-3 * ek2))
delta18star <- ((lake.outlet$Hn * lake.outlet$deltaatmoo) - e18) / (lake.outlet$Hn - (e18 * 10^-3))
delta2star <- ((lake.outlet$Hn * lake.outlet$deltaatmoh) - e2) / (lake.outlet$Hn - (e2 * 10^-3))
x18 <- (lake.outlet$d18O - lake.outlet$deltaOinput)/(m18*(delta18star-lake.outlet$d18O))
x2 <- (lake.outlet$d2H - lake.outlet$deltaHinput)/(m2*(delta2star-lake.outlet$d2H))

evapo <- x18 *  ((lake.outlet$prcp.bor.dam.area + trib.dis.iso.sum$sum.dis + upper.snake.river.monthly$month.dis)/jack.lake.area) * 1000
evaph <- x2 *  ((lake.outlet$prcp.bor.dam.area + trib.dis.iso.sum$sum.dis + upper.snake.river.monthly$month.dis)/jack.lake.area) * 1000



#### Following Gibson (2016) ####
lake.outlet$m18 <- (lake.outlet$Hn - (10^-3 * (((lake.outlet$eko+ lake.outlet$estaro/lake.outlet$a18o)))))/
  (1 - lake.outlet$Hn + (10^-3 * lake.outlet$eko))
lake.outlet$m2 <- (lake.outlet$Hn -10^-3 * (lake.outlet$ekh+ (lake.outlet$estarh/lake.outlet$a2h)))/
  (1 - lake.outlet$Hn + (10^-3 * lake.outlet$ekh))

lake.outlet$delstar18 <-((lake.outlet$Hn * lake.outlet$deltaatmoo)+lake.outlet$eko+(lake.outlet$estaro/ lake.outlet$a18o)) /
  (lake.outlet$Hn - 10^-3 * (lake.outlet$eko + (lake.outlet$estaro / lake.outlet$a18o)))
lake.outlet$delstar2 <-((lake.outlet$Hn * lake.outlet$deltaatmoh)+lake.outlet$ekh+(lake.outlet$estarh/ lake.outlet$a2h)) /
  (lake.outlet$Hn - 10^-3 * (lake.outlet$ekh + (lake.outlet$estarh / lake.outlet$a2h)))

lake.outlet$ei18 <- (lake.outlet$d18O - lake.outlet$deltaOinput) / (lake.outlet$m18 *(lake.outlet$delstar18*lake.outlet$d18O)) 
lake.outlet$ei2 <- (lake.outlet$d2H - lake.outlet$deltaHinput) / (lake.outlet$m2 *(lake.outlet$delstar2*lake.outlet$d2H)) 

((lake.outlet$prcp.bor.dam.area + trib.dis.iso.sum$sum.dis + upper.snake.river.monthly$month.dis) * lake.outlet$ei18)/jack.lake.area



####Isotopic values of evaporation####
#evaportation delta value for hydrogen, deltalake is the lake isotope value, estar is equilibrium seperation, ah2/a18o is the frationation factor, deltaatmo is the atmospheric delta value, Hn is the normalized relative humidity, ek is kinetic seperation factor.
#hydrogen
lake.outlet$deltaevaph <- ((((lake.outlet$d2H-lake.outlet$estarh)/lake.outlet$a2h)-(lake.outlet$Hn*lake.outlet$deltaatmoh)-lake.outlet$ekh)/
                             (1-lake.outlet$Hn + (lake.outlet$ekh*10^-3)))
#oxygen

lake.outlet$deltaevapo <- ((((lake.outlet$d18O-lake.outlet$estaro)/lake.outlet$a18o)-(lake.outlet$Hn*lake.outlet$deltaatmoo)-lake.outlet$eko)/
                             ((1-lake.outlet$Hn) + (lake.outlet$eko*10^-3)))


####Jasechko 2014####
evap <- lake.outlet.2016$month.dis * ((lake.outlet.2016$deltaOinput- lake.outlet.2016$d18O)/ (lake.outlet.2016$deltaevapo - lake.outlet.2016$deltaOinput))    
