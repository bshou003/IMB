library(tidyverse)
library(readxl)
library(dataRetrieval)
library(stats)

####Weather Data Collection and Lake Temperatures####
#Jackson Airport has additional measurements temp, min temp, max temp, EA, ES, 
#dewpoint but were dropped because they will not be used currently
jack.airport.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/GSOD.Jackson.Airport.csv") |> 
  subset(select = c(YEAR, MONTH, RH)) |> 
  reframe(year = YEAR,
          month = MONTH,
          RH = RH) |> 
  #Grouping by year and month
  group_by(year, month) |>
  #Calculating the monthly average of RH because analysis is conducted on a monthly basis and percentage
  summarise(RH.jack.ap = (mean(RH)/100)) |>
  #Filtering the summer months
  filter(month >= 5 & month < 10) 

#Releases from Jackson Lake, may use discharge from the USGS gauge
bor.releases <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/bor.dam.releases.csv") %>% 
  mutate(date = as.Date(DateTime),
         #converting volume from acre-foot to km3
         jck.km3 = jck_af * 1.23348e-6,
         #calculating the percentage full Jackson lake is 
         jck.per = jck.km3 / (847000 * 1.23348e-6),
         #Calculating lake surface area from the relationship developed from remote sensing
         #and a polynomial equation from excel
         jck.area.km2 = ((-38.589 * jck.km3^2) + (82.796 * jck.km3) + 64.25),
         #jackson lake releases from cfs to m3d
         jck.dam.rel.m3d = jck_qd * 60 * 60 * 24 * 0.0283168,
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")))

#BoR data has min temp, max temp, and SWE but removed. Using for precipitation
bor.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/BoR.2022.2024.txt") |> 
  merge(bor.releases) |>
  mutate(date = as.Date(DateTime),
         prcp.m = jck_pp * 0.0254, #m
         #overlake precipitaiton m3
         prcp.bor.dam.area = prcp.m * (jck.area.km2 * 1000000),
         air.temp.c = (jck_mm-32)/(9/5),#celcius
         air.temp.k = (jck_mm-32)/(9/5) + 273.15,#temp in kelvin
         #jackson lake volume km3
         jck.km3 = jck.km3,
         jck.per = jck.per,
         #area of jackson lake km2
         jck.area.km2 = jck.area.km2,
         #jackson lake dam releases cfs to m3d
         jck.dam.rel.m3d = jck.dam.rel.m3d , 
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m"))) |> 
  subset(select = -c(date)) |>
  #Grouping by year and month
  group_by(year, month) |> 
  #Taking the monthly averages and sums
  summarise(prcp.bor.m3 = sum(prcp.bor.dam.area),
            air.temp.bor.c =  mean(air.temp.c),
            air.temp.bor.k = mean(air.temp.k),
            jck.dam.rel.m3d = sum(jck.dam.rel.m3d),
            jck.km3 = mean(jck.km3),
            #Will be used to calculate more accurate on-lake precipitation amounts
            jck.area.km2 = mean(jck.area.km2)) |> 
  #Filtering the summer months
  filter(month >= 5 & month < 10)%>% 
  ungroup() %>% 
  mutate(vol.change.m3 = (jck.km3 - lag(jck.km3, default = jck.km3[1])) * 1e+9)

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

#### Moved earlier in the script will erase once I confirm things work####
# #Releases from Jackson Lake, may use discharge from the USGS gauge
# bor.releases <- read.csv("~/Documents/Data/Chapter.3/bor.dam.releases/bor.dam.releases.csv") %>% 
#   mutate(date = as.Date(DateTime),
#          #converting volume from acre-foot to km3
#          jck.km3 = jck_af * 1.23348e-6,
#          #calculating the percentage full Jackson lake is 
#          jck.per = jck_af / (847000 * 1.23348e-6),
#          #Calculating lake surface area from the relationship developed from remote sensing
#          #and a polynomial equation from excel
#          jck.area.km2 = ((-38.589 * jck.km3^2) + (82.796 * jck.km3) + 64.25),
#          year = as.numeric(format(date, format = "%Y")),
#          month = as.numeric(format(date, format = "%m")))|> 
#   #Grouping by year and month
#   group_by(year, month) |> 
#   #Filtering the summer months
#   filter(month >= 5 & month < 10) |> 
#   reframe(jck.km3 = mean(jck.km3),
#           #Will be used to calculate more accurate on-lake precipitation amounts
#           jck.area.km2 = mean(jck.area.km2)) %>% 
#   ungroup() %>% 
#   mutate(vol.change.km3 = jck.km3 - lag(jck.km3, default = jck.km3[1]))

# #Calling the lake data, Currently not using the lake data, commenting out
# JL_YSI_sur <- read_csv('~/Documents/Data/Lake_YSI/2023_YSI.csv',show_col_types = FALSE) |>
#   mutate(DATE = as.Date(DATE, "%m/%d/%Y")) |>   #Calling the YSI data for site JL1-JL16
#   group_by(SITE, Event) |>
#   filter(Depth.m == min(Depth.m)) |>
#   mutate(lake.temp.C = (Temp.F - 32) * (5/9),
#          lake.temp.k = lake.temp.C + 273.15,
#          year = as.numeric(format(DATE, format = "%Y")),
#          month = as.numeric(format(DATE, format = "%m"))) |>
#   subset(select = c(Event, SITE, lake.temp.C,lake.temp.k, year, month)) |>
#   filter(Event == 2 | Event == 4 | Event == 6)

#Calling lake outlet data
lake.outlet.ysi <- read_csv("~/Documents/Data/Trib_YSI/YSI.Tribs.csv") |> 
  filter(Setting == "Lake.Outlet") %>%
  group_by(Event) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         lake.outlet.temp.c = mean(as.numeric(Temp)),#taking the mean because the first event is triplicated
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         lake.outlet.temp.k = as.numeric(lake.outlet.temp.c) + 273.15)%>% 
  subset(select = c(year, month, lake.outlet.temp.c, lake.outlet.temp.k, Event)) %>% 
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
# lake.outlet.ysi$Event <- NA
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 2] <- 7
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 4] <- 8
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 6] <- 9

####Calling Isotope Data####
#OPIC (precipitation) data, Gabe Bowen isoscapes https://wateriso.utah.edu/waterisotopes/index.html
OPIC <- read_csv("~/Documents/Data/Chapter.3/Isotope.Data/OPIC.Data.csv") |>
  rename(month = MONTH)

gw <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "AMK Tap") |> 
  group_by(Event) %>% 
  mutate(d18Og = mean(d18O),
         d2Hg = mean(d2H)) %>% 
  subset(select = -c(d18O, d2H, SITE,seq_position, Setting.Type,on,Original_name, location)) %>% 
  distinct()


stream.isotopes <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Stream" | Setting.Type == "River") |> 
  subset(select = -c(seq_position, Setting.Type)) |> 
  filter(SITE != "26W" & SITE != "26E")

lake.outlet <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Lake.outlet") |> 
  subset(select = -c(SITE,seq_position, Setting.Type, on, Original_name, location)) |> 
  group_by(Event) |>
  arrange(Event) |>
  #Several samples were analyzed multiple times, these are averaged to simplify
  mutate(d18O = mean(d18O),
         d2H = mean(d2H),
         dxs = mean(dxs))|>
  distinct() |> 
  merge(lake.outlet.ysi) |> 
  #merge(bor.releases) |>
  merge(bor.monthly) |> 
  merge(jack.airport.monthly) |> 
  merge(OPIC) #|>
  # merge(gw) |>
  # ET doesn't contain 2024, temporarily removed
  #merge(et)
#Have done this outside the previous bit of script, was getting NA's, but this works
lake.outlet <- lake.outlet %>% 
  mutate(d18Olag = d18O - lag(d18O),
         d2Hlag = d2H - lag(d2H))

rm(bor.monthly, bor.releases, jack.airport.monthly,OPIC, lake.outlet.ysi,et, gw, upper.snake.precip.month)
####Fractionation Factors for both oxygen and hydrogen following Horita and Wesolowski (1994) & Majoube (1971), a 
#good description is presented in Gibson (2016) ####
#Need to ensure excel/csv column name matches this script, temperature values need to be kelvin
#From Mercer(2022)
lake.outlet$a2hplus <- exp(((24.844 * (10^6 / lake.outlet$lake.outlet.temp.k^2)) - 
                                   (76.248 * (10^3 / lake.outlet$lake.outlet.temp.k)) + 52.612)/1000)
lake.outlet$a18oplus <- exp(((1.137 * (10^6 / lake.outlet$lake.outlet.temp.k^2)) - 
                                    (0.4156 * (10^3 / lake.outlet$lake.outlet.temp.k)) - 2.0667)/1000)

####Equilibrium Separation####
#Equilibrium Separation for hydrogen & oxygen using Fractionation Factor from Horita & Wesolowski (1994)
lake.outlet$ehplus <- (lake.outlet$a2hplus -1) * 1000
lake.outlet$eoplus <- (lake.outlet$a18oplus -1) * 1000 

####Humidity inputs####
#Saturation Pressure for the atmosphere
#From Ward and Elliot (1995)
#(kPa) For this I will need the temperature (celcius) of the air collected from Jackson Lake Dam
lake.outlet$Psatair <- exp(((16.78* lake.outlet$air.temp.bor.c) - 116.9) / 
                             (lake.outlet$air.temp.bor.c + 237.3)) 
#Saturation Pressure for the lake water
#(kPa) For this I will need the water temperature (celcius) from the YSI and the interpolation maps.
lake.outlet$Psatwater <- exp(((16.78* lake.outlet$lake.outlet.temp.c) - 116.9) / 
                               (lake.outlet$lake.outlet.temp.c + 237.3)) 
#Normalized Relative Humidity
#Psatair & Psatwater indicate saturation vapor pressure with respect to air and water temperature. (Mook, 2000*)
lake.outlet$Hn <- (lake.outlet$RH.jack.ap * (lake.outlet$Psatair / 
                                                    lake.outlet$Psatwater))

####Kinetic Separation Factor####
theta = 1
ck18O <- 14.2
ck2H <- 12.5

#Kinetic Separation Factor
lake.outlet$ekh <- theta * ck2H * (1-lake.outlet$Hn)
lake.outlet$eko <- theta * ck18O * (1-lake.outlet$Hn)
####Atmospheric Isotope Values####
#deltaprcp is the delta value of the precipitationfrom Gabe Bowens precipitation map, and eplus is the equilibrium separation calculated from above#oxygen 
lake.outlet$deltaatmoh1 <- (lake.outlet$deltaprcph - lake.outlet$ehplus)     
lake.outlet$deltaatmoo1 <- (lake.outlet$deltaprcpo - lake.outlet$eoplus)

lake.outlet$deltaatmoh2 <- (lake.outlet$deltaprcph - (log10(lake.outlet$a2hplus) * 1000))  
lake.outlet$deltaatmoo2 <- (lake.outlet$deltaprcpo - (log10(lake.outlet$a18oplus) * 1000))

lake.outlet$deltaatmoh3 <- (lake.outlet$deltaprcph - lake.outlet$ehplus)/(lake.outlet$a2hplus)  
lake.outlet$deltaatmoo3 <- (lake.outlet$deltaprcpo - lake.outlet$eoplus)/(lake.outlet$a18oplus) 

k = 0.5 #Suggested for highly seasonal climates Gibson (2015)
#k is a value that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015))
lake.outlet$deltaatmoh4 <- (lake.outlet$deltaprcph - (k * lake.outlet$ehplus))/(1+(10^-3*k*lake.outlet$ehplus))  
lake.outlet$deltaatmoo4 <- (lake.outlet$deltaprcpo - (k * lake.outlet$eoplus))/(1+(10^-3*k*lake.outlet$eoplus)) 

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

#Formatting stream.isotopes dataframe#
stream.isotopes <- stream.isotopes %>% 
  group_by(Event, SITE) %>% 
  mutate(d18O = mean(d18O),
         d2H = mean(d2H),
         dxs = mean(dxs),
         SITE = as.numeric(SITE)) %>% 
  distinct()%>% 
  subset(select = c(SITE, Event,d18O, d2H,dxs,month, year))

#Creating a data frame for Upper Snake#
river <- stream.isotopes %>% 
  filter(SITE == 15) %>% 
  mutate(Event = as.numeric(Event)) %>% 
  subset(select = c(SITE, Event,d18O, d2H,dxs,month, year)) %>% 
  arrange(Event)

#Calling USGS Data#
upper.snake.river.monthly <- strmcall(siteid, parameter, start, end) 
#Summing the USGS data on a monthly basis
upper.snake.river.monthly <- upper.snake.river.monthly %>% 
  mutate(year = as.numeric(format(Date, format = "%Y")),
         month = as.numeric(format(Date, format = "%m")),
         day = as.numeric(format(Date, format = "%d")),
         SITE = 15,
         daily.total = X_00060_00003 * 24 * 60 * 60 * 0.0283168) %>%  #converting cfs to cfd then to m3d
  group_by(month, year) %>% 
  mutate(month.dis.r = sum(daily.total))%>%  #summing m3d by each month 
  select(subset = -c(agency_cd,site_no,Date,X_00060_00003,X_00060_00003_cd, day, daily.total)) %>% 
  distinct()

#Adding event data based on year and month
# upper.snake.river.monthly$Event <- NA
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 7 & upper.snake.river.monthly$year == 2023] <- 7
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 8 & upper.snake.river.monthly$year == 2023] <- 8
# upper.snake.river.monthly$Event[upper.snake.river.monthly$month == 9 & upper.snake.river.monthly$year == 2023] <- 9

#Merging the isotope data with discharge data.
upper.snake.river.monthly <- upper.snake.river.monthly %>% 
  merge(river) %>% #merging to isotope data
  mutate(month.dis.18o.r = month.dis.r * d18O,
         month.dis.2h.r = month.dis.r * d2H,
         dxs.r = dxs) %>% 
  select(subset = -c(SITE,d18O,d2H,dxs))

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
  mutate(dis.18O.t = dis * d18O * 60 * 60 * 24, #m3s to m3d
         dis.2H.t = dis * d2H * 60 * 60 * 24) %>%  #m3s to m3d 
  group_by(Event) %>% 
  reframe(Event = Event,
          sum.dis.t = sum(dis) * 60 * 60 * 24, #m3s to m3d
          sum.disv.t = sum(disv)* 60 * 60 * 24, #m3s to m3d
          sumd18o.t = sum(dis.18O.t),
          sumd2h.t = sum(dis.2H.t)) %>% 
  distinct()

#jack.lake.area <- 1.03366e+8 #meters
lake.outlet <- lake.outlet %>% 
  mutate(prcp.bor.area.18o = prcp.bor.m3 * deltaprcpo,
         prcp.bor.area.2h = prcp.bor.m3 * deltaprcph,
         prcp.bor.m3 = prcp.bor.m3) %>%  
  merge(upper.snake.river.monthly) %>% 
  merge(trib.dis.iso.sum)
#2016#
#U refers to the streams, P is precipitation and is the overlake precipitaiton amount, R is snake river inflow
lake.outlet$deltaHinput <- (lake.outlet$sumd2h.t + lake.outlet$prcp.bor.area.2h + lake.outlet$month.dis.2h.r)  / 
  (lake.outlet$prcp.bor.m3 + lake.outlet$sum.dis.t + lake.outlet$month.dis.r) 
lake.outlet$deltaOinput <- (lake.outlet$sumd18o.t + lake.outlet$prcp.bor.area.18o+ lake.outlet$month.dis.18o.r)  / 
  (lake.outlet$prcp.bor.m3 + lake.outlet$sum.dis.t + lake.outlet$month.dis.r)

#calculation of m and delta star 2016#
lake.outlet$m2 <- (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)) / 
  (1-lake.outlet$Hn+10^-3*lake.outlet$ekh)
lake.outlet$m18 <- (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)) / 
  (1-lake.outlet$Hn+10^-3*lake.outlet$eko)

lake.outlet$deltastarh <- (((lake.outlet$Hn * lake.outlet$deltaatmoh3) + lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus))
lake.outlet$deltastaro <- (((lake.outlet$Hn * lake.outlet$deltaatmoo3) + lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus))

#E/I#
lake.outlet$ei2 <- (lake.outlet$d2H-lake.outlet$deltaHinput)/((lake.outlet$m2 *(lake.outlet$deltastarh-lake.outlet$d2H)))
lake.outlet$ei18 <- (lake.outlet$d18O-lake.outlet$deltaOinput)/((lake.outlet$m18 *(lake.outlet$deltastaro-lake.outlet$d18O)))
lake.outlet$ei2 <- (lake.outlet$deltaHinput- lake.outlet$d2H)/((lake.outlet$m2 *(lake.outlet$deltastarh-lake.outlet$d2H)))
lake.outlet$ei18 <- (lake.outlet$deltaOinput-lake.outlet$d18O)/((lake.outlet$m18 *(lake.outlet$deltastaro-lake.outlet$d18O)))
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


t <- lake.outlet$prcp.bor.m3 + lake.outlet$sum.dis.t + lake.outlet$month.dis.r - lake.outlet$jck.dam.rel.m3d + lake.outlet$vol.change.m3 
#Evaporation#
lake.outlet$deltaevaph <- (((lake.outlet$d2H*lake.outlet$a2hplus)-(lake.outlet$Hn*lake.outlet$deltaatmoh3)
                                 -lake.outlet$ehplus-lake.outlet$ekh)/((1-lake.outlet$Hn) + (lake.outlet$ekh/10^3)))
#oxygen
lake.outlet$deltaevapo <- (((lake.outlet$d18O*lake.outlet$a18oplus)-(lake.outlet$Hn*lake.outlet$deltaatmoo3)
                                 -lake.outlet$eoplus-lake.outlet$eko)/((1-lake.outlet$Hn) + (lake.outlet$eko/10^3)))


####Jasechko 2014####
lake.outlet$evap <- (lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaOinput- lake.outlet$d18O)/ 
                                   (lake.outlet$deltaevapo - lake.outlet$deltaOinput)) )  + lake.outlet$vol.change.m3 
lake.outlet$evap.mm <- (lake.outlet$evap * 100) / (lake.outlet$jck.area.km2*1000000)


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

 
####Jasechko 2014####
evap <- lake.outlet.2016$month.dis * ((lake.outlet.2016$deltaOinput- lake.outlet.2016$d18O)/ (lake.outlet.2016$deltaevapo - lake.outlet.2016$deltaOinput))    
