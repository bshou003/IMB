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
         air.temp.c = (jck_mm-32)/(9/5), #Celsius
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
  mutate(vol.change.m3 = (jck.km3 - lag(jck.km3, default = jck.km3[1])) * 1e+9) |>
  #Filtering the summer months
  filter(month >= 5 & month < 10)%>% 
  ungroup() 
  

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

#Isotope Precipitation from Yellowstone
prcip.iso <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/precipitation.isotopes/yellowstone_precip_iso.csv") %>% 
  subset(select = c(Start_Date, Collection_Date, d2H, d18O)) %>% 
  mutate(Start_Date = as.Date(Start_Date),
         Collection_Date = as.Date(Collection_Date),
         year = as.numeric(format(Collection_Date, format = "%Y")),
         month = as.numeric(format(Collection_Date, format = "%m"))) %>% 
  group_by(month) %>% 
  subset(select = -c(Start_Date, Collection_Date)) %>% 
  reframe(month=month,
          d2H.p = mean(d2H),
         d18O.p =  mean(d18O),
         dxs.p = d2H.p - (8*d18O.p)) %>%
  ungroup() %>% 
  distinct()

et.thorn <- read.csv("~/Documents/Data/Chapter.3/IMB/variables.data.tables/et.thorn")
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

monthly.weather.dam.releases <- lake.outlet.ysi |> 
merge(bor.monthly) |> 
  merge(jack.airport.monthly) %>% 
  merge(prcip.iso)
write.csv(monthly.weather.dam.releases, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/monthly.weather.dam.releases")
## Changing the event name from the YSI into the events that match my labeling scheme
# lake.outlet.ysi$Event <- NA
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 2] <- 7
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 4] <- 8
# lake.outlet.ysi$Event[lake.outlet.ysi$Event == 6] <- 9

####Calling Isotope Data####
#OPIC (precipitation) data, Gabe Bowen isoscapes https://wateriso.utah.edu/waterisotopes/index.html
# OPIC <- read_csv("~/Documents/Data/Chapter.3/Isotope.Data/OPIC.Data.csv") |>
#   rename(month = MONTH)

# gw <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
#   filter(Setting.Type == "AMK Tap") |> 
#   group_by(Event) %>% 
#   mutate(d18Og = mean(d18O),
#          d2Hg = mean(d2H),
#          dxg = mean(dxs)) %>% 
#   subset(select = -c(d18O, d2H, SITE,seq_position, Setting.Type,ON, dxs)) %>% 
#   distinct()
#Calling voronoi areas
#vaall <- read.csv("~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/vaall")
#Calling Lake isotopes
# lake.isotopes <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |>
#   filter(Setting.Type == "Lake") %>% 
#   group_by(Event, month, year, SITE ) %>% 
#   mutate(d18O.l = mean(d18O),
#          d2H.l = mean(d2H),
#          dxs.l = mean(dxs)) %>% 
#   subset(select = c(Event, d18O.l, d2H.l, dxs.l, month, year, SITE)) %>% 
#   distinct() %>% 
#   merge(vaall)
# 
# lake.iso.weighted <- lake.isotopes %>% 
#   mutate(d18O.l = d18O.l * v.area.m2,
#          d2H.l = d2H.l * v.area.m2,
#          dxs.l = dxs.l * v.area.m2) %>% 
#   group_by(Event) %>% 
#   reframe(Event = Event,
#             d18O.l = sum(d18O.l)/sum(v.area.m2),
#             d2H.l = sum(d2H.l)/sum(v.area.m2),
#             dxs.l = sum(dxs.l)/sum(v.area.m2)) %>% 
#   distinct()


stream.isotopes <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Stream" | Setting.Type == "River") |> 
  subset(select = -c(seq_position, Setting.Type,ON)) |> 
  filter(SITE != "26W" & SITE != "26E" & SITE != "15.7" & SITE != "1.7" & SITE != "34.700000000000003") %>%
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

lake.outlet <- read.csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") |> 
  filter(Setting.Type == "Lake.outlet") |> 
  subset(select = -c(SITE,seq_position, Setting.Type, ON)) |> 
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
  merge(jack.airport.monthly)%>% 
  merge(prcip.iso) %>%
  merge(et.thorn)
  #merge(lake.iso.weighted)
  #merge(OPIC) #|>
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
lake.outlet$deltaatmoh1 <- (lake.outlet$d2H.p - lake.outlet$ehplus)     
lake.outlet$deltaatmoo1 <- (lake.outlet$d18O.p - lake.outlet$eoplus)

lake.outlet$deltaatmoh2 <- (lake.outlet$d2H.p - (log10(lake.outlet$a2hplus) * 1000))  
lake.outlet$deltaatmoo2 <- (lake.outlet$d18O.p - (log10(lake.outlet$a18oplus) * 1000))

lake.outlet$deltaatmoh3 <- (lake.outlet$d2H.p - lake.outlet$ehplus)/(lake.outlet$a2hplus)  
lake.outlet$deltaatmoo3 <- (lake.outlet$d18O.p - lake.outlet$eoplus)/(lake.outlet$a18oplus) 

k = 0.5 #Suggested for highly seasonal climates Gibson (2015)
#k is a value that ranges from 0.5 (higly seasonal)-1 (non-seasonal) (Gibson (2015))
lake.outlet$deltaatmoh4 <- (lake.outlet$d2H.p - (k * lake.outlet$ehplus))/(1+(10^-3*k*lake.outlet$ehplus))  
lake.outlet$deltaatmoo4 <- (lake.outlet$d18O.p - (k * lake.outlet$eoplus))/(1+(10^-3*k*lake.outlet$eoplus)) 

#Evaporation#
lake.outlet$deltaevaph1 <- (((lake.outlet$d2H*lake.outlet$a2hplus)-(lake.outlet$Hn*lake.outlet$deltaatmoh1)
                             -lake.outlet$ehplus-lake.outlet$ekh)/((1-lake.outlet$Hn) + (lake.outlet$ekh/10^3)))
lake.outlet$deltaevapo1 <- (((lake.outlet$d18O*lake.outlet$a18oplus)-(lake.outlet$Hn*lake.outlet$deltaatmoo1)
                             -lake.outlet$eoplus-lake.outlet$eko)/((1-lake.outlet$Hn) + (lake.outlet$eko/10^3)))
lake.outlet$deltaevaph2 <- (((lake.outlet$d2H*lake.outlet$a2hplus)-(lake.outlet$Hn*lake.outlet$deltaatmoh2)
                             -lake.outlet$ehplus-lake.outlet$ekh)/((1-lake.outlet$Hn) + (lake.outlet$ekh/10^3)))
lake.outlet$deltaevapo2 <- (((lake.outlet$d18O*lake.outlet$a18oplus)-(lake.outlet$Hn*lake.outlet$deltaatmoo2)
                             -lake.outlet$eoplus-lake.outlet$eko)/((1-lake.outlet$Hn) + (lake.outlet$eko/10^3)))
lake.outlet$deltaevaph3 <- (((lake.outlet$d2H*lake.outlet$a2hplus)-(lake.outlet$Hn*lake.outlet$deltaatmoh3)
                             -lake.outlet$ehplus-lake.outlet$ekh)/((1-lake.outlet$Hn) + (lake.outlet$ekh/10^3)))
lake.outlet$deltaevapo3 <- (((lake.outlet$d18O*lake.outlet$a18oplus)-(lake.outlet$Hn*lake.outlet$deltaatmoo3)
                             -lake.outlet$eoplus-lake.outlet$eko)/((1-lake.outlet$Hn) + (lake.outlet$eko/10^3)))
lake.outlet$deltaevaph4 <- (((lake.outlet$d2H*lake.outlet$a2hplus)-(lake.outlet$Hn*lake.outlet$deltaatmoh4)
                            -lake.outlet$ehplus-lake.outlet$ekh)/((1-lake.outlet$Hn) + (lake.outlet$ekh/10^3)))
lake.outlet$deltaevapo4 <- (((lake.outlet$d18O*lake.outlet$a18oplus)-(lake.outlet$Hn*lake.outlet$deltaatmoo4)
                            -lake.outlet$eoplus-lake.outlet$eko)/((1-lake.outlet$Hn) + (lake.outlet$eko/10^3)))

lake.outlet <- lake.outlet %>% 
  mutate(evap.dxs1 = deltaevaph1 - (8 * deltaevapo1),
         evap.dxs2 = deltaevaph2 - (8 * deltaevapo2),
         evap.dxs3 = deltaevaph3 - (8 * deltaevapo3),
         evap.dxs4 = deltaevaph4 - (8 * deltaevapo4))

isotopic.variables <- lake.outlet %>% 
  subset(select = c(Event, year, month,a2hplus,a18oplus,ehplus,eoplus,ekh,eko,deltaatmoh1,deltaatmoo1, deltaatmoh2,deltaatmoo2,deltaatmoh3,deltaatmoo4 ,deltaevaph1,
                    deltaevapo1,deltaevaph2,deltaevapo2,deltaevaph3,deltaevapo3,deltaevaph4,deltaevapo4))
  

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
         dxs.r = dxs * month.dis.r) %>% 
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
         dis.2H.t = dis * d2H * 60 * 60 * 24,
         dis.dxs.t = dis * dxs * 60 *60 *24) %>%  #m3s to m3d 
  group_by(Event) %>% 
  reframe(Event = Event,
          sum.dis.t = sum(dis) * 60 * 60 * 24, #m3s to m3d
          sum.disv.t = sum(disv)* 60 * 60 * 24, #m3s to m3d
          sumd18o.t = sum(dis.18O.t),
          sumd2h.t = sum(dis.2H.t),
          sum.dxs.t = sum(dis.dxs.t)) %>% 
  distinct()

lake.outlet <- lake.outlet %>% 
  mutate(prcp.bor.area.18o = prcp.bor.m3 * d18O.p,
         prcp.bor.area.2h = prcp.bor.m3 * d2H.p,
         prcp.bor.m3 = prcp.bor.m3,
         prcp.area.dxs = dxs.p * prcp.bor.m3) %>%  
  merge(upper.snake.river.monthly) %>% 
  merge(trib.dis.iso.sum) %>% 
  mutate(dsxs.tr = (sum.dxs.t + dxs.r + dxs.p)/(sum.dis.t + month.dis.r +prcp.bor.m3))

####Inputs averaging####
#U refers to the streams, P is precipitation and is the overlake precipitation amount, R is snake river inflow
lake.outlet$deltaHinput <- (lake.outlet$sumd2h.t + lake.outlet$prcp.bor.area.2h + lake.outlet$month.dis.2h.r)  / 
  (lake.outlet$prcp.bor.m3 + lake.outlet$sum.dis.t + lake.outlet$month.dis.r) 
lake.outlet$deltaOinput <- (lake.outlet$sumd18o.t + lake.outlet$prcp.bor.area.18o+ lake.outlet$month.dis.18o.r)  / 
  (lake.outlet$prcp.bor.m3 + lake.outlet$sum.dis.t + lake.outlet$month.dis.r)



#calculation of m and delta star 2016#
lake.outlet$m2 <- (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)) / 
  (1-lake.outlet$Hn+10^-3*lake.outlet$ekh)
lake.outlet$m18 <- (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)) / 
  (1-lake.outlet$Hn+10^-3*lake.outlet$eko)

lake.outlet$deltastarh1 <- (((lake.outlet$Hn * lake.outlet$deltaatmoh1) + lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus))
lake.outlet$deltastaro1 <- (((lake.outlet$Hn * lake.outlet$deltaatmoo1) + lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus))

lake.outlet$deltastarh2 <- (((lake.outlet$Hn * lake.outlet$deltaatmoh2) + lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus))
lake.outlet$deltastaro2 <- (((lake.outlet$Hn * lake.outlet$deltaatmoo2) + lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus))

lake.outlet$deltastarh3 <- (((lake.outlet$Hn * lake.outlet$deltaatmoh3) + lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus))
lake.outlet$deltastaro3 <- (((lake.outlet$Hn * lake.outlet$deltaatmoo3) + lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus))

lake.outlet$deltastarh4 <- (((lake.outlet$Hn * lake.outlet$deltaatmoh4) + lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$ekh + lake.outlet$ehplus)/lake.outlet$a2hplus))
lake.outlet$deltastaro4 <- (((lake.outlet$Hn * lake.outlet$deltaatmoo4) + lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus)/
  (lake.outlet$Hn - 10^-3 *((lake.outlet$eko + lake.outlet$eoplus)/lake.outlet$a18oplus))
#E/I#
lake.outlet$ei21 <- (lake.outlet$d2H-lake.outlet$deltaHinput)/((lake.outlet$m2 *(lake.outlet$deltastarh1-lake.outlet$d2H)))
lake.outlet$ei181 <- (lake.outlet$d18O-lake.outlet$deltaOinput)/((lake.outlet$m18 *(lake.outlet$deltastaro1-lake.outlet$d18O)))

lake.outlet$ei22 <- (lake.outlet$d2H-lake.outlet$deltaHinput)/((lake.outlet$m2 *(lake.outlet$deltastarh2-lake.outlet$d2H)))
lake.outlet$ei182 <- (lake.outlet$d18O-lake.outlet$deltaOinput)/((lake.outlet$m18 *(lake.outlet$deltastaro2-lake.outlet$d18O)))

lake.outlet$ei23 <- (lake.outlet$d2H-lake.outlet$deltaHinput)/((lake.outlet$m2 *(lake.outlet$deltastarh3-lake.outlet$d2H)))
lake.outlet$ei183 <- (lake.outlet$d18O-lake.outlet$deltaOinput)/((lake.outlet$m18 *(lake.outlet$deltastaro3-lake.outlet$d18O)))

lake.outlet$ei24 <- (lake.outlet$d2H-lake.outlet$deltaHinput)/((lake.outlet$m2 *(lake.outlet$deltastarh4-lake.outlet$d2H)))
lake.outlet$ei184 <- (lake.outlet$d18O-lake.outlet$deltaOinput)/((lake.outlet$m18 *(lake.outlet$deltastaro4-lake.outlet$d18O)))


####dxs E/I ####
lake.outlet$eid1 <- (lake.outlet$dsxs.tr - lake.outlet$dxs)/(lake.outlet$evap.dxs1 - lake.outlet$dxs)
lake.outlet$eid2 <- (lake.outlet$dsxs.tr - lake.outlet$dxs)/(lake.outlet$evap.dxs2 - lake.outlet$dxs)
lake.outlet$eid3 <- (lake.outlet$dsxs.tr - lake.outlet$dxs)/(lake.outlet$evap.dxs3 - lake.outlet$dxs)
lake.outlet$eid4 <- (lake.outlet$dsxs.tr - lake.outlet$dxs)/(lake.outlet$evap.dxs4 - lake.outlet$dxs)

lake.outlet$evaph1 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaHinput - lake.outlet$d2H)/ 
                                                       (lake.outlet$deltaevaph1-lake.outlet$deltaHinput))
lake.outlet$evapo1 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaOinput - lake.outlet$d18O)/ 
                                                       (lake.outlet$deltaevapo1-lake.outlet$deltaOinput))

lake.outlet$evaph2 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaHinput - lake.outlet$d2H)/ 
                                                       (lake.outlet$deltaevaph2-lake.outlet$deltaHinput))
lake.outlet$evapo2 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaOinput - lake.outlet$d18O)/ 
                                                       (lake.outlet$deltaevapo1-lake.outlet$deltaOinput))

lake.outlet$evaph3 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaHinput - lake.outlet$d2H)/ 
                                                       (lake.outlet$deltaevaph3-lake.outlet$deltaHinput))
lake.outlet$evapo3 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaOinput - lake.outlet$d18O)/ 
                                                       (lake.outlet$deltaevapo3-lake.outlet$deltaOinput))
lake.outlet$evaph4 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaHinput - lake.outlet$d2H)/ 
                                                       (lake.outlet$deltaevaph4-lake.outlet$deltaHinput))
lake.outlet$evapo4 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$deltaOinput - lake.outlet$d18O)/ 
                                                       (lake.outlet$deltaevapo4-lake.outlet$deltaOinput))

lake.outlet$evapd1 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$dsxs.tr - lake.outlet$dxs)/ 
                                                       (lake.outlet$evap.dxs1-lake.outlet$dsxs.tr))
lake.outlet$evapd2 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$dsxs.tr - lake.outlet$dxs)/ 
                                                       (lake.outlet$evap.dxs2-lake.outlet$dsxs.tr))
lake.outlet$evapd3 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$dsxs.tr - lake.outlet$dxs)/ 
                                                       (lake.outlet$evap.dxs3-lake.outlet$dsxs.tr))
lake.outlet$evapd4 <- lake.outlet$jck.dam.rel.m3d * ((lake.outlet$dsxs.tr - lake.outlet$dxs)/ 
                                                       (lake.outlet$evap.dxs4-lake.outlet$dsxs.tr))


rt <- lake.outlet$eid3 * ((lake.outlet$jck.km3 * 1000000000)/(lake.outlet$jck.area.km2 * 1000000* lake.outlet$et.m.tw))


lake.outlet.longer <- lake.outlet %>% 
subset(select = -c(month, year)) %>% 
  pivot_longer(- Event, names_to = "variables", values_to = "values")
  
ggplot(lake.outlet.longer, aes(x=variables, y=values))+
  geom_boxplot()+
  facet_wrap(~variables,  scales = "free_y")

ggplot() +
  geom_point(data = lake.outlet, aes(x = eid3, y = prcp.bor.m3))
ggplot() +
  geom_point(data = lake.outlet, aes(x = eid3, y = jck.area.km2))

t <- t %>% 
  mutate(id = rownames(t),
         event = col


t <- lake.outlet.longer[-1] %>% t() %>% as.data.frame() %>% setNames(lake.outlet.longer[,1])
ggplot()+
  geom_boxplot(data = t)


ggplot()+
  geom_point(data=lake.outlet, aes(x=jck.dam.rel.m3d, y=month.dis.r))


#########Prcipitation vs discharge and sampling time Upper Snake Gauge#####
library(ggplot2)
library(ggpubr)

upper.snake.precip.month <- read.csv("~/Documents/Data/Chapter.3/Weather/upper.snake.river.precip.csv") |> 
  mutate(date = as.POSIXct(DATE),
         prcp.m = PRCP * 0.254)

bor.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/BoR.2022.2024.csv") |> 
  mutate(prcp.m = jck_pp * 0.254)

upper.snake.precip.month$dateTime <- with(upper.snake.precip.month,ymd(upper.snake.precip.month$DATE) + hms(upper.snake.precip.month$TIME))
bor.monthly$dateTime <- with(bor.monthly,ymd(bor.monthly$DateTime) + hms(bor.monthly$time))

event1 <- readNWISuv("13010065", "00060","2022-04-30", "2022-05-14")
event2 <- readNWISuv("13010065", "00060","2022-06-03", "2022-06-18")
event3 <- readNWISuv("13010065", "00060","2022-06-11", "2022-06-26")
event4 <- readNWISuv("13010065", "00060","2022-06-30", "2022-07-15")
event5 <- readNWISuv("13010065", "00060","2022-07-11", "2022-07-25")
event6 <- readNWISuv("13010065", "00060","2022-08-14", "2022-08-29")
event7 <- readNWISuv("13010065", "00060","2023-06-21", "2023-07-06")
event8 <- readNWISuv("13010065", "00060","2023-08-07", "2023-08-22")
event9 <- readNWISuv("13010065", "00060","2023-09-07", "2023-09-22")
event10 <- readNWISuv("13010065", "00060","2024-06-03", "2024-06-18")
event11 <- readNWISuv("13010065", "00060","2024-06-30", "2024-07-14")
event12 <- readNWISuv("13010065", "00060","2024-08-02", "2024-08-17")
e1p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-04-30'), as.Date('2022-05-14')))
e2p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-06-03'), as.Date('2022-06-18')))
e3p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-06-11'), as.Date('2022-06-26')))
e4p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-06-30'), as.Date('2022-07-15')))
e5p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-07-11'), as.Date('2022-07-25')))
e6p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2022-08-14'), as.Date('2022-08-29')))
e7p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2023-06-21'), as.Date('2023-07-06')))
e8p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2023-08-07'), as.Date('2023-08-22')))
e9p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2023-09-07'), as.Date('2023-09-22')))
e10p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2024-06-03'), as.Date('2024-06-18')))
e11p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2024-06-30'), as.Date('2024-07-14')))
e12p <- upper.snake.precip.month %>% 
  filter(between(date, as.Date('2024-08-02'), as.Date('2024-08-17')))


d1 <- ggplot()+
  geom_line(data = (event1), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-13 00:00:00")))
d2 <- ggplot()+
  geom_line(data = (event2), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-17 15:45:00")))
d3 <- ggplot()+
  geom_line(data = (event3), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-25 11:00:00")))
d4 <- ggplot()+
  geom_line(data = (event4), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-14 14:45:00")))
d5 <- ggplot()+
  geom_line(data = (event5), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-24 15:30:00")))
d6 <- ggplot()+
  geom_line(data = (event6), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-08-28 10:00:00")))
d7 <- ggplot()+
  geom_line(data = (event7), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2023-07-05 20:00:00")))
d8 <- ggplot()+
  geom_line(data = (event8), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2023-08-21 20:45:00")))
d9 <- ggplot()+
  geom_line(data = (event9), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2023-09-21 17:45:00")))
d10 <- ggplot()+
  geom_line(data = (event10), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2024-06-17 19:45:00")))
d11 <- ggplot()+
  geom_line(data = (event11), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2024-07-13 18:45:00")))
d12 <- ggplot()+
  geom_line(data = (event12), aes(x = dateTime, y = X_00060_00000))+
  geom_vline(xintercept = as.POSIXct(as.Date("2024-08-16 18:00:00")))



coeff <- 0.0001

e1 <- ggplot() +
  geom_line(data = event1, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e1p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-13 00:00:00")), color = "red")

e2 <- ggplot() +
  geom_line(data = event2, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e2p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-17 15:45:00")), color = "red")

e3 <- ggplot() +
  geom_line(data = event3, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e3p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis"))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-25 11:00:00")), color = "red")

e4 <- ggplot() +
  geom_line(data = event4, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e4p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-14 14:45:00")), color = "red")

e5 <- ggplot() +
  geom_line(data = event5, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e5p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis"))   +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-24 15:30:00")), color = "red")

e6 <- ggplot() +
  geom_line(data = event6, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e6p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-08-28 10:00:00")), color = "red")

e7 <- ggplot() +
  geom_line(data = event7, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e7p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-07-05 20:00:00")), color = "red")

e8 <- ggplot() +
  geom_line(data = event8, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e8p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-08-21 20:45:00")), color = "red")

e9 <- ggplot() +
  geom_line(data = event9, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e9p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-09-21 17:45:00")), color = "red")

e10 <- ggplot() +
  geom_line(data = event10, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e10p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2024-06-17 19:45:00")), color = "red")

e11 <- ggplot() +
  geom_line(data = event11, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e11p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2024-07-13 18:45:00")), color = "red")

e12 <- ggplot() +
  geom_line(data = event12, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e12p, aes(x= dateTime, yend = prcp.m/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis"))+
  geom_vline(xintercept = as.POSIXct(as.Date("2024-08-16 18:00:00")), color = "red")

ggarrange(e1, e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12, ncol = 3, nrow = 4, align = 'h')
ggarrange(e7,e8,e9,e10,e11,e12, ncol = 3, nrow = 2, align = 'h')

#########Prcipitation vs discahrage and sampling time BOR#####
bor.monthly  <- read.csv("~/Documents/Data/Chapter.3/Weather/BoR.2022.2024.csv") |> 
  mutate(DateTime= as.POSIXct(DateTime))

bor.monthly$dateTime <- with(bor.monthly,ymd(bor.monthly$DateTime) + hms(bor.monthly$time))

event1 <- readNWISuv("13010065", "00060","2022-04-30", "2022-05-14")
event2 <- readNWISuv("13010065", "00060","2022-06-03", "2022-06-18")
event3 <- readNWISuv("13010065", "00060","2022-06-11", "2022-06-26")
event4 <- readNWISuv("13010065", "00060","2022-06-30", "2022-07-15")
event5 <- readNWISuv("13010065", "00060","2022-07-11", "2022-07-25")
event6 <- readNWISuv("13010065", "00060","2022-08-14", "2022-08-29")
event7 <- readNWISuv("13010065", "00060","2023-06-21", "2023-07-06")
event8 <- readNWISuv("13010065", "00060","2023-08-07", "2023-08-22")
event9 <- readNWISuv("13010065", "00060","2023-09-07", "2023-09-22")
event10 <- readNWISuv("13010065", "00060","2024-06-03", "2024-06-18")
event11 <- readNWISuv("13010065", "00060","2024-06-30", "2024-07-14")
event12 <- readNWISuv("13010065", "00060","2024-08-02", "2024-08-17")
e1p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-04-30'), as.Date('2022-05-14')))
e2p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-06-03'), as.Date('2022-06-18')))
e3p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-06-11'), as.Date('2022-06-26')))
e4p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-06-30'), as.Date('2022-07-15')))
e5p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-07-11'), as.Date('2022-07-25')))
e6p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2022-08-14'), as.Date('2022-08-29')))
e7p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2023-06-21'), as.Date('2023-07-06')))
e8p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2023-08-07'), as.Date('2023-08-22')))
e9p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2023-09-07'), as.Date('2023-09-22')))
e10p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2024-06-03'), as.Date('2024-06-18')))
e11p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2024-06-30'), as.Date('2024-07-14')))
e12p <- bor.monthly %>% 
  filter(between(DateTime, as.Date('2024-08-02'), as.Date('2024-08-17')))

coeff <- 0.001

e1 <- ggplot() +
  geom_line(data = event1, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e1p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")
  )+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-13 00:00:00")), color = "red")

e2 <- ggplot() +
  geom_line(data = event2, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e2p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")
  )+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-17 15:45:00")), color = "red")

e3 <- ggplot() +
  geom_line(data = event3, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e3p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)"))+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-06-25 11:00:00")), color = "red")

e4 <- ggplot() +
  geom_line(data = event4, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e4p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-14 14:45:00")), color = "red")

e5 <- ggplot() +
  geom_line(data = event5, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e5p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)"))   +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-07-24 15:30:00")), color = "red")

e6 <- ggplot() +
  geom_line(data = event6, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e6p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-08-28 10:00:00")), color = "red")

e7 <- ggplot() +
  geom_line(data = event7, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e7p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-07-05 20:00:00")), color = "red")

e8 <- ggplot() +
  geom_line(data = event8, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e8p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-08-21 20:45:00")), color = "red")

e9 <- ggplot() +
  geom_line(data = event9, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e9p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-09-21 17:45:00")), color = "red")

e10 <- ggplot() +
  geom_line(data = event10, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e10p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2024-06-17 19:45:00")), color = "red")

e11 <- ggplot() +
  geom_line(data = event11, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e11p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2024-07-13 18:45:00")), color = "red")

e12 <- ggplot() +
  geom_line(data = event12, aes(x = dateTime, y=X_00060_00000)) + 
  geom_segment(data = e12p, aes(x= dateTime, yend = jck_pp/ coeff, y=0),linewidth =2) + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the discharge (cfs)
    name = "discharge (cfs)",
    # Add a rainfall (in) and specify its features
    sec.axis = sec_axis(~.*coeff, name="rainfall (in)"))+
  geom_vline(xintercept = as.POSIXct(as.Date("2024-08-16 18:00:00")), color = "red")

ggarrange(e1, e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12, ncol = 3, nrow = 4, align = 'h')
ggarrange(e7,e8,e9,e10,e11,e12, ncol = 3, nrow = 2, align = 'h')



plot(e8)

ggplot() +
  geom_line(data = event8, aes(x = dateTime, y=X_00060_00000)) + 
  geom_vline(xintercept = as.POSIXct(as.Date("2023-08-21 20:45:00")), color = "red")