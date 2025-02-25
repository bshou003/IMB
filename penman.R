library(tidyverse)
library(readxl) #Calling in readxl to read in excel files

#Calling NSRDB or NCEI 1991-2010 averaged solar data. This is the monthly averages.
solar <- read.csv("~/Documents/Data/Chapter.3/Weather/Jackson.solar.1991.2010.monthly.average.csv") %>% 
  subset(select = c(MO, AVGLO)) %>% 
  rename(Month = MO)

#Calling weather data from GSOD Jackson Hole Airport
jack.airport  <- read.csv("~/Documents/Data/Chapter.3/Weather/GSOD.Jackson.Airport.csv") %>% 
  subset(select = -c(STNID, NAME, Notes)) %>% 
  rename(Year = YEAR,
         Month = MONTH,
         Day = DAY) %>% 
  merge(solar) %>% 
  mutate(date = as.Date(date),
         AVGLO =  0.0864 *AVGLO) #W m-2 day-1 to MJ m-2 day-1
#For June 2024 I have limited solar data so I am using the Hargreaves method to 
# calculate solar radiation
jack.airport.june <- jack.airport %>% 
  filter(Month == 6 & Year == 2024) %>% 
  mutate(albedo = 0.07,,
         dr = 1 + (0.033 * cos(((2 * pi)/365)*DOY)),
         del = 0.409 * sin(((2 * pi*DOY)/365 )- 1.39),
         latrad = (pi/180) * 43.605628,
         sunset = acos(-tan(latrad) * tan(del)),
         sin = sunset * sin(latrad) * sin(del),
         cos = sin(sunset) * cos(latrad) * cos(del),
         sincos = sin + cos,
         #extraterrestrial radiation
         ra = ((24*60)/pi) * 0.0820 * dr * sincos,
         #Hargreaves method after calculating the extraterrestrial radiation
         rn = 0.095 * sqrt(MAX - MIN) * ra)

#Calling solar (GHI) and albedo from https://nsrdb.nrel.gov/data-viewer on a 60 minute basis
nsrdb.solar.2022 <- read.csv("~/Documents/Data/Chapter.3/Weather/nsrdb_ghi_albedo_jackson_2022.csv", skip = 2) 
nsrdb.solar.2023 <- read.csv("~/Documents/Data/Chapter.3/Weather/nsrdb_ghi_albedo_jackson_2023.csv", skip = 2)

nsrdb.solar <- nsrdb.solar.2022 %>% 
  bind_rows(nsrdb.solar.2023) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(Surface.Albedo = mean(Surface.Albedo),
            ghi = mean(GHI) * 0.0864,
            albedo = 0.07)

#Calling weather data
jack.airport  <- jack.airport %>% 
  merge(nsrdb.solar) %>% 
  filter(Month >=5 & Month <= 9)
#Calling the AMK micromet station data
#Need to ensure columns line up with penman.parameters3
amkmm <- read.csv("~/Documents/Data/Chapter.3/Weather/2024_Onset.csv", skip = 1) %>% 
  rename(dateTime = Date.Time..GMT.06.00,
         par = PAR..µmol.m..s..LGR.S.N..10077818..SEN.S.N..689846.,
         prcp = Rain..mm..LGR.S.N..10077818..SEN.S.N..699504.,
         ghi = Solar.Radiation..W.m...LGR.S.N..10077818..SEN.S.N..711483.,
         WDSP = Wind.Speed..m.s..LGR.S.N..10077818..SEN.S.N..9675023.,
         gs = Gust.Speed..m.s..LGR.S.N..10077818..SEN.S.N..9675023.,
         wd = Wind.Direction..ø..LGR.S.N..10077818..SEN.S.N..9675023.,
         TEMP = Temp...C..LGR.S.N..10077818..SEN.S.N..9858920.,
         RH = RH.....LGR.S.N..10077818..SEN.S.N..9858920.,
         dp = DewPt...C..LGR.S.N..10077818..SEN.S.N..9858920.) %>% 
  mutate(dateTime = as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S"),
         time = format(as.POSIXct(dateTime),format = "%H:%M:%S"),
         date = as.Date(dateTime)) %>% 
  group_by(date) %>% 
  reframe(MIN = min(TEMP),
         MAX = max(TEMP),
         RH = mean(RH),
         prcp = sum(prcp, na.rm = TRUE),
         TEMP = mean(TEMP),
         dp = mean(dp),
         WDSP = mean(WDSP),
         ghi = mean(ghi) * 0.0864,
         albedo = 0.07) %>% 
  mutate(DOY = seq(from = 172, to = 266 ),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         day = as.numeric(format(date, format = "%d"))) %>% 
  filter(date != as.Date("24-06-20"))

#Calling the Fire on the Mountain micromet data
fotmmm <- read.csv("~/Documents/Data/Chapter.3/Weather/Micromet_FOTM.csv", skip = 2) %>% 
  rename(ghi = W.m..Solar.Radiation,
         MIN = X.C.Min.Air.Temperature,
         MAX = X.C.Max.Air.Temperature,
         TEMP = X.C.Air.Temperature,
         RH = X.C.RH.Sensor.Temp,
         WDSP = m.s.Wind.Speed,
         dateTime = Timestamps,
         prcp = mm.Precipitation) %>% 
  mutate(dateTime = as.POSIXct(dateTime, format = "%Y/%m/%d %H:%M:%S"),
         time = format(as.POSIXct(dateTime),format = "%H:%M:%S"),
         date = as.Date(dateTime)) %>% 
  group_by(date) %>% 
  reframe(MIN = min(TEMP),
          MAX = max(TEMP),
          RH = mean(RH),
          prcp = sum(prcp, na.rm = TRUE),
          TEMP = mean(TEMP),
          WDSP = mean(WDSP),
          ghi = mean(ghi) * 0.0864,
          albedo = 0.07) %>% 
  filter(date != as.Date("2024-06-18")) %>% 
  mutate(DOY = seq(from = 171, to = 274),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         day = as.numeric(format(date, format = "%d"))) 
#roughness length governing momentum transfer
zom = 0.0002
#roughness length governing transfer of heat and vapor
zoh = 0.00002
#zero plane displacement height
d = 0
#Height of the wind measurement
zm = 2 
#Height of the humidity measurement
zh = 2
#Soil (water) heat flux
g0 = 0.3
#Von Karman constant
vonkarmaen = 0.41
latentheatvapor = 2453
#Want to double check the solar information but everything else looks good
penman.parameters.amk <- amkmm %>% 
  mutate(
    #aerodynamic resistance (ra) (Allen 1998)
    ra1 = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
      (vonkarmaen^2 * WDSP),
    #Atmospheric pressure (kPA) (Allen 1998)
    pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
    #saturation vapor pressure
    es = (.611 * exp((17.3 * TEMP)/(TEMP + 237.3))),
    ea = (RH/100)*es,
    #Slope of the saturation vapor pressure curve (kPa C-1) (Following Dingman, 2015)
    delta = (2508.3 / (TEMP+237.3)^2)*exp((17.3 * TEMP)/(TEMP+237.3)),
    #air density (allen 1998)
    rhoa = (pressure)/(((1.01*(TEMP + 273))*0.287)),
    #The psychometric constant (allen 1998)
    psy = 0.000665*pressure,
    #specific heat of air (allen 1998)
    cp = (psy * 0.622 * 2.453)/ pressure,
    dr = 1 + (0.033 * cos(((2 * pi)/365)*DOY)),
    del = 0.409 * sin(((2 * pi*DOY)/365 )- 1.39),
    latrad = (pi/180) * 43.605628,
    sunset = acos(-tan(latrad) * tan(del)),
    sin = sunset * sin(latrad) * sin(del),
    cos = sin(sunset) * cos(latrad) * cos(del),
    sincos = sin + cos,
    ra = ((24*60)/pi) * 0.0820 * dr * sincos,
    rso = (0.75 + ((2*10^-5)* 2064.106)) * ra,
    rns = (1 - albedo) * ghi,
    tmaxk =(MAX + 273.16)^4,
    tmink = (MIN + 273.16)^4,
    tmmk = (tmaxk+tmink)/2,
    rnl2term = 0.34 - (0.14 * sqrt(ea)),
    rnl3term = ((1.35* ghi)/rso) - 0.35,
    rnl = (4.903 * 10^-9) * tmmk * rnl2term * rnl3term,
    rn = rns - rnl,
    estsr = 0.10 * sqrt(MAX - MIN) * ra,
    #Evapotranspiration
    et = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra1)))/
                     (latentheatvapor * (delta+ psy))),
    #Evapotranspiration with a water conduction flux
    etwf = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra1)))/
                      (latentheatvapor * (delta+ psy))))

penman.parameters.fotm <- fotmmm %>% 
  mutate(
    #aerodynamic resistance (ra) (Allen 1998)
    ra1 = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
      (vonkarmaen^2 * WDSP),
    #Atmospheric pressure (kPA) (Allen 1998)
    pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
    #saturation vapor pressure
    es = (.611 * exp((17.3 * TEMP)/(TEMP + 237.3))),
    ea = (RH/100)*es,
    #Slope of the saturation vapor pressure curve (kPa C-1) (Following Dingman, 2015)
    delta = (2508.3 / (TEMP+237.3)^2)*exp((17.3 * TEMP)/(TEMP+237.3)),
    #air density (allen 1998)
    rhoa = (pressure)/(((1.01*(TEMP + 273))*0.287)),
    #The psychometric constant (allen 1998)
    psy = 0.000665*pressure,
    #specific heat of air (allen 1998)
    cp = (psy * 0.622 * 2.453)/ pressure,
    dr = 1 + (0.033 * cos(((2 * pi)/365)*DOY)),
    del = 0.409 * sin(((2 * pi*DOY)/365 )- 1.39),
    latrad = (pi/180) * 43.605628,
    sunset = acos(-tan(latrad) * tan(del)),
    sin = sunset * sin(latrad) * sin(del),
    cos = sin(sunset) * cos(latrad) * cos(del),
    sincos = sin + cos,
    ra = ((24*60)/pi) * 0.0820 * dr * sincos,
    rso = (0.75 + ((2*10^-5)* 2064.106)) * ra,
    rns = (1 - albedo) * ghi,
    tmaxk =(MAX + 273.16)^4,
    tmink = (MIN + 273.16)^4,
    tmmk = (tmaxk+tmink)/2,
    rnl2term = 0.34 - (0.14 * sqrt(ea)),
    rnl3term = ((1.35* ghi)/rso) - 0.35,
    rnl = (4.903 * 10^-9) * tmmk * rnl2term * rnl3term,
    rn = rns - rnl,
    #Evapotranspiration 
    et = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra1)))/
                     (latentheatvapor * (delta+ psy))),
    #Evapotranspiration with a water conduction flux
    etwf = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra1)))/
                      (latentheatvapor * (delta+ psy))))

#Want to double check the solar information but everything else looks good
penman.parameters.gsod <- jack.airport %>% 
  mutate(
    #aerodynamic resistance (ra) (Allen 1998)
    ra1 = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
      (vonkarmaen^2 * WDSP),
    #Atmospheric pressure (kPA) (Allen 1998)
    pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
    #saturation vapor pressure
    es = (.611 * exp((17.3 * TEMP)/(TEMP + 237.3))),
    ea = (RH/100)*es,
    #Slope of the saturation vapor pressure curve (kPa C-1) (Following Dingman, 2015)
    delta = (2508.3 / (TEMP+237.3)^2)*exp((17.3 * TEMP)/(TEMP+237.3)),
    #air density (allen 1998)
    rhoa = (pressure)/(((1.01*(TEMP + 273))*0.287)),
    #The psychometric constant (allen 1998)
    psy = 0.000665*pressure,
    #specific heat of air (allen 1998)
    cp = (psy * 0.622 * 2.453)/ pressure,
    dr = 1 + (0.033 * cos(((2 * pi)/365)*DOY)),
    del = 0.409 * sin(((2 * pi*DOY)/365 )- 1.39),
    latrad = (pi/180) * 43.605628,
    sunset = acos(-tan(latrad) * tan(del)),
    sin = sunset * sin(latrad) * sin(del),
    cos = sin(sunset) * cos(latrad) * cos(del),
    sincos = sin + cos,
    ra = ((24*60)/pi) * 0.0820 * dr * sincos,
    rso = (0.75 + ((2*10^-5)* 2064.106)) * ra,
    rns = (1 - albedo) * ghi,
    tmaxk =(MAX + 273.16)^4,
    tmink = (MIN + 273.16)^4,
    tmmk = (tmaxk+tmink)/2,
    rnl2term = 0.34 - (0.14 * sqrt(ea)),
    rnl3term = ((1.35* ghi)/rso) - 0.35,
    rnl = (4.903 * 10^-9) * tmmk * rnl2term * rnl3term,
    rn = rns - rnl,
    #Evapotranspiration 
    et = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra1)))/
                     (latentheatvapor * (delta+ psy))),
    #Evapotranspiration with a water conduction flux
    etwf = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra1)))/
                      (latentheatvapor * (delta+ psy))))

penman.parameters.gsod.june <- jack.airport.june %>% 
  mutate( 
    #aerodynamic resistance (ra) (Allen 1998)
    ra1 = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
      (vonkarmaen^2 * WDSP),
    #Atmospheric pressure (kPA) (Allen 1998)
    pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
    #saturation vapor pressure
    es = (.611 * exp((17.3 * TEMP)/(TEMP + 237.3))),
    ea = (RH/100)*es,
    #Slope of the saturation vapor pressure curve (kPa C-1) (Following Dingman, 2015)
    delta = (2508.3 / (TEMP+237.3)^2)*exp((17.3 * TEMP)/(TEMP+237.3)),
    #air density (allen 1998)
    rhoa = (pressure)/(((1.01*(TEMP + 273))*0.287)),
    #The psychometric constant (allen 1998)
    psy = 0.000665*pressure,
    #specific heat of air (allen 1998)
    cp = (psy * 0.622 * 2.453)/ pressure,
    #Evapotranspiration
    et = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra1)))/
                     (latentheatvapor * (delta+ psy))),
    #Evapotranspiration with a water conduction flux
    etwf = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra1)))/
                      (latentheatvapor * (delta+ psy))))

####Summing the penman calculation on a monthly basis####
#Summing the solar unobserved data#
penman.june.unobserved <- penman.parameters.gsod.june %>% 
  filter(Day < 21) %>% 
  reframe(et = sum(etwf))
#Summing the AMK ET data#
monthly.penman.amk <- penman.parameters.amk %>% 
  group_by(month, year) %>% 
  reframe(et = sum(etwf))
#Fire on the Mountain ET data#
monthly.penman.fotm <- penman.parameters.fotm %>% 
  group_by(month, year) %>% 
  reframe(et = sum(etwf))
#Monthly GSOD ET data#
monthly.penman.gsod <- penman.parameters.gsod %>% 
  group_by(Month, Year) %>% 
  reframe(et = sum(etwf)) %>% 
  subset(select = c(Month, Year, et)) %>% 
  distinct()
#Adding the unobserved data to the amk Penman. For June
monthly.penman.amk[1,3] <- monthly.penman.amk[1,3] + 61.51282
