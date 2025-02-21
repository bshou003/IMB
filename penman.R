library(tidyverse)
library(readxl) #Calling in readxl to read in excel files

#Have not got this to work 2024/12/18

#Calling NSRDB or NCEI 1991-2010 averaged solar data. This is the monthly averages.
jack.solar <- read.csv("~/Documents/Data/Chapter.3/Weather/Jackson.solar.1991.2010.monthly.average.csv")
solar <- jack.solar %>% 
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

        

#Calling solar (GHI) and albedo from https://nsrdb.nrel.gov/data-viewer on a 60 minute basis
nsrdb.solar.2022 <- read.csv("~/Documents/Data/Chapter.3/Weather/nsrdb_ghi_albedo_jackson_2022.csv", skip = 2) 
nsrdb.solar.2023 <- read.csv("~/Documents/Data/Chapter.3/Weather/nsrdb_ghi_albedo_jackson_2023.csv", skip = 2)

nsrdb.solar <- nsrdb.solar.2022 %>% 
  bind_rows(nsrdb.solar.2023) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(Surface.Albedo = mean(Surface.Albedo),
            ghi = mean(GHI) * 0.0864,
            albedo = 0.07)
#   ungroup() %>% 
#   mutate(date = seq(from = as.Date("2022/01/01"), by = "day", length.out = 730)) %>% 
#   mutate(ghi = ghi * 0.0864)
# write_csv(nsrdb.solar, "~/Documents/Data/Chapter.3/Weather/nsrdb_jackson_daily.csv")

jack.airport  <- jack.airport %>% 
  merge(nsrdb.solar) %>% 
  filter(Month >=5 & Month <= 9)
#FOR MIN AND MAX TEMP NEED TO PULL OUT FOR EACH DAY
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
         date = as.Date(dateTime),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         day = as.numeric(format(date, format = "%d"))) %>% 
  group_by(date) %>% 
  reframe(MIN = min(TEMP),
         MAX = max(TEMP),
         prcp = sum(prcp, na.rm = TRUE),
         TEMP = mean(TEMP),
         dp = mean(dp),
         WDSP = mean(WDSP),
         ghi = mean(ghi) * 0.0864)

zom = 0.0002
zoh = 0.00002
d = 0
zm = 2 
zh = 2
g0 = 0.3
vonkarmaen = 0.41
latentheatvapor = 2453
#Want to double check the solar information but everything else looks good
penman.parameters2 <- jack.airport %>% 
  mutate(
    #aerodynamic resistance (ra) (Allen 1998)
    ra = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
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
    ettest = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra)))/
                     (latentheatvapor * (delta+ psy))),
    ettest2 = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra)))/
                      (latentheatvapor * (delta+ psy))))

monthly.penman <- penman.parameters2 %>% 
  group_by(Month, Year) %>% 
  reframe(et = sum(ettest2))

ggplot() +
  geom_point(data = penman.parameters2, aes(x= date, y = ettest))+
  geom_point(data = penman.parameters2, aes(x = date, y = ettest2), color = "red")

penman.parameters1 <- jack.airport %>% 
  mutate(
    #aerodynamic resistance (ra) (Allen 1998)
    ra = (log((zm - d) / zom) * log((zh - d) / zoh)) / 
      (vonkarmaen^2 * WDSP),
    #Atmospheric pressure (kPA) (Allen 1998)
    pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
    #saturation vapor pressure
    es = (.611 * exp((17.3 * TEMP)/(TEMP + 237.3))),
    ea = (RH/100)*es,
    #Slope of the saturation vapor pressure curve (kPa C-1) (Following Dingman, 2015)
    delta = (2508.3 / (TEMP+237.3)^2)*exp((17.3 * TEMP)/(TEMP+237.3)),
    #Confident in everything above
    #Humidity Ration
    #X = (((286.9/461.5) * ea)/(pressure - ea)),
    #air density (allen 1998)
    rhoa = (pressure)/(((1.01*(TEMP + 273))*0.287)),
    #rhoa2 = (((pressure * 1000)/(286.9 * (TEMP + 273.12))) * (1 + X)) / (1 + (X * (286.9/461.5))),
    #rho3 = ((287.04 + ea)/(287.04 + (TEMP +273.15))) * (1-(0.378*(ea/(287.04 + ea)))),
    #specific heat of air
    #cp2 = (1.005 + (1.82 * X))/1000,
    #The psychometric constant
    psy = 0.000665*pressure,
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
    ettest = 1000*(((delta * rn) + (rhoa * cp * ((es-ea) / ra)))/
      (latentheatvapor * (delta+ psy))),
    ettest2 = 1000*(((delta * (rn - (rn*g0))) + (rhoa * cp * ((es-ea) / ra)))/
                     (latentheatvapor * (delta+ psy))))

    deltaterm = (delta)/(delta + (psy * (1 + (0.34 * WDSP)))),
    psiterm = (psy)/(delta + (psy * (1 + (0.34 * WDSP)))),
    tempterm = ((900)/(TEMP+273)) * WDSP,
    etmax = 0.6108 * exp(((17.27 * MAX)/(MAX + 237.3))),
    etmin = 0.6108 * exp(((17.27 * MIN)/(MIN + 237.3))),
   
    #vapor pressure
    ea = (RH/100)* ((etmin + etmax)/2),
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
    rng = 0.408 *rn,
    etrad = deltaterm * rng,
    etwind = psiterm * (tempterm * (es-ea)),
    et = etrad + etwind,
    ettest = ((delta * rn) + (rhoa * cp * (es-ea / ra)))/
      (latentheatvapor * (deltaterm + psy)))








penman.parameters <- jack.airport %>% 
  mutate(
  #Slope of the saturation vapor pressure curve (kPa C-1)
  delta = ((4098 * (0.6108 * exp((17.27*TEMP) / (TEMP + 237.3)))) / ((TEMP + 237.3)^2)),
  #Atmospheric pressure (kPA)
  pressure = 101.3 * (((293 - (0.0065 * 2064.106))/293)^5.26), 
  #The psychometric constant
  psy = 0.000665 * pressure,
  deltaterm = (delta)/(delta + (psy * (1 + (0.34 * WDSP)))),
  psiterm = (psy)/(delta + (psy * (1 + (0.34 * WDSP)))),
  tempterm = ((900)/(TEMP+273)) * WDSP,
  etmax = 0.6108 * exp(((17.27 * MAX)/(MAX + 237.3))),
  etmin = 0.6108 * exp(((17.27 * MIN)/(MIN + 237.3))),
  es = (etmax + etmin)/2,
  ea = (RH/100)* ((etmin + etmax)/2),
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
  rng = 0.408 *rn,
  etrad = deltaterm * rng,
  etwind = psiterm * (tempterm * (es-ea)),
  et = etrad + etwind)

