Cinput    <- read.csv("Input/CropC_Daycent/CinputsAtEcodprov_19Oct2023.csv") #C input proportion not crop specific
Ecodc     <- read.csv("Input/CropC_Daycent/ecodcinputbycrop.csv") # C input crop specific 
EcoP      <- read.csv("Input/CropC_Daycent/EcoProvTexSites.csv") # C ini values
SiteAdj   <- read.csv("Input/CropC_Daycent/SiteAdjDelta_19Oct2023.csv") # Delta C not crop specific
#Slyield   <- read.csv("Input/CropC_Daycent/slyieldadj19102023.csv") # yield data, no need
Weather_y <- read.csv("Input/CropC_Daycent/weather_master.csv") #weahter


Baseline <- SiteAdj %>%
  filter(year %in% c(2010:2014)) %>%
  group_by(SiteName, ECODISTRIC, Texture) %>%
  summarise(BDelta_new = mean(deltaSOC))

BDelta <- SiteAdj %>%
  group_by(SiteName) %>%
  summarise(BDelta = first(BDelta))

Baseline <- Baseline %>%
  left_join(BDelta, by = "SiteName") %>%
  left_join(EcoP, by = c("SiteName" = "sitename"))

cor.test(Baseline$CARB30, Baseline$BDelta) #-0.821
cor.test(Baseline$CARB30, Baseline$BDelta_new) #-0.568


#To check CARB30 and deltaSOC
SiteAdj <- SiteAdj %>%
  left_join(EcoP, by = c("SiteName" = "sitename"))

cor.test(SiteAdj$CARB30, SiteAdj$deltaSOC) #-0.3822515
cor.test(SiteAdj$CARB30, SiteAdj$deltaSOCAdj) # 0.425596

#Replace the BDelta with new baseline (2010-2014) for new deltaSOC after 2015
SiteAdj_new <- SiteAdj %>%
  filter(year >= 2015) %>%
  left_join(Baseline, by = "SiteName") %>%
  mutate(deltaSOCAdj_new = deltaSOC - BDelta_new) %>%
  left_join(EcoP, by = c("SiteName" = "sitename"))

cor.test(SiteAdj_new$CARB30, SiteAdj_new$deltaSOCAdj_new)
