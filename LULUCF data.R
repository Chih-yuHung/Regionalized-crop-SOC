#This is the data from 
#"\\Ncr.Int.ec.gc.ca\shares\G\GHGD_AFOLU\1_InventoryContributions\2_LULUCF\2024\Data\Inputs\AAFC\crf2024v25102023.zip"
library(data.table)
library(tidyverse)
LULUCF_2024 <- fread("Input/crf2024v25102023.txt")


#colname is not in the file
colnames(LULUCF_2024)<-c("INVYEAR","LULUC","PRELM","RU","SLC","EVENT","SUBEVENT","SOILTYPE","SOILGG",
                   "SOILTEXT","AREA","CARBONPOOL","GHG","ER","CINREASE","CDECREASE","CNET","LUCYEAR")



Tillage <-LULUCF_2024 %>% 
  filter(EVENT == 2) %>%
  filter(SUBEVENT %in% c(21:26))

Tillage_1990 <-Tillage %>%
  filter(RU %in% c(34,35,37)) %>%
  filter(INVYEAR == 1990) %>%
  filter(SUBEVENT == 21) #Convential to reduced only 
