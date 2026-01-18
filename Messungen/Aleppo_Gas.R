#Kurs Forests in the climate system WiSe 2025/26 
#Kira Eder
#Auswertung Aleppo-Daten

##### Packages und Daten einlesen #####

library(tidyverse)
library(ggplot2)
library(patchwork)
Aleppo <- read.csv("data/aleppo_pine_eCO2_heat_shoot_day.csv", sep = ",", skip = 2, header = TRUE)
summary(Aleppo)
Aleppo$Treat <- as.factor(Aleppo$Treat)

## aggregieren
dat_treat <- Aleppo %>% 
  group_by(Treat, Steps) %>% 
  summarise(across(everything(), ~mean(.x,na.rm = TRUE)), .group = 'keep')
dat_treat

##### Leaf Area ####
###### Anet & E plotten ######
par(mfrow = c(2,1))
ggplot(data = dat_treat, aes(y = Anet_la, x = Steps, col = Treat))+
  geom_point(show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  labs(y = "Anet (µmol CO₂ m⁻² s⁻¹)", x = "Steps (°C)")+
  theme_minimal()+
  ggplot(data = dat_treat, aes(y = E_la, x = Steps, col = Treat))+
  geom_point()+
  geom_line()+
  labs(y = "E (mmol m⁻² s⁻¹)", x = "Steps (°C)")+
  theme_minimal()
par(mfrow = c(1,1))


###### Stomatäre Leitfähigkeit plotten #####
ggplot(data = dat_treat, aes(y = Gs_la, x = VPD, col = Treat))+
  geom_point()+
  geom_line()+
  labs(y = "Gs (mol m⁻² s⁻¹)", x = "VPD (kPa)")+
  theme_minimal()

##### Canopy ####
###### Anet & E plotten #####
ggplot(data = dat_treat, aes(y = Anet_canopy, x = Steps, col = Treat))+
  geom_point(show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  labs(y = "Anet (µmol CO₂ m⁻² s⁻¹)", x = "Steps (°C)")+
  theme_minimal()+
ggplot(data = dat_treat, aes(y = E_canopy, x = Steps, col = Treat))+
  geom_point()+
  geom_line()+
  labs(y = "E (mmol m⁻² s⁻¹)", x = "Steps (°C)")+
  theme_minimal()

###### Stomatäre Leitfähigkeit plotten ######
ggplot(data = dat_treat, aes(y = Gs_canopy, x = VPD, col = Treat))+
  geom_point()+
  geom_line()+
  labs(y = "Gs (mol m⁻² s⁻¹)", x = "VPD (kPa)")+
  theme_minimal()


