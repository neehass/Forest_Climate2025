#Kurs Forests in the climate system WiSe 2025/26 
#Kira Eder
#Auswertung Yatir-Daten

##### Packages und Daten einlesen #####

library(tidyverse)
library(patchwork)
library(ggplot2)
Yatir <- read.csv2("data/Gasexchange_Forest_Yatir_2013.csv", sep = ",", skip = 2)
Yatir$time <- as.POSIXct(strptime(Yatir$datetime, '%d.%m.%y %H:%M'))
Yatir$day <- as.numeric(strftime(Yatir$time, "%j"))
Yatir$month <- as.numeric(strftime(Yatir$time, "%m"))

## aggregieren
cols <- c(4:13) 
Yatir[cols] <- lapply(Yatir[cols], function(x)
  as.numeric(as.character(x))
)
Yatir.day.mean <- aggregate(Yatir, by = list(day = Yatir$day), FUN = mean, na.rm = T)
plot(Yatir.day.mean$GPP_gf~Yatir.day.mean$day)

Yatir.month.mean <- aggregate(Yatir, by = list(month = Yatir$month), FUN = mean, na.rm = T)
plot(Yatir.month.mean$GPP_gf~Yatir.month.mean$month)


## auf 10 -15 Uhr aggregieren
Yatir_10_15 <- subset(Yatir, hour >= 10 & hour <= 15)

Yatir.month.mean.day <- aggregate(
  Yatir_10_15,
  by = list(month = Yatir_10_15$month),
  FUN = mean,
  na.rm = TRUE
)

# Dopplung in day und month Spalte beheben
Yatir.month.mean.day[ ,16] <- NULL
Yatir.month.mean.day[ ,16] <- NULL
  
## plotten ohne aggregieren
plot(Yatir$day,Yatir$GPP_gf)
#aggregieren mit mutate & group_by über 24h
GPP_mean_daily <- Yatir %>%
  mutate(day = as.numeric(strftime(time, "%j"))) %>%
  group_by(day) %>%
  summarise(mean_GPP_gf = mean(as.numeric(GPP_gf), na.rm = TRUE))
GPP_mean_daily <- GPP_mean_daily[-366,]

plot(GPP_mean_daily$mean_GPP_gf ~ GPP_mean_daily$day)
GPP_mean_monthly <- Yatir %>%
  mutate(month = as.numeric(strftime(time, "%m"))) %>%
  group_by(month) %>%
  summarise(mean_GPP_gf = mean(as.numeric(GPP_gf), na.rm = TRUE))

plot(GPP_mean_monthly$mean_GPP_gf ~ GPP_mean_monthly$month)
p1 <- ggplot(GPP_mean_monthly, aes(x = month, y = mean_GPP_gf)) +
  geom_point(color = "blue", size = 2)+
  geom_line(color = "blue", linewidth = 1) +
  labs(y = "GPP (µmol CO₂ m⁻² s⁻¹)", x = "month") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p1

#min und max berechnen
GPP_mean_daily %>% 
  slice_max(mean_GPP_gf, n = 1, with_ties = FALSE)
GPP_mean_daily %>% 
  slice_min(mean_GPP_gf, n = 1, with_ties = FALSE)

# nicht aggregiert
Yatir <- merge(Yatir, GPP_mean_daily, by= "day")
plot(Yatir$day,Yatir$ET_g_re_gf)

##### Evapotranspiration (Fragestellung 2) ######
#aggregiert über 24h
ET_mean_daily <- Yatir %>%
  mutate(day = as.numeric(strftime(time, "%j"))) %>%
  group_by(day) %>%
  summarise(ET_mean = mean(as.numeric(ET_g_re_gf), na.rm = TRUE))
plot(ET_mean_daily$ET_mean ~ ET_mean_daily$day)
ET_mean_daily %>% 
  slice_min(ET_mean, n = 1, with_ties = FALSE)
ET_mean_monthly <- Yatir %>%
  mutate(month = as.numeric(strftime(time, "%m"))) %>%
  group_by(month) %>%
  summarise(ET_mean = mean(as.numeric(ET_g_re_gf), na.rm = TRUE))
plot(ET_mean_monthly$ET_mean ~ ET_mean_monthly$month)
p2 <- ggplot(ET_mean_monthly, aes(x = month, y = ET_mean)) +
  geom_point(color = "red", size = 2)+
  geom_line(color = "red", linewidth = 1) +
  labs(y = "ET (g H₂O m⁻² s⁻¹)", x = "month") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p2
p1 / p2

## GPP & ET täglich
par(mfrow = c(2,1))
plot(GPP_mean_daily$mean_GPP_gf ~ GPP_mean_daily$day)
plot(ET_mean_daily$ET_mean ~ ET_mean_daily$day)

## GPP & ET monatlich
par(mfrow = c(2,1))
plot(GPP_mean_monthly$mean_GPP_gf ~ GPP_mean_monthly$month)
plot(ET_mean_monthly$ET_mean ~ ET_mean_monthly$month)

##### T,VPD, SWC (Fragestellung 3) #####
par(mfrow= c(1,1))
#monatlich
plot(Yatir.month.mean$Tair_15m_C_gf~Yatir.month.mean$month)
plot(Yatir.month.mean$VPD_Pa_gf~Yatir.month.mean$month)
plot(Yatir.month.mean$SWC_vol~Yatir.month.mean$month)
plot(Yatir.month.mean$GPP_gf~Yatir.month.mean$month)

##### Plotten Zeitraum 10-15 Uhr ######
# Jahresverlauf GPP
p1 <- ggplot(Yatir.month.mean.day, aes(x = month, y = GPP_gf)) +
  geom_point(color = "blue", size = 2)+
  geom_line(color = "blue", linewidth = 1) +
  labs(y = "GPP (µmol CO₂ m⁻² s⁻¹)", x = "month") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p1
Yatir.month.mean.day %>% 
  slice_max(GPP_gf, n = 1, with_ties = FALSE)
Yatir.month.mean.day %>% 
  slice_min(GPP_gf, n = 1, with_ties = FALSE)

## GPP & Evapotranspiration
p1 <- ggplot(Yatir.month.mean.day, aes(x = month, y = GPP_gf)) +
  geom_point(color = "blue", size = 2)+
  geom_line(color = "blue", linewidth = 1) +
  labs(y = "GPP (µmol CO₂ m⁻² s⁻¹)", x = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p1
p2 <- ggplot(Yatir.month.mean.day, aes(x = month, y = ET_g_re_gf)) +
  geom_point(color = "red", size = 2)+
  geom_line(color = "red", linewidth = 1) +
  labs(y = "ET (g H₂O m⁻² s⁻¹)", x = "month") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p2
p1 / p2


## GPP mit T, VPD, SWC
p3 <- ggplot(Yatir.month.mean.day, aes(x = month, y = Tair_15m_C_gf)) +
  geom_point(color = "green", size = 2)+
  geom_line(color = "green", linewidth = 1) +
  labs(y = "T (°C)", x = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p3
p4 <- ggplot(Yatir.month.mean.day, aes(x = month, y = VPD_Pa_gf)) +
  geom_point(color = "plum", size = 2)+
  geom_line(color = "plum", linewidth = 1) +
  labs(y = "VPD (Pa)", x = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
p5 <- ggplot(Yatir.month.mean.day, aes(x = month, y = SWC_vol)) +
  geom_point(color = "orange", size = 2)+
  geom_line(color = "orange", linewidth = 1) +
  labs(y = "SWC (%)", x = "month") +
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  theme_minimal()
(p1 / p3 /p4 /p5 )+
plot_annotation() &
  theme(
    plot.margin = margin(t = 10),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8)
  )

##### einfache lineare Modelle #####
mods <- lapply(c("ET_g_re_gf","Tair_15m_C_gf", "VPD_Pa_gf", "SWC_vol"), function(v)
  lm(Yatir.month.mean.day$GPP_gf ~ Yatir.month.mean.day[[v]])
)

names(mods) <- c("ET_g_re_gf", "Tair_15m_C_gf", "VPD_Pa_gf", "SWC_vol")
sapply(mods, function(m) summary(m)$r.squared)
summary(mods$ET_g_re_gf)

