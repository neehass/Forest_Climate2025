library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

#### CO2
## CO2 plotten --> reicht eine climate datei zu nehmen, da CO2 ueberall den Wert 280ppm hat
co2 <- read.csv("climate_cr.txt", sep = '', head = TRUE)
co2_filter <- co2 %>% 
  filter(Year >= 900 & Year <= 1100) 

## Plot
ggplot(co2_filter, aes(x = Year)) +
  # CO2-Zeitreihe
  geom_line(
    aes(y = CO2_ppm, linetype = "Development"),
    color = "purple",
    linewidth = 0.9
  ) +
  # Referenzlinie bei 280 ppm
  geom_hline(
    aes(yintercept = 280, linetype = "Default"),
    color = "purple",
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name = "",
    values = c(
      "Development" = "solid",
      "Default" = "dotdash"
    )
  ) +
  labs(
    x = "Year",
    y = expression(CO[2]~"(ppm)")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot abspeichern
ggsave(
  "co2_development_vs_default.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)


