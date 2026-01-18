library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

#### Temperatur
# Boreal
temp1 <- read.csv("climate_cr.txt", sep = '', head = TRUE)
temp2 <- read.csv("climate_nc.txt", sep = '', head = TRUE)
temp3 <- read.csv("climate_ns.txt", sep = '', head = TRUE)
temp4 <- read.csv("climate_sc.txt", sep = '', head = TRUE)
temp5 <- read.csv("climate_ss.txt", sep = '', head = TRUE)
temp6 <- read.csv("climate_crT.txt", sep = '', head = TRUE)
temp7 <- read.csv("climate_ncT.txt", sep = '', head = TRUE)
temp8 <- read.csv("climate_nsT.txt", sep = '', head = TRUE)
temp9 <- read.csv("climate_scT.txt", sep = '', head = TRUE)
temp10 <- read.csv("climate_ssT.txt", sep = '', head = TRUE)

# Temperat
temp11 <- read.csv("climate_b.txt", sep = '', head = TRUE)
temp12 <- read.csv("climate_f.txt", sep = '', head = TRUE)
temp13 <- read.csv("climate_p.txt", sep = '', head = TRUE)
temp14 <- read.csv("climate_sl.txt", sep = '', head = TRUE)
temp15 <- read.csv("climate_ssp.txt", sep = '', head = TRUE)
temp16 <- read.csv("climate_bT.txt", sep = '', head = TRUE)
temp17 <- read.csv("climate_fT.txt", sep = '', head = TRUE)
temp18 <- read.csv("climate_pT.txt", sep = '', head = TRUE)
temp19 <- read.csv("climate_slT.txt", sep = '', head = TRUE)
temp20 <- read.csv("climate_sspT.txt", sep = '', head = TRUE)

# Hilfsfunktionen
calc_annual_mean <- function(df) {
  rowMeans(df[, 5:16], na.rm = TRUE)
}

calc_group_mean <- function(list_of_dfs) {
  annual_means <- lapply(list_of_dfs, calc_annual_mean)
  rowMeans(do.call(cbind, annual_means))
}


## Boreal
b_default <- calc_group_mean(list(temp1, temp2, temp3, temp4, temp5))
b_dev     <- calc_group_mean(list(temp6, temp7, temp8, temp9, temp10))

df_boreal <- data.frame(
  Year = temp1$Year,
  Default = b_default,
  Development = b_dev
) |>
  dplyr::filter(Year >= 900 & Year <= 1100) |>
  tidyr::pivot_longer(
    cols = c(Default, Development),
    names_to = "Scenario",
    values_to = "Temp"
  ) |>
  dplyr::mutate(Region = "Boreal")

## Temperat
t_default <- calc_group_mean(list(temp11, temp12, temp13, temp14, temp15))
t_dev     <- calc_group_mean(list(temp16, temp17, temp18, temp19, temp20))

df_temperate <- data.frame(
  Year = temp1$Year,
  Default = t_default,
  Development = t_dev
) |>
  dplyr::filter(Year >= 900 & Year <= 1100) |>
  tidyr::pivot_longer(
    cols = c(Default, Development),
    names_to = "Scenario",
    values_to = "Temp"
  ) |>
  dplyr::mutate(Region = "Temperate")


## Zusammenfuehren
df_temp_all <- dplyr::bind_rows(df_boreal, df_temperate)


## Plotten
p_temp <- ggplot(
  df_temp_all,
  aes(
    x = Year,
    y = Temp,
    color = Region,
    linetype = Scenario,
    group = interaction(Region, Scenario)
  )
) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    values = c(
      "Boreal" = "lightcoral",
      "Temperate" = "red"
    )
  ) +
  scale_linetype_manual(
    values = c("Default" = "dotdash", "Development" = "solid")
  ) +
  labs(
    x = "Year",
    y = "Mean annual Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

p_temp

# Plot abspeichern
ggsave(
  "temperature_boreal_temperate.png",
  plot = p_temp,
  width = 8,
  height = 5,
  dpi = 300
)