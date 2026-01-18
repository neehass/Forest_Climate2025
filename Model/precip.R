library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

### Niederschlag
# Boreal
precip1 <- read.csv("climate_cr.txt", sep = '', head = TRUE)
precip2 <- read.csv("climate_nc.txt", sep = '', head = TRUE)
precip3 <- read.csv("climate_ns.txt", sep = '', head = TRUE)
precip4 <- read.csv("climate_sc.txt", sep = '', head = TRUE)
precip5 <- read.csv("climate_ss.txt", sep = '', head = TRUE)
precip6 <- read.csv("climate_crT.txt", sep = '', head = TRUE)
precip7 <- read.csv("climate_ncT.txt", sep = '', head = TRUE)
precip8 <- read.csv("climate_nsT.txt", sep = '', head = TRUE)
precip9 <- read.csv("climate_scT.txt", sep = '', head = TRUE)
precip10 <- read.csv("climate_ssT.txt", sep = '', head = TRUE)



# Temperat
precip11 <- read.csv("climate_b.txt", sep = '', head = TRUE)
precip12 <- read.csv("climate_f.txt", sep = '', head = TRUE)
precip13 <- read.csv("climate_p.txt", sep = '', head = TRUE)
precip14 <- read.csv("climate_sl.txt", sep = '', head = TRUE)
precip15 <- read.csv("climate_ssp.txt", sep = '', head = TRUE)
precip16 <- read.csv("climate_bT.txt", sep = '', head = TRUE)
precip17 <- read.csv("climate_fT.txt", sep = '', head = TRUE)
precip18 <- read.csv("climate_pT.txt", sep = '', head = TRUE)
precip19 <- read.csv("climate_slT.txt", sep = '', head = TRUE)
precip20 <- read.csv("climate_sspT.txt", sep = '', head = TRUE)



## Hilfsfunktionen um den Code nicht zu doppeln:
calc_annual_mean <- function(df) {
  rowMeans(df[, 17:28], na.rm = TRUE)
}

calc_group_mean <- function(list_of_dfs) {
  annual_means <- lapply(list_of_dfs, calc_annual_mean)
  rowMeans(do.call(cbind, annual_means))
}

## Boreal
# Default
boreal_dev <- calc_group_mean(
  list(precip1, precip2, precip3, precip4, precip5)
)

# Development
boreal_default <- calc_group_mean(
  list(precip6, precip7, precip8, precip9, precip10)
)

Year <- precip1$Year

df_boreal <- data.frame(
  Year = Year,
  Default = boreal_default,
  Development = boreal_dev
) |>
  dplyr::filter(Year >= 900 & Year <= 1100) |>
  tidyr::pivot_longer(
    cols = c(Default, Development),
    names_to = "Scenario",
    values_to = "Precip"
  ) |>
  dplyr::mutate(Region = "Boreal")


## Temperat
# Default
temp_dev <- calc_group_mean(
  list(precip11, precip12, precip13, precip14, precip15)
)

# Development
temp_default <- calc_group_mean(
  list(precip16, precip17, precip18, precip19, precip20)
)

# Jahr gleich setzen
Year <- precip1$Year

df_temperate <- data.frame(
  Year = Year,
  Default = temp_default,
  Development = temp_dev
) |>
  dplyr::filter(Year >= 900 & Year <= 1100) |>
  tidyr::pivot_longer(
    cols = c(Default, Development),
    names_to = "Scenario",
    values_to = "Precip"
  ) |>
  dplyr::mutate(Region = "Temperate")

# Datensaetze zusammen fuehren
df_all <- dplyr::bind_rows(df_boreal, df_temperate)


## Gemeinsamer Plot
ggplot(
  df_all,
  aes(
    x = Year,
    y = Precip,
    linetype = Scenario,
    color = Region,
    group = interaction(Region, Scenario)
  )
) +
  geom_line(linewidth = 1.1) +
  scale_linetype_manual(
    values = c("Default" = "dotdash", "Development" = "solid")
  ) +
  scale_color_manual(
    values = c(
      "Boreal" = "lightblue",
      "Temperate" = "blue"
    )
  ) +
  labs(
    x = "Year",
    y = "Precipitation (mm/month)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot abspeichern
ggsave(
  filename = "precipitation_boreal_temperate.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)
