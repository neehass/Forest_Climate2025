library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)

#### CMASS

## Funktion um Daten einzuteilen
## Jahre nach Spin-up werden dargestellt
read_cmass_scenario <- function(files, region, pft_exclude) {
  files %>%
    map_df(~ read.csv(.x, sep = "", header = TRUE)) %>%
    select(-Lon, -Lat) %>%
    filter(Year >= 1000 & Year <= 1080) %>%
    group_by(Year) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(-Year, names_to = "Type", values_to = "Value") %>%
    filter(!Type %in% pft_exclude) %>%
    mutate(Region = region)
}


## PFTs jeweils fuer boreal und temperate Region
pft_exclude_boreal <- c("TrBE", "TrBR", "TrIBE", "C4G",
                        "TeNe", "TeBS", "TeBE")

pft_exclude_temperate <- c("TrBE", "TrBR", "TrIBE", "C4G",
                           "BNE", "BINE", "BNS", "BBS")


## Basisszenario
cmass_basis_boreal <- read_cmass_scenario(
  files = c("cmass_1.out", "cmass_5.out", "cmass_3.out",
            "cmass_4.out", "cmass_2.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

cmass_basis_temperate <- read_cmass_scenario(
  files = c("cmass_5t.out", "cmass_3t.out", "cmass_2t.out",
            "cmass_4t.out", "cmass_1t.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

cmass_basis <- bind_rows(cmass_basis_boreal, cmass_basis_temperate)

## Temperaturszenario +4°C, +300ppm
# Boreal
cmass_T_boreal <- read_cmass_scenario(
  files = c("cmass_crt.out", "cmass_nct.out", "cmass_nst.out",
            "cmass_sct.out", "cmass_sst.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperat
cmass_T_temperate <- read_cmass_scenario(
  files = c("cmass_bt.out", "cmass_ft.out", "cmass_pt.out",
            "cmass_slt.out", "cmass_sspt.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

cmass_T <- bind_rows(cmass_T_boreal, cmass_T_temperate)

## Niederschlagszenario -30mm, +300ppm
# Boreal
cmass_P_boreal <- read_cmass_scenario(
  files = c("cmass_crp.out", "cmass_ncp.out", "cmass_nsp.out",
            "cmass_scp.out", "cmass_sspp.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperat
cmass_P_temperate <- read_cmass_scenario(
  files = c("cmass_bp.out", "cmass_fp.out", "cmass_pp.out",
            "cmass_slp.out", "cmass_ssppp.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

cmass_P <- bind_rows(cmass_P_boreal, cmass_P_temperate)

## Gleiche y-Achsen
ylims_temp <- range(cmass_T$Value, na.rm = TRUE)

base_aes <- aes(
  x = Year,
  y = Value,
  color = Type,
  linetype = Region
)


## Plotten
plot_basis <- ggplot(cmass_basis, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "a) Baseline",
    x = "Year",
    y = expression(C~mass~(kg~C~m^{-2}))
  ) +
  theme(legend.position = "none")


plot_temp <- ggplot(cmass_T, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "b) +4°C, +300 ppm",
    x = "Year",
    y = NULL
  ) +
  theme(legend.position = "none")


plot_precip <- ggplot(cmass_P, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "c) −30 mm, +300 ppm",
    x = "Year",
    y = NULL,
    color = "PFT",
    linetype = "Region"
  ) +
  theme(legend.position = "bottom")

legend <- cowplot::get_legend(plot_precip)

row_plots <- plot_grid(
  plot_basis,
  plot_temp,
  plot_precip + theme(legend.position = "none"),
  ncol = 3,
  align = "h"
)

final_plot <- plot_grid(
  row_plots,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

final_plot

## Plot abspeichern
ggsave(
  filename = "Cmass_Baseline_Temp_Precip_Comparison.png",
  plot = final_plot,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 300
)

# Funktion zum Extrahieren bestimmter Jahre und PFT
extract_cmass_values <- function(data, years, pft = "Total") {
  data %>%
    filter(Year %in% years, Type == pft) %>%
    select(Region, Year, Type, Value) %>%
    arrange(Region, Year)
}

# Baseline: Total für Boreal & Temperate bei 1000 und 1080
baseline_total <- extract_cmass_values(
  data = cmass_basis,
  years = c(1000, 1080),
  pft = "Total"
)

# Temperaturszenario: Total für Boreal & Temperate bei 1000 und 1080
temp_total <- extract_cmass_values(
  data = cmass_T,
  years = c(1000, 1080),
  pft = "Total"
)

# Niederschlagsszenario: Total für Boreal & Temperate bei 1000, 1020, 1040, 1080
precip_total <- extract_cmass_values(
  data = cmass_P,
  years = c(1000, 1020, 1040, 1080),
  pft = "Total"
)

# Alle Ergebnisse in einer Liste speichern
results_cmass <- list(
  Baseline = baseline_total,
  Temperature = temp_total,
  Precipitation = precip_total
)

# Ergebnisse anzeigen
results_cmass