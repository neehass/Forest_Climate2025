library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)


########### CFLUX
###########
## Daten einlesen und auf die Jahre nach Spin-up begrenzen
read_cflux_scenario <- function(files, region) {
  files %>%
    map_df(~ read.csv(.x, sep = "", header = TRUE)) %>%
    select(-Lon, -Lat) %>%
    filter(Year >= 1000 & Year <= 1080) %>%
    group_by(Year) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(-Year, names_to = "Type", values_to = "Value") %>%
    mutate(Region = region)
}

# Basis
cflux_basis_boreal <- read_cflux_scenario(
  files = c("cflux_1.out", "cflux_5.out", "cflux_3.out",
            "cflux_4.out", "cflux_2.out"),
  region = "Boreal"
)

cflux_basis_temperate <- read_cflux_scenario(
  files = c("cflux_5t.out", "cflux_3t.out", "cflux_2t.out",
            "cflux_4t.out", "cflux_1t.out"),
  region = "Temperate"
)

cflux_basis <- bind_rows(cflux_basis_boreal, cflux_basis_temperate)

## Temperaturszenario +4°C, +300ppm
cflux_T_boreal <- read_cflux_scenario(
  files = c("cflux_crt.out", "cflux_nct.out", "cflux_nst.out",
            "cflux_sct.out", "cflux_sst.out"),
  region = "Boreal"
)

cflux_T_temperate <- read_cflux_scenario(
  files = c("cflux_bt.out", "cflux_ft.out", "cflux_pt.out",
            "cflux_slt.out", "cflux_sspt.out"),
  region = "Temperate"
)

cflux_T <- bind_rows(cflux_T_boreal, cflux_T_temperate)

## Niederschlagsszenario -30mm, +300ppm
cflux_P_boreal <- read_cflux_scenario(
  files = c("cflux_crp.out", "cflux_ncp.out", "cflux_nsp.out",
            "cflux_scp.out", "cflux_sspp.out"),
  region = "Boreal"
)

cflux_P_temperate <- read_cflux_scenario(
  files = c("cflux_bp.out", "cflux_fp.out", "cflux_pp.out",
            "cflux_slp.out", "cflux_ssppp.out"),
  region = "Temperate"
)

cflux_P <- bind_rows(cflux_P_boreal, cflux_P_temperate)

## Y-Achse auf Temperaturszenario einstellen
ylims_temp <- range(cflux_T$Value, na.rm = TRUE)

base_aes <- aes(
  x = Year,
  y = Value,
  color = Type,
  linetype = Region
)

## Plot
plot_basis <- ggplot(cflux_basis, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "a) Baseline",
    x = "Year",
    y = expression(C~flux~(kg~C~m^{-2}~yr^{-1}))
  ) +
  theme(legend.position = "none")


plot_temp <- ggplot(cflux_T, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "b) +4°C, +300 ppm",
    x = "Year",
    y = NULL
  ) +
  theme(legend.position = "none")


plot_precip <- ggplot(cflux_P, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "c) −30 mm, +300 ppm",
    x = "Year",
    y = NULL,
    color = "Fluxes and Pools",
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

## Abspeichern
ggsave(
  filename = "Cflux_Baseline_Temp_Precip_Comparison.png",
  plot = final_plot,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 300
)

# Funktion zum Extrahieren spezifischer Jahre und Typen
extract_year_type <- function(data, years, types) {
  data %>%
    filter(Year %in% years, Type %in% types) %>%
    select(Region, Year, Type, Value) %>%
    arrange(Region, Type, Year)
}

# Baseline: Soil und Veg für Boreal & Temperate bei 1000 und 1080
baseline_values <- extract_year_type(
  data = cflux_basis,
  years = c(1000, 1080),
  types = c("Soil", "Veg")
)

# Temperaturszenario: Soil und Veg für Boreal & Temperate bei 1000 und 1080
temp_values <- extract_year_type(
  data = cflux_T,
  years = c(1000, 1080),
  types = c("Soil", "Veg")
)

# Niederschlagsszenario
# Soil für 1000 und 1080, Veg für 1000, 1020 und 1080
precip_values_soil <- extract_year_type(
  data = cflux_P,
  years = c(1000, 1080),
  types = c("Soil")
)

precip_values_veg <- extract_year_type(
  data = cflux_P,
  years = c(1000, 1020, 1080),
  types = c("Veg")
)

# Alles zusammen in einer Liste 
results <- list(
  Baseline = baseline_values,
  Temperature = temp_values,
  Precipitation_Soil = precip_values_soil,
  Precipitation_Veg = precip_values_veg
)

# Werte anzeigen
results