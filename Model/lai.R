library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)


### LAI

## Funktion um Daten einzuteilen
## Zeitraum auf nach Spin-up begrenzt
read_lai_scenario <- function(files, region, pft_exclude) {
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

## PFT Einteilung fuer boreale und temperate Region
pft_exclude_boreal <- c("TrBE", "TrBR", "TrIBE", "C4G",
                        "TeNe", "TeBS", "TeBE")

pft_exclude_temperate <- c("TrBE", "TrBR", "TrIBE", "C4G",
                           "BNE", "BINE", "BNS", "BBS")


## Basisszenario
# Boreal
lai_basis_boreal <- read_lai_scenario(
  files = c("lai_1.out", "lai_5.out", "lai_3.out",
            "lai_4.out", "lai_2.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperat
lai_basis_temperate <- read_lai_scenario(
  files = c("lai_5t.out", "lai_3t.out", "lai_2t.out",
            "lai_4t.out", "lai_1t.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

lai_basis <- bind_rows(lai_basis_boreal, lai_basis_temperate)


## Temperaturszenario +4 +300
# Boreal
lai_T_boreal <- read_lai_scenario(
  files = c("lai_crt.out", "lai_nct.out", "lai_nst.out",
            "lai_sct.out", "lai_sst.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperat
lai_T_temperate <- read_lai_scenario(
  files = c("lai_bt.out", "lai_ft.out", "lai_pt.out",
            "lai_slt.out", "lai_sspt.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

lai_T <- bind_rows(lai_T_boreal, lai_T_temperate)


## Niederschlagsszenario -30 +300
# Boreal
lai_P_boreal <- read_lai_scenario(
  files = c("lai_crp.out", "lai_ncp.out", "lai_nsp.out",
            "lai_scp.out", "lai_sspp.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperat
lai_P_temperate <- read_lai_scenario(
  files = c("lai_bp.out", "lai_fp.out", "lai_pp.out",
            "lai_slp.out", "lai_ssppp.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

lai_P <- bind_rows(lai_P_boreal, lai_P_temperate)


base_aes <- aes(
  x = Year,
  y = Value,
  color = Type,
  linetype = Region
)

# Y-Achsen auf Temperaturszenario angleichen
ylims_temp <- range(lai_T$Value, na.rm = TRUE)

# Plotten
plot_basis <- ggplot(lai_basis, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "a) Baseline",
    x = "Year",
    y = expression(Leaf~Area~Index~(LAI)~(m^2~leaf~m^{-2}~ground))
  ) +
  theme(legend.position = "none")


plot_temp <- ggplot(lai_T, base_aes) +
  coord_cartesian(ylim = ylims_temp) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "b) +4°C, +300 ppm",
    x = "Year",
    y = NULL
  ) +
  theme(legend.position = "none")


plot_precip <- ggplot(lai_P, base_aes) +
  coord_cartesian(ylim = ylims_temp) +
  geom_line(linewidth = 1) +
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
  filename = "LAI_Baseline_Temp_Precip_Comparison.png",
  plot = final_plot,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 300
)


# Funktion zum Extrahieren bestimmter Jahre und PFT
extract_lai_values <- function(data, years, pft = "Total") {
  data %>%
    filter(Year %in% years, Type == pft) %>%
    select(Region, Year, Type, Value) %>%
    arrange(Region, Year)
}

# Baseline: Total für Boreal & Temperate bei 1000 und 1080
baseline_total <- extract_lai_values(
  data = lai_basis,
  years = c(1000, 1080),
  pft = "Total"
)

# Temperaturszenario: Total für Boreal & Temperate bei 1000, 1060, 1080
temp_total <- extract_lai_values(
  data = lai_T,
  years = c(1000, 1060, 1080),
  pft = "Total"
)

# Niederschlagsszenario: Total für Boreal & Temperate bei 1000, 1020, 1060, 1080
precip_total <- extract_lai_values(
  data = lai_P,
  years = c(1000, 1020, 1060, 1080),
  pft = "Total"
)

# Alle Ergebnisse in einer Liste speichern
results_lai <- list(
  Baseline = baseline_total,
  Temperature = temp_total,
  Precipitation = precip_total
)

# Ergebnisse anzeigen
results_lai