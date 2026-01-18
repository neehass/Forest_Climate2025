library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

## ANPP 
## Funktion um Daten richtig einzuteilen 
## Der Zeitraum wird auf nach den Spin-up begrenzt
read_anpp_scenario <- function(files, region, pft_exclude) {
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

## PFTs fuer die jeweilige Region
pft_exclude_boreal <- c("TrBE", "TrBR", "TrIBE", "C4G",
                        "TeNe", "TeBS", "TeBE")

pft_exclude_temperate <- c("TrBE", "TrBR", "TrIBE", "C4G",
                           "BNE", "BINE", "BNS", "BBS")


## Daten einlesen 
# Basis Szenario Boreal
anpp_basis_boreal <- read_anpp_scenario(
  files = c("anpp_1.out", "anpp_5.out", "anpp_3.out",
            "anpp_4.out", "anpp_2.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Basis Szenario Temperat
anpp_basis_temperate <- read_anpp_scenario(
  files = c("anpp_5t.out", "anpp_3t.out", "anpp_2t.out",
            "anpp_4t.out", "anpp_1t.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

anpp_basis <- bind_rows(anpp_basis_boreal, anpp_basis_temperate)

# Temperaturszenario Boreal
anpp_T_boreal <- read_anpp_scenario(
  files = c("anpp_crt.out", "anpp_nct.out", "anpp_nst.out",
            "anpp_sct.out", "anpp_sst.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Temperaturszenario Temperat
anpp_T_temperate <- read_anpp_scenario(
  files = c("anpp_bt.out", "anpp_ft.out", "anpp_pt.out",
            "anpp_slt.out", "anpp_sspt.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

anpp_T <- bind_rows(anpp_T_boreal, anpp_T_temperate)

# Niederschlagsszenario Boreal
anpp_P_boreal <- read_anpp_scenario(
  files = c("anpp_crp.out", "anpp_ncp.out", "anpp_nsp.out",
            "anpp_scp.out", "anpp_sspp.out"),
  region = "Boreal",
  pft_exclude = pft_exclude_boreal
)

# Niederschlagsszenario Temperat
anpp_P_temperate <- read_anpp_scenario(
  files = c("anpp_bp.out", "anpp_fp.out", "anpp_pp.out",
            "anpp_slp.out", "anpp_ssppp.out"),
  region = "Temperate",
  pft_exclude = pft_exclude_temperate
)

anpp_P <- bind_rows(anpp_P_boreal, anpp_P_temperate)

# Achse auf Temperatur Szenario anpassen
ylims_temp <- range(anpp_T$Value, na.rm = TRUE)

base_aes <- aes(
  x = Year,
  y = Value,
  color = Type,
  linetype = Region
)

## Plotten
plot_basis <- ggplot(anpp_basis, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "a) Baseline",
    x = "Year",
    y = "Annual net primary production (NPP) (kg C m⁻² yr⁻¹)"
  ) +
  theme(legend.position = "none")

plot_temp <- ggplot(anpp_T, base_aes) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylims_temp) +
  theme_minimal() +
  labs(
    title = "b) +4°C, +300 ppm",
    x = "Year",
    y = NULL
  ) +
  theme(legend.position = "none")

plot_precip <- ggplot(anpp_P, base_aes) +
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
  filename = "ANPP_Baseline_Temp_Precip_Comparison.png",
  plot = final_plot,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 300
)




# Funktion, um ANPP für bestimmte Jahre und PFT "Total" zu extrahieren
extract_anpp_values <- function(data, years, pft = "Total") {
  data %>%
    filter(Year %in% years, Type == pft) %>%
    select(Region, Year, Type, Value) %>%
    arrange(Region, Year)
}


# Baseline: Total für Boreal & Temperate bei 1000 und 1080
baseline_total <- extract_anpp_values(
  data = anpp_basis,
  years = c(1000, 1080),
  pft = "Total"
)

# Temperaturszenario: Total für Boreal & Temperate bei 1000 und 1080
temp_total <- extract_anpp_values(
  data = anpp_T,
  years = c(1000, 1080),
  pft = "Total"
)

# Niederschlagsszenario: Total für Boreal & Temperate bei 1000 und 1080
precip_total <- extract_anpp_values(
  data = anpp_P,
  years = c(1000, 1020, 1080),
  pft = "Total"
)

# Alles in einer Liste zusammenfassen
results_anpp <- list(
  Baseline = baseline_total,
  Temperature = temp_total,
  Precipitation = precip_total
)

# Werte anzeigen
results_anpp