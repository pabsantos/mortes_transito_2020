library(microdatasus)
library(tidyverse)
library(lubridate)
library(ggrepel)

source("R/utils.R")

dados <- fetch_datasus(
  year_start = 2019,
  year_end = 2020,
  uf = "all",
  information_system = "SIM-DOEXT",
  vars = c("CODMUNOCOR", "DTOBITO", "CAUSABAS")
)

estados <- read_tsv("data/lista_estados.tsv", col_types = c("c", "c", "c"))

mortes <- clean_data(dados)

regiao_delta <- calc_deltas(mortes, regiao)

tipo_delta <- calc_deltas(mortes, tipo_vitima)

uf_delta <- calc_deltas(mortes, uf)

pie_data <- arrange_pie_data(tipo_delta)

tipos_pie_plot <- plot_pie(
  pie_data,
  label_size = 2.5,
  label_pos = 1.7, 
  legend_size = 6, 
  line_size = 0.2,
  key_size = 0.4
)

ggsave(
  "plot/pie_chart.png",
  plot = tipos_pie_plot,
  device = "png",
  width = 6,
  height = 3.5,
  dpi = 300
)

br <- geobr::read_state()
