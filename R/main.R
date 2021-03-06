library(microdatasus)
library(tidyverse)
library(lubridate)
library(tmap)
library(sf)

source("R/utils.R")

dados <- read_datasus()

if (!exists("data/datasus.rds")) {
  saveRDS(dados, "data/datasus.rds")
}

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

br <- st_read("data/BRUFE250GC_SIR.shp")

br <- br %>% 
  left_join(uf_delta, by = c("SIGLA" = "uf"))

br_delta_plot <- plot_uf_delta(br)

tmap_save(
  tm = br_delta_plot,
  filename = "plot/br_delta_plot.png",
  width = 6,
  height = 6,
  dpi = 300,
)

full_delta <- mortes %>% calc_full_delta()

writexl::write_xlsx(mortes, "data/base_datasus_2019_2020.xlsx")
