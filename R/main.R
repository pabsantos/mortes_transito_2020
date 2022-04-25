library(microdatasus)
library(tidyverse)
library(lubridate)

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
