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

mortes %>% 
  group_by(regiao, ano = year(dtobito)) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = ano, values_from = n, names_prefix = "ano_")
