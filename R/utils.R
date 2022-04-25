clean_data <- function(data) {
  data %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(
      across(codmunocor:causabas, ~as.character(.x)),
      dtobito = dmy(dtobito),
      cod_uf = str_sub(codmunocor, 1, 2),
      causa_maior = str_sub(causabas, 1, 2)
    ) %>% 
    filter(causa_maior %in% paste0("V", seq(0, 8, 1))) %>% 
    mutate(
      tipo_vitima = case_when(
        causa_maior == "V0" ~ "Pedestres",
        causa_maior == "V1" ~ "Ciclistas",
        causa_maior %in% c("V2", "V3") ~ "Motociclistas",
        causa_maior %in% c("V4", "V5") ~ "Ocupantes de automóvel",
        causa_maior == "V6" ~ "Ocupantes de caminhão",
        causa_maior == "V7" ~ "Ocupantes de ônibus",
        TRUE ~ "Outros"
      ),
      regiao = case_when(
        str_sub(cod_uf, 1, 1) == "1" ~ "Norte",
        str_sub(cod_uf, 1, 1) == "2" ~ "Nordeste",
        str_sub(cod_uf, 1, 1) == "3" ~ "Sudeste",
        str_sub(cod_uf, 1, 1) == "4" ~ "Sul",
        str_sub(cod_uf, 1, 1) == "5" ~ "Centro-Oeste"
      )
    ) %>% 
    left_join(estados, by = "cod_uf")
}

