read_datasus <- function() {
  if (exists("data/datasus.rds")) {
    readRDS("data/datasus.rds")
  } else {
    fetch_datasus(
      year_start = 2019,
      year_end = 2020,
      uf = "all",
      information_system = "SIM-DOEXT",
      vars = c("CODMUNOCOR", "DTOBITO", "CAUSABAS")
    )
  }
}


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

calc_deltas <- function(data, var) {
  data %>% 
    group_by({{ var }}, ano = year(dtobito)) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = ano, values_from = n, names_prefix = "ano_") %>% 
    mutate(delta = (ano_2020 - ano_2019) / ano_2019)
}

arrange_pie_data <- function(data) {
  data %>% 
    select(-delta) %>% 
    ungroup() %>% 
    mutate(
      perc_2019 = ano_2019 / sum(tipo_delta$ano_2019),
      perc_2020 = ano_2020 / sum(tipo_delta$ano_2020),
      ypos_2019 = cumsum(perc_2019) - 0.5 * perc_2019,
      ypos_2020 = cumsum(perc_2020) - 0.5 * perc_2020,
    ) %>% 
    select(-ano_2019, -ano_2020) %>% 
    pivot_longer(
      perc_2019:ypos_2020,
      names_to = c("type", "ano"), 
      names_sep = "_"
    ) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(
      label = scales::percent(perc, accuracy = 0.1)
    )
}

plot_pie <- function(
    data, label_size, label_pos, legend_size, line_size, key_size
  ) {
  data %>% 
    ggplot(aes(x = "", y = perc, fill = tipo_vitima)) +
    geom_bar(color = "white", width = 1, stat = "identity", lwd = line_size) +
    coord_polar("y") +
    facet_wrap(~ano) +
    geom_text(
      aes(label = label, x = label_pos),
      position = position_stack(vjust = 0.5),
      size = label_size
    ) +
    theme_void() +
    labs(fill = "") +
    scale_fill_brewer(palette = "Dark2") +
    theme(
      legend.text = element_text(size = legend_size),
      legend.key.size = unit(key_size, "cm"))
}

plot_uf_delta <- function(sf) {
  tm_shape(sf) +
    tm_borders(col = "grey20", lwd = 0.4) +
    tm_fill(
      col = "delta",
      title = "",
      palette = "-RdYlGn",
      n = 6,
      legend.format = list(fun = scales::percent_format(accuracy = 1))
    ) +
    tm_text("SIGLA", size = 0.5) +
    tm_layout(
      legend.format = list(text.separator = "a"),
      frame = NA
    )
}
