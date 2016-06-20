# Pacotes -----------------------------------------------------------------
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

# Funções Auxiliares ------------------------------------------------------

converter_numerico <- function(x){
  x %>% 
    str_trim() %>%
    str_replace_all(fixed("."), "") %>%
    str_replace_all(fixed(","), ".") %>%
    as.numeric()
}

processamento_7019 <- failwith(data.frame(matrix(NA, ncol = 9)), function(x){
  arq <- paste0("data-raw/7019/", x) %>%
    read_html()
  tabelas <- arq %>%
    html_table()
  h5s <- arq %>%
    html_nodes("h5") %>%
    html_text()
  if(stringr::str_detect(paste(h5s, " "), "Relatório não encaminhado pela Instituição Financeira.")){
    tabela <- data.frame(matrix(NA, ncol = 9))
    names(tabela) <- c("conta", "nome", "carteira_vencida_15_dias",
                       "carteira_a_vencer_3_meses", "carteira_a_vencer_3_a_12_meses",
                       "carteira_a_vencer_1_a_3_anos", "carteira_a_vencer_3_a_5_anos",
                       "carteira_a_vencer_5_a_15_anos", "carteira_a_vencer_acima_de_15_anos"
    )
  } else {
    tabela <- tabelas[[2]]
    names(tabela) <- c("conta", "nome", "carteira_vencida_15_dias",
                       "carteira_a_vencer_3_meses", "carteira_a_vencer_3_a_12_meses",
                       "carteira_a_vencer_1_a_3_anos", "carteira_a_vencer_3_a_5_anos",
                       "carteira_a_vencer_5_a_15_anos", "carteira_a_vencer_acima_de_15_anos"
    )
    tabela <- tabela[-c(1,2),]
    tabela <- tabela %>%
      mutate(
        conta = stringr::str_trim(conta),
        nome = stringr::str_trim(nome)
      ) %>%
      mutate_each(funs(converter_numerico), starts_with("carteira"))
  }
  return(tabela)
})

processamento_7024 <- failwith(data.frame(matrix(NA, ncol = 6)), function(x){
  arq <- paste0("data-raw/7024/", x) %>%
    read_html()
  tabelas <- arq %>%
    html_table()
  h5s <- arq %>%
    html_nodes("h5") %>%
    html_text()
  if(stringr::str_detect(paste(h5s, " "), "Relatório não encaminhado pela Instituição Financeira.")){
    tabela <- data.frame(matrix(NA, ncol = 6))
    names(tabela) <- c("conta", "nome", "instituicoes_financeiras_ligadas",
                       "instituicoes_financeiras_nao_ligadas", "securitizadoras_ligadas",
                       "securitizadoras_nao_ligadas"
    )
  } else {
    tabela <- tabelas[[2]]
    names(tabela) <- c("conta", "nome", "instituicoes_financeiras_ligadas",
                       "instituicoes_financeiras_nao_ligadas", "securitizadoras_ligadas",
                       "securitizadoras_nao_ligadas"
    )
    tabela <- tabela[-c(1,2),]
    tabela <- tabela %>%
      mutate(
        conta = stringr::str_trim(conta),
        nome = stringr::str_trim(nome)
      ) %>%
      mutate_each(funs(converter_numerico), -conta, -nome)
  }
  return(tabela)
})

# Processamento -----------------------------------------------------------

# processamento 7019

arq_7019 <- list.files("data-raw/7019/")
names(arq_7019) <- arq_7019
tabelas <- plyr::ldply(arq_7019, processamento_7019, .progress = "text")
tabelas <- tabelas %>% 
  select(-starts_with("X"))
tabelas <- tabelas %>% 
  mutate(.id = str_replace_all(.id, fixed(".html"), "")) %>%
  separate(.id, into = c("cnpj", "ano", "trimestre"), sep = "-")

write.csv(tabelas, file = "data/base_7019.csv", row.names = F)
saveRDS(tabelas, file = "data/base_7019.rds")

# processamento 7024

arq_7024 <- list.files("data-raw/7024/")
names(arq_7024) <- arq_7024
tabelas <- plyr::ldply(arq_7024, processamento_7024, .progress = "text")
tabelas <- tabelas %>% 
  select(-starts_with("X"))
tabelas <- tabelas %>% 
  mutate(.id = str_replace_all(.id, fixed(".html"), "")) %>%
  separate(.id, into = c("cnpj", "ano", "trimestre"), sep = "-")

write.csv(tabelas, file = "data/base_7024.csv", row.names = F)
saveRDS(tabelas, file = "data/base_7024.rds")

