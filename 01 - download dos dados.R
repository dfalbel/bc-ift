# Pacotes -----------------------------------------------------------------
library(httr)
library(dplyr)

# Parâmetros --------------------------------------------------------------

cong <- readRDS("inst/tb_cnpjs_e_conglomerados.rds")
url <- "https://www3.bcb.gov.br/iftimagem/IFTServlet"

# Funções Auxiliares ------------------------------------------------------
caminho_arq <- function(codquadro, cnpj, ano, trimestre){
  sprintf("data-raw/%s/%d-%d-%d.html", codquadro, cnpj, ano, trimestre)
}

GET2 <- dplyr::failwith(NULL, function(url, query, arq){
  httr::GET(url, query = query, write_disk(arq), config = config(ssl_verifypeer = FALSE))
})

# Download ----------------------------------------------------------------

# Downlaod do 7029 --

for (cnpj in cong$cnpj) {
  for (ano in 2001 + 0:8) {
    for (trimestre in 1:4) {
      parametros <- list(
        operacao  = "buscaquadro",
        codquadro = 7019,
        cnpj      = cnpj,
        ano       = ano,
        trimestre = trimestre
      )
      arq <- caminho_arq(7019, cnpj, ano, trimestre)
      GET2(url, query = parametros, arq)
      Sys.sleep(1)
    }
  }
}
