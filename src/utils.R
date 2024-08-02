library (package = "data.table")
library (package = "ggplot2")
library (package = "stringr")
library (package = "roxygen2")

pasta.dados <- function (
    ficheiro
) {
  return (file.path ("dados", ficheiro))
}

faixas.etarias <- fread (
  file = pasta.dados ("faixas-etarias.csv")
)

FAIXA.ETARIA.MIN <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MAX <- faixas.etarias [, max (FE.id)]

duração.faixa.etaria <- function (el.idade, el.sexo) {
  return (faixas.etarias [
    FE.id == el.idade & (is.na (FE.sexo) | FE.sexo == el.sexo),
    FE.idade.max - FE.idade.min
  ])
}
