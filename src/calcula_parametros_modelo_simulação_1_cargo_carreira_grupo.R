# Script que calcula os parâmetros do modelo de simulação nº 1 para a evolução
# dos postos de trabalho no sector das administrações públicas por subsector
# segundo cargo, carreira, grupo, escalões etários e sexo. Neste modelo a
# evolução dos postos de trabalho é feita com base numa amostragem de um
# conjunto com variações do número de postos de trabalho.

# Este script ilustra a criação de um ficheiro CSV com os parâmetros do modelo a
# partir da leitura do ficheiro CSV que tem os dados pré-processados da DGAEP.

source ("src/utils.R")

dados <- fread (
  file = pasta.dados ("postos-trabalho-por-cargo-carreira-grupo.csv")
)

#' Calculo dos parâmetros do modelo de simulação nº 1
#'
#' Esta função calcula os parâmetros do modelo de simulação nº 1 para a evolução
#' dos postos de trabalho no sector das administrações públicas por subsector
#' segundo cargo, carreira, grupo, escalões etários e sexo.
#'
#' Neste modelo a evolução dos postos de trabalho é feita com base numa
#' amostragem de um conjunto com as variações do número de postos de trabalho.
#' Os parâmetros são guardados num ficheiro CSV.
#'
#' Por omissão a variação do número de postos é calculada entre 6 meses. O
#' parâmetro `delta` permite especificar outra duração temporal para o cálculo
#' da variação.
#'
#' @param delta duração temporal usada no cálculo da variação do número de
#'   postos de trabalho. Unidade é anos. Por omissão o valor é 0.5 (6 meses).
#'
#' @return `data.table` com as variações do número de postos de trabalho por
#'   semestre, por administração, por cargo/carreira/grupo, por faixa etária e
#'   por sexo.
#' @export
#'
#' @examples
#'
#' `calcula.parâmetros.modelo.simulação (delta=1)`
calcula.parâmetros.modelo.simulação <- function (
    delta = 0.5
) {
  calcula.diferença <- function (
    o.tempo, a.administração, o.cargo, a.faixa_etária, o.sexo,
    postos_de_trabalho_atual
  ) {
    postos_de_trabalho_seguinte <- dados [
      tempo == o.tempo + delta &
        administração == a.administração &
        cargo == o.cargo &
        faixa_etária == a.faixa_etária &
        sexo == o.sexo,
      postos_de_trabalho
    ]
    return (postos_de_trabalho_seguinte - postos_de_trabalho_atual)
  }
  tabela.parametros <- dados [
    ,
    calcula.diferença (tempo, administração, cargo, faixa_etária, sexo, postos_de_trabalho),
    by = .(
      tempo, administração, cargo, faixa_etária, sexo
    )
  ]
  fwrite (
    x = tabela.parametros,
    file = pasta.dados (
      sprintf (
        "parametros-modelo-1_postos-trabalho-por-cargo-carreira-grupo_delta=%s.csv",
        str_remove (
          sprintf ("%.1f", delta),
          "[.]?0+$"
        )
      )
    ),
    quote = TRUE
  )
  return (tabela.parametros)
}
