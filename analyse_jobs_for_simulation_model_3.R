
library (package = "data.table")
library (package = "ggplot2")

el.data <- fread (
  file = "posto-trabalho-por-admin-cargo-genero-data.csv"
)

el.data  [
  ,
  `:=` (
    time_number = ano + (mês - 6) / 12.0,
    dia = NULL,
    mês = NULL,
    ano = NULL,
    idade_num = NULL,
    idade = NULL
  )
]

calcula.parametros.modelo.simulação <- function (
) {
  analisa.cargo <- function (
    cargo,
    subconjunto.dados
  ) {
    cat (sprintf ("%s\n", cargo))
    resultado <- subconjunto.dados [
      ,
      calculo.parametros (.SD),
      by = .(
        administração,
        `idade_idx`,
        género
      )
    ]
    return (resultado)
  }
  calculo.parametros <- function (
    subconjunto.dados
  ) {
    postos.trabalho.a <- function (
      el.time
    ) {
      return (subconjunto.dados [
        time_number == el.time,
        `postos de trabalho`
      ])
    }
    variacao.dados <- subconjunto.dados [
      time_number >= min.time + 0.5,
      .(
        delta.postos.trabalho.6.meses = `postos de trabalho` - postos.trabalho.a (time_number - 0.5)
      ),
      by = .(time_number)
    ]
    resultado <- variacao.dados [
      ,
      .(
        media.delta.postos.trabalho.6.meses = mean (delta.postos.trabalho.6.meses),
        # mediana.delta.postos.trabalho.6.meses = median (delta.postos.trabalho.6.meses),
        variacao.delta.postos.trabalho.6.meses = sd (delta.postos.trabalho.6.meses)
      )
    ]
    return (resultado)
  }

  min.time <- el.data [
    ,
    min (time_number)
  ]
  parametros.modelo.simulação <- el.data [
    ,
    analisa.cargo (
      cargo,
      .SD
      ),
    by = .(
      cargo
    )
  ]
  fwrite (
    x = parametros.modelo.simulação,
    file = "parametros-modelo-3-simulação-postos-trabalho.csv"
  )
  return (parametros.modelo.simulação)
}
