
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

theme_set (
  theme_bw (
    base_size = 12
  )
)

calcula.parametros.modelo.simulação <- function (
) {
  analisa.cargo <- function (
    cargo,
    subconjunto.dados
  ) {
    cat (sprintf ("%s\n", cargo))
    # tabelar ####
    resultado <- subconjunto.dados [
      ,
      calculo.parametros (.SD),
      by = .(
        administração,
        `idade_idx`,
        género
      )
    ]
    # criar gráfico ####
    el.plot <- ggplot (
      data = resultado
    ) + geom_pointrange (
      mapping = aes (
        x = as.factor (idade_idx),
        y = media.delta.postos.trabalho.6.meses,
        ymin = media.delta.postos.trabalho.6.meses - variacao.delta.postos.trabalho.6.meses,
        ymax = media.delta.postos.trabalho.6.meses + variacao.delta.postos.trabalho.6.meses
      )
    ) + scale_x_discrete (
      name = "idade",
      labels = c (
        "24 ou menos",
        "25 a 34",
        "35 a 44",
        "45 a 54",
        "55 a 65",
        "65 ou mais"
      )
    ) + scale_y_continuous (
      name = "postos de trabalho"
    ) + facet_grid (
      cols = vars (administração),
      rows = vars (género)
    # ) + theme_classic (
    ) + labs (
      title = cargo
    )
    # salvar gráfico ####
    num.administrações <- nrow (resultado [, administração, by = .(administração)])
    print (num.administrações)
    ggsave (
      filename = sprintf ("modelo-3-simulação_delta-postos-trabalho_6-meses_%s.png", gsub ("/", "_", cargo)),
      plot = el.plot,
      device = "png",
      units = "px",
      width = 1800 * num.administrações / 5 + 50,
      height = 950,
      dpi = 72
    )
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

  # main ####

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
