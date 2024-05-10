# Script para calcular os dados para o modelo 2 de simulação do emprego público.
# Neste modelo assumimos que há uma percentagem das pessoas que pertencem a uma
# faixa etária f_i que passam para a faixa etária seguinte f_{i+1}. Esta
# percentagem é proporcional à divisão entre a duração do intervalo de tempo e a
# diferença de idades máxima e mínima da faixa etária f_i.

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

setkey (
  el.data,
  time_number,
  idade_idx
)

faixas.etarias <- fread (
  file = "faixas-etarias.csv"
)

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
    cat (sprintf ("* ** %s ** *\n", cargo))
    # tabelar ####
    resultado <- subconjunto.dados [
      ,
      calculo.parametros (.SD),
      by = .(
        administração,
        género
      )
    ]
    # criar gráfico ####
    el.plot <- ggplot (
      data = resultado
    ) + geom_point (
      mapping = aes (
        x = as.factor (idade_idx),
        y = delta.postos.trabalho.6.meses
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
    ) + labs (
      title = cargo
    )
    # salvar gráfico ####
    num.administrações <- nrow (resultado [, administração, by = .(administração)])
    ggsave (
      filename = sprintf ("modelo-2-simulação_delta-postos-trabalho_6-meses_%s.png", gsub ("/", "_", cargo)),
      plot = el.plot,
      device = "png",
      units = "px",
      width = 1800 * num.administrações / 5 + 50,
      height = 800,
      dpi = 72
    )
    return (resultado)
  }
  calculo.parametros <- function (
    subconjunto.dados
  ) {
    # sub-funções ####
    valor <- function (
      el.idade,
      el.time
    ) {
      F.idade <- faixas.etarias [
        FE.id == el.idade,
        FE.idade.max - FE.idade.min + 1
      ]
      ptfi <- subconjunto.dados [
        time_number == el.time - 0.5 &
          idade_idx == el.idade,
        `postos de trabalho`
      ]
      factor1 <- (1 - min (1, 0.5 / F.idade)) * ptfi
      if (el.idade > 0) {
        F.idade.m1 <- faixas.etarias [
          FE.id == el.idade - 1,
          FE.idade.max - FE.idade.min + 1
        ]
        ptfim1 <- subconjunto.dados [
          time_number == el.time - 0.5 &
            idade_idx == el.idade - 1,
          `postos de trabalho`
        ]
        factor2 <- -(1 - min (1, 0.5 / F.idade.m1)) * ptfim1
      }
      else {
        factor2 <- 0
      }
      return (factor1 + factor2)
    }
    # main ####
    resultado <- subconjunto.dados [
      time_number >= min.time + 0.5,
      .(
        delta.postos.trabalho.6.meses = `postos de trabalho` - valor (idade_idx, time_number)
      ),
      by = .(
        idade_idx,
        time_number
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
    file = "parametros-modelo-2-simulação-postos-trabalho.csv",
    quote = TRUE
  )
  return (parametros.modelo.simulação)
}
