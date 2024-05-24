library (package = "ggplot2")
library (package = "data.table")

dados.reais <- fread (
  file = "posto-trabalho-por-admin-cargo-genero-data.csv"
)
dados.reais  [
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
parametros.modelo <- fread (
  file = "parametros-modelo-3-simulação-postos-trabalho.csv"
)
admins <- data.table (
  admins = unique (parametros.modelo [, administração])
)
cargos <- data.table (
  cargo = unique (parametros.modelo [, cargo])
)
faixas.etarias <- fread (
  file = "faixas-etarias.csv"
)

delta.faixa.etaria <- function (
    idade_idx
) {
  return (faixas.etarias [
    FE.id == idade_idx,
    FE.idade.max - FE.idade.min + 1
  ])
}

corre.simulador <- function (
    el.administracao,
    el.cargo,
    num.simulacoes,
    duracao.simulacao,
    tempo.ponto.partida = dados.reais [, max (time_number)],
    delta.tempo = 0.5
) {
  corre.simulacoes <- function (
    el.idade_idx,
    el.genero,
    parametros.cargo.admin.idad.gen
  ) {
    rec.simula.postos.trabalho <- function (
    x,
    n
    ) {
      if (n == 0) {
        return (NULL)
      }
      else {
        nx <- max (0, x + rnorm (
          n = 1,
          mean = media,
          sd = variacao
        ))
        return (c (
          nx,
          rec.simula.postos.trabalho (nx, n - 1)
        ))
      }
    }
    corre.simulacao <- function (
    ) {
      result <- data.table (
        `postos de trabalho` = rec.simula.postos.trabalho (
          valor.ponto.partida,
          duracao.simulacao),
        time_number = seq (
          from = tempo.ponto.partida + delta.tempo,
          length.out = duracao.simulacao,
          by = delta.tempo
        )
      )
      return (result)
    }
    # main ####
    valor.ponto.partida <- dados.reais [
      `administração` == el.administracao &
        idade_idx == el.idade_idx &
        `género` == el.genero &
        cargo == el.cargo &
        time_number == tempo.ponto.partida,
      `postos de trabalho`
    ]
    runs <- data.table (
      run = c (1:num.simulacoes)
    )
    media <- parametros.cargo.admin.idad.gen [
      ,
      media.delta.postos.trabalho.6.meses
    ]
    variacao <- parametros.cargo.admin.idad.gen [
      ,
      variacao.delta.postos.trabalho.6.meses
    ]
    result <- runs [
      ,
      corre.simulacao (),
      by = .(run)
    ]
    return (result)
  }
  corre.simulacao.so.com.envelhecimento <- function () {
    
    rec.simula.postos.trabalho <- function (
      acumulador,
      vector,
      n
    ) {
      if (n == 0) {
        return (acumulador)
      }
      else {
        pt <- function (idade, genero) {
          return (vector [idade_idx == idade & género == genero, postos.trabalho])
        }
        prox.vector <- copy (vector)
        prox.vector [
          ,
          `:=`(
            time_number = time_number + delta.tempo,
            postos.trabalho = max (
              0,
              (1 - delta.tempo / delta.faixa.etaria (idade_idx)) * pt (idade_idx, género)
              + ifelse (
                idade_idx > 0,
                delta.tempo / delta.faixa.etaria (idade_idx - 1) * pt (idade_idx - 1, género),
                0
                )
            )
          ),
          by = .(idade_idx, género)
        ]
        prox.acumulador <- rbind (acumulador, prox.vector)
        return (rec.simula.postos.trabalho (prox.acumulador, prox.vector, n - 1))
      }
    }
    vector.partida <- dados.reais [
      `administração` == el.administracao &
        cargo == el.cargo &
        time_number == tempo.ponto.partida,
      .(
        idade_idx,
        género,
        postos.trabalho = as.double (`postos de trabalho`),
        time_number
      )
    ]
    return (rec.simula.postos.trabalho (vector.partida, vector.partida, duracao.simulacao))
  }
  # main ####
  dados.simulados.so.envelhecimento <- corre.simulacao.so.com.envelhecimento ()
  dados.simulados <- parametros.modelo [
    cargo == el.cargo &
      administração == el.administracao,
    corre.simulacoes (
      el.idade_idx = idade_idx,
      el.genero = género,
      parametros.cargo.admin.idad.gen = .SD
    ),
    by = .(género, idade_idx)
  ]
  subconjunto.dados.reais <- dados.reais [
    administração == el.administracao &
      cargo == el.cargo
  ]
  el.plot <- ggplot (
  ) + geom_line (
    data = subconjunto.dados.reais,
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = as.factor (idade_idx)
    )
  ) + geom_point (
    data = dados.simulados,
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = as.factor (idade_idx)
    )
  ) + geom_line (
    data = dados.simulados.so.envelhecimento,
    mapping = aes (
      x = time_number,
      y = postos.trabalho
    ),
    # show.legend = FALSE,
    colour = "black",
    alpha = 0.5,
    size = 0.75
  ) + facet_grid (
    cols = vars (idade_idx),
    rows = vars (género)
  ) + scale_colour_discrete (
    name = "idade",
    limits = factor (c (0, 1, 2, 3, 4, 5)),
    labels = c ("24 ou menos", "25 a 34", "35 a 44", "45 a 54", "55 a 65", "65 ou mais")
  ) + scale_x_continuous (
    name = "tempo"
  ) + theme_bw ()
  return (el.plot)
}
