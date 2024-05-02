# Simulador do emprego público por administração, cargo, faixa etária e sexo. Utiliza os dados calculados pelo script analysis_of_jobs.R
#
# Pedro Mariano

library (package = "data.table")
library (package = "ggplot2")

simula.postos.trabalho <- function (
  el.administracao,
  el.idade_idx,
  el.idade,
  el.genero,
  el.cargo,
  duracao,
  subconjunto.dados.variacoes
) {
  valor.ponto.partida <- dados.brutos [
    `administração` == el.administracao &
      idade_idx == el.idade_idx &
      `género` == el.genero &
        cargo == el.cargo &
        time_number == tempo.ponto.partida,
    `postos de trabalho`
  ]
  
  cat (sprintf ("%s @ %d\n", el.cargo, valor.ponto.partida))
  
  rec.simula.postos.trabalho <- function (
    x,
    n
  )
  {
    if (n == 0) {
      return (NULL)
    }
    else {
      nx <- max (
        0,
        x + subconjunto.dados.variacoes [
          sample (.N, 1),
          delta.postos.trabalho.6.meses
        ]
      )
      return (c (nx, rec.simula.postos.trabalho (nx, n - 1)))
    }
  }
  result <- data.table (
    # `administração` = rep (el.administracao, duracao),
    # idade_idx = rep (el.idade_idx, duracao),
    # idade = rep (el.idade, duracao),
    # `género` = rep (el.genero, duracao),
    # cargo = rep (el.cargo, duracao),
    `postos de trabalho` = rec.simula.postos.trabalho (valor.ponto.partida, duracao),
    time_number = seq (from=tempo.ponto.partida+0.5, length.out=duracao, by = 0.5)
  )
}

cria.grafico <- function (
    cargo,
    subconjunto.dados
) {
  cat (sprintf ("* ** %s ** *\n", cargo))
  el.plot <- ggplot (
    data = subconjunto.dados
  ) + geom_point (
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_idx),
      shape = `género`,
      size = 2
    )
  ) + geom_vline (
    xintercept = tempo.ponto.partida + 0.5,
    colour = "black"
  ) + geom_line (
    data = subconjunto.dados [`género` == "m"],
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_idx),
      group = idade_idx
    )
  ) + geom_line (
    data = subconjunto.dados [`género` == "f"],
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_idx),
      group = idade_idx
    )
  ) + facet_wrap (
    facets = vars (`administração`)
  ) + labs (
    title = cargo
  ) + scale_colour_discrete (
    name = "idade",
    labels = c (
      "24 ou menos",
      "25 a 34",
      "35 a 44",
      "45 a 54",
      "55 a 65",
      "65 ou mais"
    )
  ) + scale_size (
    guide = "none"
  ) + scale_shape (
    labels = c (
      "mulher",
      "homem"
    )
  ) + scale_x_continuous (
    name = "tempo"
  )
  ggsave (
    filename = sprintf ("simulação_postos-de-trabalho_VS_tempo_%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 72
  )
  return (TRUE)
}

## ler dados ####

dados.brutos <- fread (
  file = "posto-trabalho-por-admin-cargo-genero-data.csv"
)
dados.variacoes <- fread (
  file = "variações-posto-trabalho-por-admin-cargo-genero-idade.csv"
)

dados.brutos  [
  ,
  `:=` (
    time_label = sprintf ("%d/%02d/%0d", ano, `mês`, dia),
    time_number = ano + (mês - 6) / 12.0
  )
]

## simular ####

tempo.ponto.partida <- dados.brutos [, max (time_number)]

dados.simulados <- dados.variacoes [
  !is.na (delta.postos.trabalho.6.meses),
  simula.postos.trabalho (
    el.administracao = `administração`,
    el.idade_idx = idade_idx,
    el.idade = idade,
    el.genero = `género`,
    el.cargo = cargo,
    duracao = 10,
    subconjunto.dados.variacoes = .SD
  ),
  by = .(
    cargo,
    `administração`,
    idade_idx,
    idade,
    `género`
  )
]

## apresentar gráficos ####

enchilada.dados <- rbind (
  dados.simulados,
  dados.brutos [
    ,
    .(
      cargo,
      `administração`,
      idade_idx,
      idade,
      `género`,
      `postos de trabalho`,
      time_number
    )
  ],
  use.names = TRUE
)

enchilada.dados [
  ,
  cria.grafico(
    cargo = cargo,
    subconjunto.dados = .SD
  ),
  by = .(cargo)
]
