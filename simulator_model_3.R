
library (package = "data.table")
library (package = "ggplot2")

simular <- function (
    num.simulacoes = 30,
    duracao.simulacao = 10
) {
  corre.simulacoes <- function (
    el.cargo,
    el.administracao,
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
        nx <- max (
          0,
          x + rnorm (
            n = 1,
            mean = media,
            sd = variacao
          )
        )
        return (c (nx, rec.simula.postos.trabalho (nx, n - 1)))
      }
    }
    corre.simulacao <- function (
    ) {
      result <- data.table (
        `postos de trabalho` = rec.simula.postos.trabalho (
          valor.ponto.partida,
          duracao.simulacao
        ),
        time_number = seq (
          from = tempo.ponto.partida + 0.5,
          length.out = duracao.simulacao,
          by = 0.5
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
    cat (sprintf ("%s @ %d\n", el.cargo, valor.ponto.partida))
    media <- parametros.cargo.admin.idad.gen [, media.delta.postos.trabalho.6.meses]
    variacao <- parametros.cargo.admin.idad.gen [, variacao.delta.postos.trabalho.6.meses]
    result <- runs [
      ,
      corre.simulacao (),
      by = .(run)
    ]
    return (result)
  }
  # main ####
  dados.simulados <- parametros.modelo [
    ,
    corre.simulacoes (
      el.cargo = cargo,
      el.administracao = administração,
      el.idade_idx = idade_idx,
      el.genero = género,
      parametros.cargo.admin.idad.gen = .SD
    )
    ,
    by = .(
      cargo,
      administração,
      género,
      idade_idx
    )
  ]
  assign (
    x = "dados.simulados",
    value = dados.simulados,
    envir = .GlobalEnv
  )
  return (dados.simulados)
}

cria.grafico <- function (
    cargo
) {
  cat (sprintf ("* ** %s ** *\n", cargo))
  el.cargo <- cargo
  subconjunto.dados.reais <- dados.reais [cargo == el.cargo]
  subconjunto.dados.simulados <- dados.simulados [cargo == el.cargo]
  if (nrow (subconjunto.dados.simulados) == 0) {
    cat ("Sem dados!\n")
    return (FALSE)
  }
  el.plot <- ggplot (
  ) + geom_vline (
    xintercept = tempo.ponto.partida + 0.25,
    colour = "black"
  ) + geom_line (
    data = subconjunto.dados.reais,
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = as.factor (idade_idx)
    )
  ) + geom_point (
    data = subconjunto.dados.simulados,
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = as.factor (idade_idx)
    )
  ) + facet_grid (
    cols = vars (administração),
    rows = vars (género)
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
  num.administrações <- nrow (subconjunto.dados.reais [, administração, by = .(administração)])
  ggsave (
    filename = sprintf ("simulação-modelo-3_postos-de-trabalho_VS_tempo_%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1800 * num.administrações / 5 + 150,
    height = 800,
    dpi = 72
  )
  return (TRUE)
}

# iniciar ####

theme_set (
  theme_bw (
    base_size = 12
  )
)


# ler dados ####

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

# simular ####

tempo.ponto.partida <- dados.reais [, max (time_number)]

if (!exists (x = "dados.simulados")) {
  simular ()
}

# criar gráficos ####

cargos <- data.table (
  cargo = unique (parametros.modelo [, cargo])
)

cargos [
  ,
  cria.grafico (cargo),
  by = .(cargo)
]
