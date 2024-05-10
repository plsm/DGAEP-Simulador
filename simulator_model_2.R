
library (package = "data.table")
library (package = "ggplot2")

delta.faixa.etaria <- function (
    idade_idx
) {
  return (faixas.etarias [
    FE.id == idade_idx,
    FE.idade.max - FE.idade.min + 1
  ])
}

simular <- function (
    num.simulacoes = 30,
    duracao.simulacao = 10
) {
  corre.simulacoes <- function (
    el.cargo,
    el.administracao,
    el.genero,
    parametros.cargo.admin.gen
  ) {
    variacao <- function (
      idade
    ) {
      dt <- parametros.cargo.admin.gen [idade_idx == idade]
      # cat (sprintf ("Devolver um valor aleatório da idade %d de:\n", idade))
      # print (dt)
      return (dt [
        sample (.N, 1),
        delta.postos.trabalho.6.meses
      ])
    }
    rec.simula.postos.trabalho <- function (
      matriz,
      n,
      t
    ) {
      if (n == 0) {
        return (matriz)
      }
      else {
        vector <- matriz [time_number == t, ]
        vector [, time_number := time_number + 0.5]
        cvector <- vector [
          ,
          .(
            id = idade_idx,
            pt = `postos de trabalho`
          )
        ]
        anterior <- function (
          idade
        ) {
          return (ifelse (
            idade == 0,
            0,
            min (
              1,
              0.5 / delta.faixa.etaria (idade_idx = idade)
              ) * cvector [id == idade - 1, pt]
            ))
        }
        # cat ("cvector\n")
        # print (cvector)
        vector [
          ,
          `postos de trabalho` := max (
            0,
            (1 - min (1, 0.5 / delta.faixa.etaria (idade_idx = idade_idx))) * `postos de trabalho` +
              anterior (idade_idx) +
              variacao (idade_idx)
          ),
          by = .(idade_idx)
        ]
        # cat ("update\n")
        # print (vector)
        matriz <- rbind (matriz, vector)
        return (rec.simula.postos.trabalho (matriz, n - 1, t + 0.5))
      }
    }
    simulador <- function () {
      
    }
    cat (sprintf ("* ** %s ** *\n", el.cargo))
    # print (parametros.cargo.admin.gen)
    vetor.partida <- dados.reais [
      `administração` == el.administracao &
        `género` == el.genero &
        cargo == el.cargo &
        time_number == tempo.ponto.partida,
      .(
        idade_idx,
        `postos de trabalho` = as.double (`postos de trabalho`),
        time_number
      )
    ]
    # cat (sprintf ("Dados reais do cargo %s:\n", el.cargo))
    # print (dados.reais [cargo == el.cargo])
    # cat ("Dados reais\n")
    # print (dados.reais [
    #   `administração` == el.administracao &
    #     `género` == el.genero &
    #     cargo == el.cargo])
    # cat ("vector partida\n")
    # print (vetor.partida)
    if (nrow (vetor.partida) == 0) {
      cat ("VAZIO!!\n")
      return (data.table ())
    }
    else {
      resultado <- rec.simula.postos.trabalho (
        vetor.partida,
        duracao.simulacao,
        tempo.ponto.partida
      )
      # print (resultado)
      return (resultado)
    }
  }
  # main ####
  runs <- data.table (
    run = c (1:num.simulacoes)
  )
  dados.simulados <- parametros.modelo [
    ,
    corre.simulacoes (
      el.cargo = cargo,
      el.administracao = administração,
      el.genero = género,
      parametros.cargo.admin.gen = .SD
    )
    ,
    by = .(
      cargo,
      administração,
      género
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
    filename = sprintf ("simulação-modelo-2_postos-de-trabalho_VS_tempo_%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1800 * num.administrações / 5 + 150,
    height = 800,
    dpi = 72
  )
  return (TRUE)
}

criar.graficos <- function (
) {
  cargos <- data.table (
    cargo = unique (parametros.modelo [, cargo])
  )
  
  cargos [
    ,
    cria.grafico (cargo),
    by = .(cargo)
  ]

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
  file = "parametros-modelo-2-simulação-postos-trabalho.csv"
)

faixas.etarias <- fread (
  file = "faixas-etarias.csv"
)

# main ####

# parametros.modelo <- parametros.modelo [cargo == "Forças Armadas "]

tempo.ponto.partida <- dados.reais [, max (time_number)]

if (!exists (x = "dados.simulados")) {
  simular ()
}

criar.graficos ()
