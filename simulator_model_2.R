
library (package = "data.table")
library (package = "ggplot2")

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
        cat ("cvector\n")
        print (cvector)
        vector [
          ,
          `postos de trabalho` :=
            (1 - min (1, 0.5 / 10)) * `postos de trabalho` +
            min (1, 0.5 / 10) * cvector [idade_idx - 1 == id, pt],
          by = .(idade_idx)
        ]
        cat ("update\n")
        print (vector)
        matriz <- rbind (matriz, vector)
        return (rec.simula.postos.trabalho (matriz, n - 1, t + 0.5))
      }
    }
    simulador <- function () {
      
    }
    cat (sprintf ("* ** %s ** *\n", el.cargo))
    print (parametros.cargo.admin.gen)
    vetor.partida <- dados.reais [
      `administração` == el.administracao &
        `género` == el.genero &
        cargo == el.cargo &
        time_number == tempo.ponto.partida,
      .(
        idade_idx,
        `postos de trabalho`,
        time_number
      )
    ]
    print (vetor.partida)
    resultado <- rec.simula.postos.trabalho (
      vetor.partida,
      duracao.simulacao,
      tempo.ponto.partida
    )
    print (resultado)
    c <- d
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

# simular ####

tempo.ponto.partida <- dados.reais [, max (time_number)]

if (!exists (x = "dados.simulados")) {
  simular ()
}

a <- b
# cria gráficos ####

cargos <- data.table (
  cargo = unique (parametros.modelo [, cargo])
)

cargos [
  ,
  cria.grafico (cargo),
  by = .(cargo)
]
