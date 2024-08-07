# script utilizado no dashboard do modelo de simulação 6. Para correr no
# rstudio, é preciso alterar o caminho na função `pasta.dados`.

library (package = "data.table")
library (package = "ggplot2")
library (package = "stringr")
library (package = "roxygen2")

pasta.dados <- function (
  ficheiro
) {
  return (file.path ("../dados", ficheiro))
}

faixas.etarias <- fread (
  file = pasta.dados ("faixas-etarias.csv")
)

FAIXA.ETARIA.MIN <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MAX <- faixas.etarias [, max (FE.id)]
FAIXA.ETARIA.MIN.IDADE <- faixas.etarias [, min (FE.idade.min)]

fes.homem <- faixas.etarias [
  is.na (FE.sexo) | FE.sexo == "H"
]

fes.mulher  <- faixas.etarias [
  is.na (FE.sexo) | FE.sexo == "M"
]

duração.faixa.etaria <- function (el.idade, el.sexo) {
  return (faixas.etarias [
    FE.id == el.idade & (is.na (FE.sexo) | FE.sexo == el.sexo),
    FE.idade.max - FE.idade.min
  ])
}

dados.reais <- fread (
  file = pasta.dados (
    "postos-trabalho-por-ministerios-secretarias-regionais.csv"
  )
)

delta <- 0.5

vector.sexos <- data.table (sexo = c ("H", "M"))

parâmetros.modelo <- fread (
  file = pasta.dados (
    sprintf (
      "parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=%s.csv",
      str_remove (
        sprintf ("%.1f", delta),
        "[.]?0+$"
      )
    )
  )
)

admins <- data.table (
  admins = unique (parâmetros.modelo [, administração])
)
minist <- data.table (
  minist = unique (parâmetros.modelo [, ministério_secretaria])
)

#' Calcula os gráficos apresentados no dashboard.
#'
#' @param a.administração Administração escolhida pelo utilizador.
#' @param o.ministério.secretaria Ministério/secretaria escolhida pelo utilizador.
#' @param número.corridas Número de corridas feitas com variação do número de postos de trabalho.
#' @param duração.simulação Duração da simulação (número de iterações).
#'
#' @return Gráficos apresentado no dashboard.
#' @export
#'
#' @examples
cria.gráficos <- function (
    a.administração,
    o.ministério.secretaria,
    número.corridas,
    duração.simulação
) {
  calcula.simulações.com.variações <- function (
    o.sexo
  ) {
    iteração.simulação <- function (
    acumulador,
    vector.estado,
    tempo,
    n
    ) {
      if (n == 0)
        return (acumulador)
      else {
        # actualizar o acumulador com o valor actual do vector estado ####
        novo.acumulador <- rbind (
          acumulador,
          fes.certo [
            ,
            .(
              tempo = tempo,
              faixa_etária = FE.id,
              postos_de_trabalho = vector.estado [
                FE.idade.min <= idade.minima &
                  FE.idade.max >= idade.minima + intervalo.idades
                ,
                sum (pt)
              ]
            ),
            by = .(FE.id)
          ]
        )
        # novas contratações, saídas, mortes, outras variações do número de postos de trabalho ####
        variações <- sub.parametros [
          ,
          .(
            FE.idade.min = fes.certo [FE.id == faixa_etária, FE.idade.min],
            FE.idade.max = fes.certo [FE.id == faixa_etária, FE.idade.max],
            variação = rnorm (n = 1, mean = media.delta, sd = desvio.padrão.delta)
          ),
          by = .(faixa_etária)
        ]
        variação.faixa.etária <- function (a.idade.minima, o.intervalo.idades) {
          intervalor.usar <- o.intervalo.idades - ifelse (a.idade.minima == FAIXA.ETARIA.MIN.IDADE, delta, 0)
          return (variações [
            FE.idade.min <= a.idade.minima &
              FE.idade.max >= a.idade.minima + o.intervalo.idades,
            variação * intervalor.usar / (FE.idade.max - FE.idade.min)
          ])
        }
        vector.estado.passo.1.1 <- vector.estado [
          ,
          .(
            pt = max (0, pt + variação.faixa.etária (a.idade.minima = idade.minima, o.intervalo.idades = intervalo.idades))
          ),
          by = .(idade.minima, intervalo.idades)
        ]
        # novas contratações no intervalo de idades com a idade mais baixa ####
        vector.estado.passo.1.2 <- data.table (
          idade.minima = FAIXA.ETARIA.MIN.IDADE,
          intervalo.idades = delta,
          pt = max (0, variações [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, variação / fes.certo [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, FE.idade.max - FE.idade.min]])
        )
        # envelhecimento ####
        vector.estado.passo.2 <- vector.estado.passo.1.1 [
          ,
          .(
            idade.minima = idade.minima + delta,
            intervalo.idades = intervalo.idades,
            pt = pt
          )
        ]
        # processar os triplos (idade mínima, intervalo de idades, postos de trabalho) de modo a que as idades fiquem entre as faixas etárias existentes ####
        ## triplos que estão dentro do intervalo de uma faixa etária ####
        vector.estado.passo.3.1 <- fes.certo [
          ,
          vector.estado.passo.2 [
            FE.idade.min <= idade.minima &
              idade.minima + intervalo.idades <= FE.idade.max
          ],
          by = .(FE.id, FE.idade.min, FE.idade.max)
        ] [
          ,
          .(idade.minima, intervalo.idades, pt)
        ]
        ## triplos que estão acima de uma faixa etária ####
        vector.estado.passo.3.2 <- fes.certo [
          ,
          vector.estado.passo.2 [
            FE.idade.min <= idade.minima &
              idade.minima < FE.idade.max &
              FE.idade.max < idade.minima + intervalo.idades,
            .(
              idade.minima,
              intervalo.idades = FE.idade.max - idade.minima,
              pt = pt * (FE.idade.max - idade.minima) / intervalo.idades
            )
          ],
          by = .(FE.id, FE.idade.min, FE.idade.max)
        ] [
          ,
          .(idade.minima, intervalo.idades, pt)
        ]
        ## triplos que estão abaixo de uma faixa etária ####
        vector.estado.passo.3.3 <- fes.certo [
          ,
          vector.estado.passo.2 [
            idade.minima < FE.idade.min &
              FE.idade.min < idade.minima + intervalo.idades &
              idade.minima + intervalo.idades <= FE.idade.max,
            .(
              idade.minima = FE.idade.min,
              intervalo.idades = idade.minima + intervalo.idades - FE.idade.min,
              pt = pt * (idade.minima + intervalo.idades - FE.idade.min) / intervalo.idades
            )
          ],
          by = .(FE.id, FE.idade.min, FE.idade.max)
        ] [
          ,
          .(idade.minima, intervalo.idades, pt)
        ]
        # juntar tudo ####
        lpt <- list (
          vector.estado.passo.1.2,
          vector.estado.passo.3.1,
          vector.estado.passo.3.2,
          vector.estado.passo.3.3
        )
        vector.estado.passo.4 <- rbindlist (lpt)
        return (iteração.simulação (
          acumulador = novo.acumulador,
          vector.estado = vector.estado.passo.4,
          tempo = tempo + delta,
          n = n - 1
        ))
      }
    }
    sub.parametros <- parâmetros.modelo [
      administração == a.administração &
        ministério_secretaria == o.ministério.secretaria &
        sexo == o.sexo
    ]
    tempo.partida <- dados.reais [
      administração == a.administração &
        ministério_secretaria == o.ministério.secretaria &
        sexo == o.sexo,
      max (tempo)
    ]
    if (o.sexo == "H")
      fes.certo <- fes.homem
    else
      fes.certo <- fes.mulher
    vector.partida <- dados.reais [
      tempo == tempo.partida &
        administração == a.administração &
        ministério_secretaria == o.ministério.secretaria &
        sexo == o.sexo,
      .(
        idade.minima = fes.certo [FE.id == faixa_etária, FE.idade.min],
        intervalo.idades = fes.certo [FE.id == faixa_etária, FE.idade.max - FE.idade.min],
        pt = postos_de_trabalho
      )
    ]
    resultado <- vector.corridas [
      ,
      iteração.simulação (data.table (), vector.partida, tempo.partida, duração.simulação)
      ,
      by = .(corrida)
    ]
    return (resultado)
  }
  calcula.simulações.só.envelhecimento <- function (
    o.sexo
  ) {
    tempo.partida <- dados.reais [
      administração == a.administração &
        ministério_secretaria == o.ministério.secretaria &
        sexo == o.sexo,
      max (tempo)
    ]
    if (o.sexo == "H")
      fes.certo <- fes.homem
    else
      fes.certo <- fes.mulher
    vector.partida <- dados.reais [
      tempo == tempo.partida &
        administração == a.administração &
        ministério_secretaria == o.ministério.secretaria &
        sexo == o.sexo,
      .(
        idade.minima = fes.certo [FE.id == faixa_etária, FE.idade.min],
        intervalo.idades = fes.certo [FE.id == faixa_etária, FE.idade.max - FE.idade.min],
        pt = postos_de_trabalho
      )
    ]
    # inicializar os triplos (idade mínima, intervalo de idades, postos de trabalho) para a duração da simulação ####
    acumulador.passo.1.1 <- data.table (
      tempo = delta * c (0:duração.simulação) + tempo.partida
    )
    acumulador.passo.1.2 <- acumulador.passo.1.1 [
      ,
      {function () vector.partida} ()
      ,
      by = .(tempo)
    ]
    # ajustar a idade mínima conforme o tempo ####
    acumulador.passo.1.2 [
      ,
      idade.minima := idade.minima + tempo - tempo.partida
    ]
    # processar os triplos (idade mínima, intervalo de idades, postos de trabalho) de modo a que as idades fiquem entre as faixas etárias existentes ####
    ## triplos que estão dentro do intervalo de uma faixa etária ####
    acumulador.passo.3.1 <- fes.certo [
      ,
      acumulador.passo.1.2 [
        FE.idade.min <= idade.minima &
          idade.minima + intervalo.idades <= FE.idade.max
      ],
      by = .(FE.id, FE.idade.min, FE.idade.max)
    ] [
      ,
      .(tempo, idade.minima, intervalo.idades, pt)
    ]
    ## triplos que estão acima de uma faixa etária ####
    acumulador.passo.3.2 <- fes.certo [
      ,
      acumulador.passo.1.2 [
        FE.idade.min <= idade.minima &
          idade.minima < FE.idade.max &
          FE.idade.max < idade.minima + intervalo.idades,
        .(
          tempo,
          idade.minima,
          intervalo.idades = FE.idade.max - idade.minima,
          pt = pt * (FE.idade.max - idade.minima) / intervalo.idades
        )
      ],
      by = .(FE.id, FE.idade.min, FE.idade.max)
    ] [
      ,
      .(tempo, idade.minima, intervalo.idades, pt)
    ]
    ## triplos que estão abaixo de uma faixa etária ####
    acumulador.passo.3.3 <- fes.certo [
      ,
      acumulador.passo.1.2 [
        idade.minima < FE.idade.min &
          FE.idade.min < idade.minima + intervalo.idades &
          idade.minima + intervalo.idades <= FE.idade.max,
        .(
          tempo,
          idade.minima = FE.idade.min,
          intervalo.idades = idade.minima + intervalo.idades - FE.idade.min,
          pt = pt * (idade.minima + intervalo.idades - FE.idade.min) / intervalo.idades
        )
      ],
      by = .(FE.id, FE.idade.min, FE.idade.max)
    ] [
      ,
      .(tempo, idade.minima, intervalo.idades, pt)
    ]
    # juntar tudo ####
    lpt <- list (
      acumulador.passo.3.1,
      acumulador.passo.3.2,
      acumulador.passo.3.3
    )
    acumulador.passo.4 <- rbindlist (lpt)
    # calcular os postos de trabalho pelas faixas etárias ####
    acumulador.passo.5 <- fes.certo [
      ,
      acumulador.passo.4 [
        FE.idade.min <= idade.minima &
          FE.idade.max >= idade.minima + intervalo.idades
        ,
        .(
          faixa_etária = FE.id,
          postos_de_trabalho = sum (pt)
        ),
        by = .(tempo)
      ],
      by = .(FE.id)
    ]
    return (acumulador.passo.5)
  }
  # main ####
  subconjunto.dados.reais <- dados.reais [
    administração == a.administração &
      ministério_secretaria == o.ministério.secretaria
  ]
  vector.corridas <- data.table (
    corrida = c(1:número.corridas)
  )
  ## simulações com variações do número de postos de trabalho ####
  dados.simulados.com.variações <- vector.sexos [
    ,
    calcula.simulações.com.variações (o.sexo = sexo),
    by = .(sexo)
  ]
  ## simulações só com envelhecimento da força laboral ####
  dados.simulados.só.envelhecimento <- vector.sexos [
    ,
    calcula.simulações.só.envelhecimento (o.sexo = sexo),
    by = .(sexo)
  ]
  # criar gráfico ####
  el.plot <- ggplot (
  ) + geom_line (
    data = subconjunto.dados.reais,
    mapping = aes (
      x = tempo,
      y = postos_de_trabalho,
      colour = as.factor (faixa_etária)
    )
  ) + geom_point (
    data = dados.simulados.com.variações,
    mapping = aes (
      x = tempo,
      y = postos_de_trabalho,
      colour = as.factor (faixa_etária)
    )
  ) + geom_line (
    data = dados.simulados.só.envelhecimento,
    mapping = aes (
      x = tempo,
      y = postos_de_trabalho
    ),
    colour = "black",
    alpha = 0.5,
    linewidth = 1
  ) + facet_grid (
    cols = vars (faixa_etária),
    rows = vars (sexo),
    space = "free_y"
  ) + scale_colour_discrete (
    name = "idade",
    breaks = factor (unique (faixas.etarias [, FE.id])),
    labels = unique (faixas.etarias [, FE.etiqueta])
  ) + scale_x_continuous (
    name = "tempo"
  ) + scale_y_continuous (
    name = "número de postos de trabalho"
  ) + theme_bw ()
  return (el.plot)
}
