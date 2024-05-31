library (package = "data.table")
library (package = "stringr")

dados.reais <- fread (
  file = "posto-trabalho-por-subsetor-ministerios-secretarias.csv"
)

faixas.etarias <- fread (
  file = "faixas-etarias2.csv"
)

FAIXA.ETARIA.MIN <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MAX <- faixas.etarias [, max (FE.id)]

duracao.faixa.etaria <- function (el.idade) {
  return (faixas.etarias [
    FE.id == el.idade,
    FE.idade.max - FE.idade.min
  ])
}

corre.simulação <- function (
    numero.corridas = 30,
    duracao = 10,
    delta = 0.5,
    tempo.partida = NA
) {
  ciclo.administração <- function (
    nome.administração,
    sub.parametros
  ) {
    cat (sprintf ("%s\n", nome.administração))
    return (sub.parametros [
      ,
      ciclo.ministerio.secretaria (nome.administração, ministério_secretaria, .SD),
      by = .(ministério_secretaria)
    ])
  }
  ciclo.ministerio.secretaria <- function (
    nome.administração,
    nome.ministerio.secretaria,
    sub.parametros
  ) {
    cat (sprintf ("  %s\n", nome.ministerio.secretaria))
    return (sub.parametros [
      ,
      ciclo.sexo (nome.administração, nome.ministerio.secretaria, sexo, .SD),
      by = .(sexo)
    ])
  }
  ciclo.sexo <- function (
    nome.administração,
    nome.ministerio.secretaria,
    nome.sexo,
    sub.parametros
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
        cat ("Recursivo\n")
        print (vector.estado)
        novo.acumulador <- rbind (
          acumulador,
          faixas.etarias [
            ,
            .(
              tempo = tempo,
              faixa_etária = FE.id,
              `postos de trabalho` = vector.estado [
                FE.idade.min <= idade.minima &
                  FE.idade.max >= idade.minima + intervalo.idades
                ,
                sum (pt)
              ]
            ),
            by = .(FE.id)
          ]
        )
        variação.faixa.etária <- function (
          faixa.etária.id,
          média,
          desvio
        ) {
          variação <- rnorm (n = 1, mean = média, sd = desvio)
          idade.min <- faixas.etarias [FE.id == faixa.etária.id, FE.idade.min]
          idade.max <- faixas.etarias [FE.id == faixa.etária.id, FE.idade.max]
          intervalo <- idade.max - idade.min
          return (vector.estado [
            idade.min <= idade.minima & idade.max >= idade.minima + intervalo.idades,
            .(
              idade.minima,
              intervalo.idades,
              pt = max (0, pt + variação * intervalo.idades / intervalo)
            )
          ])
        }
        # novas contratações, saídas, mortes, outras variações do número de postos de trabalho
        variações <- sub.parametros [
          ,
          .(
            FE.idade.min = faixas.etarias [FE.id == faixa_etária, FE.idade.min],
            FE.idade.max = faixas.etarias [FE.id == faixa_etária, FE.idade.max],
            variação = rnorm (n = 1, mean = media.delta, sd = desvio.padrão.delta)
          ),
          by = .(faixa_etária)
        ]
        vector.estado.passo.1 <- (variações [
          ,
          vector.estado [
            FE.idade.min <= idade.minima & idade.minima + intervalo.idades <= FE.idade.max,
            .(
              idade.minima,
              intervalo.idades,
              pt = max (0, pt + variação * intervalo.idades / (FE.idade.max - FE.idade.min))
            )
          ],
          by = .(FE.idade.min, FE.idade.max)
        ]) [, .(idade.minima, intervalo.idades, pt)]
        cat ("Varições\n")
        print (variações)
        cat ("Passo 1\n")
        print (vector.estado.passo.1)
        vector.estado.passo.1 <- sub.parametros [
          ,
          variação.faixa.etária (faixa_etária, media.delta, desvio.padrão.delta),
          by = .(faixa_etária, media.delta, desvio.padrão.delta)
        ]
        cat ("Passo 1\n")
        print (vector.estado.passo.1)
        # envelhecimento
        vector.estado.passo.2 <- vector.estado.passo.1 [
          ,
          .(
            idade.minima = idade.minima + delta,
            intervalo.idades = intervalo.idades,
            pt = pt
          )
        ]
        cat ("Passo 2\n")
        print (vector.estado.passo.2)
        # processar os triplos (idade minima, intervalo de idades, postos de trabalho) de modo a que as idades fiquem entre as faixas etárias existentes
        cat (" ...3.1\n")
        vector.estado.passo.3.1 <- (
          faixas.etarias [
            ,
            vector.estado.passo.2 [
              FE.idade.min <= idade.minima & idade.minima + intervalo.idades <= FE.idade.max
            ],
            by = .(FE.id, FE.idade.min, FE.idade.max)
          ]
        ) [, .(idade.minima, intervalo.idades, pt)]
        # vector.estado.passo.3.1 <- vector.estado.passo.2 [
        #   nrow (faixas.etarias [FE.idade.min <= idade.minima & idade.minima + intervalo.idades <= FE.idade.max]) > 0
        # ]
        cat (" ...3.2\n")
        vector.estado.passo.3.2 <- (faixas.etarias [
          ,
          vector.estado.passo.2 [
            FE.idade.min <= idade.minima & idade.minima < FE.idade.max & FE.idade.max < idade.minima + intervalo.idades,
            .(
              idade.minima,
              intervalo.idades = FE.idade.max - idade.minima,
              pt = pt * (FE.idade.max - idade.minima) / intervalo.idades
            )
          ],
          by = .(FE.id, FE.idade.min, FE.idade.max)
        ]) [, .(idade.minima, intervalo.idades, pt)]
        cat (" ...3.3\n")
        vector.estado.passo.3.3 <- (
          faixas.etarias [
            ,
            vector.estado.passo.2 [
              idade.minima <= FE.idade.min & FE.idade.min < idade.minima + intervalo.idades & idade.minima + intervalo.idades <= FE.idade.max,
              .(
                idade.minima = FE.idade.min,
                intervalo.idades = idade.minima + intervalo.idades - FE.idade.min,
                pt = pt * (idade.minima + intervalo.idades - FE.idade.min) / intervalo.idades
              )
            ],
            by = .(FE.id, FE.idade.min, FE.idade.max)
          ]
        ) [, .(idade.minima, intervalo.idades, pt)]
        lpt <- list (vector.estado.passo.3.1, vector.estado.passo.3.2, vector.estado.passo.3.3)
        print (lpt)
        vector.estado.passo.3 <- rbindlist (lpt)
        cat ("Passo 3\n")
        print (vector.estado.passo.3)
        return (iteração.simulação (
          acumulador = novo.acumulador,
          vector.estado = vector.estado.passo.3,
          tempo = tempo + delta,
          n = n - 1
        ))
      }
    }
    cat (sprintf ("    %s\n", nome.sexo))
    vector.partida <- dados.reais [
      tempo == tempo.partida &
        administração == nome.administração &
        ministério_secretaria == nome.ministerio.secretaria &
        sexo == nome.sexo,
      .(
        idade.minima = faixas.etarias [FE.id == faixa_etária, FE.idade.min],
        intervalo.idades = faixas.etarias [FE.id == faixa_etária, FE.idade.max - FE.idade.min],
        pt = `postos de trabalho`
      )
    ]
    print (vector.partida)
    print (sub.parametros)
    resultado <- vector.corridas [
      ,
      iteração.simulação (data.table (), vector.partida, tempo.partida, duracao)
      ,
      by = .(corrida)
    ]
    print (resultado)
    a <- b
    return (resultado)
  }
  vector.corridas <- data.table (
    corrida = c(1:numero.corridas)
  )
  if (is.na(tempo.partida)) {
    tempo.partida <- dados.reais [, max(tempo)]
  }
  parametros.modelo <- fread (
    file = sprintf (
      "parametros-modelo-5-simulação-postos-trabalho_delta=%s.csv",
      str_remove (
        sprintf ("%.1f", delta),
        "[.]?0+$"
      )
    )
  )
  print (parametros.modelo)
  return (parametros.modelo [
    ,
    ciclo.administração (administração, .SD),
    by = .(administração)
  ])
}
