library (package = "data.table")
library (package = "stringr")
library (package = "ggplot2")

set.seed (42)

pasta.dados <- function (
    ficheiro
) {
  return (file.path ("dados", ficheiro))
}

dados.reais <- fread (
  file = pasta.dados ("postos-trabalho-por-ministerios-secretarias-regionais.csv")
)

faixas.etarias <- fread (
  file = pasta.dados ("faixas-etarias.csv")
)

fes.homem <- faixas.etarias [
  is.na (FE.sexo) | FE.sexo == "H"
]

fes.mulher  <- faixas.etarias [
  is.na (FE.sexo) | FE.sexo == "M"
]


FAIXA.ETARIA.MIN.ID <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MIN.IDADE <- faixas.etarias [, min (FE.idade.min)]

duracao.faixa.etaria <- function (el.idade, el.sexo) {
  return (faixas.etarias [
    FE.id == el.idade & (is.na (FE.sexo) | FE.sexo == el.sexo),
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
    result <- sub.parametros [
      ,
      ciclo.sexo (nome.administração, nome.ministerio.secretaria, sexo, .SD),
      by = .(sexo)
    ]
    a <- b
    
    return (result)
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
        # novas contratações, saídas, mortes, outras variações do número de postos de trabalho
        variações <- sub.parametros [
          ,
          .(
            FE.idade.min = fes.certo [FE.id == faixa_etária, FE.idade.min],
            FE.idade.max = fes.certo [FE.id == faixa_etária, FE.idade.max],
            variação = rnorm (n = 1, mean = media.delta, sd = desvio.padrão.delta)
          ),
          by = .(faixa_etária)
        ]
        vector.estado.passo.1.1 <- variações [
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
        ] [
          ,
          .(idade.minima, intervalo.idades, pt)
        ]
        # novas contratações no intervalo de idades com a idade mais baixa
        vector.estado.passo.1.2 <- data.table (
          idade.minima = FAIXA.ETARIA.MIN.IDADE,
          intervalo.idades = delta,
          pt = max (0, variações [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, variação / fes.certo [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, FE.idade.max - FE.idade.min]])
        )
        # envelhecimento
        vector.estado.passo.2 <- vector.estado.passo.1.1 [
          ,
          .(
            idade.minima = idade.minima + delta,
            intervalo.idades = intervalo.idades,
            pt = pt
          )
        ]
        # processar os triplos (idade mínima, intervalo de idades, postos de trabalho) de modo a que as idades fiquem entre as faixas etárias existentes
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
        vector.estado.passo.3.2 <- fes.certo [
          ,
          vector.estado.passo.2 [
            FE.idade.min <= idade.minima &
              idade.minima < FE.idade.max &
              FE.idade.max <= idade.minima + intervalo.idades,
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
        vector.estado.passo.3.3 <- fes.certo [
          ,
          vector.estado.passo.2 [
            idade.minima <= FE.idade.min &
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
    # main ####
    cat (sprintf ("    %s\n", nome.sexo))
    if (nome.sexo == "H")
      fes.certo <- fes.homem
    else
      fes.certo <- fes.mulher
    print (fes.certo)
    vector.partida <- dados.reais [
      tempo == tempo.partida &
        administração == nome.administração &
        ministério_secretaria == nome.ministerio.secretaria &
        sexo == nome.sexo,
      .(
        idade.minima = fes.certo [FE.id == faixa_etária, FE.idade.min],
        intervalo.idades = fes.certo [FE.id == faixa_etária, FE.idade.max - FE.idade.min],
        pt = postos_de_trabalho
      )
    ]
    cat ("Vector partida\n")
    print (vector.partida)
    cat ("Sub parâmetros\n")
    print (sub.parametros)
    resultado <- vector.corridas [
      ,
      iteração.simulação (data.table (), vector.partida, tempo.partida, duracao)
      ,
      by = .(corrida)
    ]
    print (resultado)
    cat ("*****************************")
    
    
    el.plot <- ggplot (
    ) + geom_point (
      data = resultado,
      mapping = aes (
        x = tempo,
        y = postos_de_trabalho,
        colour = as.factor (faixa_etária)
      )
    ) + facet_grid (
      cols = vars (faixa_etária)
    ) + scale_colour_discrete (
      name = "idade",
      limits = factor (c (0, 1, 2, 3, 4, 5)),
      labels = c ("24 ou menos", "25 a 34", "35 a 44", "45 a 54", "55 a 65", "65 ou mais")
    ) + scale_x_continuous (
      name = "tempo"
    ) + theme_bw ()
    ggsave (
      filename = "teste-gráfico.png",
      plot = el.plot,
      device = "png",
      units = "px",
      width = 1800,
      height = 800,
      dpi = 72
    )
    return (resultado)
  }
  vector.corridas <- data.table (
    corrida = c(1:numero.corridas)
  )
  if (is.na(tempo.partida)) {
    tempo.partida <- dados.reais [, max(tempo)]
  }
  parametros.modelo <- fread (
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
  print (parametros.modelo)
  return (parametros.modelo [
    ,
    ciclo.administração (administração, .SD),
    by = .(administração)
  ])
}

corre.simulação.caso.particular <- function (
  a.administração,
  o.ministerio.secretaria,
  o.sexo,
  numero.corridas = 30,
  tempo.partida = NA,
  duracao = 10,
  delta = 0.5,
  debug = 2
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
      if (n - debug > 0) {
        cat ("* ** estado ** *\n")
        print (vector.estado)
      }
      
      # novas contratações, saídas, mortes, outras variações do número de postos de trabalho
      variações <- sub.parametros [
        ,
        .(
          FE.idade.min = fes.certo [FE.id == faixa_etária, FE.idade.min],
          FE.idade.max = fes.certo [FE.id == faixa_etária, FE.idade.max],
          variação = rnorm (n = 1, mean = media.delta, sd = desvio.padrão.delta)
        ),
        by = .(faixa_etária)
      ]
      if (n - debug > 0) {
        cat ("* ** variações ** *\n")
        print (variações)
      }
      
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
      if (n - debug > 0) {
      cat (" * ** passo 1.1 ** *\n")
      print (vector.estado.passo.1.1) }
      # novas contratações no intervalo de idades com a idade mais baixa
      vector.estado.passo.1.2 <- data.table (
        idade.minima = FAIXA.ETARIA.MIN.IDADE,
        intervalo.idades = delta,
        pt = max (0, variações [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, variação / fes.certo [FE.idade.min == FAIXA.ETARIA.MIN.IDADE, FE.idade.max - FE.idade.min]])
      )
      if (n - debug > 0) {
        cat (" * ** passo 1.2 ** *\n")
      print (vector.estado.passo.1.2) }
      # envelhecimento
      vector.estado.passo.2 <- vector.estado.passo.1.1 [
        ,
        .(
          idade.minima = idade.minima + delta,
          intervalo.idades = intervalo.idades,
          pt = pt
        )
      ]
      if (n - debug > 0 || TRUE) {
        cat (" * ** passo 2 ** *\n")
        print (vector.estado.passo.2)
      }
      # processar os triplos (idade mínima, intervalo de idades, postos de trabalho) de modo a que as idades fiquem entre as faixas etárias existentes
      ## triplos que estão dentro do intervalo de uma faixa etária
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
      if (n - debug > 0) {
        cat (" * ** passo 3.1 ** *\n")
      print (vector.estado.passo.3.1) }
      ## triplos que estão acima de uma faixa etária
      vector.estado.passo.3.2 <- fes.certo [
        ,
        vector.estado.passo.2 [
          FE.idade.min <= idade.minima &
            idade.minima < FE.idade.max &
            FE.idade.max <= idade.minima + intervalo.idades,
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
      if (n - debug > 0) {
        cat (" * ** passo 3.2 ** *\n")
      print (vector.estado.passo.3.2) }
      ## triplos que estão abaixo de uma faixa etária
      vector.estado.passo.3.3 <- fes.certo [
        ,
        vector.estado.passo.2 [
          idade.minima <= FE.idade.min &
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
      if (n - debug > 0) {
        cat (" * ** passo 3.3 ** *\n")
      print (vector.estado.passo.3.3) }
      lpt <- list (
        vector.estado.passo.1.2,
        vector.estado.passo.3.1,
        vector.estado.passo.3.2,
        vector.estado.passo.3.3
      )
      vector.estado.passo.4 <- rbindlist (lpt)
      if (n - debug > 0) {
        cat (" * ** passo 4 ** *\n")
        print (vector.estado.passo.4) }
      return (iteração.simulação (
        acumulador = novo.acumulador,
        vector.estado = vector.estado.passo.4,
        tempo = tempo + delta,
        n = n - 1
      ))
    }
  }
  # main ####
  parametros.modelo <- fread (
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
  sub.parametros <- parametros.modelo [
    administração == a.administração &
      ministério_secretaria == o.ministerio.secretaria &
      sexo == o.sexo
    ]
  cat ("* sub parâmetros *\n")
  print (sub.parametros)
  if (is.na (tempo.partida)) {
    tempo.partida <- dados.reais [
      administração == a.administração &
        ministério_secretaria == o.ministerio.secretaria &
        sexo == o.sexo,
      max(tempo)
    ]
  }
  vector.corridas <- data.table (
    corrida = c(1:numero.corridas)
  )
  if (o.sexo == "H")
    fes.certo <- fes.homem
  else
    fes.certo <- fes.mulher
  print (fes.certo)
  vector.partida <- dados.reais [
    tempo == tempo.partida &
      administração == a.administração &
      ministério_secretaria == o.ministerio.secretaria &
      sexo == o.sexo,
    .(
      idade.minima = fes.certo [FE.id == faixa_etária, FE.idade.min],
      intervalo.idades = fes.certo [FE.id == faixa_etária, FE.idade.max - FE.idade.min],
      pt = postos_de_trabalho
    )
  ]
  cat ("Vector partida\n")
  print (vector.partida)
  cat ("Sub parâmetros\n")
  print (sub.parametros)
  resultado <- vector.corridas [
    ,
    iteração.simulação (data.table (), vector.partida, tempo.partida, duracao)
    ,
    by = .(corrida)
  ]
  print (resultado)
  cat ("*****************************\n")
  
  
  el.plot <- ggplot (
  ) + geom_point (
    data = resultado,
    mapping = aes (
      x = tempo,
      y = postos_de_trabalho,
      colour = as.factor (faixa_etária)
    )
  ) + facet_grid (
    cols = vars (faixa_etária)
  ) + scale_colour_discrete (
    name = "idade",
    limits = factor (c (0, 1, 2, 3, 4, 5)),
    labels = c ("24 ou menos", "25 a 34", "35 a 44", "45 a 54", "55 a 65", "65 ou mais")
  ) + scale_x_continuous (
    name = "tempo"
  ) + theme_bw ()
  ggsave (
    filename = "teste-gráfico.png",
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1800,
    height = 800,
    dpi = 72
  )
  return (resultado)
}
