library (package = "data.table")
library (package = "stringr")

pasta.dados <- function (
    ficheiro
) {
  return (file.path ("dados", ficheiro))
}

dados <- fread (
  file = pasta.dados ("postos-trabalho-por-ministerios-secretarias-regionais.csv")
)

faixas.etarias <- fread (
  file = pasta.dados ("faixas-etarias.csv")
)

FAIXA.ETARIA.MIN <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MAX <- faixas.etarias [, max (FE.id)]

duração.faixa.etaria <- function (el.idade, el.sexo) {
  return (faixas.etarias [
    FE.id == el.idade & (is.na (FE.sexo) | FE.sexo == el.sexo),
    FE.idade.max - FE.idade.min
  ])
}

calcula.parametros.modelo.simulação <- function (
    delta = 0.5
) {
  ciclo.administração <- function (
    nome.administração,
    sub.dados
  ) {
    cat (sprintf ("%s\n", nome.administração))
    return (sub.dados [
      ,
      ciclo.ministerio.secretaria (ministério_secretaria, .SD),
      by = .(ministério_secretaria)
    ])
  }
  ciclo.ministerio.secretaria <- function (
    nome.ministerio.secretaria,
    sub.dados
  ) {
    cat (sprintf ("  %s\n", nome.ministerio.secretaria))
    return (sub.dados [
      ,
      ciclo.sexo (sexo, .SD),
      by = .(sexo)
    ])
  }
  ciclo.sexo <- function (
    nome.sexo,
    sub.dados
  ) {
    cat (sprintf ("    %s\n", nome.sexo))
    calcula.postos.trabalho <- function (
      o.tempo,
      a.faixa.etária
    ) {
      return (
        sub.dados [
          tempo == o.tempo &
            faixa_etária == a.faixa.etária
          ,
          postos_de_trabalho
        ]
      )
    }
    calcula.delta <- function (
      o.tempo,
      a.faixa.etária,
      o.postos.de.trabalho.atual.faixa.corrente
    ) {
      valor <- (
        o.postos.de.trabalho.atual.faixa.corrente
        - calcula.postos.trabalho (
            o.tempo = o.tempo - delta,
            a.faixa.etária = a.faixa.etária
        )
        * (
          1
          - delta / duração.faixa.etaria (
            el.idade = a.faixa.etária,
            el.sexo = nome.sexo))
        )
      if (a.faixa.etária > FAIXA.ETARIA.MIN) {
        valor <- (
          valor
          - calcula.postos.trabalho (
              o.tempo = o.tempo - delta,
              a.faixa.etária = a.faixa.etária - 1
          )
          * delta
          / duração.faixa.etaria (
            el.idade = a.faixa.etária - 1,
            el.sexo = nome.sexo)
        )
      }
      resultado <- data.table (
        delta = valor
      )
      return (resultado)
    }
    # main ####
    tabela.deltas <- sub.dados [
      ,
      calcula.delta (
        o.tempo = tempo,
        a.faixa.etária = faixa_etária,
        o.postos.de.trabalho.atual.faixa.corrente = postos_de_trabalho
      ),
      by = .(
        tempo,
        faixa_etária
      )
    ]
    resultado <- tabela.deltas [
      ,
      .(
        media.delta = mean (delta),
        desvio.padrão.delta = sd (delta)
      ),
      by = .(
        faixa_etária
      )
    ]
    return (resultado)
  }
  # main ####
  tabela.parametros <- dados [
    ,
    ciclo.administração (administração, .SD),
    by = .(administração)
  ]
  fwrite (
    x = tabela.parametros,
    file = pasta.dados (
      sprintf (
        "parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=%s.csv",
        str_remove (
          sprintf ("%.1f", delta),
          "[.]?0+$"
        )
      )
    ),
    quote = TRUE
  )
  return (tabela.parametros)
}
