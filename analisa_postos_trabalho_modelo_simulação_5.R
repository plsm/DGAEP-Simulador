library (package = "data.table")
library (package = "stringr")

dados <- fread (
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
          `postos de trabalho`
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
          - delta / duracao.faixa.etaria (
            el.idade = a.faixa.etária))
        )
      if (a.faixa.etária > FAIXA.ETARIA.MIN) {
        valor <- (
          valor
          - calcula.postos.trabalho (
              o.tempo = o.tempo - delta,
              a.faixa.etária = a.faixa.etária - 1
          )
          * delta
          / duracao.faixa.etaria (
            el.idade = a.faixa.etária - 1)
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
        o.postos.de.trabalho.atual.faixa.corrente = `postos de trabalho`
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
    file = sprintf (
      "parametros-modelo-5-simulação-postos-trabalho_delta=%s.csv",
      str_remove (
        sprintf ("%.1f", delta),
        "[.]?0+$"
      )
    ),
    quote = TRUE
  )
  return (tabela.parametros)
}
