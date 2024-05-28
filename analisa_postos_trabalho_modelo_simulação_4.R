library (package = "data.table")

dados <- fread (
  file = "posto-trabalho-por-subsetor-ministerios-secretarias.csv"
)

faixas.etarias <- fread (
  file = "faixas-etarias.csv"
)

FAIXA.ETARIA.MIN <- faixas.etarias [, min (FE.id)]
FAIXA.ETARIA.MAX <- faixas.etarias [, max (FE.id)]

duracao.faixa.etaria <- function (el.idade) {
  return (faixas.etarias [
    FE.id == el.idade,
    FE.idade.max - FE.idade.min + 1
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
    calcula.delta <- function (
      o.tempo,
      a.faixa.etária,
      o.postos.de.trabalho.atual.faixa.corrente
    ) {
      o.postos.de.trabalho.anterior.faixa.corrent <- sub.dados [
        tempo == o.tempo - delta &
          faixa_etária == a.faixa.etária
        ,
        `postos de trabalho`
      ]
      valor <- o.postos.de.trabalho.atual.faixa.corrente - o.postos.de.trabalho.anterior.faixa.corrent * (1 - delta / duracao.faixa.etaria (a.faixa.etária))
      if (a.faixa.etária > FAIXA.ETARIA.MIN) {
        o.postos.de.trabalho.anterior.faixa.anterior <- sub.dados [
          tempo == o.tempo - delta &
            faixa_etária == a.faixa.etária - 1
          ,
          `postos de trabalho`
        ]
        valor <- valor - o.postos.de.trabalho.anterior.faixa.anterior * delta / duracao.faixa.etaria(el.idade = a.faixa.etária - 1)
      }
      resultado <- data.table (
        delta = valor
      )
      return (resultado)
    }
    return (sub.dados [
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
    ])
  }
  tabela.parametros <- dados [
    ,
    ciclo.administração (administração, .SD),
    by = .(administração)
  ]
  fwrite (
    x = tabela.parametros,
    file = sprintf (
      "parametros-modelo-4-simulação-postos-trabalho_delta=%f.csv",
      delta
    ),
    quote = TRUE
  )
  return (tabela.parametros)
}