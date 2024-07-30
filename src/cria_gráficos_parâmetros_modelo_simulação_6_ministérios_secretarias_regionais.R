library (package = "data.table")
library (package = "ggplot2")
library (package = "stringr")

pasta.dados <- function (
    ficheiro
) {
  return (file.path ("dados", ficheiro))
}

dados.reais <- fread (
  file = pasta.dados ("postos-trabalho-por-ministerios-secretarias-regionais.csv")
)

x_parâmetros <- dados.reais [, max (tempo)] + 2

faixas.etarias <- fread (
  file = pasta.dados ("faixas-etarias.csv")
)

cria.gráficos.parâmetros.modelo.simulação.com.dados.reais <- function (
    delta = 0.5
) {
  ciclo.administração <- function (
    nome.administração,
    sub.dados.parâmetros
  ) {
    cat (sprintf ("%s\n", nome.administração))
    return (sub.dados.parâmetros [
      ,
      ciclo.ministério.secretaria (nome.administração, ministério_secretaria, .SD),
      by = .(ministério_secretaria)
    ])
  }
  ciclo.ministério.secretaria <- function (
    nome.administração,
    nome.ministerio.secretaria,
    sub.dados.parâmetros
  ) {
    cat (sprintf ("  %s\n", nome.ministerio.secretaria))
    subconjunto.dados.reais <- dados.reais [
      administração == nome.administração & ministério_secretaria == nome.ministerio.secretaria
    ]
    gráfico <- ggplot (
    ) + geom_line (
      data = subconjunto.dados.reais,
      mapping = aes (
        x = tempo,
        y = postos_de_trabalho,
        colour = as.factor (faixa_etária)
      )
    ) + geom_point (
      data = sub.dados.parâmetros,
      mapping = aes (
        x = x_parâmetros,
        y = media.delta
      )
    ) + geom_linerange (
      data = sub.dados.parâmetros,
      mapping = aes (
        x = x_parâmetros,
        ymin = media.delta - desvio.padrão.delta,
        ymax = media.delta + desvio.padrão.delta,
      )
    ) + facet_grid (
      cols = vars (faixa_etária),
      rows = vars (sexo)
    ) + scale_colour_discrete (
      name = "idade",
      limits = factor (c (0, 1, 2, 3, 4, 5)),
      labels = c ("24 ou menos", "25 a 34", "35 a 44", "45 a 54", "55 a 65", "65 ou mais")
    ) + scale_x_continuous (
      name = "tempo"
    ) + theme_bw ()
    ggsave (
      filename = sprintf (
        "parâmetros-modelo-6_ministérios-secretarias-regionais_com-dados-reais_%s_%s_delta=%s.png",
        nome.administração,
        nome.ministerio.secretaria,
        delta.str
      ),
      device = "png",
      units = "px",
      width = 1800,
      height = 800,
      dpi = 92
    )
    return (TRUE)
  }
  # main ####
  delta.str <- str_remove (
    sprintf ("%.1f", delta),
    "[.]?0+$"
  )
  dados.parametros <- fread (
    file = pasta.dados (
      sprintf (
        "parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=%s.csv",
        delta.str
      )
    )
  )
  dados.parametros [
    ,
    ciclo.administração (
      nome.administração = administração,
      sub.dados = .SD
    ),
    by = .(administração)
  ]
  return (TRUE)
}

cria.gráficos.parâmetros.modelo.simulação.sem.dados.reais <- function (
    delta = 0.5
) {
  ciclo.administração <- function (
    nome.administração,
    sub.dados.parâmetros
  ) {
    cat (sprintf ("%s\n", nome.administração))
    return (sub.dados.parâmetros [
      ,
      ciclo.ministério.secretaria (nome.administração, ministério_secretaria, .SD),
      by = .(ministério_secretaria)
    ])
  }
  ciclo.ministério.secretaria <- function (
    nome.administração,
    nome.ministerio.secretaria,
    sub.dados.parâmetros
  ) {
    cat (sprintf ("  %s\n", nome.ministerio.secretaria))
    # print (sub.dados.parâmetros)
    gráfico <- ggplot (
      data = sub.dados.parâmetros,
    ) + geom_point (
      mapping = aes (
        x = faixa_etária,
        colour = sexo,
        y = media.delta
      )
    ) + geom_linerange (
      mapping = aes (
        x = faixa_etária,
        colour = sexo,
        ymin = media.delta - desvio.padrão.delta,
        ymax = media.delta + desvio.padrão.delta
      )
    ) + scale_x_continuous (
      name = "faixa etária",
      breaks = c (0, 1, 2, 3, 4, 5),
      labels = c ("24 ou menos", "25 a 34", "35 a 44", "45 a 54", "55 a 65", "65 ou mais")
    ) + scale_y_continuous (
      name = "variação postos de trabalho"
    ) + facet_grid (
      rows = vars (sexo)
    ) + labs (
      title = nome.administração,
      subtitle = nome.ministerio.secretaria
    ) + theme_bw ()
    ggsave (
      filename = sprintf (
        "parâmetros-modelo-6_ministérios-secretarias-regionais_sem-dados-reais_%s_%s_delta=%s.png",
        nome.administração,
        nome.ministerio.secretaria,
        delta.str
      ),
      device = "png",
      units = "px",
      width = 600,
      height = 500,
      dpi = 92
    )
    return (TRUE)
  }
  # main ####
  delta.str <- str_remove (
    sprintf ("%.1f", delta),
    "[.]?0+$"
  )
  dados.parametros <- fread (
    file = pasta.dados (
      sprintf (
        "parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=%s.csv",
        delta.str
      )
    )
  )
  dados.parametros [
    ,
    ciclo.administração (
      nome.administração = administração,
      sub.dados = .SD
    ),
    by = .(administração)
  ]
  return (TRUE)
}
