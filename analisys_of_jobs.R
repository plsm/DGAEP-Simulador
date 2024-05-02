library (package = "data.table")
library (package = "ggplot2")

el.data <- fread (file="posto-trabalho-por-admin-cargo-genero-data.csv")

el.data  [
  ,
  `:=` (
    time_label = sprintf ("%d/%02d/%0d", ano, `mês`, dia),
    time_number = ano + (mês - 6) / 12.0
  )
]

analisa.cargo <- function (
    cargo,
    subconjunto.dados
) {
  cat (sprintf ("* ** %s ** *\n", cargo))
  ### cria gráfico evolutivo postos de trabalho ####
  el.plot <- ggplot (
    data = subconjunto.dados
  ) + geom_point (
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_num),
      shape = `género`,
      size = 2
    )
  ) + geom_line (
    data = subconjunto.dados [`género` == "m"],
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_num),
      group = idade_num
    )
  ) + geom_line (
    data = subconjunto.dados [`género` == "f"],
    mapping = aes (
      x = time_number,
      y = `postos de trabalho`,
      colour = factor (idade_num),
      group = idade_num
    )
  ) + facet_wrap (
    facets = vars (`administração`)
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
  )
  ggsave (
    filename = sprintf ("postos-de-trabalho_VS_tempo_%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 72
  )
  ## calcula variações ####
  postos.de.trabalho.de <- function (
    el.time,
    el.admin,
    el.idade,
    el.genero,
    el.cargo
  )
  {
    return (subconjunto.dados [
      time_number == el.time &
        `administração` == el.admin &
        idade_idx == el.idade &
        `género` == el.genero
      , `postos de trabalho`])
  }
  variacao.dados <- subconjunto.dados [
    ,
    .(
      idade,
      delta.postos.trabalho.6.meses =
        `postos de trabalho` -
        postos.de.trabalho.de (time_number - 0.5, `administração`, idade_idx, `género`, cargo),
      delta.postos.trabalho.1.ano =
        `postos de trabalho` -
        postos.de.trabalho.de (time_number - 1, `administração`, idade_idx, `género`, cargo),
      delta.postos.trabalho.2.anos =
        `postos de trabalho` -
        postos.de.trabalho.de (time_number - 2, `administração`, idade_idx, `género`, cargo),
      delta.postos.trabalho.5.anos =
        `postos de trabalho` -
        postos.de.trabalho.de (time_number - 5, `administração`, idade_idx, `género`, cargo)
    ),
    by = .(dia, `mês`, ano, `administração`, idade_idx, `género`)
  ]
  ## cria gráfico variações número de postos de trabalho ####
  
  el.plot <- ggplot (
    data = variacao.dados
  ) + geom_boxplot (
    mapping = aes (
      x = factor (idade_idx),
      y = delta.postos.trabalho.6.meses
    )
  ) + scale_x_discrete (
    name = "idade",
    labels = c (
      "24 ou menos",
      "25 a 34",
      "35 a 44",
      "45 a 54",
      "55 a 65",
      "65 ou mais"
    )
  ) + facet_grid (
    rows = vars (`género`),
    cols = vars (`administração`)
  )
  ggsave (
    filename = sprintf ("delta-postos-trabalho_6-meses_%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 72
  )
  return (variacao.dados)
}

executa.analise <- function ()
{
  variacoes.postos <- el.data [
    ,
    analisa.cargo (cargo, .SD),
    by = .(cargo)
  ]
  fwrite (
    x = variacoes.postos,
    file = "variações-posto-trabalho-por-admin-cargo-genero-idade.csv"
  )
  return (variacoes.postos)
}
