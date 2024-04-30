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

plot.by.cargo <- function (cargo, sub.data)
{
  cat (sprintf ("%s\n", cargo))
  el.plot <- ggplot (
    data = sub.data
  ) + geom_point (
    mapping = aes (
      x = idade_idx,
      y = `postos de trabalho`,
      colour = `género`,
      shape = `administração`
    )
  ) + facet_wrap (
    facets = vars (time_label)
  ) + labs (
    title = cargo
  )
  ggsave (
    filename = sprintf ("postos-trabalho_cargo-%s.png", gsub ("/", "_", cargo)),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 72
  )
  return (TRUE)
}

plot.by.time <- function (day, month, year, sub.data)
{
  cat (sprintf ("%d/%d/%d\n", day, month, year))
  el.plot <- ggplot (
    data = sub.data
  ) + geom_line (
    mapping = aes (
      x = idade,
      y = `postos de trabalho`
    )
  ) + facet_wrap (
    facets = vars (cargo)
  ) + labs (
    title = sprintf ("%d/%d/%d", day, month, year)
  )
  ggsave (
    filename = sprintf ("postos-trabalho_%02d-%02d-%d.png", year, month, day),
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 70
  )
  return (TRUE)
}

create.plots.by.cargo <- function ()
{
  el.data [
    ,
    plot.by.cargo (cargo, .SD),
    by = .(cargo)
  ]
}

create.plots.by.tyme <- function ()
{
el.data [
  ,
  plot.by.time (dia, `mês`, ano, .SD),
  by = .(dia, `mês`, ano)
]
}

postos.de.trabalho.de <- function (
    el.time,
    el.admin,
    el.idade,
    el.genero,
    el.cargo
)
{
  return (el.data [
    time_number == el.time &
      `administração` == el.admin &
      idade_idx == el.idade &
      `género` == el.genero &
      cargo == el.cargo
      , `postos de trabalho`])
}

variacao.postos.trabalho <- function ()
{
  copy.data <- copy (el.data)
  copy.data [
    ,
    delta.postos.trabalho := `postos de trabalho` - postos.de.trabalho.de (time_number - 1, `administração`, idade_idx, `género`, cargo),
    by = .(time_number, `administração`, idade_idx, `género`, cargo)
  ]
}

variacao.postos.trabalho <- function ()
{
  delta.data <- copy.data [!is.na(delta.postos.trabalho)]
  
  el.plot <- ggplot (
    data = delta.data,
  ) + geom_boxplot (
    mapping = aes (
      x = idade,
      y = delta.postos.trabalho
    )
  ) + facet_grid (
    rows = vars (`género`),
    cols = vars (cargo)
  )
  
  ggsave (
    filename = "delta-postos-trabalho.png",
    plot = el.plot,
    device = "png",
    units = "px",
    width = 1850,
    height = 950,
    dpi = 72
  )
}


# setnames (
#   copy.data,
#   old = c("time_number", "postos de trabalho"),
#   new = c("time.number.c", "postos.trabalho.c")
# )
