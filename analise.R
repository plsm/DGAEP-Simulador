library (package = "data.table")
library (package = "ggplot2")

el.data <- fread (file="posto-trabalho-por-admin-cargo-genero-data.csv")

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
    vars = vars (dia, `mês`, ano)
  ) + labs (
    title = cargo
  )
  ggsave (
    filename = sprintf ("postos-trabalho_cargo-%s.png", cargo),
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

el.data [
  ,
  plot.by.cargo (cargo, .SD),
  by = .(cargo)
]

create.plots.by.tyme <- function ()
{
el.data [
  ,
  plot.by.time (dia, `mês`, ano, .SD),
  by = .(dia, `mês`, ano)
]
}


