
#' choose colours
cvi_colours = list(
  cvi_purples = c("#006064", "#0D47A1", "#37474F", "#26C6DA", "#80CBC4", "#90A4AE", "#29B6F6", "#AD1457", "#FFA726", "#B71C1C", "#FF9800", "#E6EE9C"),
  my_favourite_colours = c("#006064", "#80CBC4",  "#AD1457"))


#' create a function that generates an actual colour palette from our simple list of colours.

#'@export
#'@param name the name of the colour palette we want to use,
#'@param nnumeric variable
#'@all_palettes the list of colour palettes we want to extract our choice from
#'@param type whether we want a discrete or continuous colour palette
cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}
