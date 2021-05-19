
#' Map theme
#'
#' This function creates a blank theme for ggplot to map.
#'
#' @return A theme for ggplot
#' @export
theme_map <- function(base_size=9, base_family="") { # 3
  ggplot2::theme_bw(base_size=base_size, base_family=base_family) %+replace%
    ggplot2::theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}


#' Colour Palettes
#'
#' This function creates colour palettes.
#'
#' @param palette colour palette name
#' @param num number of colours to create
#' @param na.val a colour value for NA, defaults to light grey
#' @return a ggplot colour scale
#' @export
scale_fill_map <- function(palette, num, na.value = "grey90") { # 3

  mycolours <- dplyr::case_when(palette == "Bright" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Set2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Pastel" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Pastel2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Kelly" ~ grDevices::colorRampPalette(c("#B0DFE5",RColorBrewer::brewer.pal(9, "Set3")[c(1, 11, 7, 12, 6, 4, 8, 10, 3)], "#FADADD"))(num),
                   palette == "CNISP" ~ grDevices::colorRampPalette(c("#E7E7EF", "#646CAC", "#393F93", "#0E0F40"))(num),
                   palette == "PHAC" ~ grDevices::colorRampPalette(c("#F4CDD0","#ECACB0", "#D33F49","#B72A33", "#851E25"))(num),
                   palette == "Teal" ~ grDevices::colorRampPalette(c("#D1F0ED", "#3CBBB1", "#2D8B83", "#0F2E2C"))(num),
                   palette == "Beach" ~ grDevices::colorRampPalette(c("#FCB9AA", "#FFDBCC", "#ECEAE4", "#A2E1DB", "#55CBCD"))(num),
                   palette == "Summer" ~ grDevices::colorRampPalette(c("#53CFDA", "#EFF2E6", "#FF7994", "#FFC900", "#FFED00"))(num),
                   palette == "Mauve" ~ grDevices::colorRampPalette(c("#F7DFD3", "#E2C3C8", "#AFAFC7", "#5F7DAF"))(num)
                   )

  ggplot2::scale_fill_manual(values = mycolours, na.value = na.value)

}



#' mapping points coordinates
#'
#' This function converts your coordinates to match the mapping to overlay in ggplot.
#'
#' @param data a dataset with long and lat coordinates
#' @param long the longitude variable name
#' @param lat the latitude variable name
#' @return Your coordinates transformed.
#' @export
coord_transform <- function(data, long, lat) { # 3
  sp::coordinates(data) <- c(long, lat)
  sp::proj4string(data) <- sp::CRS("+proj=longlat +datum=WGS84")
  data <- sp::spTransform(data, sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

  data.frame(data)
}

#' ggplot coordinates
#'
#' This function adds coord_sf to ggplot with the proper mapping.
#'
#' @return A coordinate system for ggplot2
#' @export
crs_coord <- function() { # 3
  ggplot2::coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

}


#' mapping provincial shapefile
#'
#' This function maps the provincial shapefile.
#'
#' @param data a dataset with long and lat coordinates
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Provincial map.
#' @export
geom_prov <- function(data = PROV, fill = "PT", colour = NA, size = 0.1) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}

#' mapping regional shapefile
#'
#' This function maps the regional shapefile.
#'
#' @param data a dataset with long and lat coordinates
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Provincial map.
#' @export
geom_reg <- function(data = REG, fill = "region", colour = NA, size = 0.1) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}


#' mapping fsa shapefile
#'
#' This function maps the FSA shapefile.
#'
#' @param data a dataset with long and lat coordinates
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return FSA map.
#' @export
geom_fsa <- function(data = FSA, fill = "PRNAME", colour = "white", size = 0.2) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}

#' adding labels
#'
#' This function adds labels in the center of each province.
#'
#' @param data a dataset with long and lat coordinates
#' @param label the label variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Provincial map labels
#' @export
label_prov <- function(data = PROV, label = "PT", colour = "grey20", size = 3) {
  ggrepel::geom_text_repel(data = data, aes(X, Y, label = data[[label]]), size = size, color = colour, point.size = NA, max.overlaps = Inf, min.segment.length = 0)

}


#' adding regional labels
#'
#' This function adds labels in the center of each region.
#'
#' @param data a dataset with long and lat coordinates
#' @param label the label variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Regional map labels.
#' @export
label_reg <- function(data = REG, label = "region", colour = "grey20", size = 4) {
  ggrepel::geom_text_repel(data = data, aes(X, Y, label = data[[label]]), size = size, color = colour, point.size = NA, max.overlaps = Inf, min.segment.length = 0)

}
