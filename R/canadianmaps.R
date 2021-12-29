
globalVariables(c("aes", '%+replace%', 'element_blank', 'unit', 'element_line',
                  'element_text', 'element_blank'))

#' Provincial data set with geometry
#'
#' A dataset containing the Canadian province geometry shapes
#'
#' @format A data frame with 13 rows and 12 variables:
#' \describe{
#'   \item{PRUID}{ID column for each province}
#'   \item{region}{national region}
#'   \item{PT}{province or territory}
#'   ...
#' }
#' @source \url{https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm}
"PROV"

#' Regional data set with geometry
#'
#' A dataset containing the Canadian province geometry shapes and set regions
#'
#' @format A data frame with 13 rows and 12 variables:
#' \describe{
#'   \item{PRUID}{ID column for each province}
#'   \item{region}{national region}
#'   \item{PT}{province or territory}
#'   ...
#' }
#' @source \url{https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm}
"REG"



#' Map theme
#'
#' Blank ggplot theme for mapping.
#'
#' @return A blank theme for ggplot
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

#' Wallis graph theme
#'
#' Custom ggplot theme for graphing.
#'
#' @return A theme for ggplot
#' @export
theme_wallis <- function() {

  ggplot2::theme(axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 13),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color="gray", linetype="dashed"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        aspect.ratio = 1/3,
        legend.key = element_blank())
}


#' Colour Palettes Fill
#'
#' Create a custom number of colours to use for graphing or mapping based on preset colour palettes.
#'
#' @param palette colour palette name
#' @param num number of colours to create
#' @param na.value a colour value for NA, defaults to light grey
#' @return a ggplot colour palette
#' @export
scale_fill_map <- function(palette, num, na.value = "grey90", rev=FALSE) { # 3


  mycolours <- dplyr::case_when(palette == "Bright" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Set2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Pastel" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Pastel2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Kelly" ~ grDevices::colorRampPalette(c("#B0DFE5",RColorBrewer::brewer.pal(9, "Set3")[c(1, 11, 7, 12, 6, 4, 8, 10, 3)], "#FADADD"))(num),
                   palette == "Teal" ~ grDevices::colorRampPalette(c("#D1F0ED", "#3CBBB1", "#2D8B83", "#0F2E2C"))(num),
                   palette == "Beach" ~ grDevices::colorRampPalette(c("#FCB9AA", "#FFDBCC", "#ECEAE4", "#A2E1DB", "#55CBCD"))(num),
                   palette == "Summer" ~ grDevices::colorRampPalette(c("#53CFDA", "#EFF2E6", "#FF7994", "#FFC900", "#FFED00"))(num),
                   palette == "Mauve" ~ grDevices::colorRampPalette(c("#F7DFD3", "#E2C3C8", "#AFAFC7", "#5F7DAF"))(num),
                   palette == "Anada" ~ grDevices::colorRampPalette(c("#D0E0F7","#BCD4F4","#943C44", "#E45C64", "#fdbc00", "#e1ad01"))(num),
                   palette == "Viv" ~ grDevices::colorRampPalette(c("#B4B4D7", "#525183", "#ACBCD3"))(num),
                   palette == "Starbs" ~ grDevices::colorRampPalette(c("#0C5C41", "#5A9C84", "#D3E4DF",  "#3EC8A9"))(num),
                   palette == "Purples" ~ grDevices::colorRampPalette(c("#E7E7EF", "#646CAC", "#393F93", "#0E0F40"))(num),
                   palette == "PHAC" ~ grDevices::colorRampPalette(c("#F4CDD0","#D33F49","#B72A33", "#851E25"))(num),
                   palette == "CNISP" ~ grDevices::colorRampPalette(c("#B1B2D1", "#101573", "#DBE0E6", "#26374A"))(num),
                   palette == "Jess" ~ grDevices::colorRampPalette(c("#FFC55E", "#D49D43", "#556F60", "#3E543F"))(num),
                   palette == "HAI" ~ grDevices::colorRampPalette(c("#BF2431","#563c98",  "#154360", "#48C9B0"))(num)
                   )

  if(rev == TRUE) {
    ggplot2::scale_fill_manual(values = rev(mycolours), na.value = na.value)

  } else{
    ggplot2::scale_fill_manual(values = mycolours, na.value = na.value)
  }

}

#' Colour Palettes Line Color
#'
#' Create a custom number of colours to use for graphing or mapping based on preset colour palettes.
#'
#' @param palette colour palette name
#' @param num number of colours to create
#' @param na.value a colour value for NA, defaults to light grey
#' @return a ggplot colour palette
#' @export
scale_color_map <- function(palette, num, na.value = "grey90", rev=FALSE) { # 3


  mycolours <- dplyr::case_when(palette == "Bright" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Set2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Pastel" ~ grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, "Pastel2")[c(3, 1, 5, 6, 2, 4)])(num),
                   palette == "Kelly" ~ grDevices::colorRampPalette(c("#B0DFE5",RColorBrewer::brewer.pal(9, "Set3")[c(1, 11, 7, 12, 6, 4, 8, 10, 3)], "#FADADD"))(num),
                   palette == "Teal" ~ grDevices::colorRampPalette(c("#D1F0ED", "#3CBBB1", "#2D8B83", "#0F2E2C"))(num),
                   palette == "Beach" ~ grDevices::colorRampPalette(c("#FCB9AA", "#FFDBCC", "#ECEAE4", "#A2E1DB", "#55CBCD"))(num),
                   palette == "Summer" ~ grDevices::colorRampPalette(c("#53CFDA", "#EFF2E6", "#FF7994", "#FFC900", "#FFED00"))(num),
                   palette == "Mauve" ~ grDevices::colorRampPalette(c("#F7DFD3", "#E2C3C8", "#AFAFC7", "#5F7DAF"))(num),
                   palette == "Anada" ~ grDevices::colorRampPalette(c("#D0E0F7","#BCD4F4","#943C44", "#E45C64", "#fdbc00", "#e1ad01"))(num),
                   palette == "Viv" ~ grDevices::colorRampPalette(c("#B4B4D7", "#525183", "#ACBCD3"))(num),
                   palette == "Starbs" ~ grDevices::colorRampPalette(c("#0C5C41", "#5A9C84", "#D3E4DF",  "#3EC8A9"))(num),
                   palette == "Purples" ~ grDevices::colorRampPalette(c("#E7E7EF", "#646CAC", "#393F93", "#0E0F40"))(num),
                   palette == "PHAC" ~ grDevices::colorRampPalette(c("#F4CDD0","#D33F49","#B72A33", "#851E25"))(num),
                   palette == "CNISP" ~ grDevices::colorRampPalette(c("#B1B2D1", "#101573", "#DBE0E6", "#26374A"))(num),
                   palette == "Jess" ~ grDevices::colorRampPalette(c("#FFC55E", "#D49D43", "#556F60", "#3E543F"))(num),
                   palette == "HAI" ~ grDevices::colorRampPalette(c("#BF2431","#563c98",  "#154360", "#48C9B0"))(num)
                   )

  if(rev == TRUE) {
    ggplot2::scale_color_manual(values = rev(mycolours), na.value = na.value)

  } else{
    ggplot2::scale_color_manual(values = mycolours, na.value = na.value)
  }

}


#' Transforming map coordinates
#'
#' Converts your longitude and latitude coordinates to match the maps to properly overlay in ggplot.
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


#' ggplot map coordinate system
#'
#' Used to visualise simple feature (sf) objects. Required to plot geom objects.
#'
#' @return a coordinate system for ggplot2
#' @export
crs_coord <- function() { # 3
  ggplot2::coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

}


#' Mapping provincial data
#'
#' Maps provincial data using statscan province shapefile.
#'
#' @param data a dataset with geometry variable
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Provincial map.
#' @export
geom_prov <- function(data = PROV, fill = "PT", colour = NA, size = 0.1) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}

#' Mapping regional data
#'
#' Maps regional data using statscan province shapefile.
#'
#' @param data a dataset with geometry variable
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return Regional map.
#' @export
geom_reg <- function(data = REG, fill = "region", colour = NA, size = 0.1) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}


#' Mapping FSA data
#'
#' Maps FSA data using statscan FSA shapefile.
#'
#' @param data a dataset with geometry variable
#' @param fill the colour fill variable
#' @param colour outline colour, default is NA
#' @param size size of outline
#' @return FSA map.
#' @export
geom_fsa <- function(data = FSA, fill = "PRNAME", colour = "white", size = 0.2) {
  ggplot2::geom_sf(data = data, aes(fill = data[[fill]]), color = colour, size = size)
}


#' Province text
#'
#' Adds text labels in the center of each province.
#'
#' @param data a dataset with geometry variable
#' @param label the label variable
#' @param colour text colour
#' @param size text size
#' @return Provincial map labels
#' @export
text_prov <- function(data = PROV, label = "PT", colour = "grey20", size = 3) {
  ggrepel::geom_text_repel(data = data, aes(X, Y, label = data[[label]]), size = size, color = colour, point.size = NA,  min.segment.length = 0.1, box.padding = 0.4)

}

#' Province labels
#'
#' Adds text labels in the center of each province with light grey bubble.
#'
#' @param data a dataset with geometry variable
#' @param label the label variable
#' @param colour label colour
#' @param size label size
#' @param outline outline color
#' @param alpha transparency
#' @return Provincial map labels
#' @export
label_prov <- function(data = PROV, label = "PT", colour = "grey20", size = 3, outline = NA, alpha=0.7) {
  ggrepel::geom_label_repel(data = data, aes(X, Y, label = data[[label]]), size = size, color = colour, point.size = NA,  min.segment.length = 0.1, box.padding = 0.2, alpha=alpha, label.r=1, label.padding=0.7, label.size = outline)

}


#' Regional labels
#'
#' Adds text labels in the center of each region with light grey bubble.
#'
#' @param data a dataset with geometry variable
#' @param label the label variable
#' @param colour label colour
#' @param size label size
#' @return Regional map labels.
#' @export
label_reg <- function(data = REG, label = "region", colour = "grey20", size = 4) {
  ggplot2::geom_text(data = data, aes(X, Y, label = data[[label]]), size = size, color = colour)

}
