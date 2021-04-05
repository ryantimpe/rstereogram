#' Convert an autostereogram raster to a ggplot raster
#'
#' Convert an autostereogram raster to a ggplot raster
#'
#' @param magiceye_raster Output from \code{*_to_magiceye()}
#' @return A ggplot version of
#' @export
#' @examples
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_r.png",
#'                           package = "rstereogram", mustWork = TRUE)
#'  demo_image <- png::readPNG(demo_file)
#'
#'  demo_image %>%
#'     image_to_magiceye() %>%
#'     ggmagiceye()

ggmagiceye <- function(magiceye_raster){

  magiceye_raster %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::mutate(y = dplyr::n() - dplyr::row_number() + 1) %>%
    tidyr::pivot_longer(-y, names_to = "x", values_to = "value") %>%
    dplyr::mutate(x = as.numeric(stringr::str_remove(x, "V"))) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x, y) +
    ggplot2::geom_raster(ggplot2::aes(fill = value)) +
    ggplot2::scale_fill_identity()+
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

}
