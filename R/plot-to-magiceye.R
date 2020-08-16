#' Convert a ggplot2 object to a 3D autostereogram
#'
#' Convert a 2ggplot2 object to a 3D autostereogram. Set dot colors used in final image.
#'
#' @param ggobj A ggplot object.
#' @param colors Array of at least 2 color hex codes. Colors should be visually distinct and avoid using too many.
#' @param width Width of output matrix. Larger values take longer to render, but will have more detail.
#' @param height Height of output matrix. Larger values take longer to render, but will have more detail.
#' @return A raster matrix for \code{plot()}.
#' @export
#' @examples
#' df <- data.frame(
#' group = c("Dog", "Cat", "Mouse"),
#' value = c(25, 30, 50)
#' )
#'
#' library(ggplot2)
#' ggplot(df, aes(x="", y=value, fill=group))+
#'   geom_bar(width = 1, stat = "identity") +
#'   coord_polar(theta="y") -> pp
#'
#' pp %>% ggplot_to_magiceye() %>% plot()


ggplot_to_magiceye <- function(ggobj, colors =  c("#00436b", "#ffed89"),
                               height = 6, width = 6){

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("This function requires ggplot2 to be installed.")}

  #Make the plot barebones
  pp <- ggobj +
    ggplot2::theme_void()

  tmp = tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, pp, height = height, width = width, units = "in", dpi = 75)

  tmp %>% png::readPNG() %>% image_to_magiceye(colors = colors)
}
