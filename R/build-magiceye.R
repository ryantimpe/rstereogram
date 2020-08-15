#' Convert a 2D image to a 3D autostereogram
#'
#' Convert a 2D greyscale image to a 3D autostereogram. Set dot colors used in final image.
#'
#' @param image Matrix image, usually from \code{png::readPNG()} or \code{jpeg::readJPEG()}.
#' @param use_colors Array of at least 2 color hex codes. Colors should be visually distinct and avoid using too many.
#' @return A raster matrix for \code{plot()}.
#' @export
#' @examples
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg",
#'                           package = "rstereogram", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#'
#'  mosaic <- demo_image %>%
#'     image_to_mosaic()
#'

image_to_magiceye <- function(image, use_colors = c("#00436b", "#ffed89")){

  #image and object size
  # Columns are the 2nd coordinate, which is x
  maxX = dim(image)[2]
  maxY = dim(image)[1]

  #Process image
  img2 <- 1-rowSums(image[,,-4], dims = 2)/3

  #Algorithm
  DPI = 72 #DPI of output device... need to verify
  E = round(2.5 * DPI) # Eye separation is assumed to be 2.5 inches
  mu = 1/3 #Depth of field (fraction of viewing distance)

  #Stereo separation corresponding to position Z
  separation <- function(Z){
    round((1-mu*Z)/ (2-mu*Z) * E)
  }
  #Far plane = separation(0); near plane = separation(1)

  #Scan each line independently
  Z <- img2
  for(y in 1:maxY){
    pix <- rep(0, maxX) #Array of pixel colors for this row

    same <- 1:maxX #Array that points to a pixel to the right...
    #... that is constrained to be this color
    # Each pixel is initially linked with itself
    # Lines 23-24 in paper

    for(x in 1:maxX){
      s = separation(Z[y, x])
      #lines 28 and 29. Using mod to offset odd s
      # left = x - (s + round((y%%2)*(s%%2)))/2 #Pixels at left and right...
      left = round(x - s/2) #Pixels at left and right...
      right = left + s #... must be the same...

      #... or must they?
      if(left > 0 && right < maxX){
        visible  = TRUE #First perform hidden-surface removal
        t = 1 #We will check the points (x-t, y) and (x+t, y)
        zt = 0 #Z-coord of ray at these two points

        while(visible && zt < 1){
          zt = Z[y, x] + 2*(2 - mu*Z[y, x]*t/(mu*E))

          visible = (Z[y, x-t] < zt) && (Z[y, x+t] < zt) #false if obscured
          t = t+1
        }
        #Done with hidden-surface removal
        # Paper lines 42-48
        if(visible){
          k = same[left]
          while(k != left && k != right){
            if(k < right){
              left <- k
            } else {
              left <- right
              right <- k
            }
          }
          same[left] <- right
        }
      }
    }

    #Now set the pixels on this scan line
    for(x in maxX:1){
      if(same[x] == x){
        pix[x] <- sample(seq_along(use_colors), 1)
      } else {
        pix[x] <- pix[same[x]]
      }

      Z[y, x] <- pix[x]
    }
  }

  #Convert numbers into colors
  Z2 <- as.character(Z)
  for(ii in seq_along(use_colors)){
    Z2 <- replace(Z2,
                  Z2 == as.character(ii),
                  use_colors[ii]
    )
  }

  # out_list <- list(
  #   magic_eye = Z2,
  #   dims = c(maxY, maxX)
  # )
  #
  # return(out_list)
  return(as.raster(matrix(Z2, nrow = maxY)))

}

png::readPNG("demo_r.png") %>% image_to_magiceye() %>% plot()

png::readPNG("demo_whoops.png") %>%
  image_to_magiceye(use_colors = c("#FFFFFF", "#3399FF", "#FF9911")) %>% plot()
