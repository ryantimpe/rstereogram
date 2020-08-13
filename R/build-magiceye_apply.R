# Algorithm for drawing an autostereogram

DPI = 72 #DPI of output device... need to verify
E = round(2.5 * DPI) # Eye separation is assumed to be 2.5 inches
mu = 1/3 #Depth of field (fraction of viewing distance)

#Stereo separation corresponding to position Z
separation <- function(Z){
  round((1-mu*Z)/ (2-mu*Z) * E)
}

far = separation(0) #Separation of the far plane when Z=0

img = png::readPNG("demo_shapes.png")

#image and object size
# Columns are the 2nd coordinate, which is x
maxX = dim(img)[2]
maxY = dim(img)[1]

plot(as.raster(img))

#Magic Eye function
# LIne 13 in paper

#Object's dept is Z[x][y], which is between 0 and 1
# 0 is on the far plane, 1 is on the near plane
# x and y are coordinates of current point

dim(img)
#Img2 is a matrix of the Z values for this black & white image.
# Values of 1 are the near-field, 0 are the far field
img2 <- 1-rowSums(img, dims = 2)/3



use_colors = c("#9affd0", #Aqua
               #"#ffb5f5", #Pink
               "#5384ff", #Blue
               "#ff9e53"#, #Orange
               #"#ffed89", #Yellow
               #"#de89ff", #Purple
               #"#ff6141"#, #Red/Orange

               #"#ff25ab" #Bright pink

)
#Scan each line independently
Z <- img2

plot(as.raster(Z))

Z2 <- apply(Z, 1, function(yy){

  pix <- rep(0, maxX) #Array of pixel colors for this row

  same <- 1:maxX #Array that points to a pixel to the right...
  #... that is constrained to be this color
  # Each pixel is initially linked with itself
  # Lines 23-24 in paper

  for(x in 1:maxX){
    s = separation(yy[x])
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
        zt = yy[x] + 2*(2 - mu*yy[x]*t/(mu*E))

        visible = (yy[x-t] < zt) && (yy[x+t] < zt) #false is obscured
        t = t+1
      }
      #Done with hidden-surface removal
      #... so record the fact that pixels at
      # RYAN IS CONFUSED HERE
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
  }
  return(pix)
})





# plot(as.raster((Z)))
#
Z3 <- as.character(Z2)
for(ii in seq_along(use_colors)){
  Z3 <- replace(Z3,
         Z3 == as.character(ii),
         use_colors[ii]
  )
}


plot(as.raster(matrix(Z3, nrow = maxY)))
