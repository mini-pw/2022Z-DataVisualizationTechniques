library(plotly)
library(dplyr)

draw_cube <- function(.data, center, side, color){
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = center[1] + c(-1, -1,  1,  1, -1, -1,  1,  1)*side/2,
    y = center[2] + c(-1,  1,  1, -1, -1,  1,  1, -1)*side/2,
    z = center[3] + c(-1, -1, -1, -1,  1,  1,  1,  1)*side/2,
    i = c(7, 0, 2, 3, 4, 4, 6, 1, 4, 0, 7, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 0, 0, 6, 7, 1, 6, 5, 5, 3, 6),
    #intensity = rep(1, length = 8),
    #color = rep(1, length = 8),
    #colors = color
    facecolor = rep(color, 12),
    hoverinfo = 'none',
    lighting = list(ambient = 1,diffuse = 0)
    
    
  )
}
draw_circle <- function(.data, bot, rad, num, color){
  ang <- seq(0, 2*pi, length = num+1)
  ang <- ang[-length(ang)]
  x <- c(bot[1], bot[1] + rad*cos(ang))
  y <- c(bot[2], bot[2] + rad*sin(ang))
  z <- rep(bot[3], num+1)
  i <- rep(0, each = num)
  j <- 1:(num)
  k <- j%%(num) + 1
  
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = x,
    y = y,
    z = z,
    i = i,
    j = j,
    k = k,
    #intensity = rep(1, length = num+2),
    #color = rep(1, length = num+2),
    #colors = color,
    facecolor = rep(color, num),
    hoverinfo = 'none',
    lighting = list(ambient = 0.5)
    
  )
}
draw_cone <- function(.data, bot, rad, hight, num, color){
  ang <- seq(0, 2*pi, length = num+1)
  ang <- ang[-length(ang)]
  x <- c(bot[1], bot[1] + rad*cos(ang), bot[1])
  y <- c(bot[2], bot[2] + rad*sin(ang), bot[2])
  z <- rep(bot[3], num+2)
  z[num+2] = bot[3] + hight
  i <- rep(c(0, num+1), each = num)
  a <- 1:(num)
  b <- a%%(num) + 1
  j <- c(a, b)
  k <- c(b, a)
  
  
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = x,
    y = y,
    z = z,
    i = i,
    j = j,
    k = k,
    #intensity = rep(1, length = num+2),
    #color = rep(1, length = num+2),
    #colors = color,
    facecolor = rep(color, 2*num),
    hoverinfo = 'none',
    lighting = list(ambient = 0.7,diffuse = 0.8),
    lightposition = list(
      x = -1000,
      y = 1, 
      z = 2
    )
    
    
  )
}
draw_square <- function(.data, center, side, color){
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = center[1] + c(1, -1,  -1,  1)*side/2,
    y = center[2] + c(1,  1,  -1, -1)*side/2,
    z = rep(center[3], 4),
    i = c(0, 0),
    j = c(1, 2),
    k = c(2, 3),
    facecolor = rep(color, 2),
    hoverinfo = 'none',
    lighting = list(ambient = 0.5,diffuse = 0)
    
    
  )
}
draw_cuboid <- function(.data, center, side, color){
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = center[1] + c(-1, -1,  1,  1, -1, -1,  1,  1)*side[1]/2,
    y = center[2] + c(-1,  1,  1, -1, -1,  1,  1, -1)*side[2]/2,
    z = center[3] + c(-1, -1, -1, -1,  1,  1,  1,  1)*side[3]/2,
    i = c(7, 0, 2, 3, 4, 4, 6, 1, 4, 0, 7, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 0, 0, 6, 7, 1, 6, 5, 5, 3, 6),
    #intensity = rep(1, length = 8),
    #color = rep(1, length = 8),
    #colors = color
    facecolor = rep(color, 12),
    hoverinfo = 'none',
    lighting = list(ambient = 1,diffuse = 0)
    
    
  )
}
draw_present <- function(.data, center, side, color){
  draw_cuboid(.data,center + c((side[1]+0.1)/4, (side[2]+0.1)/4, 0), c((side[1]-0.1)/2, (side[2]-0.1)/2,side[3]), color[1]) %>% 
  draw_cuboid(center + c((side[1]+0.1)/4, -(side[2]+0.1)/4, 0), c((side[1]-0.1)/2, (side[2]-0.1)/2,side[3]), color[1]) %>% 
  draw_cuboid(center + c(-(side[1]+0.1)/4, (side[2]+0.1)/4, 0), c((side[1]-0.1)/2, (side[2]-0.1)/2,side[3]), color[1]) %>% 
  draw_cuboid(center + c(-(side[1]+0.1)/4, -(side[2]+0.1)/4, 0), c((side[1]-0.1)/2, (side[2]-0.1)/2,side[3]), color[1]) %>% 
  draw_cuboid(center + c(0,0,0.005), c(0.1,side[2],side[3]+0.01), color[2]) %>% 
  draw_cuboid(center + c(0,0,0.005), c(side[1],0.1,side[3]+0.01), color[2])
}
draw_star <- function(.data, bot, side, color){
  ang <- seq(0, 2*pi, length = 4)
  ang <- ang[-length(ang)]
  rad <- side/sqrt(3)
  add_trace(
    p = .data,
    type = 'mesh3d',
    x = c(bot[1], bot[1] + rad*cos(ang)),
    y = c(bot[2], bot[2] + rad*sin(ang)),
    z = c(bot[3], rep(bot[3] + side/2*sqrt(3),3)),
    i = c(0, 0, 0, 1),
    j = c(1, 2, 3, 2),
    k = c(2, 3, 1, 3),
    #intensity = rep(1, length = 8),
    #color = rep(1, length = 8),
    #colors = color
    facecolor = rep(color, 4),
    hoverinfo = 'none',
    lighting = list(ambient = 1,diffuse = 1),
    contour = list(
      color = "#000000",
      visible = T,
      width = 10
    )
  ) %>% 
    add_trace(
      type = 'mesh3d',
      x = c(bot[1], bot[1] + rad*cos(ang)),
      y = c(bot[2], bot[2] + rad*sin(ang)),
      z = c(bot[3] + sqrt(27)/4*side, rep(bot[3] + side/4*sqrt(3),3)),
      i = c(0, 0, 0, 1),
      j = c(1, 2, 3, 2),
      k = c(2, 3, 1, 3),
      #intensity = rep(1, length = 8),
      #color = rep(1, length = 8),
      #colors = color
      facecolor = rep(color, 4),
      hoverinfo = 'none',
      lighting = list(ambient = 1,diffuse = 1),
      contour = list(
        color = "#000000",
        visible = T,
        width = 10
      )
    )
}

lim <- 4
colors <- c("#8F221D", "#A0CE5A", "#FEBA3E", "#859AA2", "#FFF778","#E582B0", "#F38DA7","#DFE0E2", "#0094D7")

plot_ly() %>% 
  layout(scene = list(
    xaxis=list(visible = F, range = c(-lim,lim)),
    yaxis=list(visible = F, range = c(-lim,lim), scaleanchor = "x",scaleratio = 1),
    zaxis=list(visible = F, range = c(-lim,lim), scaleanchor = "x",scaleratio = 1),
    bgcolor = "#001B3A",
    aspectmode = "cube"
                      )) %>% 
  draw_cube(c(0,0,-0.3),0.7,"brown") %>% 
  draw_cone(c(0,0,0), 1, 3, 10, "green") %>% 
  draw_circle(c(0,0,-0.6501), 4,10, "white") %>% 
  draw_present(c(0.7,0.7,-0.45),c(0.4,0.5,0.4), c("red", "yellow")) %>% 
  draw_present(c(0.3,0.7,-0.35),c(0.3,0.3,0.6), c("blue", "yellow")) %>% 
  draw_present(c(-0.8,-0.8,-0.55),c(0.7,0.7,0.2), c("#78F400", "yellow")) %>% 
  draw_star(c(0,0,2.9), 0.5,"yellow") -> fig


for (i in  1:100){
  size <- 0.1
  r <- 0
  while (r < 0.1/3){
    r <- sqrt(runif(1))
  }
  ang <- 2 * pi * runif(1)
  x <- r*cos(ang)
  y <- r*sin(ang)
  z <- 3*(1-r)
  fig <- fig %>% draw_cube(c(x,y,z), size, sample(colors, 1))
}
fig
htmlwidgets::saveWidget(as_widget(fig), "index.html")

  



