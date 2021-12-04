# Load packages
library(pacman)
pacman::p_load(tidyverse, showtext, sysfonts)

sysfonts::font_add_google("Playfair Display", ## name of Google font
                          "Playfair")  ## name that will be used in R
showtext_auto() # graphics devices will use showtext to draw text

# Layered Full
thickness <- 2
gap <- 5
noOfCircles <- 8
overallRadius <- (thickness + gap) * noOfCircles
arcs <- data.frame(
  start = rep(0, length.out = noOfCircles),
  end = runif(noOfCircles ,0.2, 2 * pi),
  radius = seq(from = thickness + gap, to = overallRadius, by = thickness + gap)
)

g <- ggplot(arcs)
for(y in 14:1 ) {
  for(x in 1:7) {
    xc <- (x + 0.5 * y %% 2) * overallRadius * 1 * 2 
    #xc <- (runif(1,1,7) + 0.5 * y %% 2) * overallRadius * 0.6 * 2 
    yc <- y * overallRadius * 0.4 * 2
    #yc <- (runif(1,1,7) + 0.5 * y %% 2) * overallRadius * 0.6 * 2 
    g <- g + ggforce::geom_arc_bar(aes(x0 = {{xc}}, y0 = {{yc}}, 
                                       r0 = radius - thickness, 
                                       r = radius, 
                                       start = 0, end = 2 * pi),
                                   fill = 'blue', alpha = 1, colour = NA)
    g <- g + ggforce::geom_arc_bar(aes(x0 = {{xc}}, y0 = {{yc}}, 
                                       r0 = radius - thickness, 
                                       r = radius - thickness - gap, 
                                       start = 0, end = 2 * pi),
                                   fill = 'white', alpha = 1, colour = NA)
  }
}

g <- g + 
  coord_equal() + 
  # Remove space between data points and axes (plus right padding too)
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(family = "Playfair", hjust = .5, 
                                  margin=margin(20,0,30,0), size = 40), #top, right, bottom, left
        plot.caption = element_text(size = 8, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  labs(title = "Circles",
       #subtitle = "",
       caption = "V.0.1, 2.12.2021  |  Visualisation by @ChrisWoodsSays    ") # Included as caption right margin doesn;t seem to work
print(g)

ggsave(g, here::here("output","BlueCircles.jpg"))