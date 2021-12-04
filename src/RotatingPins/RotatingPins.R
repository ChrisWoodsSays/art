# Load packages
library(pacman)
pacman::p_load(tidyverse, showtext, sysfonts)

sysfonts::font_add_google("Playfair Display", ## name of Google font
                          "Playfair")  ## name that will be used in R

sysfonts::font_add_google("Open Sans", ## name of Google font
                          "Open Sans")  ## name that will be used in R
showtext_auto() # graphics devices will use showtext to draw text

sysfonts::font_families()
sysfonts::font_families_google()

# Layered Full
size <- 5
gap <- 5
noOfRows <- 20
noOfCols <- 20

seq = seq(from = 1, to = noOfRows * noOfCols, by = 1) - 1

seqX = seq %% noOfCols
seqY = floor(seq / noOfRows)

startX = seqX * (size + gap)
startY = seqY * (size + gap)

angle = pi/2 - pi/2 * seqX / (noOfCols - 1)
endX = startX + size * sin(angle)
endY = startY + size * cos(angle)

pins <- data.frame(
  seq, startX, startY, angle, endX, endY
)

g <- ggplot(pins) +
  geom_segment(aes(x = startX, xend = endX, y = startY, yend = endY),
                                   colour = 'blue', alpha = 1,
               lineend = 'round', size = 1.5) +
  coord_equal() + 
  # Remove space between data points and axes (plus right padding too)
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(family = "Playfair", hjust = .5, 
                                  margin=margin(2, 0, 2, 0, unit = "cm"), size = 60), #top, right, bottom, left
        plot.caption = element_text(family = "Open Sans", 
                                    margin = margin(c(4, 20, 1, 1), unit = "cm"), size = 8, hjust = 1),
        plot.margin = margin(c(1, 1, 1, 1), unit = "cm")) + 
  labs(title = "Rotating Pins",
       #subtitle = "",
       caption = "V.0.1, 4.12.2021  |  Visualisation by @ChrisWoodsSays   ") # Included as caption right margin doesn;t seem to work
print(g)

ggsave(here::here("output", paste0("RotatingPins", Sys.time(), ".jpg")), g, width = 210, height = 297, units = "mm")

