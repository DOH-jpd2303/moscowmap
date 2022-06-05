# load libraries
library(rgdal)
library(sf)
library(broom)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)

# Path to map data on local drive
dir <- 'C:/Users/jondo/OneDrive/Documents/Coding/Data/Moscow Shapefiles/'


# Read in and prep data ---------------------------------------------------
# Read in the .shp and .dbf files from folder
shp <- readOGR(dsn = paste0(dir, 'moscow_adm.shp'))
dbf <- st_read(paste0(dir, 'moscow_adm.dbf')) 

# Reformat the features dataframe for labels we will be adding to the map
dbf <- dbf %>%
  mutate(id = as.numeric(rownames(.)),
         lab = paste0(.data$OKRUGS, ": ", .data$RAION),
         y = ntile(id, 15),
         x = (id-1) %% 3,
         x = x/5)

# Join the edited features DF to the main shapefile
shp@data <- shp@data %>% 
  left_join(dbf)

# Custom map theme to make the chart look better
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Plots -------------------------------------------------------------------
# Plot the map itself
p <- ggplot(shp@data, aes(geometry = geometry, fill = OKRUGS)) +
  geom_sf(color = 'black') +
  geom_sf_text(aes(label = as.numeric(as.factor(RAION))), size = 3) +
  theme_map() +
  scale_color_manual(values = rep('black', 146))

# Next, add a legend that labels the RAIONs from the file
leg <- ggplot(shp@data, aes(y = rev(id), x = x)) +
  geom_tile(color = 'white', fill = NA)  +
  geom_text(aes(label = paste0(id, ': ', RAION)), hjust = 0, size = 2.5) +
  theme_map() 

# Combine it into a single grid, output to file
out <- grid.arrange(p, leg, ncol = 2, widths = c(1, 0.4),
                    top = textGrob('Moscow: Raions and Okrugs'))
ggsave(plot = out, filename = 'moscowplot.png', dpi = 300, height = 10, width = 20)


