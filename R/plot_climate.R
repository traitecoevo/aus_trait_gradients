

au_basemap <- function() {

  au_map <- raster::raster("data/australia.tif") %>%
    # aggregate(fact=6) %>%
    raster::as.data.frame(xy = T)

  au_basemap <-
    ggplot() +
    geom_raster(data = au_map, aes(
      x = x,
      y = y,
      fill = factor(australia)
    )) +
    myTheme
  au_basemap
}

plot_climate_space <- function(traits_sites, au_bioclim) {
  # Transform raster data into a tibble
  au_bioclim_table <-
    au_bioclim %>%
    raster::as.data.frame() %>%
    na.omit() %>%
    as_tibble() %>%
    mutate(region = as.factor("Australia"))

  ggplot() +
  geom_polygon(
    data = Whittaker_biomes,
    aes(
      x = temp_c,
      y = precp_cm,
      fill = biome
    ),
    colour = "gray98",
    # colour of polygon border
    size = 0.1
  ) +
  # add the temperature - precipitation data points
  geom_point(
    data = au_bioclim_table,
    aes(x = Temp / 10, y = Prec / 10, color = "Australia"),
    alpha = 0.2,
    stroke = 0,
    size = 0.5,
    inherit.aes = FALSE
  ) +

  # add the AusTraits Sites
  geom_point(
    data = traits_sites,
    aes(x = Temp / 10, y = Prec / 10, color = "AusTraits sites"),
    alpha = 0.3,
    stroke = 0,
    size = 1,
    inherit.aes = FALSE,
    position = "jitter"
  ) +

  # set color for  the temperature - precipitation data points and the the AusTraits Sites
  scale_colour_manual(name = "Australian climate space", values = c("#FF7F50", "#233D4D")) +
  scale_fill_manual(
    name = "Whittaker biomes",
    breaks = names(Ricklefs_colors),
    labels = names(Ricklefs_colors),
    values = alpha(Ricklefs_colors, 0.5)
  ) +
  theme_classic() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab(expression(Temperature(degree * C))) +
  ylab(" Precipitation (cm/yr)") +
  theme(text = element_text(size = 12)) +
  theme(
    legend.justification = c(-0.1, 0),
    legend.position = c(0.005, 0.25),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
    # legend.key.size = unit(1, "cm")
  )

}


plot_site_map_by_trait <- function(trait_df) {
  df <-
    trait_df %>%
    dplyr::select(location_name, latitude, longitude, trait_name) %>%
    filter(
      latitude > (-45), latitude < (-9.5), longitude < (153), longitude > (110)
    ) %>%
    filter(
      !location_name %in% c(
        "site_at_-17.4167_degS_and_151.8833_degE",
        "site_at_-16.5833_degS_and_150.3167_degE",
        "site_at_-16.9333_degS_and_149.1833_degE",
        "site_at_-16.9833_degS_and_149.8833_degE"
      )
    ) %>%
    drop_na()


    au_basemap() +
    geom_pointdensity(
      data = df,
      aes(y = latitude, x = longitude),
      inherit.aes = FALSE,
      show.legend = FALSE,
      adjust = 1,
      size = 0.5,
      alpha = 0.8
    ) +
    scale_color_viridis_b(option = "plasma") +
    theme(
      legend.justification = c(-0.1, 0),
      legend.position = c(0.05, 0.05),
      legend.direction = "horizontal"
    ) +
    scale_fill_grey(
      name = "",
      start = 0.8,
      guide = "none",
      na.value = "white"
    ) + xlab("") + ylab("") +
    facet_wrap(~trait_name)
}

myTheme <-

  theme(
    legend.position = "none",

    panel.grid.major = element_blank(),

    panel.grid.minor = element_blank(),

    panel.background = element_blank(),

    panel.border = element_rect(colour = "black", fill = NA, size = 1),

    axis.ticks.length = unit(1, "mm"),

    axis.ticks = element_line(size = 1),

    axis.line.x = element_line(),

    axis.line.y = element_line(),

    axis.title.x = element_text(size = 12, margin = ggplot2::margin(10, 0, 0, 0)),

    axis.title.y = element_text(size = 12, margin = ggplot2::margin(0, 10, 0, 0)),

    axis.text.x = element_text(size = 9, colour = "#666666", margin = ggplot2::margin(15, 0, 0, 0, "pt")),

    axis.text.y = element_text(size = 9, colour = "#666666", margin = ggplot2::margin(0, 15, 0, 0, "pt")),

    panel.spacing.y = unit(1, "points"),

    legend.title = element_text(size = 12),

    legend.text = element_text(size = 10)
  )

# created two plotting functions for the CHELSA vs. Worldclim comparison, may be redundant or could be refined (also dependent on local TIF files which are too large to commit)

plot_worldclim_climate_data <- function(variable = c("temp","prec")) {

  library(raster)

  # Download bioclim data using library (raster)
  bioclim <- getData("worldclim", var = "bio", res = 10)

  if(variable == "temp"){
    # Pick BIO1 (Mean Annual Temperature; T)
    bioclim <- bioclim[[c(1)]]
  }

  if(variable == "prec"){
    # Pick BI12 (Annual Precipitation; P)
    bioclim <- bioclim[[c(12)]]
  }

  # Load Australia landmass binary map
  au_map <- raster("data/australia.tif")

  # Clip bioclim data with the au map
  ## crop and mask
  bioclim_au <- crop(bioclim, extent(au_map)) #%>% mask(.,au_map)
  new.bioclim <-
    raster::projectRaster(bioclim_au, au_map) # harmonize the spatial extent and projection
  au_bioclim <- raster::mask(new.bioclim, au_map)
  #plot
  sp::plot(au_bioclim, main = "Worldclim")
}

