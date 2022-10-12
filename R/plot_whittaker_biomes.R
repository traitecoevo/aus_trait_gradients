library(plotbiomes)

austraits_climate_space <- function(core_trait){
    woody_field_traits_georef_tree_form_climate_wide %>%
    drop_na(core_trait) -> site_data
  ggplot() +
  geom_polygon(
    data = Whittaker_biomes,
    aes(x    = temp_c,
        y    = precp_cm,
        fill = biome),
    colour = "gray98",
    # colour of polygon border
    size   = 0.1
  ) +
  
  # set color for  the temperature - precipitation data points and the the AusTraits Sites
  scale_colour_manual(name = "Australian climate space", values = c("#FF7F50", "#233D4D")) +
  scale_fill_manual(
    name   = "Whittaker biomes",
    breaks = names(Ricklefs_colors),
    labels = names(Ricklefs_colors),
    values =  alpha(Ricklefs_colors, 0.5)
    
  ) +
  theme_classic() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab(expression(Temperature (degree * C))) +
  ylab(" Precipitation (cm/yr)") +
  theme(text = element_text(size = 12))  +
  theme(
    legend.justification = c(-0.1, 0),
    legend.position = c(0.005, 0.25),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  ) +
  geom_point(
    data = site_data,
    aes(x = Temp,
        y = Prec/10)
  ) +
    ggtitle(core_trait) ->p
}
