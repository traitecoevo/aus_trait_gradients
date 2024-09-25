austraits_climate_space <- function(core_trait){
  
  aus_data = bind_cols(au_values_prec, au_values_temp)
  if(length(core_trait) == 1){
  if(core_trait == "legend"){
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
      theme(text = element_text(size = 20))  +
      theme(
        legend.justification = c(-0.1, 0),
        legend.position = c(0.005, 0.25),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_rect(fill=NA)
      ) -> p
    
    p <- get_legend(p)
  } else{
  
   woody_field_traits_georef_mean_climate_wide %>%
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
  theme(text = element_text(size = 20))  +
  theme(
    legend.justification = c(-0.1, 0),
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  ) +
  geom_point(
    data = aus_data,
    aes(x = temp,
        y = prec/10),
    col = "orange", alpha = 0.1
  ) +
  geom_point(
    data = site_data,
    aes(x = Temp,
        y = Prec/10)
    )+
    ggtitle(core_trait) ->p
  }
}
  else{
    

    woody_field_traits_georef_mean_climate_wide %>%
      pivot_longer(cols = c(core_traits)) %>%
      drop_na(value) %>%
      group_by(cell) %>%
      nest() %>%
      mutate(Temp = map_dbl(data, ~unique(.x$Temp)),
             Prec = map_dbl(data, ~unique(.x$Prec)))-> site_data
        ggplot() +
      geom_polygon(
        data = Whittaker_biomes,
        aes(x    = temp_c,
            y    = precp_cm*10,
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
      xlab(expression(Mean~annual~temperature~(degree * C))) +
      ylab("Mean annual precipitation (mm)") +
      theme(text = element_text(size = 20))  +
      theme(
        legend.justification = c(-0.1, 0),
        legend.position=c(.005,.5),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)
      ) +
      geom_point(
        data = aus_data,
        aes(x = temp,
            y = prec),
        col = "orange", alpha = 0.1
      ) +
      geom_point(
        data = site_data,
        aes(x = Temp,
            y = Prec)
      )->p 
  }
  
}
