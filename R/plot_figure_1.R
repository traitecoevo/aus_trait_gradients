plot_figure_1 <- function(trait_name, new_name, data, position){
  letters2<-c(letters[1:26], paste0("a",letters[1:26]))
  end<- position*5
  start<- end - 4
  sub_labels <- tibble(env = c("MAT ~(degree*C)","VPD~(kPa)","PAR (kJ~m^{-2}~day^{-1})","Elevation~(m)", "MAP~(mm~yr^{-1})"), label = paste("(",letters2[start:end],")", sep=""))
  
  if(position == 1){
  core_traits <- c(trait_name)
  
  trait_names <- tibble(trait_name = core_traits,
                        trait = new_name)
  
  data %>%
    filter(trait_name %in% core_traits) %>%
    left_join(by="trait_name", trait_names)%>%
    select(trait, value, Temp = avg_temp_WC, VPD = VPD_WC, PAR = solar_WC,elev = elev_WC, prec = prec_WC) %>%
    pivot_longer(-c("trait", "value"), names_to = "env", values_to = "env_value") %>%
    mutate(env = factor(env, levels = c("Temp","VPD","PAR","elev","prec"),labels=c("MAT ~(degree*C)","VPD~(kPa)","PAR (kJ~m^{-2}~day^{-1})","Elevation~(m)","MAP~(mm~yr^{-1})"))) %>%
    left_join(sub_labels) %>% 
    mutate(env = factor(env, levels = c("MAT ~(degree*C)","VPD~(kPa)","PAR (kJ~m^{-2}~day^{-1})","Elevation~(m)","MAP~(mm~yr^{-1})")))%>%
    ggplot(aes(env_value, value)) + 
    geom_hex() + 
    scale_y_log10() +
    scale_x_log10() + 
    theme_bw() + 
    facet_wrap(~env, scales = "free", nrow=1, strip.position = "top",labeller = label_parsed) + 
    ylab(new_name) +
    geom_text(mapping = aes(x = 0, y = Inf, label = label, group=env),
              vjust="inward",hjust=-0.25,
              inherit.aes = FALSE) +
    theme(axis.title.x=element_blank()) +
    theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12), axis.text=element_text(size=7)) +
    theme(strip.text.x.top = element_text(angle=0, size = 12), strip.background = element_rect(fill="grey")) -> plot
  plot
  }
  else{
    core_traits <- c(trait_name)

    trait_names <- tibble(trait_name = core_traits,
                          trait = new_name)

    woody_field_traits_georef_tree_form_climate %>%
      filter(trait_name %in% core_traits) %>%
      left_join(by="trait_name", trait_names)%>%
      select(trait, value, Temp = avg_temp_WC, VPD = VPD_WC, PAR = solar_WC,elev = elev_WC, prec = prec_WC) %>%
      pivot_longer(-c("trait", "value"), names_to = "env", values_to = "env_value") %>%
      mutate(env = factor(env, levels = c("Temp","VPD","PAR","elev","prec"),labels=c("MAT ~(degree*C)","VPD~(kPa)","PAR (kJ~m^{-2}~day^{-1})","Elevation~(m)","MAP~(mm~yr^{-1})"))) %>%
      left_join(sub_labels) %>%
      mutate(env = factor(env, levels = c("MAT ~(degree*C)","VPD~(kPa)","PAR (kJ~m^{-2}~day^{-1})","Elevation~(m)","MAP~(mm~yr^{-1})")))%>%
      ggplot(aes(env_value, value)) +
      geom_hex() +
      scale_y_log10() +
      scale_x_log10() +
      theme_bw() +
      facet_wrap(~env, scales = "free", nrow=1, strip.position = "top",labeller = label_parsed) +
      ylab(new_name) +
      geom_text(mapping = aes(x = 0, y = Inf, label = label, group=env),
                vjust="inward",hjust=-0.25,
                inherit.aes = FALSE) +
      theme(axis.title.x=element_blank()) +
      theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12), axis.text=element_text(size=7)) +
      theme(strip.text.x.top = element_blank(), strip.background = element_rect(fill="grey")) -> plot
    plot
    }
}
