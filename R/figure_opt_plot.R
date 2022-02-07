figure_opt_plot <- function(enviro_variable, logit){
  correlations %>%
    filter(env == enviro_variable)%>%
    rename(cor=value)-> correlation_for_plot
  
  woody_field_traits_georef_tree_form_climate%>%
    filter(trait_name %in% trait_to_plot) %>%
    select(trait_name, value, enviro_variable)%>%
    pivot_longer(-c("trait_name", "value"), names_to = "env", values_to = "env_value")%>%
    left_join(sub_labels) %>%
    left_join(correlation_for_plot) %>%
    mutate(cor = round(cor, digits=2)) -> data_for_plot
  
  if(logit == "no"){
  data_for_plot%>%
  ggplot(aes(env_value, value,group = trait_name)) +
  geom_point() + 
  scale_y_log10() +
  scale_x_log10(limits=c(from =0.5, to=5)) + 
  theme_bw() + 
  theme(axis.title.x = element_blank())+
  theme(axis.text=element_text(size=11)) +
  geom_text(mapping = aes(x = 0, y = Inf, label = label, group=env),
            hjust = "inward", vjust = "inward",
            inherit.aes = FALSE, check_overlap = TRUE, size=4) +
  geom_text(mapping = aes(x = 0, y = 0, label = paste("r = ",cor), group=env),
            hjust = "left", vjust = "bottom",
                inherit.aes = FALSE, size=4)-> plot
  }
  else{
    data_for_plot %>%
      ggplot(aes(env_value, value,group = trait_name)) +
      geom_point() + 
      scale_x_log10(limits=c(from =0.5, to=5)) + 
      theme_bw() + 
      theme(axis.title.x = element_blank())+
      theme(axis.text=element_text(size=11)) +
      geom_text(mapping = aes(x = 0, y = Inf, label = label, group=env),
                vjust="inward",hjust=-0.25,
                inherit.aes = FALSE, check_overlap = TRUE, size=4) +
      geom_text(mapping = aes(x = 0, y = -Inf, label = paste("r = ",cor), group=env),
                hjust = "left", vjust = "bottom",
                inherit.aes = FALSE, size=4) -> plot
  }
}

figure_opt_plot_log_x <- function(enviro_variable, logit){
  correlations %>%
    filter(env == enviro_variable)%>%
    rename(cor=value)-> correlation_for_plot
  
  woody_field_traits_georef_tree_form_climate%>%
    filter(trait_name %in% trait_to_plot) %>%
    select(trait_name, value, enviro_variable)%>%
    pivot_longer(-c("trait_name", "value"), names_to = "env", values_to = "env_value")%>%
    left_join(sub_labels) %>%
    left_join(correlation_for_plot) %>%
    mutate(cor = round(cor, digits=2)) -> data_for_plot  
  
  if(logit == "no"){
    data_for_plot %>%
      ggplot(aes(env_value, value,group = trait_name)) +
      geom_point() + 
      scale_y_log10() +
      xlim(5,30) +
      theme_bw() + 
      theme(axis.title.x = element_blank())+
      theme(axis.text=element_text(size=11)) +
      geom_text(mapping = aes(x = -Inf, y = Inf, label = label, group=env),
                vjust="inward",hjust=-0.25,
                inherit.aes = FALSE, check_overlap = TRUE, size=4) +
      geom_text(mapping = aes(x = -Inf, y = 0, label = paste("r = ",cor), group=env),
                hjust = "left", vjust = "bottom",
                inherit.aes = FALSE, size=4)-> plot
  }
  else{
    data_for_plot %>%
      ggplot(aes(env_value, value,group = trait_name)) +
      geom_point() + 
      theme_bw() + 
      xlim(5,30) +
      theme(axis.title.x = element_blank())+
      theme(axis.text=element_text(size=11)) +
      geom_text(mapping = aes(x = -Inf, y = Inf, label = label, group=env),
                vjust="inward",hjust=-0.25,
                inherit.aes = FALSE, check_overlap = TRUE, size=4)+
      geom_text(mapping = aes(x = -Inf, y = -Inf, label = paste("r = ",cor), group=env),
                hjust = "left", vjust = "bottom",
                inherit.aes = FALSE, size=4) -> plot
  }
}
  