plot_trait_by_dataset <- function(dataset_id_index, trait){
  woody_field_traits_georef_tree_form %>%
    filter(trait_name == trait) %>%
    filter(dataset_id == dataset_id_index) -> p  
  if(trait == "leaf_delta13C"){
  woody_field_traits_georef_tree_form %>%
    filter(trait_name == trait) %>%
    ggplot(aes(x=Prec_BC, y=value)) + geom_point(color = "gray60") +scale_x_log10()+  
    ylab(trait) + xlab("Prec") + geom_point(data=p, aes(x=Prec_BC, y=value), color ="red", na.rm=T)
  file_name <- paste(dataset_id_index,".png", sep="")
  path <- paste("output/traits_by_dataset/",trait, sep = "")
  dir.create(path)
ggsave(device=png, filename = file_name, path = path)
  }
  else{
    woody_field_traits_georef_tree_form %>%
      filter(trait_name == trait) %>%
      ggplot(aes(x=Prec_BC, y=value)) + geom_point(color = "gray60") + scale_y_log10() +scale_x_log10()+  
      ylab(trait) + xlab("Prec") + geom_point(data=p, aes(x=Prec_BC, y=value), color ="red", na.rm=T)
    file_name <- paste(dataset_id_index,".png", sep="")
    path <- paste("output/traits_by_dataset/",trait, sep = "")
    dir.create(path)
    ggsave(device=png, filename = file_name, path = path)
  }
}

plot_trait_by_dataset_site_level <- function(dataset_id_index, trait){
  woody_field_traits_georef_tree_form %>%
    filter(trait_name == trait) %>%
    filter(dataset_id == dataset_id_index) -> p  
  if(trait == "leaf_delta13C"){
  woody_field_traits_georef_tree_form %>%
    filter(trait_name == trait) %>%
    ggplot(aes(x=Prec_BC, y=value)) + geom_point(color = "gray60") +scale_x_log10()+  
    ylab(trait) + xlab("Prec") + geom_point(data=p, aes(x=Prec_BC, y=value), color ="red", na.rm=T)
  file_name <- paste(dataset_id_index,".png", sep="")
  path <- paste("output/traits_by_dataset_site_specific/",trait, sep = "")
  dir.create(path)
  ggsave(device=png, filename = file_name, path = path)
  }
  else{
    woody_field_traits_georef_tree_form %>%
      filter(trait_name == trait) %>%
      ggplot(aes(x=Prec_BC, y=value)) + geom_point(color = "gray60") + scale_y_log10() +scale_x_log10()+  
      ylab(trait) + xlab("Prec") + geom_point(data=p, aes(x=Prec_BC, y=value), color ="red", na.rm=T)
    file_name <- paste(dataset_id_index,".png", sep="")
    path <- paste("output/traits_by_dataset_site_specific/",trait, sep = "")
    dir.create(path)
    ggsave(device=png, filename = file_name, path = path)
  }
  
}