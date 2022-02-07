error_check <- function(data, trait_to_check){
  data %>%
    filter(trait_name == trait_to_check) %>%
    group_by(taxon_name, trait_name) %>%
    filter(n()>2) %>%
    summarise(max_value = max(value, na.rm=T), min_value = min(value, na.rm=T), sd_value = sd(value, na.rm=T), mean_value = mean(value, na.rm=T)) %>%
    mutate(max_min_ratio = max_value/min_value, max_min_sum = max_value + min_value, cv_value = sd_value/mean_value*100)-> error_check
  
  data %>%
    filter(taxon_name %in% error_check$taxon_name) %>%
    group_by(trait_name, taxon_name) %>%
    distinct(dataset_id) %>%
    mutate(studies = paste0(dataset_id, collapse= "")) %>%
    slice(1) %>%
    inner_join(error_check) -> error_check
}