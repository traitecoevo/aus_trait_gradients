austraits$sites %>% 
  subset(site_property %in% c("latitude (deg)","longitude (deg)")) %>%
  group_by(site_name,dataset_id) %>%
  spread(key = site_property,value = value) -> site_coordinates

austraits$traits %>% 
  filter(trait_name %in% c("leaf_area","leaf_dry_mass","specific_leaf_area","leaf_N_per_dry_mass",
                           "leaf_N_per_area")) %>%
  select(trait_name, value, observation_id, taxon_name,dataset_id,site_name, context_name, date) %>%
  group_by(trait_name, observation_id, taxon_name, dataset_id, site_name, context_name, date) %>%
  summarise(value = first(value)) %>%
  ungroup() %>%
  spread(key = trait_name, value = value) %>% 
  left_join(site_coordinates,by=c("dataset_id","site_name")) %>%
  mutate(specific_leaf_area = as.numeric(specific_leaf_area),
         leaf_N_per_dry_mass = as.numeric(leaf_N_per_dry_mass),
         leaf_area = as.numeric(leaf_area),
         leaf_dry_mass = as.numeric(leaf_dry_mass),
         leaf_N_per_area = as.numeric(leaf_N_per_area)) %>%
  mutate(leaf_N_calc_SLA = leaf_N_per_dry_mass / specific_leaf_area,
         leaf_N_calc_LA = leaf_N_per_dry_mass*leaf_dry_mass/leaf_area,
         leaf_N_per_area_combined = ifelse(!is.na(leaf_N_per_area),leaf_N_per_area,leaf_N_calc_SLA),
         leaf_N_per_area_combined = ifelse(!is.na(leaf_N_per_area_combined),leaf_N_per_area_combined,leaf_N_calc_LA),
         specific_leaf_area_combined = ifelse(!is.na(specific_leaf_area),specific_leaf_area,leaf_area/leaf_dry_mass),
         leaf_area_combined = ifelse(!is.na(leaf_area),leaf_area,specific_leaf_area*leaf_dry_mass)) -> calculated_data

calculated_data %>%
  select(observation_id, taxon_name, dataset_id, site_name, context_name, date, 
         leaf_N_per_area_combined,`latitude (deg)`,`longitude (deg)`) %>%
  mutate(units = "g/m2") -> leaf_N_per_area_calcuated

calculated_data %>%
  select(observation_id, taxon_name, dataset_id, site_name, context_name, date, 
         leaf_area_combined,`latitude (deg)`,`longitude (deg)`) %>%
  mutate(units = "mm2") -> leaf_area_calcuated


all_data %>% filter(!is.na(`latitude (deg)`)) %>%
  filter(!is.na(leaf_N_per_area_combined)) %>%
  distinct(taxon_name) %>% nrow()
  nrow()
