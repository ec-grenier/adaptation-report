packages = c("glue", "dplyr", "sf", "rnaturalearth", "ncdf4")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

map.df = st_read('/project/cil/sacagawea_shares/gcp/climate/_spatial_data/world-combo-201710')

continents = read_csv('/project/cil/gcp/regions/continents2.csv', show_col_types = FALSE) %>% 
  select(`alpha-3`, region, `sub-region`) %>% 
  rename(iso = `alpha-3`,
         continent = region,
         sub_region = `sub-region`) %>%
  mutate(iso = ifelse(iso=='SXM', 'SMX', iso),
         continent = ifelse(iso=='ATA', 'Antarctica', continent),
         continent = ifelse(iso=='RUS', 'Asia', continent),
         sub_region = ifelse(iso=='ATA', 'Antarctica', sub_region)) %>%
  bind_rows(data.frame(iso = "KO-", continent = "Europe"),
            data.frame(iso = "CA-", continent = "Asia"),
            data.frame(iso = "SP-", continent = "Asia"),
            data.frame(iso = "CL-", continent = "Oceania"),) %>%
  mutate(continent = ifelse(continent == "Americas", sub_region, continent)) %>%
  select(iso, continent)

map.df = map.df %>% left_join(continents, by=c("ISO" = "iso"))

# map.df = map.df %>%
#   mutate(
#     # Get centroid longitude for each region
#     centroid_lon = st_coordinates(st_centroid(geometry))[, 1],
#     
#     # Assign continent
#     continent = case_when(
#       ISO == "RUS" & (hierid == "RUS.14.399.399" | hierid == "RUS.14.Rdd2e9902f80893b8") ~ "Asia",
#       ISO == "RUS" & centroid_lon < 60 ~ "Europe",
#       ISO == "RUS" & centroid_lon >= 60 ~ "Asia",
#       ISO == "RUS" & centroid_lon >= 60 ~ "Asia",
#       ISO == "RUS" ~ "Asia",  # default for any edge cases
#       TRUE ~ continent  # keep existing continent for non-Russia
#     )
#   )

map.df = map.df %>% st_drop_geometry()

map.df = map.df %>% select(hierid, ISO, continent) %>% rename(region=hierid, iso=ISO)
write.csv(map.df, '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/misc/continent-to-IR.csv', row.names=F)

# TEST
map.shp = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')


map.shp = map.shp %>% left_join(map.df, by=c("hierid"="region")) #%>% filter(continent == "Asia")


p = ggplot(data = map.shp) +
  geom_sf(aes(fill=continent), lwd = 0.05, color = NA) +
  theme_bw() +     
  theme(plot.title = element_text(hjust=0.5, size = 10), 
        plot.caption = element_text(hjust=0.5, size = 7), 
        legend.title = element_text(hjust=0.5, size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        axis.title= element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())

print(p)
ggsave('/project/cil/home_dirs/egrenier/misc/continent_map.pdf', p)

# Approved by emily