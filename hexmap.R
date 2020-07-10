
#  Based on https://github.com/jbaileyh/geogrid  


library(geogrid)
library(sf)
library(tmap)
library(tigris)


###### Code copied out of the mapping repo for getting monterey county boundaries.     


### Get local districts and bind together --------

districts_un <- school_districts(type = "unified" , state = "CA", year = 2019, class = "sf") %>%
    st_transform(4269) %>% rename(LEA = UNSDLEA)

districts_elem <- school_districts(type = "elementary" , state = "CA", year = 2019, class = "sf") %>%
    st_transform(4269) %>% rename(LEA = ELSDLEA)

districts_sec <- school_districts(type = "secondary" , state = "CA", year = 2019, class = "sf") %>%
    st_transform(4269) %>% rename(LEA = SCSDLEA)

districts <- districts_un %>%
    rbind(districts_elem) %>%
    rbind(districts_sec) %>%
    st_transform(4269)




### Get geography for tracts and county of monterey and only use the land areas  ------------

monterey_tracts <- tracts("CA", "Monterey", class = "sf") %>%
    filter(ALAND > 0) %>% 
    st_as_sf(monterey_tracts) %>%
    st_transform(4269) %>% st_union()


monterey_county <- counties("CA", class = "sf") %>%
    filter(NAME == "Monterey")  %>%
    st_transform(4269)


### Only districts within Monterey County ----------
# This line is all districts that are partially in Monterey County, so lots of districts that are mostly in SLO or Santa Cruz, etc.


outside_names <- "Reef|Coal|Pajar|Aroma|Shan|Not D|Coast|Paso|Bitter|Pleas|Cien|Jeff|Willow|Benito|Miguel|(9-12)|Joint"

 monterey_districts <-  districts %>%
     st_intersection(monterey_tracts) %>%
     filter(!str_detect(NAME, outside_names)) %>%
     rbind(districts %>% filter(str_detect(NAME, "South Monter") )  ) %>% #  Add SMCJUHSD back in; it is wonky with the st_intersection 
      st_collection_extract("POLYGON") 


# monterey_districts <- districts[st_contains(monterey_tracts, districts, sparse = FALSE),] %>%
#     rbind(districts %>% filter(str_detect(NAME, "South Monter") )  ) %>%
#     st_intersection(monterey_tracts) %>%
#     filter(FUNCSTAT == "E") %>%  
#     mutate(elem = if_else(LOGRADE == "KG",1,0),
#            high = if_else(HIGRADE == "12",1,0) ) %>% 
#     st_collection_extract("POLYGON")  # To address error cause by two intersections 
# 


# ----------
    



original_shapes <- monterey_districts
original_shapes$SNAME <- substr(original_shapes$NAME, 1, 6)


rawplot <- tm_shape(original_shapes) + 
    tm_polygons("ALAND", palette = "viridis") +
    tm_text("SNAME")
rawplot



par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
    new_cells <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
    plot(new_cells, main = paste("Seed", i, sep = " "))
}


new_cells_hex <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = 1) #1 8 10
resulthex <- assign_polygons(original_shapes, new_cells_hex)


hexplot <- tm_shape(resulthex) + 
    tm_polygons("ALAND", palette = "viridis") +
    tm_text("SNAME")

tmap_arrange(rawplot, hexplot,  nrow = 2)

    
####  Alt mapping version instead of tmap



# generate plot
plot_2 <- ggplot(resulthex, aes(long.x,
                                lat.x,
                                fill=Value,
                                group=group)) +
    geom_polygon(col="white") +
    scale_fill_viridis(option="magma") +
    coord_equal() + theme_void()

# arrange plot with comparison to the original
grid.arrange(plot_1, plot_2, nrow=1, ncol=2)



ggplot(resulthex, aes(long.x,
                      lat.x,
                      fill=Value,
                      group=group)) +
    geom_polygon(col="white") +
    geom_text(aes(V1,V2, label = substr(lad16nm,1,4)), size=3,color = "white") +
    scale_fill_viridis(option="magma",
                       begin = 0, end = 0.9,
                       name = "Male Life Expectancy at Birth") +
    coord_equal() + theme_void()+theme(legend.position="bottom") +
    guides(fill = guide_legend(title.position = "top")) +
    labs(title="Male Life Expectancy in the South East Region of England")



