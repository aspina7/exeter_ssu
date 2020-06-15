

## USE SLIPPYMATH PACKAGE TO PULL OPEN TOPO MAP 
  ## then could use ggplot - but also pull together function at the same time 
  ## for r4epis! 
  ## https://github.com/milesmcbain/slippymath
  ## https://opentopomap.org/#map=12/12.0425/39.0605
  ## https://github.com/riatelab/cartography/tree/master/R
  ## ggspatial for plotting raster bricks 
  ## carto ppl osrm to calculate distances between points 

## install packages from github 

## Installing required packages from github 
# git_packages <- c("afrihealthsites",  ## to get locations of health centres
#                   "afriadmin"   ## to get admin boundaries
# )
# for (pkg in git_packages) {
#   
#   ## install packages if not already present from approrpriate repo
#   if (!pkg %in% rownames(installed.packages())) {
#     
#     if (pkg == "afrihealthsites") {
#       remotes::install_github("afrimapr/afrihealthsites")
#     }
#     
#     if (pkg == "sitrep") {
#       remotes::install_github("afrimapr/afriadmin") 
#     }
#     
#   }
#   
#   ## load packages to this current session 
#   library(pkg, character.only = TRUE)
# }



library(slippymath)
library(sf) 
library(stars)
library(ggplot2)
library(dplyr)
library(cartography)

query_bbox <-
  st_bbox(c(xmin = 38.5, 
            xmax = 40,
            ymin = 11.4, 
            ymax = 12),
          crs = st_crs("+proj=longlat +ellps=WGS84"))

## make in to a sf object 
query_bbox_sf <- st_as_sfc(query_bbox)


## use the tiles package to download tiles
tiles <- cartography::getTiles(
  query_bbox_sf,
  type = "opentopomap",
  zoom = NULL,
  crop = FALSE,
  verbose = FALSE,
  apikey = NA,
  cachedir = FALSE,
  forceDownload = FALSE
)


## change rasterbrick to stars object
stars_obj <- stars::st_as_stars(tiles)

## plot as stars
ggplot() + 
  geom_stars(data = stars_obj)

## fail to plot as raster
ggplot() + 
  geom_raster(data = as.data.frame(tiles, xy = TRUE),  aes(x = x, y = y))

## using the raster package
raster::plotRGB(tiles)



ggplot() +
  ggspatial::layer_spatial(tiles, interpolate = TRUE)


## compare to the original ggspatial tiles
ggplot(query_bbox_sf) + geom_sf() + ggspatial::annotation_map_tile()



## Read in the WHO list of health sites 
df_who_sites <- rio::import("https://github.com/afrimapr/afrihealthsites/blob/master/data/df_who_sites.rda?raw=true")



## retrieve province boundaries from the Global Administrative 
## level = 1 specifies provinces 
## must be possible to do this as sf directly no? Is available on GADM.org
provinces <- raster::getData("GADM", country = "ET", level = 2)

## changing GADM to a sf object 
provinces <- st_as_sf(provinces) %>% 
  dplyr::filter(VARNAME_2 == "North Wollo")



## only keep those in Amhara
clinics <- dplyr::filter(df_who_sites, 
                         Country == "Ethiopia" & 
                           Admin1 == "Amhara" & 
                           !is.na(Lat)) %>% 
  ## make in to a points dataframe 
  st_as_sf(coords = c("Long", "Lat"), 
           crs = st_crs(provinces))

## only keep clinics in north wollo 
clinics <- clinics %>% 
  mutate(include = st_intersects(clinics, provinces, sparse = FALSE)) %>% 
  filter(include & !`Facility name` %in% c("Lalibela Hospital"))



############# read in population dataset 

population <- stars::read_stars(here::here("improving_healthcare", "eth_ppp_2020.tif"))

pop_sub <- st_crop(population, provinces)

pop_sub_poly <- st_as_sf(pop_sub)



## using osrm with a local map server 

## download docker 
    ## you have to enable visualisation in bios as well as make sure it is ticked in control panel
## create a new folder somewhere (e.g. osrm_folder on your desktop) 
## manually download a pbf file to that folder from geofabrik 
  ## (nb using wget in powershell does not seem to actually download or save anything)
## in powershell:
  ## set working directory to that folder 
    ##  Set-Location C:\Users\Spina\Desktop\osrm_folder
  ##  follow these steps but skip the bit on wget and change names to your file 
    ## (https://github.com/Project-OSRM/osrm-backend#using-docker), 
  

library(osrm)
library(cartography)
options(osrm.server = "http://localhost:5000/")

lali_hospo <- filter(clinics, facility_type_9 == "Hospital")


iso <- osrmIsochrone(loc = lali_hospo, returnclass="sf",
                     breaks = seq(from = 0, to = 720, by = 60), res = 50)


osm3 <- getTiles(x = iso, crop = FALSE, type = "osm", zoom = 12)
tilesLayer(x = osm3)
bks <- sort(c(unique(iso$min), max(iso$max)))
cols <- paste0(carto.pal("turquoise.pal", n1 = length(bks)-1), 80)
choroLayer(x = iso, var = "center", breaks = bks,
           border = NA, col = cols,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)















# ### Using the heidelberg uni version of OSRM 
# ## https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html
# 
# ## install 
# remotes::install_github("GIScience/openrouteservice-r")
# ## load package
# library(openrouteservice)
# 
# 
# ## If using the online version have to 
# ## have to sign up and get an API key 
# ## https://openrouteservice.org/dev/#/signup
# 
# ## set the API key 
# ors_api_key("5b3ce3597851110001cf6248e5c02d0b29c6430caa063887cbed479b")
# 
# 
# 
# ## if building own server for querying locally 
# ## download docker 
#   ## you have to enable visualisation in bios as well as make sure it is ticked in control panel
# ## download the orm docker repo (https://github.com/GIScience/openrouteservice/tree/development) 
#   ## unzip on to desktop or wherever 
# ## open up powershell (not IDE one, just normal) 
#   ## set the working directory to the folder
#     ##  Set-Location C:\Users\Spina\Desktop\openrouteservice-development
#   ## then follow the steps from (https://github.com/GIScience/openrouteservice/blob/development/docker/README.md) 
#     ## essentially just run those first two lines of code (one by one)
#       ## cd docker
#       ## docker-compose up -d
#     ## will set up an example using heidelberg data
# ## If want to use a different OSM.pbf file (the instructions not super clear) 
#     ## clone again for a fresh start 
#     ## delete the heidelberg.osm.gz file from the data folder 
#     ## copy that newly downloaded pbf (which you can get from geofabrik) to the data folder
#     ## edit the docker-compose Yaml file in the "docker" folder 
#       ## just edit the file name from heidelberg to the name of your pbf (multiple filetypes accepted - heidelberg just not a pbf...) 
#       ## set the build_graphs argument to TRUE
#       ## edit the java_opts, so the end of the string is -Xms4g -Xmx4g -XX:MaxMetaspaceSize=512m
#     ## (delete the graphs file (as this needs to be rebuilt for your new pbf))
#     ## in the app.congif.sample file, (found in conf folder) - edit for making more isochrones
#       ## edit these bits to increase https://ask.openrouteservice.org/t/long-distance-isochrones/633/17
#       ## also change maximum locations to 500 - to be able to run multile points
#     ## re-run the steps from open up powershell above
# ## actually wtf i dont know follow this post 
#   ## https://ask.openrouteservice.org/t/source-of-great-britain-osm-file/1767/7
# 
# 
# ## set to run off local server 
# options(openrouteservice.url = "http://localhost:8080/ors")
# 
# 
# res <- filter(clinics, facility_type_9 %in% c("Health Clinic", "Health Post")) %>% 
#   st_coordinates() %>% 
#   ors_isochrones(
#   # data.frame(lon = 39.0533, lat = 12.0395), 
#                       range = 3600, 
#                       interval = 1200, 
#                       output = "sf", 
#                       profile = "driving-car")
# 
# ### for plotting individual time groups (e.g. having each group in a sep list)
# # values <- levels(factor(res$value))
# # ranges <- split(res, values)
# # ranges <- ranges[rev(values)]
# # 
# # names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
# # 
# # mapview::mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)
# 
# locations = split(res, res$group_index)
# 
# # locations <- lapply(locations, function(loc) {
# #   g <- st_geometry(loc)
# #   g[-which.min(values)] <- st_sfc(Map(st_difference,
# #                                       g[match(values[-which.min(values)], loc$value)],
# #                                       g[match(values[-which.max(values)], loc$value)]))
# #   st_geometry(loc) <- g
# #   loc
# # })
# 
# isochrones <- unsplit(locations, res$group_index)
# 
# pal <- setNames(heat.colors(length(values)), values)
# mapview::mapview(isochrones, zcol = "value", col = pal, col.regions = pal,
#         alpha.regions = 0.5, homebutton = FALSE)
# 
# 
# 
# ggplot() + 
#   ggspatial::layer_spatial(tiles, interpolate = TRUE) +  
#   geom_sf(data = provinces, fill = NA) +
#   geom_sf(data = clinics, aes(colour = facility_type_9)) + 
#   xlim(c(38.5,40)) + 
#   geom_sf(data = isochrones, aes(group = group_index, fill = as.factor(value)))
#   # geom_sf(data = res, aes(group = group_index, fill = as.factor(value)))

