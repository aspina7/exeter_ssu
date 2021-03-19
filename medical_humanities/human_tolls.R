## human tolls: 
## this project will create an animation showing
## images of human faces and colour them according to outcome
## based on the progression of the covid-19 pandemic in devon 

## TODO: 
## change to use annotation_custom as per this slide deck
## https://cedricscherer.netlify.app/slides/2019-08-28-intro-ggplot-statsizw#110


library(dplyr) 
library(magick)  ## for combining to make animations
library(ggimage) ## to plot images with ggplot2
library(rio)
library(janitor)
library(tsibble)
library(stringr)
library(ggplot2)

## images of human faces were sourced from the Tufts face database
## https://www.kaggle.com/kpvisionlab/tufts-face-database
## sign in through google using university account 

# drawn_imgs <- "TD_CS_[[:digit:]].jpg|TD_CS_[[:digit:]][[:digit:]].jpg|TD_CS_[[:digit:]][[:digit:]][[:digit:]].jpg"
# real_imgs  <- "TD_RGB_E_[[:digit:]].jpg|TD_RGB_E[[:digit:]][[:digit:]].jpg|TD_RGB_E[[:digit:]][[:digit:]][[:digit:]].jpg"
# 
# all_imgs <- list.files(here::here("medical_humanities", "Tufts faces"), 
#            pattern = drawn_imgs, 
#            recursive = TRUE)
# 
# ## start an empty list 
# faces <- list() 
# 
# for (i in 1:length(all_imgs)) {
#   faces[[i]] <- image_read(here::here("medical_humanities", "Tufts faces", all_imgs[i]))
# }



## read in wave tile for plotting 
img <- jpeg::readJPEG(here::here("medical_humanities", "wave_tile.jpg"))


small_wave <- grid::rasterGrob(
  png::readPNG(here::here("medical_humanities", "small_wave.png")),
  interpolate = T)

medium_wave <- grid::rasterGrob(
  png::readPNG(here::here("medical_humanities", "medium_wave.png")),
  interpolate = T)

big_wave <- grid::rasterGrob(
  png::readPNG(here::here("medical_humanities", "big_wave.png")),
  interpolate = T)


## get covid data 
## https://github.com/traffordDataLab/covid-19/blob/master/global.R


## download daily case counts from phe
cases <- rio::import("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")

## clean names
cases <- clean_names(cases)

## filter to have only covid cases in devon 
cases <- cases %>% 
  filter(area_name %in% c("North Devon", 
                          "Torridge", 
                          "Mid Devon", 
                          "East Devon", 
                          "Exeter", 
                          "West Devon", 
                          "Teignbridge", 
                          "Plymouth", 
                          "South Hams", 
                          "Torbay"))


## download weekly case counts from ons
## from https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard


deaths2020 <- rio::import(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2020/lahbtablesweek01to532020datawk82021.xlsx", 
  sheet = "Occurrences - All data")

deaths <- rio::import("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek36.xlsx", 
                      sheet = "Occurrences - All data")

## drop extra rows and put the var names appropraite
names(deaths2020) <- deaths2020[3, ]
deaths2020 <- clean_names(deaths2020)
deaths2020 <- deaths2020[c(-1, -2, -3), ]
deaths2020$year <- 2020

names(deaths) <- deaths[3, ]
deaths <- clean_names(deaths)
deaths <- deaths[c(-1, -2, -3), ]
deaths$year <- 2021

## combine last year and this
deaths <- bind_rows(deaths2020, deaths)

## filter to have only covid deaths in devon 
deaths <- deaths %>% 
  filter(cause_of_death == "COVID 19", 
         area_name %in% c("North Devon", 
                          "Torridge", 
                          "Mid Devon", 
                          "East Devon", 
                          "Exeter", 
                          "West Devon", 
                          "Teignbridge", 
                          "Plymouth", 
                          "South Hams", 
                          "Torbay"))


## backup
og_cases <- cases
og_deaths <- deaths

#### pull together counts by week 
cases <- cases %>% 
  mutate(week_number = yearweek(specimen_date)) %>% 
  group_by(week_number) %>% 
  summarise(cases = sum(daily_lab_confirmed_cases, na.rm = TRUE))

deaths <- deaths %>% 
  mutate(week_number = str_c(year, " W", str_pad(week_number, 2, pad = 0)), 
         week_number = yearweek(week_number), 
         number_of_deaths = as.numeric(number_of_deaths)) %>% 
  group_by(week_number) %>% 
  summarise(deaths = sum(number_of_deaths, na.rm = TRUE))

## get a dataset total cases and deaths for each week in devon 
agg_counts <- left_join(cases, deaths, by = "week_number")





## get a linelist of cases by week 
# linelist <- select(agg_counts, week_number, cases) %>% 
#   tidyr::uncount(weights = cases)
# 
# ## add in a month variable
# linelist$month <- format(linelist$week_number, format = "%B %Y")
# 
# ## add in who died 
# linelist$died <- NA
# 
# for (i in as.character(unique(linelist$week_number))) {
#   
#   num_deaths <- deaths$deaths[deaths$week_number == yearweek(i)]
#   
#   if (length(num_deaths) == 0) {
#     num_deaths <- 0
#   }
#   
#   remaining_cases <- cases$cases[cases$week_number == yearweek(i)] - num_deaths
#   
#   linelist$died[linelist$week_number == yearweek(i)] <- c(rep.int(TRUE, num_deaths), 
#                                                          rep.int(FALSE, remaining_cases))
# }
# 
# ## random sample which face to show 
# linelist$face <- sample(1:length(faces), nrow(linelist), replace = TRUE)
# 
# ## add in which images fit to which case
# linelist$faces2 <- here::here("medical_humanities", "Tufts faces", all_imgs[linelist$face])
# 
# ## add in an order number for plotting faces one by one
# linelist <- mutate(linelist, id = as.numeric(rownames(linelist)))
# 
# ## make a date var so can use that in gganimate plots 
# linelist <- mutate(linelist, report_date = as.Date(week_number))

##################### option 1 #################################################


# #### Version A: uses magick to plot individual images and then combine them (SUPER SLOW!)
# 
# ## empty obj for plots 
# outputs <- list()
# 
# ### print through each case and colour if died or not 
# for (i in 1:nrow(linelist)){
#   
#   ## get the corresponding face number
#   face_number <- pull(linelist[i, "face"])
#   
#   ## to show the deads
#   if (linelist[i, "died"] == TRUE) {
#     ## plot from the faces list (as is in "magick" format)
#     img <- image_scale(image_negate(faces[[face_number]]), "500x500") %>% 
#       image_annotate(paste0("Human tolls, Devon - ", linelist[i, "month"]), 
#                      color = "white", size = 20)
#     outputs[[i]] <- img
#     } else {
#     ## if alive just show normally
#     img <- image_scale(faces[[face_number]], "500x500") %>% 
#       image_annotate(paste0("Human tolls, Devon - ", linelist[i, "month"]), 
#                      size = 20)
#     outputs[[i]] <- img
#   }
# }
# 
# outputs <- image_join(outputs) 
# 
# animation <- image_animate(outputs, fps = 10, dispose = "previous")
# print(animation)
# 
# 
# ##### Version B: uses ggplot and gganimate to go through individual faces (SUPER SLOW!)
# 
# linelist$x <- 5
# linelist$y <- 5
# 
# blabla <- ggplot(data = linelist, aes(x = x, y = y)) +
#   ## plot the images and scale by the multiplier
#   geom_image(aes(image = faces2), size = 0.5) + 
#   ## limit x axes 
#   xlim(c(0,12)) + 
#   ## remove all things except images
#   theme_void() + 
#   ## add titale and month
#   # ggtitle("Human tolls", subtitle = str_c("Devon, {month}")) + 
#   gganimate::transition_time(id) + 
#   gganimate::ease_aes("back-in")
# 
# gganimate::animate(blabla,
#                    # nframes = nrow(linelist)/2, 
#                    fps = 10)

##################### option 2 #################################################

# ## loops over weeks in ggplot to plot on a fixed grid 
# ## uses magick to create animation
# 
# 
# ## empty obj for plots 
# outputs <- list()
# 
# img <- image_graph(600, 340, res = 96)
# 
# for (i in as.character(unique(linelist$week_number))) {
#   ## create a dataset up to the current week of interest
#   subsetter <- filter(linelist, week_number <= yearweek(i))
#   
#   ## get the number of cases currently dealing with
#   current_cases <- nrow(subsetter) 
#   
#   ## get a multiplier for scaling images by number of cases 
#   ausmass <- str_count(current_cases)
#   
#   multiplier <- if_else(current_cases < 5, 0.25, 
#           current_cases / as.numeric(
#             str_pad(20, ausmass + 2, 
#                     side = "right", pad = 0)))
#   
#   
#   ## calculate how many rows going to need
#   num_rows <- ceiling(current_cases / 5)
#   
#   ## get the x coordinates 
#   if (current_cases < 5) {
#     
#     ## for those under 5 simply divide 10(the max x val) by n+1 then seq between
#     by_val <- 10 / (current_cases + 1)
#     
#     subsetter$x <- seq(from = by_val, to = by_val * current_cases, by = by_val)
#     
#   } else {
#     ## if above five will always be the same
#     ## subset because not all rows will have all five slots filled
#     subsetter$x <- rep.int(c(2, 4, 6, 8, 10), num_rows)[1:current_cases]
#   }
#   
#   
#   ## get the y coordinates 
#   ## sort so that the correct numbers repeat together
#   subsetter$y <- sort(
#     ## repeat five times (one for each x)
#     rep.int(
#       ## sequence between 5 and max rows in tens 
#       seq.int(5, num_rows * 10, by = 10), 
#       5
#     )
#     ## subset because not all rows will have all five slots filled
#   )[1:current_cases]
#   
#   ## plot for each x and y coordinate 
#   face_plot <- ggplot(data = subsetter, aes(x = x, y = y)) +
#     ## plot the images and scale by the multiplier
#     geom_image(aes(image = faces2), size = multiplier) + 
#     ## limit x axes 
#     xlim(c(0,12)) + 
#     ## remove all things except images
#     theme_void() + 
#     ## add title and month
#     ggtitle("Human tolls", subtitle = paste0("Devon, ", max(subsetter$month)))
#   
#   ## save in to output 
#   outputs[[i]] <- print(face_plot)
# 
# }
# 
# dev.off()
# animation <- image_animate(img, fps = 5)
# print(animation)

##################### option 3 #################################################


# ## get the number of cases currently dealing with
# current_cases <- nrow(linelist) 
# 
# ## calculate how many rows going to need
# num_rows <- ceiling(current_cases / 5)
# 
# ## get the x coordinates 
# linelist$x <- rep.int(c(2, 4, 6, 8, 10), num_rows)[1:current_cases]
# 
# linelist$y <- sort(
#   ## repeat five times (one for each x)
#   rep.int(
#     ## sequence between 5 and max rows in tens 
#     seq.int(5, num_rows * 10, by = 10), 
#     5
#   )
#   ## subset because not all rows will have all five slots filled
# )[1:current_cases]
# 
# outputs <- ggplot(data = linelist, aes(x = x, y = y)) +
#   ## plot the images and scale by the multiplier
#   geom_image(aes(image = faces2), size = 0.05) + 
#   ## limit x axes 
#   xlim(c(0,12)) + 
#   ## remove all things except images
#   theme_void() + 
#   ## add titale and month
#   ggtitle("Human tolls", subtitle = str_c("Devon, {format(frame_time, '%B %Y')}")) + 
#   gganimate::transition_time(report_date) + 
#   gganimate::shadow_mark(paste = TRUE)
# 
# 
# gganimate::anim_save("testing.gif", animation = outputs)


## plot wave 
wave <- ggplot(agg_counts, aes(x = as.Date(week_number))) + 
  geom_area(aes(y = cases), fill = "#1d354f") +
  geom_area(aes(y = deaths), fill = "dark red") + 
  
  theme_void() + 
  theme(panel.background = element_rect(fill = "#f2e2be"), 
        # plot.margin=unit(c(0,0,-12,-5), "mm")
  )
  

ggsave(
  here::here("medical_humanities", "counts.jpg"), 
  wave, 
  width = 20.32, 
  height = 13.67, 
  units = "cm"
)


wave <- wave + 
  ## add first small wave 
  annotation_custom(
    small_wave,
    xmin = as.Date(yearweek("2020 W13")),
    xmax = as.Date(yearweek("2020 W20")),
    ymin = 0,
    ymax = 725) + 
  
  ## add second medium wave 
  annotation_custom(
    medium_wave,
    xmin = as.Date(yearweek("2020 W40")),
    xmax = as.Date(yearweek("2020 W50")),
    ymin = 1000,
    ymax = 3000) + 
  
  ## add third big wave 
  annotation_custom(
    big_wave,
    xmin = as.Date(yearweek("2020 W50")),
    xmax = as.Date(yearweek("2021 W04")),
    ymin = 1000,
    ymax = 4000) +  
  
  


agg_counts$tile_path <- here::here("medical_humanities", "wave_tile.jpg")

ggplot(agg_counts, aes(x = as.Date(week_number), y = cases)) + 
  ggpattern::geom_polygon_pattern( 
    pattern          = "image",
    pattern_type     = "tile",
    pattern_filename  = "C:/Users/Spina/Desktop/exeter_ssu/medical_humanities/wave_tile.jpg")




## to animate 
wave_anim <- wave + 
  gganimate::transition_manual(week_number, cumulative = TRUE)

gganimate::anim_save("testing2.gif", animation = wave_anim)
