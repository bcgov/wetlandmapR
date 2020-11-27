# SCRIPT TO POLYGONIZE, CLEAN, and SMOOTH RASTER OUTPUT (in tiles)
# To do: add functionnality to stitch tiles using "summarize"
# To do: compare smoothing results

#  *****REQUIRES DEV VERSION OF STARS**** devtools::install_github("r-spatial/stars")

library(smoothr)
library(dplyr)
library(stars)
library(sf)

#### RASTER TO POLYGON FUNCTION ####

# READ CLASSIFIED RASTER
  r <- stars::read_stars('data/out/3CategoryPrediction/20190926-103025_map_recl.tif', proxy = F)
  d <- st_dimensions(r)
  int <- 1000
  x_seq <- seq(d$x$from,d$x$to+int,int)
  
# CLEANING FUNCTION (PER TILE)
  ras_to_clean_pol <- function(seq_along = 1,
                               out_folder = "data/clean/",
                               out_format = "kml",
                               wetland_class_number = c(2),
                               min_area_drop_m2 = 625 * 4,
                               max_area_fill_m2 = 625 * 4,
                               raw_out = F,
                               clean_out = F,
                               smooth_out = T,
                               smooth_method = "chaikin",
                               return = T
                               ){
    
    write.csv("", paste0(out_folder,seq_along,"_start.txt"))
    # CLIP MY TILE
    my_tile <- r[1,x_seq[seq_along]:x_seq[seq_along+1],] %>% st_as_stars()
    # print("ok subset")
    
    # POLYGONIZE (AND DISSOLVE/MERGE BY CLASS)
    p <- sf::st_as_sf(my_tile,
                      as_points = F,
                      merge = T)
    # print("ok poly")
    
    # RENAME FIELDS
      names(p) <- c("class","geometry")
  
    # SELECT WETLANDS OF INTEREST
    p <- p %>%
      filter(class %in% wetland_class_number)
  
    # CALCULATE AREA (m2) (based on trim 25x25 data, so 625 m2 is one pixel)
    p <- p %>%
      mutate(area = st_area(.))
    # print("ok area")
    
    if(raw_out == T){
      write_sf(p, paste0(out_folder,seq_along,"_raw.",out_format))
      # print("ok raw")
      }
  
    # FILTER AREA (i.e. DROP CRUMBS)
    p <- p %>%
      filter(area > units::as_units(min_area_drop_m2, "m2"))
      # print("ok crumbs")
      
    # FILL HOLES
    p <- p %>%
      fill_holes(threshold = units::as_units(max_area_fill_m2, "m2"))
      # print("ok holes")
      
    # WRITE CLEAN
    if(clean_out == T){
      write_sf(p, paste0(out_folder,seq_along,"_clean.",out_format))
      # print("ok clean")
      }
    
    # SMOOTH POLYGONS
    if(smooth_out == T){
      p_sm <- smooth(p, method = smooth_method)
      write_sf(p_sm, paste0(out_folder,seq_along,"_smooth.",out_format))
      # print("ok smooth")
      }
    
    if(return == "clean"){
      return(p)
    }
    if(return == "smooth"){
      return(p_sm)
    }
  }

pol <- do.call(rbind, lapply(1:length(x_seq), ras_to_clean_pol))
