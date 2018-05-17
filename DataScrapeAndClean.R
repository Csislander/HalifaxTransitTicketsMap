

library(ggmap)
library(tidyverse)
library(leaflet)
library(dplyr)
library(rvest)
library(cancensus)
library(rgdal)
require(sp)

#####
# Code to scrape the ticket data, grab the shape file and population density data, and clean it up for a graph
#####

# Scrape the url for the ticket seller locations
url <- "https://www.halifax.ca/transportation/halifax-transit/fares-tickets-passes/retail-ticket-sales"

transit_ticket_site <- read_html(url)

location_table_headers <- transit_ticket_site %>%
    html_nodes(".c-table__description") %>%
    html_text(trim = TRUE)

location_tables <- transit_ticket_site %>%
    html_nodes(".tablefield") %>%
    html_table()

for(table_header_index in 1:length(location_table_headers)){
    location_header <- location_table_headers[table_header_index]
    if(location_header != "Other Locations" & location_header != "Municipal Customer Service Centres" & location_header != "Ground transportation Booth"){
       location_tables[[table_header_index]] <- location_tables[[table_header_index]] %>%
          mutate(location_gmap = ifelse(grepl(location_header, Location), Location, paste(location_header, Location)))
    }
    else if(location_header == "Ground transportation Booth"){
      location_tables[[table_header_index]] <- location_tables[[table_header_index]] %>%
        mutate(location_gmap = paste("Stanfield International Airport", Location))
    }

    else{
      location_tables[[table_header_index]] <- location_tables[[table_header_index]] %>%
        mutate(location_gmap = Location)
    }
}



location_tables <- bind_rows(location_tables)
location_tables$lat <- 0
location_tables$long <- 0

#Fix up some names that aren't correct when geocoded
location_tables[c(15,23,34,42,47,48),]$location_gmap <- c("West End Mall Halifax", "St. Mary's University Halifax - Student Info Booth", "Superstore Portland St- Lotto Booth", "The Passage Pharmasave Eastern Passage", "Sobeys Tantallon", "Shoppers Drug Mart Fall River")

# Look up all the geocodes with google maps API
for(place in location_tables$location_gmap){
  geo_place <- geocode(place, output = "all")
  while(geo_place$status != "OK"){
    Sys.sleep(30)
    geo_place <- geocode(place, output = "all")
  }

  location_tables[which(location_tables$location_gmap == place),]$lat <- geo_place$results[[1]]$geometry$location$lat
  location_tables[which(location_tables$location_gmap == place),]$long <- geo_place$results[[1]]$geometry$location$lng
}

# Save the google geocodes so we don't have to use the API again
write_csv(location_tables, "location_tables.csv")

location_tables <- read_csv("location_tables.csv")

# Transit ticket image from google images for each icon
ticketIcon <- makeIcon(iconUrl = "BusTicket.png",
                 iconWidth = 35, iconHeight = 35)


# Now grab canada population and population density data
# See canada census mapper site: https://censusmapper.ca/
options(cancensus.api_key='<census-mapper-key-here>')
census_data <- get_census(dataset='CA16', regions=list(CT="2050123.02",CSD="1209034"), vectors=c("v_CA16_401","v_CA16_406"), labels="detailed", geo_format=NA, level='DA')

# Get halifax district shape files from stats can: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
map_shapes <- readOGR("gda_000b11a_e.shp",
                  GDAL1_integer64_policy = TRUE)
# Keep only the files for halifax area
halifax_map_shapes <- subset(map_shapes, map_shapes$DAUID %in% census_data$GeoUID)

# Merge it with the population data
halifax_map_shapes <- sp::merge(halifax_map_shapes, census_data[,c(1,8,9)], by.x = "DAUID", by.y = "GeoUID")

#save.image("~/HalifaxTransitTickets/TransitTicketsCleanData.RData")
