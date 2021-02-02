#DATA PREPARATION
#TODO: configure this Rscript to accept filename parameters and return an RDATA file.

# raw data import
library(readxl)
library(dplyr)
library(sf)
library(stringr)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

source_data <- read_excel_allsheets(filename = "data/20190709_matrices_OD_Tarragona - municipios.xlsx")

# get dates from sheet names
od_matrices <- source_data[grep("MAT", names(source_data))]
od_matrices<-
  lapply(od_matrices, 
         function(x) 
         {
           result<-x[!(names(x) %in% c("Índice"))]
           names(result)<-result[2, ]
           result = result[-1:-3, ]
           return(result)
         }
  )

od_dates<-as.Date(gsub("[^[:digit:]. ]","",names(od_matrices)), "%Y%m%d") 
for (d in seq_along(od_dates)) {
  od_matrices[[d]]$date<-od_dates[d]
}


od_matrix<-bind_rows(od_matrices)
colnames(od_matrix)<-c("from", "to", "time_range", "residence", "trips", "date")
od_matrix$trips<-as.integer(od_matrix$trips)
od_matrix<-od_matrix %>% 
  mutate(time_range=
           case_when(
             time_range =="P1" ~ "06:00 - 11:00",
             time_range =="P2" ~ "11:00 - 14:00",
             time_range =="P3" ~ "14:00 - 22:00",
             time_range =="P4" ~ "22:00 - 06:00"
           )
  ) %>% 
  mutate(residence=
           case_when(
             residence == "extranjero" ~ "Tourists - Internationals",
             residence == "resto_Espana" ~ "Tourists - Spaniards",
             residence == "resto_Tarragona" ~ "Locals",
             TRUE ~ "Locals"
           )
  ) %>% 
  group_by(from,to,time_range,residence,date) %>% 
  summarise(trips =sum(trips))

edges<-od_matrix

rm(list=setdiff(ls(), "edges"))

nodes <- st_read(dsn = "data/gis-data.gpkg", layer = 'nodes', stringsAsFactors = F) %>% 
  select(c("ID","label","size")) %>% 
  rename(id=ID) %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  st_transform(4326) %>% 
  cbind(st_coordinates(.)) %>% 
  rename(x=X,y=Y) %>% 
  ungroup()

st_geometry(nodes) <- NULL

# Geographical context
background <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326)

borders_cgc <- st_read(dsn = "data/gis-data.gpkg", layer = 'zones', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  select(label) %>% 
  filter(str_detect(label,"Salou")|str_detect(label,"Vila"))

borders_other <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(!str_detect(NOMMUNI,"Salou")&!str_detect(NOMMUNI,"Vila-seca"))

urban <- st_read(dsn = "data/gis-data.gpkg", layer = 'urban', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(habitantes>100)

