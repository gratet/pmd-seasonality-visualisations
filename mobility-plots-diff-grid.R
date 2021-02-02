

#DATA PREPARATION
#TODO: configure this Rscript to accept filename parameters and return an RDATA file.

# raw data import
library(readxl)
library(dplyr)
library(sf)
library(igraph)
library(ggraph)
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



# diff: asked by reviewer 3
# We add a single trip wen no trips where recorded, so we can calculate the percentage.
library(tidyr)
edges <-
  edges %>%
  spread(date, trips, fill = 1, convert = FALSE) %>% 
  mutate(Wednesday=((`2018-08-08`-`2019-01-23`))) %>% 
  mutate(Saturday=((`2018-08-11`-`2019-01-26`))) %>% 
  select(from, to,time_range, residence, Wednesday, Saturday) %>% 
  gather(date, trips, c(Wednesday,Saturday)) %>% 
  as.data.frame()





# Context
background <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326)

borders_cgc <- st_read(dsn = "data/gis-data.gpkg", layer = 'zones', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  select(label) %>% 
  filter(str_detect(label,"Salou")|str_detect(label,"Vila"))

borders_other <- st_read(dsn = "data/gis-data.gpkg", layer = 'municipalities', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(!str_detect(name,"Salou")&!str_detect(name,"Vila-seca"))

urban <- st_read(dsn = "data/gis-data.gpkg", layer = 'urban', stringsAsFactors = F) %>% 
  st_transform(4326) %>% 
  filter(habitantes>100)

# GROUPINGS
edges.salou_by_date <- edges %>%
  group_by(from,to,residence,date) %>%
  summarize(trips = sum(trips)) %>% 
  filter(from==3|from==4|from==5|to==3|to==4|to==5)

# Edges and nodes must be defined first
build_ggraph<-
  function(edges,nodes,selfloops=FALSE){
    
    require(dplyr)
    
    edges.filtered<-
      edges %>% 
      left_join(nodes,by=c("from"="id")) %>% 
      mutate(origin=label) %>% 
      select(-c(label,x,y)) %>% 
      left_join(nodes,by=c("to"="id")) %>%
      mutate(destination=label) %>% 
      select(-c(label,x,y,starts_with('size'))) %>%
      mutate(curved=
               case_when(
                 from==1&&to==3 ~ TRUE,#tarragona-saloutur
                 from==3&&to==1 ~ TRUE,#saloutur-tarragona
                 from==6&&to==5 ~ TRUE,#tarragona-saloutur
                 from==5&&to==6 ~ TRUE,#saloutur-tarragona
                 TRUE ~ FALSE
               )
      ) %>% 
      mutate(strength=
               case_when(
                 from==3&&to==3 ~ 2,#salou
                 from==4&&to==4 ~ 0,#salou
                 from==5&&to==5 ~ 0,#salou

                 from==1&&to==3 ~ 0.45,#salouturi-tarragona
                 from==3&&to==1 ~ -0.40,#tarragona-saloutur
                 from==5&&to==6 ~ -0.25,#salouturi-tarragona
                 from==6&&to==5 ~ 0.30,#tarragona-saloutur
                 TRUE ~ 0
               )
      ) %>% 
      mutate(direction=
               case_when(
                 from==3&&to==3 ~ 15,
                 from==4&&to==4 ~ 200,
                 from==5&&to==5 ~ 270,
                 TRUE ~ 0
               )
      ) %>% 
      #mutate(period=format(date, '%A, %B %d, %Y')) 
      mutate(period=as.character(date)) 
    
    graph <- 
      graph_from_data_frame(d = edges.filtered,
                            directed = TRUE,
                            vertices = nodes[,c(1,2,3)])
    
    #????
    lo<-as.matrix(nodes[,c(4,5)])

    g <- ggraph(graph, layout=lo) +
      geom_sf(data = background,fill="white",color = "gray85") + 
      geom_sf(data = urban,fill= "gray95",color = NA) +
      geom_sf(data = borders_other,fill=NA,color = "gray85") + 
      geom_sf(data = borders_cgc,fill= NA,color = "gray85") +
      coord_sf(xlim = c(min(nodes$x)-0.01, max(nodes$x)+0.01), ylim = c(min(nodes$y)-0.02, max(nodes$y)+0.01))

        
    if (selfloops==TRUE) {
      g <-
        g +
        geom_edge_loop(alpha = .4,
                       angle_calc = 'along',
                       label_dodge = unit(1, 'mm'),
                       label_push = unit(1, 'mm'),
                       label_size=2,
                       start_cap = circle(3, 'mm'),
                       end_cap = circle(3, 'mm'),
                       arrow = arrow(length = unit(1, 'mm')),
                       aes(width = trips, colour = origin,
                           span=120,strength = 0.05,direction=direction,
                           label=format(as.numeric(trips), big.mark=",")
                       ))
    }
    
    g<-
      g + 
      geom_edge_parallel(
        alpha = .4,
        sep = unit(2, "mm"),
        angle_calc = 'along',
        label_dodge = unit(1, 'mm'),
        label_push = unit(1, 'mm'),
        label_size=2.25,
        start_cap = circle(3, 'mm'),
        end_cap = circle(3, 'mm'),
        arrow = arrow(length = unit(1, 'mm')),
        aes(filter=curved==FALSE,
            width = trips, colour = origin#, 
            # label=format(as.numeric(trips), big.mark=",")
        )) +
      # scale_colour_manual(values = c("steelblue","dodgerblue","firebrick","gold3","tomato","deepskyblue","mediumslateblue")) +
      # scale_edge_color_manual(values = c("steelblue","dodgerblue","firebrick","gold3","tomato","deepskyblue","mediumslateblue")) +
      # scale_colour_manual(values = c("steelblue","dodgerblue","tomato","tomato","tomato","deepskyblue","mediumslateblue")) +
      # scale_edge_color_manual(values = c("steelblue","dodgerblue","tomato","tomato","tomato","deepskyblue","mediumslateblue")) +
      # scale_colour_manual(values = c("steelblue","steelblue","tomato","tomato","tomato","steelblue","steelblue")) +
      # scale_edge_color_manual(values = c("steelblue","steelblue","tomato","tomato","tomato","steelblue","steelblue")) +
      geom_edge_arc(alpha = .4,
                    strength = filter(edges.filtered,curved==TRUE)$strength,
                    angle_calc = 'along',
                    label_dodge = unit(1, 'mm'),
                    label_push = unit(1, 'mm'),
                    label_size=2.25,
                    start_cap = circle(3, 'mm'),
                    end_cap = circle(3, 'mm'),
                    arrow = arrow(length = unit(1, 'mm')),
                    aes(filter=curved==TRUE,
                        width = trips, 
                        colour = origin#,
                        # label=format(as.numeric(trips), big.mark=",")
                    )) +
      geom_node_point(aes(colour = label, size=size))+
      #geom_node_text(aes(label=label, size=size))+
      scale_size(range = c(2,6),breaks = c(10000,30000,100000),labels = c("10,000","30,000","100,000")) +
      facet_grid(period ~ residence)
    
    
    if (selfloops==TRUE) {
      g<-
        g + scale_edge_width(range = c(0.5, 5.5),
                             #limits = c(0, 76000),
                             breaks=c(0,500,1000,2000, 5000,10000,10000,15000),
                             labels=c("0","500","1000","2000", "5000","10000","10000","15000")
        )
    } else {
      g<-
        g + scale_edge_width(range = c(0.5, 5.5)#,
                             #limits = c(0, 15000),
                             #breaks=c(500,2500,5000,10000,15000)
        )
    }
    
    
    g<-
      g + theme_graph(base_family = 'Helvetica') +
      # ggtitle(paste0(filter,' mobility patterns (differences summer-winter)')) +
      labs(colour= "Trips from", edge_colour  = "Trips from", edge_width = "Increment of trips (summer - winter)", size="Population")+
      theme(legend.position="bottom",
            panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))+guides(edge_colour=guide_legend(ncol=4,byrow=FALSE,title.position="top"),
                                             edge_width=guide_legend(ncol=4,byrow=FALSE,title.position="top"),
                                             size=guide_legend(ncol=1,byrow=FALSE,title.position="top"))
    
    
    
    return(g)
  }


build_ggraph(edges.salou_by_date, nodes, selfloops = TRUE)

ggsave(filename = "dist/img/jpg/full-relative-faceted-graph-with-self-loops.jpg",
       height=210, width=297, units='mm',
       dpi = 300)

ggsave(filename = "dist/img/pdf/full-relative-faceted-graph-with-self-loops.pdf",
       height=160, width=250, units='mm',
       dpi = 300)

