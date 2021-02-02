library(tidyr)
library(dplyr)
library(sf)
library(stringr)
library(igraph)
library(ggraph)

# diff: For the plot requested by reviewer 3
edges.diff <-
  edges %>%
  spread(date, trips, fill = 0, convert = FALSE) %>% 
  mutate(Wednesday=((`2018-08-08`-`2019-01-23`))) %>% 
  mutate(Saturday=((`2018-08-11`-`2019-01-26`))) %>% 
  select(from, to,time_range, residence, Wednesday, Saturday) %>% 
  gather(date, trips, c(Wednesday,Saturday)) %>% 
  as.data.frame()

# Goupings
edges.salou_by_date <- edges.diff %>%
  group_by(from,to,residence,date) %>%
  summarize(trips = sum(trips)) %>% 
  filter(from==3|from==4|from==5|to==3|to==4|to==5)

# Edges and nodes must be defined first
# This function allows us to create several combinations of plots.
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
    
    #Geographical layout
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
            width = trips, colour = origin, 
            label=format(as.numeric(trips), big.mark=",")
        )) +
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
                        colour = origin,
                        label=format(as.numeric(trips), big.mark=",")
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
      ggtitle(paste0(filter,' mobility patterns (differences summer-winter)')) +
      labs(colour= "Trips from", edge_colour  = "Trips from", edge_width = "Increment of trips (summer - winter)", size="Population")+
      theme(legend.position="bottom",
            panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))+guides(edge_colour=guide_legend(ncol=4,byrow=FALSE,title.position="top"),
                                             edge_width=guide_legend(ncol=4,byrow=FALSE,title.position="top"),
                                             size=guide_legend(ncol=1,byrow=FALSE,title.position="top"))
    
    
    
    return(g)
  }

#Build the flow map and save it to a device
build_ggraph(edges.salou_by_date, nodes, selfloops = TRUE)

ggsave(filename = "img/jpg/full-relative-faceted-graph-with-self-loops.jpg",
       height=210, width=297, units='mm',
       dpi = 300)

ggsave(filename = "img/pdf/full-relative-faceted-graph-with-self-loops.pdf",
       height=210, width=297, units='mm',
       dpi = 300)

