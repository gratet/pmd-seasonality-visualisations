# install.packages("ggalluvial")
library(ggalluvial)
library(stringr)
library(scales)

edges.by_type<-
  edges %>% 
  left_join(nodes,by=c("from"="id")) %>% 
  mutate(origin=label) %>% 
  select(-c(label,x,y)) %>% 
  left_join(nodes,by=c("to"="id")) %>%
  mutate(destination=label) %>% 
  ungroup() %>% 
  select(residence,origin,destination,date,time_range,trips) %>%
  filter(str_detect(origin, "Salou")) %>% 
  mutate(type=case_when(
    str_detect(destination, "Salou") ~ "Intraurban",
    !str_detect(destination, "Salou") ~ "Interurban"
  )) %>% 
  mutate(origin=str_replace(origin, "Salou - ", "")) %>% 
  mutate(destination=str_replace(destination, "Salou - ", "")) %>% 
  mutate(origin=case_when(
    origin=="PortAventura" ~ "PA",
    origin=="Residential" ~ "Res.",
    origin=="Touristic" ~ "Tour."
  )) %>% 
  mutate(destination=case_when(
    destination=="PortAventura" ~ "PA",
    destination=="Residential" ~ "Res.",
    destination=="Touristic" ~ "Tour.",
    destination=="Cambrils" ~ "C",
    destination=="Reus" ~ "R",
    destination=="Tarragona" ~ "T",
    destination=="Vila seca" ~ "VS"
  ))

lapply(unique(edges.by_type$date),
       function(x) {
         fn<-gsub(x = tolower(x),pattern = " ",replacement = "")
         
         ggplot(filter(edges.by_type,date==fn),
                aes(y = trips, axis1 = origin, axis2=destination)) +
           facet_grid(type~time_range)+
           geom_alluvium(aes(fill = residence), width = 2/12, na.rm = TRUE) +
           geom_stratum(width = 2/12, alpha = 1.0,fill = "black", color = "grey", na.rm = TRUE) +
           geom_text(stat = "stratum", size=3, color="white", aes(label = after_stat(stratum))) +
           scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
           scale_fill_brewer(name="Residence",type = "qual", palette = "Set1") +
           ylab("Trips") + theme(plot.title = element_text(hjust = 0.5)) +
           theme(legend.position = "top")
         
         # ggsave(filename = paste0("dist/img/tiff/",fn,"-faceted-alluvials.tiff"),
         #        height=170, width=247, units='mm',
         #        dpi = 300)
         ggsave(filename = paste0("dist/img/png/",fn,"-faceted-alluvials.png"),
                height=210, width=297, units='mm',
                dpi = 300)
       })


##BY HAND

week_summer <- 
  ggplot(filter(edges.by_type,date=="2018-08-08"),
       aes(y = trips, axis1 = origin, axis2=destination)) +
  facet_grid(time_range~type)+
  geom_alluvium(aes(fill = residence), width = 2/12, na.rm = TRUE) +
  geom_stratum(width = 2/12, alpha = 1.0,fill = "black", color = "grey", na.rm = TRUE) +
  geom_text(stat = "stratum", size=3, color="white", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
  scale_fill_brewer(name="Residence",type = "qual", palette = "Set1") +
  ylab("Trips") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,40000) +
  scale_y_continuous(label=comma) +
  ggtitle(as.character(format(as.Date("2018-08-08"),'%A, %B %d, %Y')))+
  theme(legend.position = "bottom")



weekend_summer<-
  ggplot(filter(edges.by_type,date=="2018-08-11"),
       aes(y = trips, axis1 = origin, axis2=destination)) +
  facet_grid(time_range~type)+
  geom_alluvium(aes(fill = residence), width = 2/12, na.rm = TRUE) +
  geom_stratum(width = 2/12, alpha = 1.0,fill = "black", color = "grey", na.rm = TRUE) +
  geom_text(stat = "stratum", size=3, color="white", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
  scale_fill_brewer(name="Residence",type = "qual", palette = "Set1") +
  ylab(" ") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,40000)+
  scale_y_continuous(label=comma) +
  ggtitle(as.character(format(as.Date("2018-08-11"),'%A, %B %d, %Y')))+
  theme(legend.position = "bottom")

prow <-
  plot_grid(week_summer + theme(legend.position="none"),
            weekend_summer + theme(legend.position="none"),
            labels = c("A","B"), ncol = 2)

# extract a legend that is laid out horizontally
plegend <- 
  get_legend(
    week_summer + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "top")
  )

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
plot <- 
  plot_grid(plegend, prow, 
            ncol = 1, rel_heights = c(.05, 1))

ggsave(plot,filename = paste0("dist/img/png/summer-faceted-alluvials.png"),
       height=210, width=297, units='mm',
       dpi = 300)


# Winter mosaic
week_winter <- 
  ggplot(filter(edges.by_type,date=="2019-01-23"),
         aes(y = trips, axis1 = origin, axis2=destination)) +
  facet_grid(time_range~type)+
  geom_alluvium(aes(fill = residence), width = 2/12, na.rm = TRUE) +
  geom_stratum(width = 2/12, alpha = 1.0,fill = "black", color = "grey", na.rm = TRUE) +
  geom_text(stat = "stratum", size=3, color="white", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
  scale_fill_brewer(name="Residence",type = "qual", palette = "Set1") +
  ylab("Trips") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(label=comma,limits=c(0,11750)) +
  ggtitle(as.character(format(as.Date("2019-01-23"),'%A, %B %d, %Y')))+
  theme(legend.position = "bottom")



weekend_winter<-
  ggplot(filter(edges.by_type,date=="2019-01-26"),
         aes(y = trips, axis1 = origin, axis2=destination)) +
  facet_grid(time_range~type)+
  geom_alluvium(aes(fill = residence), width = 2/12, na.rm = TRUE) +
  geom_stratum(width = 2/12, alpha = 1.0,fill = "black", color = "grey", na.rm = TRUE) +
  geom_text(stat = "stratum", size=3, color="white", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
  scale_fill_brewer(name="Residence",type = "qual", palette = "Set1") +
  ylab(" ") + 
  scale_y_continuous(label=comma,limits=c(0,11750)) +
  ggtitle(as.character(format(as.Date("2019-01-26"),'%A, %B %d, %Y')))+
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))

prow <-
  plot_grid(week_winter + theme(legend.position="none"),
            weekend_winter + theme(legend.position="none"),
            labels = c("A","B"), ncol = 2)

# extract a legend that is laid out horizontally
plegend <- 
  get_legend(
    week_winter + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "top")
  )

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
plot <- 
  plot_grid(plegend, prow, 
            ncol = 1, rel_heights = c(.1, 1))

ggsave(plot,filename = paste0("dist/img/png/winter-faceted-alluvials.png"),
       height=210, width=297, units='mm',
       dpi = 300)