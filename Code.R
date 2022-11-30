rm(list=ls())
library(tidyverse)
library(countrycode)
library(sysfonts)
library(showtext)
font_add_google("Mukta", "title_font")
showtext_auto()

title_font <- "title_font"
bg_color <- '#F2E7D5'

# Read data
df <- read.csv('Gold importation/data.csv', sep=',')

# Data preparation
df$Quantity..kg. <- as.numeric(gsub("'","",df$Quantity..kg.))
df$Value..CHF. <- as.numeric(gsub("'","",df$Value..CHF.))
df_plot <- df %>%
  filter(Period=='_Year', 
         Year==2021, 
         Key...text=='mined gold (according to the "Explanatory notes")',
         Land!="_ALL") %>%
  mutate(Quantity=Quantity..kg., Value=Value..CHF.) %>%
  select(Land,Quantity,Value)


# ISO Country code
df_plot[df_plot$Land=='Guiana, French',1] <- 'French Guiana'
df_plot[df_plot$Land=="CÃ´te d'Ivoire",1] <- "Ivory Coast"

df_plot <- df_plot %>%
  mutate(Land=countryname(Land, destination='country.name.en'))
df_plot[df_plot$Land=='United States',1] <- "United States of America"
df_plot[df_plot$Land=='Côte d’Ivoire',1] <- "Ivory Coast"
df_plot[df_plot$Land=='Tanzania',1] <- "United Republic of Tanzania"
df_plot[df_plot$Land=='Hong Kong SAR China',1] <- "Hong Kong S.A.R."

# Longitude & Latitude
library(maps)
library(rworldmap)
world_map <- getMap()
worldmap_df <- fortify(world_map)
worldmap_df <- worldmap_df %>%
  left_join(.,y=df_plot,by=c("id"="Land"))
worldmap_df <- worldmap_df %>%
  mutate(color=ifelse(id=='Switzerland','#FFE15D',
                      ifelse(id %in% df_plot$Land,
                             ifelse(Quantity>100000,"#8E3200",
                                    ifelse(Quantity>50000,"#D1512D",
                                           ifelse(Quantity>10000,"#D7A86E","#FFEBC1"))),"White")))




# Countries shape
country_shapes <- geom_polygon(data=worldmap_df[worldmap_df$id!='Antarctica',], 
                               aes(long,lat,group=group, fill=color),
                               show.legend=FALSE, colour='black')


library(tidygraph)
library(ggraph)
switzerland_coord <- worldmap_df %>%
  filter(id=="Switzerland") %>%
  group_by(id) %>%
  summarize(
    to_x_val=mean(long),
    to_y_val=mean(lat)
  ) %>%
  as.data.frame()

nodes <- worldmap_df %>%
  filter(id %in% df_plot$Land) %>%
  group_by(id) %>%
  summarise(
    from_x=mean(long),
    from_y=mean(lat)
  ) %>%
  mutate( to_x=switzerland_coord [1,2],
          to_y=switzerland_coord [1,3]) %>%
  right_join(.,y=df_plot,by=c("id"="Land")) %>%
  filter(Quantity >1000)

nodes[nodes$id=="Canada",2] <- -109.65690
nodes[nodes$id=="Canada",3] <- 58.7774
nodes[nodes$id=="United States of America",2] <- -104.763089
nodes[nodes$id=="United States of America",3] <- 41.895224
nodes[nodes$id=="Peru",2] <- -76.20029
nodes[nodes$id=="Peru",3] <- -9.03343
nodes[nodes$id=="Chile",2] <- -70.63807
nodes[nodes$id=="Chile",3] <- -27.96627
nodes[nodes$id=="Laos",2] <- 103.28998
nodes[nodes$id=="Laos",3] <- 19.50777


as_tibble(df_plot) %>%
  select(Land, Quantity) %>%
  arrange(desc(Quantity)) %>%
  top_n(20) %>%
  tableGrob(
    rows=NULL,
    cols = c("Country","Quantity [kg]"),
    theme=ttheme_default(
      base_size = 13,
      core=list(
        fg_params=list(
          fontfamily=font_rc,
          col=c(rep("#8E3200",3),rep("#D1512D",3),rep("#D7A86E",14)),
          hjust=c(rep(0,20), rep(0.5,20)),
          x=c(rep(0.05,20), rep(.5,20))
        )
      )
    )
  ) ->tbl

library(gridExtra)
title <- textGrob(expression(bold(underline("Top 20 (by quantity) Mining gold origin"))),
                  y=0.95,vjust=0.5, gp=gpar(fontsize=15, fontface='bold', fontfamily=title_font))
gt <- gTree(children=gList(tbl,title))

# 1- Plan + curve
ggplot()+
  country_shapes+scale_fill_identity()+
  geom_curve(data=nodes,
             aes(x=from_x, y=from_y, xend=to_x, yend=to_y, size=Quantity/200000, color=200000/Quantity),
             curvature=.5,show.legend = F, angle=120, position="identity",alpha=.8)+
  coord_equal()+
  theme_dark()+
  theme(
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 18,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,20,0)),
    plot.subtitle = element_text(family = title_font,
                                 face = "bold",
                                 color = "black",
                                 size = 14,
                                 lineheight = 1,
                                 hjust = 0.5),
    # CAPTION
    plot.caption=element_text(color="#393E46",
                              family = title_font,
                              size=12,
                              hjust=1),
    axis.text=element_blank(),
        axis.title = element_blank())+
  labs(
    title="Origin of the mining gold refined in Switzerland",
    subtitle="Year 2021",
    caption="Source: Swiss federal office for customs and border security"
  )+
  annotation_custom(gt, xmin = -220, xmax = -100, ymin = -50)


showtext_opts(dpi = 320) 
ggsave("Gold importation/Plan+curve.png", height = 10, width = 20, dpi=320)  
showtext_auto(FALSE)


# 2- Plan
ggplot()+
  country_shapes+scale_fill_identity()+
  coord_equal()+
  theme_dark()+
  theme(
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 20,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,20,0)),
    plot.subtitle = element_text(family = title_font,
                                 face = "bold",
                                 color = "black",
                                 size = 15,
                                 lineheight = 1,
                                 hjust = 0.5),
    # CAPTION
    plot.caption=element_text(color="#393E46",
                              family = title_font,
                              size=12,
                              hjust=1),
    axis.text=element_blank(),
    axis.title = element_blank()
    )+
  labs(
    title="Origin of the mining gold refined in Switzerland",
    subtitle="Year 2021",
    caption="Source: Swiss federal office for customs and border security"
  )+
  annotation_custom(gt, xmin = -220, xmax = -100, ymin = -50) #, xmin = -16920565, xmax = -14000000,  ymin=761378/2.25, ymax = 761378

showtext_opts(dpi = 320) 
ggsave("Gold importation/Plan.png", height = 10, width = 20, dpi=320)  
showtext_auto(FALSE)



# 3- Spheric plot
ggplot()+
  country_shapes+scale_fill_identity()+
  coord_map('ortho', orientation=c(48,10,0))+theme_dark()




# 4- SF data
ne_countries(scale="medium", returnclass="sf") %>%
  filter(name!="Antarctica") -> world


ggplot() +
  geom_sf(data = world, size = 0.125, fill = "#3B454A", color = "#b2b2b2") +
  geom_curve(
    data = nodes, aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = Quantity, size = Quantity),
    curvature = 0.2, arrow = arrow(length = unit(10, "pt"), type = "closed"),
  ) +
  guides(
    color = guide_legend(reverse = TRUE)
  ) +
  annotation_custom(tbl, xmin = -16920565, xmax = -14000000,  ymin=761378/2.25, ymax = 761378) + # values are in eqarea meters
  coord_sf(crs = "+proj=eqearth +wktext") +
  scale_color_distiller(
    palette = "RdYlBu", trans = "log10", name = "(Size & color\nlog10 scale)", label = scales::comma,
    breaks = c(30, 50, 100, 300, 1000), limits = c(15, 150000)
  ) +
  scale_size_continuous(
    trans = "log10", range = c(0.75, 3),
    breaks = c(30, 50, 100, 300, 1000), limits = c(15, 150000),
    guide = FALSE
  ) +
  theme_ft_rc(grid="") +
  labs(
    x = NULL, y = NULL,
    title = "Top 20 Swiss mining products importation by country",
    subtitle = "Year 2021",
    caption = "Data source: Swiss Impex database"
  ) +
  theme_ft_rc(grid="") +
  theme(legend.key.height = unit(2.8, "lines")) +
  theme(legend.position = c(0.2, 0.3)) +
  theme(axis.text.x = element_blank())



