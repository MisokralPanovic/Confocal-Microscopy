.libPaths('C:/r_packages')
library(tidyverse)
library(data.table)

# change i1_npt_1, IFIT2, RSV_N

#################
# ifit. virus
list_of_colours <- c('#e600e6',
                     '#00d0d0')
breaks_graph <- c(0, 
                  5.5, 
                  1)
plot_title <- 'IFIT1 NPtr 1 Object 1'
################

initial_data <- read.csv(paste(
  'Colocalisation/Data/',
  
  ###############
  'i1_nptr',
  ###############
  
  '.csv',
  sep = ''),
  head = TRUE, 
  sep=",")

i1_npt_1 <- initial_data %>% 
  
  ###########
filter(Condition == 'i1_npt_1',
       Object == 1)
###########

i1_npt_1 %>% group_by(Channel) %>% top_n(1, Intensity)

# change the numerical values bellow last mutate to the max intensity value
i1_npt_1 <- rbind(i1_npt_1 %>% filter(Channel == 'DAPI') %>% mutate(Intensity = Intensity/
                                                                      24.5),
                  i1_npt_1 %>% filter(Channel == 'RSV_NP') %>% mutate(Intensity = Intensity/
                                                                        230),
                  i1_npt_1 %>% filter(Channel == 'IFIT1') %>% mutate(Intensity = Intensity/
                                                                       23.7)) %>% 
  filter(Channel != 'DAPI') %>% 
  mutate(
    Lenght = Lenght*1000000
  )

i1_npt_1

plot_i1_npt_1 <- ggplot(i1_npt_1,
                        aes(x=Lenght, 
                            y = Intensity, 
                            colour = Channel)) +
  geom_line(size = 1.7) +
  scale_colour_manual(values = list_of_colours) +
  theme(
    legend.position="bottom",
    plot.title = element_text(
      size = 20, 
      face = 'bold', 
      margin = margin(10, 0, 10, 0), 
      hjust = 0.5
    ),
    legend.text = element_text(
      size=15),  
    legend.title = element_blank(),
    axis.text.y = element_text(
      angle=0, 
      size=12, 
      vjust=0.5),
    axis.title.x = element_text(
      size = 15, 
      face='bold', 
      vjust=-0.5, 
      margin = margin(0, 10, 0, 0)),
    axis.title.y = element_text(
      size = 15, 
      face='bold', 
      vjust=-0.5, 
      margin = margin(0, 10, 0, 0)),
    axis.text.x=element_text(
      angle=0, 
      size=12, 
      vjust=0.5),
    aspect.ratio = 2/1
  )+
  labs(
    title = plot_title,
    y = 'Relative Intensity',
    x =  'Lenght (um)'
  ) +
  scale_x_continuous(
    breaks= seq(0, 
                7.5, 
                1)
  )

plot_i1_npt_1

###########
dpi <- 600
width <- 10
height <- 18
file_name <- 'i1_npt_1'
###########

ggsave(filename = paste(file_name, '.svg', sep = ''), 
       plot = plot_i1_npt_1, 
       device = 'svg', 
       path = 'Colocalisation/Figures', 
       dpi = dpi, 
       height = height, 
       width = width, 
       units = 'cm')
ggsave(filename = paste(file_name, '.png', sep = ''), 
       plot = plot_i1_npt_1, 
       device = 'png', 
       path = 'Colocalisation/Figures', 
       dpi = dpi, 
       height = height, 
       width = width, 
       units = 'cm')

fwrite(i1_npt_1, 
       paste('Colocalisation/Adjusted-Data/', 
             file_name, 
             '.csv', 
             sep = ''))
