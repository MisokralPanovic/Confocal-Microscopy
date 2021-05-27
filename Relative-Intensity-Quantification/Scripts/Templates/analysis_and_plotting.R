# .libPaths('C:/r_packages')
library(tidyverse)
library(ggsignif)
library(data.table)

###########
# change 'ifit2b', 'IFIT2B', 'MDBK - IFIT2B'
###########

###########
list_of_conditions <- c(
  'Mock',
  'bRSV + bIFN',
  'bRSV dSH + bIFN'
)
old_conditions <- c(
  'Mock',
  'bRSV-N-bIFN',
  'dSH-N-bIFN'
)
list_of_colours <- c(
  '#999999', 
  '#9a6a00', 
  '#009e73'
)
###########

nuclei_standard_ifit2b <- nuclei_data %>% 
  filter(Metadata_Target == 'IFIT2B',
         Metadata_Condition == 'Mock',
         Metadata_Picture == 1)

hist(nuclei_standard_ifit2b$Intensity_MeanIntensity_DNA)

median_control_intensity <- median(nuclei_standard_ifit2b$Intensity_MeanIntensity_DNA)

nuclei_ifit2b <- nuclei_data %>% 
  filter(Metadata_Target == 'IFIT2B') %>% 
  mutate(Control_mean = median_control_intensity,
         Factor = Control_mean / Intensity_MeanIntensity_DNA)

factors <- nuclei_ifit2b$Factor

cytoplasm_ifit2b <- target_data %>% 
  filter(Metadata_Target == 'IFIT2B') %>% 
  mutate(Factor = factors,
         Adj_int = Intensity_MeanIntensity_Target*Factor)

Mock_target_data <- cytoplasm_ifit2b %>% filter(Metadata_Condition == 'Mock')

cytoplasm_ifit2b <- cytoplasm_ifit2b %>% 
  mutate(
    Control_mean = mean(Mock_target_data$Adj_int),
    Value_norm = Adj_int/Control_mean
  )

# rename the condition based on list of conditions

cytoplasm_ifit2b[cytoplasm_ifit2b==old_conditions[2]]<-list_of_conditions[2]
cytoplasm_ifit2b[cytoplasm_ifit2b==old_conditions[3]]<-list_of_conditions[3]

# data analysis -----------

boxplot(Value_norm~Metadata_Condition, cytoplasm_ifit2b)

# test normality

plot(residuals(
  lm(Value_norm~Metadata_Condition, 
     cytoplasm_ifit2b)))
shapiro.test(residuals(
  lm(Value_norm~Metadata_Condition, 
     cytoplasm_ifit2b)))

### non normal distribution

# if non normal distribution
library(car)
leveneTest(Value_norm~Metadata_Condition, 
           cytoplasm_ifit2b)

### non equal variance

# plot data -------------
boxplot(Value_norm~Metadata_Condition, 
        cytoplasm_ifit2b)
cytoplasm_ifit2b
list_of_conditions

###########
range_y <- 20
breaks_y <- 5
plot_title <- 'MDBK - IFIT2B'
y_axis_title <- 'Relative Intensities'
###########

plot_cytoplasm_ifit2b <- ggplot(
  cytoplasm_ifit2b,
  aes(x = Metadata_Condition,
      y = Value_norm,
      fill = Metadata_Condition)) +
  geom_violin(
    trim = FALSE,
    alpha = 0.5,
    scale = 'count',
    adjust = 0.7) +
  stat_summary(
    fun.data = mean_se, 
    fun.args = list(mult=1), 
    geom = "pointrange", 
    color = "black",
    show.legend = F) +
  scale_x_discrete(
    limits = list_of_conditions) +
  scale_fill_manual(
    breaks = list_of_conditions,
    values = list_of_colours) +
  theme(
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
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 15, 
      face='bold', 
      vjust=-0.5, 
      margin = margin(0, 10, 0, 0)),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    aspect.ratio = 2/1
  ) +
  labs(
    title = plot_title,
    y = y_axis_title,
    x = NULL
  ) +
  scale_y_continuous(
    breaks= seq(0, 
                range_y, 
                breaks_y), 
    
    limits = c(0, range_y)) 
  
  
  # geom_signif(
  #   comparisons = list(c(list_of_conditions[1], 
  #                        list_of_conditions[4])), 
  #   annotation = p_val[3], 
  #   y_position = 0.93*range_y - 0*(range_y*0.075), 
  #   tip_length = 0, 
  #   vjust= -0.2, 
  #   size = 0.7, 
  #   textsize = textsize_values[3])


plot_cytoplasm_ifit2b

# saving data and plot ------------------------------

###########
dpi <- 600
width <- 16
height <- 20
file_name <- 'cytoplasm_ifit2b'
###########

ggsave(filename = paste(
  file_name, '.svg', sep = ''), 
  plot = plot_cytoplasm_ifit2b, 
  device = 'svg', 
  path = 'Figures', 
  dpi = dpi, 
  height = height, 
  width = width, 
  units = 'cm')
ggsave(filename = paste(
  file_name, '.png', sep = ''), 
  plot = plot_cytoplasm_ifit2b, 
  device = 'png', 
  path = 'Figures', 
  dpi = dpi, 
  height = height, 
  width = width, 
  units = 'cm')

fwrite(cytoplasm_ifit2b, 
       paste('Adjusted-Data/', 
             file_name, 
             '.csv', 
             sep = ''))
