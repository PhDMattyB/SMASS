##############################
## Make SMASS plots great again
##
## Matt Brachmann (PhDMattyB)
##
## 08.10.2024
##
##############################


setwd('C:/Users/phdma/OneDrive/OneDrive - University of Glasgow/Documents/Parsons_Postdoc/')

library(tidyverse)

smass = read_csv('SMASS master detail 1992 - 08102024.csv')

theme_set(theme_bw())

smass %>% 
  separate(col = datefound, 
                   into = c('day', 
                            'month',
                            'year'), 
                   sep = '/') %>% 
  filter(year == 2022) %>%  
  select(class_field, 
         subclass) %>% 
  distinct()

# Plot1 - Barplot ---------------------------------------------------------

plot1_pal = c('#184e77', 
              '#1e6091', 
              '#1a759f',
              '#168aad',
              '#34a0a4',
              '#52b69a',
              '#76c893',
              '#99d98c', 
              '#b5e48c', 
              '#d9ed92', 
              '#d81159', 
              '#f72585')



smass$subclass = factor(smass$subclass, 
                             levels = c('Harbour porpoise', 
                                        'Pelagic delphinid', 
                                        'Mysticete', 
                                        'Sperm/beaked whale', 
                                        'Bottlenose dolphin', 
                                        'Cetacean (indeterminate species)', 
                                        'Grey seal', 
                                        'Harbour seal', 
                                        'Pinniped (Other)', 
                                        'Pinniped (indeterminate species)', 
                                        'Marine turtle', 
                                        'Shark'))

smass$class_field = factor(smass$class_field, 
                        levels = c('Cetacean', 
                                   'Pinniped', 
                                   'Marine Turtle', 
                                   'Shark'))

smass_plot1 = smass %>% 
  separate(col = datefound, 
           into = c('day', 
                    'month',
                    'year'), 
           sep = '/') %>% 
  filter(year == 2022) %>%
  group_by(class_field, 
           subclass) %>% 
  # na.omit() %>% 
  ggplot(aes(x = subclass))+
  geom_bar(col = 'black', 
           aes(fill = subclass))+
  scale_fill_manual(values = plot1_pal)+
  labs(y = 'Number of strandings')+
  facet_grid(~class_field, 
             scales = 'free')+
  theme(panel.grid.major.x = element_blank(), 
        axis.title.x = element_blank(), 
        # axis.text.x = element_text(size = 10, 
        #                            angle = 90),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14), 
        axis.text.y = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12, 
                                  face = 'bold'), 
        legend.title = element_blank())


ggsave('Smass_plot1.tiff', 
       plot = smass_plot1,
       dpi = 'retina', 
       units = 'cm', 
       width = 25, 
       height = 15)


# smass plot2 -------------------------------------------------------------


