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

# Plot1 - Barplot ---------------------------------------------------------


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
                                        'Pinniped (indeterminate species', 
                                        'Marine turtle', 
                                        'Shark'))

# smass$class_field = factor(smass$class_field, 
#                         levels = c('Pinniped', 
#                                    'Cetacean', 
#                                    'Marine turtle', 
#                                    'Shark'))
smass %>% 
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
  geom_bar()+
  labs(y = 'Number of strandings')+
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10, 
                                   angle = 90), 
        axis.title.y = element_text(size = 14), 
        axis.text.y = element_text(size = 12))


# smass %>% 
#   separate(col = datefound, 
#            into = c('day', 
#                     'month',
#                     'year'), 
#            sep = '/') %>% 
#   filter(year == 2022) %>%
#   group_by(class_field, 
#            subclass) %>% 
#   distinct(class_field)
