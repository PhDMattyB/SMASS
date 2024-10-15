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
library(lubridate)

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

plot2_pal = c('#d00000', 
              '#3f88c5', 
              '#ffba08', 
              '#136f63')

smass$class_field = factor(smass$class_field, 
                           levels = c('Cetacean', 
                                      'Pinniped', 
                                      'Shark',
                                      'Marine Turtle'))

smass %>% 
  separate(col = datefound, 
           into = c('day', 
                    'month',
                    'year'), 
           sep = '/') %>%
  select(class_field,
         subclass, 
         year) %>% 
  ggplot(aes(x = year, 
             fill = class_field))+
  geom_bar()+
  scale_fill_manual(values = plot2_pal)+
  labs(y = 'Number of strandings')+
  geom_hline(yintercept = 703)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, 
                                   angle = 90),
       axis.title.y = element_text(size = 14), 
       axis.text.y = element_text(size = 12), 
       axis.title.x = element_blank(), 
       legend.title = element_blank())


# smass plot 3 ------------------------------------------------------------

cum_data = smass %>% 
  select(class_field, 
         subclass, 
         datefound) %>%
  group_by(datefound) %>% 
  summarize(n = n())%>%
  separate(col = datefound,
           into = c('day',
                    'month',
                    'year'),
           sep = '/',
           remove = F) %>% 
  arrange(year, 
          month,
          day) %>%
  unite(col = 'day_month',
        day:month,
        sep = '-') %>% 
  group_by(year) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  filter(year %in% c('2018', 
                     '2019', 
                     '2020', 
                     '2021', 
                     '2022', 
                     '2023')) 
# %>% 
#   factor(day_month, 
#          levels = unique(day_month))
  
cum_data$day_month = factor(cum_data$day_month, 
                             levels = unique(cum_data$day_month)) 

cum_data$datefound2 = as.Date.factor(cum_data$datefound, 
                                         format = "%d/%m/%y") 
cum_pal = c('#f94144', 
            '#f8961e', 
            '#f9c74f', 
            '#90be6d', 
            '#43aa8b', 
            '#277da1')

ggplot(data = cum_data, 
       aes(x = datefound2, 
           y = cum_sum))+
  geom_point(aes(col = year), 
             alpha = 0.2)+
  scale_x_date(date_breaks = '1 month', 
               date_minor_breaks = '1 day', 
               date_labels = '%B',
               expand = c(0,0),
               limits = c(as.Date("2020-01-01"), 
                          as.Date("2020-12-31")))+
  scale_y_continuous(limits = c(0,1000), 
                     breaks = c(100, 
                                200, 
                                300, 
                                400, 
                                500, 
                                600, 
                                700, 
                                800, 
                                900, 
                                1000))+
  labs(y = 'Cumulative number of strandings')+
  scale_color_manual(values = cum_pal)+
  theme(axis.text.x = element_text(size = 12, 
                                   angle = 90),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(), 
        legend.title = element_blank(), 
        legend.position = c())
  # geom_line(aes(col = year))



# Scottish map ------------------------------------------------------------

library(maps)

scot = map_data('world') %>% 
  as_tibble() %>% 
  # group_by(region) %>% 
  # select(region) %>% 
  # distinct() %>% View()
  filter(region == 'UK') 
#   filter(subregion == 'Scotland')

ggplot(data = scot) +
  geom_map(data = scot, 
           map = scot, 
           aes(x = long, 
               y = lat, 
               map_id = region), 
           col = 'black',
           size = 0.5,
           fill = 'white')+
  coord_fixed(ratio = 1.3, 
              xlim = c(-8,
                       0), 
              ylim = c(55, 
                       59))+
  theme(panel.grid = element_blank(), 
        axis.title = element_blank()
        # axis.text = element_blank(),
        )


# Plot1 - Year by year variation ---------------------------------------------------------

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

 smass_year = smass %>% 
  separate(col = datefound, 
           into = c('day', 
                    'month',
                    'year'), 
           sep = '/') %>% 
  # filter(year == 2022) %>%
   filter(year %in% c(1992,
                      2002,
                      2012,
                      2022)) %>% 
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
  facet_wrap(~year)+
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


