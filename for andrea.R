
lockdowndates = tibble(
  Name = country_delta,
  dates = c('2020-01-23',
            NA,
            '2020-03-10',
            NA,
            '2020-01-30',
            NA,
            '2020-03-16',
            NA,
            '2020-03-24',
            '2020-03-23',
            NA,
            '2020-03-23',
            '2020-03-18',
            '2020-03-17',
            '2020-03-16',
            '2020-03-14',
            NA,
            NA,
            '2020-03-19',
            NA
  ) %>% as.Date(),
  reason = c('Lockdown',
             NA,
             'Lockdown',
             NA,
             'Travel Suspensions',
             NA,
             'Lockdown',
             NA,
             'Olympics Postponed',
             'Lockdown',
             NA,
             'Lockdown',
             'Emergency Declared',
             'Emergency Declared',
             'Lockdown',
             'Lockdown',
             NA,
             NA,
             'Travel Suspensions',
             NA
             
  ) 
  
)


x_length = max(plot_each_country_day$tanker_session_start_date) - as.Date('2020-01-06') 
for (i in levels(plot_each_country_day$country_fct))
{
  plot_data = 
plot_each_country_day %>%
  mutate(col = country_region_key(hotel_details_hotel_details_address_country)) %>%
  ungroup() %>%
    filter(country_fct == i) 
  
  ggplot(plot_data,  aes(tanker_session_start_date, platform_fields_direct_bookers)) + 
    geom_hline(yintercept = 0,  size =1) +
  geom_point(aes(color = col) ,size = 1.5, show.legend = F) +
  geom_line(stat='smooth', size =1) + 
  theme(panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line(),
        text = element_text(size = 20),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
    scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/3) ), date_labels =  '%b %d' )+
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Total Direct Bookers',
       title = paste0('The Effect of COVID-19 on Direct Bookings in ', i)) 

  


ggsave(paste0('andrea/',i,'.png'),  height = 200, width = 275, units = 'mm' )

ggplot(plot_data,  aes(tanker_session_start_date, platform_fields_direct_bookers)) + 
  geom_hline(yintercept = 0,  size =1) +
  geom_point(aes(color = col) ,size = 1.5, show.legend = F) +
  geom_line(stat='smooth', size =1) + 
  theme(panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line(),
        text = element_text(size = 20),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
  scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/3) ), date_labels =  '%b %d' )+
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Total Direct Bookers',
       title = paste0('The Effect of COVID-19 on Direct Bookings in ', i)) + 
  annotate(geom = 'label', label  = lockdowndates %>% filter(Name == i) %>% pull(reason),
           x = lockdowndates %>% filter(Name == i) %>% pull(dates),
           y = max(plot_data$platform_fields_direct_bookers)*0.75,
           label.size = NA,
           family = 'Roboto Medium',
           size = 6,
           hjust = (lockdowndates %>% filter(Name == i) %>% pull(dates)) > as.Date('2020-01-06') + x_length/2)+
  geom_vline(xintercept = lockdowndates %>% filter(Name == i) %>% pull(dates), linetype = 2, size = 1 ) 




ggsave(paste0('andrea/',i,' annotated.png'),  height = 200, width = 275, units = 'mm' )


plot_each_country_day2 %>%
  filter(Name == i) %>%
  ggplot( aes(tanker_session_start_date, platform_fields_direct_bookers, color = page_open_session_device_type)) + 
  geom_hline(yintercept = 0, size = 1) +
  geom_point( alpha = 0.3, size = 1.5) +
  geom_line(stat='smooth', aes(group = page_open_session_device_type), show.legend = F, size = 1) + 
  
  #facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        text = element_text(size = 20),
        legend.position = 'top')+
  scale_color_manual(values = tt_colors, guide = guide_legend(override.aes = list(alpha = 1) )) +
  scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/3) ), date_labels =  '%b %d' )+
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Device type',
       y = 'Total Direct Bookers',
       title = paste0( 'The Effect of COVID-19 on Direct Bookings, split by Device in ', i) )

ggsave(paste0('andrea/device ',i,'.png'),  height = 200, width = 275, units = 'mm' )

dom_int3 %>%
  filter(Name %in% country_delta )%>%
  filter(country_fct == i) %>%
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers, group = domestic_or_international))+ 
  geom_hline(yintercept = 0, size = 1) +
  geom_point(aes(color = domestic_or_international), alpha = 0.3, size = 1.5) +
  #facet_wrap(~country_fct, scales = 'free_y')+
  geom_line(stat='smooth', aes(color = domestic_or_international), show.legend = F, size = 1) +
  scale_color_manual(values = tt_colors,  guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(labels = NULL, limits = c(0,NA)) +
  scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/3) ), date_labels =  '%b %d' )+
  theme(legend.position = 'top',
        text = element_text(size = 20),
        panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line(),) +
  labs(x = '',
       color = 'Domestic or International',
       y = 'Total Direct Bookers',
       title = paste0('Domestic and International Direct Bookings in ', i))

ggsave(paste0('andrea/domestic ',i,'.png'),  height = 200, width = 275, units = 'mm' )

dom_int2 %>%
  mutate(lead = factor(last_search_in_session_client_name_lead_time, levels = c('1-2','3-7','8-28','29+') )) %>%
  group_by(tanker_session_start_date, lead , hotel_details_hotel_details_address_country) %>%
  summarise_if(is.numeric, sum) %>% 
  left_join(read_csv('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv') %>%
              mutate(Name = case_when(Name == 'Korea, Republic of' ~ 'South Korea',
                                      Name == 'Viet Nam' ~ 'Vietnam',
                                      T~Name  )), by = c('hotel_details_hotel_details_address_country' = 'Code') )%>%
  mutate(country_fct = factor(Name, levels = country_delta)) -> lead_andrea

lead_andrea %>%
  filter(country_fct == i) %>%
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers, group = lead))+ 
  geom_hline(yintercept = 0, size = 1) +
  geom_point(aes(color = lead), alpha = 0.3, size = 1.5) +
  #facet_wrap(~country_fct, scales = 'free_y')+
  geom_line(stat='smooth', aes(color = lead), show.legend = F, size = 1) +
  scale_color_viridis_d(option = 'C',  guide = guide_legend(override.aes = list(alpha = 1)), end = 0.8, direction = -1) + 
  scale_y_continuous(labels = NULL, limits = c(0,NA)) +
  scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/3) ), date_labels =  '%b %d' )+
  theme(legend.position = 'top',
        text = element_text(size = 20),
        panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line()) +  
  labs(x = '',
       color = 'Lead Time',
       y = 'Total Direct Bookers',
       title = paste0('Lead Time in ', i))

ggsave(paste0('andrea/lead ',i,'.png'),  height = 200, width = 275, units = 'mm' )

print(i)


}
