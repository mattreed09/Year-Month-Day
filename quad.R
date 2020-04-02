
#install.packages('patchwork')
library(patchwork)

theme_set(theme_tt('Roboto Medium') + theme(plot.margin = margin(0.5,0.5,0.5,0.5,'cm')))

for ( i in country_delta) {
plot_each_country_day %>%
  filter(Name == i) %>%
  mutate(col = country_region_key(hotel_details_hotel_details_address_country)) %>%
  ungroup() %>%
  ggplot( aes(tanker_session_start_date, platform_fields_direct_bookers)) + 
  geom_point(aes(color = col)) +
  geom_line(stat='smooth') + 
  geom_hline(yintercept = 0) +
  # facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
  scale_x_date(breaks = c(min(plot_each_country_day$tanker_session_start_date), 
                          max (plot_each_country_day$tanker_session_start_date) ), date_labels = '%b %d' ) +
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Total Direct Bookers',
       title = paste('The Effect of COVID-19 on', i)) -> book

plot_each_country_day %>%
  filter(Name == i) %>%
  mutate(col = country_region_key(hotel_details_hotel_details_address_country)) %>%
  ungroup() %>%
  ggplot( aes(tanker_session_start_date, platform_fields_searchers)) + 
  geom_point(aes(color = col)) +
  geom_line(stat='smooth') + 
  geom_hline(yintercept = 0) +
  # facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
  scale_x_date(breaks = c(min(plot_each_country_day$tanker_session_start_date), 
                          max (plot_each_country_day$tanker_session_start_date) ), date_labels = '%b %d' ) +
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Total Searches'
       ) -> search


plot_each_country_day %>%
  filter(Name == i) %>%
  mutate(col = country_region_key(hotel_details_hotel_details_address_country)) %>%
  ungroup() %>%
  ggplot( aes(tanker_session_start_date, platform_fields_direct_bookers / platform_fields_searchers)) + 
  geom_point(aes(color = col)) +
  geom_line(stat='smooth') + 
  geom_hline(yintercept = 0) +
  # facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
  scale_x_date(breaks = c(min(plot_each_country_day$tanker_session_start_date), 
                          max (plot_each_country_day$tanker_session_start_date) ), date_labels = '%b %d' ) +
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Conversion Rate'
       ) -> conv


dom_int3 %>%
  filter(Name == i )%>%
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers, group = domestic_or_international))+ 
  geom_point(aes(color = domestic_or_international), alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_line(stat='smooth', aes(color = domestic_or_international), show.legend = F) +
  scale_color_manual(values = tt_colors,  guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(labels = NULL, limits = c(0,NA)) +
  scale_x_date(breaks = c(min(dom_int3$tanker_session_start_date), 
                          max(dom_int3$tanker_session_start_date) ), date_labels =  '%b %d' )+
  theme(legend.position = 'top',
        panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line(),) +
  labs(x = '',
       color = 'Domestic or International',
       y = 'Total Direct Bookers') -> dom


(book + theme(legend.position = 'none') | dom) / (search + theme(legend.position = 'none') + conv  + theme(legend.position = 'none')) 
ggsave(paste0('quad/',i,'_quad.png'), height = 300, width = 500, units = 'mm')

}















