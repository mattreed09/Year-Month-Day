
library(tidyverse)
library(TTplot)
library(geofacet)

#setwd('..')
theme_set(theme_tt('Roboto Medium'))

file_name = 'bq-results-20200401-082106-5ey0kghintes.csv'

googledrive::drive_download(file_name)


  
country_region_key = function(x)case_when(x %in% c( 'KG', 'TJ', 'TM', 'UZ', 'CN', 'HK', 'JP', 'KP', 'KR', 'MO', 'MN', 'TW', 'BN', 'KH', 'ID', 'LA', 'MY', 'MM', 'PH', 'SG', 'TH', 'TL', 'VN', 'BD', 'BT', 'IN', 'MV', 'NP', 'PK', 'LK', 'AU', 'NZ', 'NF', 'FJ', 'NC', 'PG', 'SB', 'VU', 'GU', 'KI', 'MH', 'FM', 'NR', 'MP', 'PW', 'AS', 'CK', 'PF', 'NU', 'PN', 'WS', 'TK', 'TO', 'TV', 'WF')
                                          ~ 'APAC',
                                          x %in% c( 'BI', 'KM', 'DJ', 'ER', 'ET', 'KE', 'MG', 'MW', 'MU', 'YT', 'MZ', 'RE', 'RW', 'SC', 'SO', 'SS', 'TZ', 'UG', 'ZM', 'ZW', 'AO', 'CM', 'CF', 'TD', 'CG', 'CD', 'GQ', 'GA', 'ST', 'DZ', 'EG', 'LY', 'MA', 'SD', 'TN', 'EH', 'AX', 'DK', 'EE', 'FO', 'FI', 'GG', 'IS', 'IE', 'IM', 'JE', 'LV', 'LT', 'NO', 'SJ', 'SE', 'GB', 'BW', 'LS', 'NA', 'ZA', 'SZ', 'AF', 'IR', 'AL', 'AD', 'BA', 'HR', 'GI', 'GR', 'VA', 'IT', 'MK', 'MT', 'ME', 'PT', 'SM', 'RS', 'SI', 'ES', 'BJ', 'BF', 'CV', 'CI', 'GM', 'GH', 'GN', 'GW', 'LR', 'ML', 'MR', 'NE', 'NG', 'SH', 'SN', 'SL', 'TG', 'AM', 'AZ', 'BH', 'CY', 'GE', 'IQ', 'IL', 'JO', 'KW', 'LB', 'OM', 'PS', 'QA', 'SA', 'SY', 'TR', 'AE', 'YE', 'AT', 'BE', 'FR', 'DE', 'LI', 'LU', 'MC', 'NL', 'CH', 'BY', 'BG', 'CZ', 'HU', 'MD', 'PL', 'RO', 'RU', 'SK', 'UA')
                                          ~ 'EMEA',
                                          x %in% c( 'AI', 'AG', 'AW', 'BS', 'BB', 'BQ', 'KY', 'CU', 'CW', 'DM', 'DO', 'GD', 'GP', 'HT', 'JM', 'MQ', 'MS', 'PR', 'BL', 'KN', 'LC', 'MF', 'VC', 'SX', 'TT', 'TC', 'VG', 'VI', 'BZ', 'CR', 'SV', 'GT', 'HN', 'NI', 'PA', 'AR', 'BO', 'BR', 'CL', 'CO', 'EC', 'FK', 'GF', 'GY', 'PY', 'PE', 'SR', 'UY', 'VE')
                                          ~ 'LATAM',
                                          x %in% c( 'MX', 'CA', 'US', 'BM', 'GL', 'PM') ~ 'NORAM')


tidyup <- function(plot = last_plot()){
  ggpubr::ggarrange(plot + theme(plot.margin = margin(2,2,0,2,'cm')),
                    grid::grobTree(grid::rasterGrob(png::readPNG('ttlogo2.png'), x = 0.9)),
                    ncol = 1,
                    heights = c(1, 0.05)) 
}

country_delta = c("China", "South Korea", "Italy", "Vietnam", "Hong Kong", "Singapore", 
                  "France", "Thailand", "Japan", "Germany", "Philippines", "United Kingdom", 
                  "Portugal", "Indonesia", "Switzerland",  "Spain", #"Maldives",
                  "United States", "Malaysia", "Australia", "Mexico")
  
day_searches <- read_csv(file_name) %>% janitor::clean_names() %>% 
  filter(tanker_page_open_client_name != 'EXTENDEDSTAY') %>%
  filter(tanker_session_start_date < Sys.Date()) %>%
  mutate(week = lubridate::floor_date(tanker_session_start_date-1, unit = 'week')+1 ) %>%
  left_join(read_csv('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv') %>%
              mutate(Name = case_when(Name == 'Korea, Republic of' ~ 'South Korea',
                                      Name == 'Viet Nam' ~ 'Vietnam',
                                      T~Name  )), by = c('hotel_details_hotel_details_address_country' = 'Code') )  %>%
  filter(Name %in% country_delta)



day_searches  %>%
  filter(week == min(week)) %>%
  select(tanker_page_open_client_name) %>%
  distinct() -> keep_day

day_searches  %>%
  filter(week == '2020-03-02') %>%
  select(tanker_page_open_client_name) %>%
  distinct() -> keep_day_too

plot_each_country_day <- day_searches %>%
  inner_join(keep_day) %>%
  inner_join(keep_day_too) %>%
  group_by(Name, hotel_details_hotel_details_address_country, tanker_session_start_date) %>%
  summarise(platform_fields_direct_bookers = sum(platform_fields_direct_bookers),
            platform_fields_searchers = sum(platform_fields_searchers)) %>%
  mutate(country_fct = factor(Name, levels = country_delta ) ) 

plot_each_country_day %>%
  mutate(col = country_region_key(hotel_details_hotel_details_address_country)) %>%
  ungroup() %>%
  ggplot( aes(tanker_session_start_date, platform_fields_direct_bookers)) + 
  geom_point(aes(color = col)) +
  geom_line(stat='smooth') + 
  geom_hline(yintercept = 0) +
  facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        legend.position = 'top') +
  scale_color_viridis_d(option = 'C', begin = 0, end = 0.7) + 
  scale_x_date(breaks = c(min(plot_each_country_day$tanker_session_start_date), 
                          max (plot_each_country_day$tanker_session_start_date) ), date_labels = '%b %d' ) +
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Region',
       y = 'Total Direct Bookers',
       title = 'The Effect of COVID-19 on Direct Bookings') -> x1


ggsave(paste0('total bookers ', format(Sys.Date(), '%b%d'), '.png'),
       tidyup(x1),  height = 350, width = 500, units = 'mm' )


plot_each_country_day2 <- day_searches %>%
  inner_join(keep_day) %>%
  inner_join(keep_day_too) %>%
  group_by(Name, hotel_details_hotel_details_address_country, tanker_session_start_date, page_open_session_device_type) %>%
  summarise(platform_fields_direct_bookers = sum(platform_fields_direct_bookers),
            platform_fields_searchers = sum(platform_fields_searchers)) %>%
  mutate(country_fct = factor(Name, levels = country_delta ) ) 


plot_each_country_day2 %>%
  ggplot( aes(tanker_session_start_date, platform_fields_direct_bookers, color = page_open_session_device_type)) + 
  geom_point( alpha = 0.3) +
  geom_line(stat='smooth', aes(group = page_open_session_device_type), show.legend = F) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~country_fct, scales='free_y') + 
  theme(panel.spacing.x = unit(1,'cm'),
        legend.position = 'top')+
  scale_color_manual(values = tt_colors, guide = guide_legend(override.aes = list(alpha = 1) )) +
  scale_x_date(breaks = c(min(plot_each_country_day2$tanker_session_start_date),
                          max (plot_each_country_day2$tanker_session_start_date) ), date_labels = '%b %d' ) +
  scale_y_continuous(limits = c(0,NA), labels = NULL) + # 
  labs(x = '',
       color = 'Device type',
       y = 'Total Direct Bookers',
       title = 'The Effect of COVID-19 on Direct Bookings, split by Device') 

ggsave(paste0('device type ', format(Sys.Date(), '%b%d'), '.png'),
       tidyup(),
       height = 350, width = 500, units = 'mm' )


day_searches %>%
  inner_join(keep_day) %>%
  inner_join(keep_day_too) -> dom_int2

dom_int2 %>%
  group_by(tanker_session_start_date, domestic_or_international, hotel_details_hotel_details_address_country) %>%
  summarise_if(is.numeric, sum) %>% 
  left_join(read_csv('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv') %>%
              mutate(Name = case_when(Name == 'Korea, Republic of' ~ 'South Korea',
                                      Name == 'Viet Nam' ~ 'Vietnam',
                                      T~Name  )), by = c('hotel_details_hotel_details_address_country' = 'Code') )%>%
  mutate(country_fct = factor(Name, levels = country_delta)) -> dom_int3


dom_int3 %>%
  filter(Name %in% country_delta )%>%
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers, group = domestic_or_international))+ 
  geom_point(aes(color = domestic_or_international), alpha = 0.3) +
  geom_hline(yintercept = 0) +
  facet_wrap(~country_fct, scales = 'free_y')+
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
       y = 'Total Direct Bookers',
       title = 'Domestic and International Direct Bookings')

ggsave(paste0('dom int ',format(Sys.Date(), '%b%d'),'.png'),
       tidyup(),  height = 350, width = 500, units = 'mm' )


states <- dom_int2 %>%
  filter(tanker_session_start_date != Sys.Date()) 

states %>%
  filter(hotel_details_hotel_details_address_country == 'US') %>%
  left_join((tibble(state = c(state.abb, str_to_upper(state.name) ), name = rep(state.abb,2) )))  %>%
  mutate(name = case_when(toupper(state) %in% c('D.C.', 'DISTRICT OF COLOMBIA') ~ 'DC',
                          toupper(state) %in% c('PR','PUERTO RICO') ~ 'PR',
                          T ~ name) ) %>%
  filter(!name %in% c('PR','SD') & !is.na(name)) %>%
  group_by(tanker_session_start_date, hotel_details_hotel_details_address_country, name, domestic_or_international) %>%
  summarise_if(is.numeric, sum) %>% 
  left_join(read_csv('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv') %>%
              mutate(Name = case_when(Name == 'Korea, Republic of' ~ 'South Korea',
                                      Name == 'Viet Nam' ~ 'Vietnam',
                                      T~Name  )), by = c('hotel_details_hotel_details_address_country' = 'Code') )%>%
  mutate(country_fct = factor(Name, levels = country_delta)) -> state3



state3 %>%
  filter(domestic_or_international == 'domestic') %>%
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers))+ 
  geom_point( alpha = 0.3, aes(color = name), show.legend = F) +
  geom_hline(yintercept = 0) +
  geofacet::facet_geo(~name, scales = 'free_y')+
  geom_line(stat='smooth', show.legend = F) +
  #scale_color_manual(values = tt_colors,  guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_viridis_d(end = 0.7, option = 'C') +
  scale_y_continuous(labels = NULL, limits = c(0,NA)) +
  scale_x_date(breaks = c(min(states$tanker_session_start_date), 
                          max(states$tanker_session_start_date) ), date_labels =  '%b %d' )+
  theme(legend.position = 'top',
        panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line(),) +
  labs(x = '',
       color = 'Device Type',
       title = 'Domestic Bookers in the US',
       y = 'Total Direct Bookers')

ggsave(paste0('state domestic ',format(Sys.Date(), '%b%d'),'.png'),
       height = 350, width = 500, units = 'mm' )

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
  ggplot(aes(tanker_session_start_date, platform_fields_direct_bookers, group = lead))+ 
  geom_hline(yintercept = 0, size = 1) +
  geom_point(aes(color = lead), alpha = 0.3, size = 1.5) +
  facet_wrap(~country_fct, scales = 'free_y')+
  geom_line(stat='smooth', aes(color = lead), show.legend = F, size = 1) +
  scale_color_viridis_d(option = 'C',  guide = guide_legend(override.aes = list(alpha = 1)), end = 0.8, direction = -1) + 
  scale_y_continuous(labels = NULL, limits = c(0,NA)) +
  scale_x_date(breaks = as.Date('2020-01-06')+ round(x_length*seq(0,1,1/1) ), date_labels =  '%b %d' )+
  theme(legend.position = 'top',
        text = element_text(size = 20),
        panel.spacing.x = unit(1,'cm'),
        axis.ticks.x = element_line()) +  
  labs(x = '',
       color = 'Lead Time',
       y = 'Total Direct Bookers')

ggsave(paste0('lead times ',format(Sys.Date(), '%b%d'),'.png'),
       tidyup(),
       height = 350, width = 500, units = 'mm' )



