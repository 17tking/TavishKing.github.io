###########################
# KPI 3: Sales + Locations
##########################
# this script is to be used for analysis, tables, and plotting
#
# Packages
library(tidyverse)  #data manipulation + ggplot2
library(ggthemes)   #extra plot customization
library(scales)     #extra plot customization
library(ggpubr)     #extra plot customization
library(ggimage)    #add images to plot
library(gtsummary)  #tables if needed
library(gt)         #tables if needed
library(geobr)      #spatial data set of Brazil
library(sf)         #map graphing
options(scipen=999) #prevents scientific notation


################################
# EXPLORATORY DATA ANALYSIS KPI3
################################

#loading data
tot_state_sales <- read_csv("SQL/exports_to_R/KPI3_sales_by_location/kpi3_total_state_sales.csv") %>% 
  mutate(total_sales = total_sales/1000000,
         order_count = order_count/1000)

#Top 5 cumulative sales value
tot_state_sales %>% 
  arrange(desc(total_sales)) %>% 
  slice_head(n = 5) %>% 
  summarise(sum_sales_5 = sum(total_sales)) #10.4M

## Choropleth map
brazil_states <- read_state(code_state = "all", year = 2018)

# join brazil states table
tot_state_sales <- brazil_states %>% 
  left_join(tot_state_sales, by = c("abbrev_state" = "customer_state"))

#plotting
kpi3_brazil_sales_map <- ggplot(tot_state_sales)+
  #map of brazil by total sales
  geom_sf(aes(fill = total_sales),
          lwd = 0.5,
          color = "white")+
  #country label in state (e.g., SP, RJ)
  geom_sf_text(aes(label = abbrev_state),
               size = 2.8,
               color = "black")+
  #line + text to SP for total sales annotation
  geom_segment(aes(x= -42,
                   y= -27, 
                   xend= -47, 
                   yend= -24), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 0.9,
               color = "black")+
  annotate("text",
           x = -41.2,
           y = -27,
           label = "R$5.4M in \nsales",
           hjust = 0,
           color = "darkgreen")+
  #insight annotation
  annotate("text", 
           x = -75, 
           y = -22, 
           label = "São Paulo is also \nhome to 60% of \nproduct sellers. \n
These locations may \ncontribute to the state's \nlow delivery time and \nhigh sales.",
           hjust = 0, 
           size = 4, 
           color = "darkgreen")+
  #gradient of colors
  scale_fill_gradientn(colors = c("lightyellow2", "palegreen", "forestgreen"),
                      name = "",
                      label = label_number(suffix = "M"))+
  #adjusting map location on plot
  coord_sf(xlim = c(-76, -34), ylim = c(-33, 4.5), expand = FALSE)+
  #plot theming
  theme_map()+
  theme(#legend adjustments
        legend.position = "top",
        legend.justification = 0.5,
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1.25, "cm"),
        legend.text = element_text(size = 8),
        legend.margin = margin(),
        # Increase title size and adjust alignment
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
  #labels
  labs(title = "Total Sales in Brazil",
       subtitle = "São Paulo leads in revenue (R$)",
       caption = "Data is from 1/2017 to 9/2018")

#save plot
ggsave(kpi3_brazil_sales_map,
       filename = "R/plots/kpi3_brazil_sales_map.jpg",
       height = 5,
       width = 6)

# Proportion of sellers in states
sellers <- read_csv("Tables/processed/clean_sellers.csv")

seller_state_prop <- sellers %>% 
  group_by(seller_state) %>% 
  summarise(count = n()) %>% 
  mutate(seller_prop = round(count / sum(count)*100, 4)) %>% 
  arrange(desc(seller_prop))

###############################
###############################

## Brazil Order Count Plot - Choropleth
#plotting
kpi3_brazil_orders_map <- ggplot(tot_state_sales)+
  #map of brazil by total orders
  geom_sf(aes(fill = order_count),
          lwd = 0.5,
          color = "white")+
  #country label in state (e.g., SP, RJ)
  geom_sf_text(aes(label = abbrev_state),
               size = 2.8,
               color = "black")+
  #insight annotation
  annotate("text", 
           x = -75, 
           y = -22, 
           label = "Order volume \nfollows population, \nbut engagement from \nmore rural areas show \nhigh spending. \n
Lack of access may \nbe inhibiting growth.",
           hjust = 0, 
           size = 4, 
           color = "navajowhite4")+
  #gradient of colors
  scale_fill_gradientn(colors = c("lightyellow2", "skyblue3", "steelblue4"),
                       name = "",
                      label = label_number(suffix = "K"))+
  #adjusting map location on plot
  coord_sf(xlim = c(-76, -34), ylim = c(-33, 4.5), expand = FALSE)+
  #plot theming
  theme_map()+
  theme(#legend adjustments
        legend.position = "top",
        legend.justification = 0.5,
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1.75, "cm"),
        legend.text = element_text(size = 8),
        legend.margin = margin(),
        #Increase title size and adjust alignment
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
  #labels
  labs(title = "Total Orders in Brazil",
       subtitle = "Southeast drives volume, but growth awaits in the North",
       caption = "Data is from 1/2017 to 9/2018")

#save plot
ggsave(kpi3_brazil_orders_map,
       filename = "R/plots/kpi3_brazil_orders_map.jpg",
       height = 5,
       width = 6)

##############################
##############################

## Average Delivery Days for Brazil States - Choropleth Map
kpi3_brazil_delivery_map <- ggplot(tot_state_sales)+
  geom_sf(aes(fill = avg_delivery_days),
          lwd = 0.5,
          color = "white")+
  geom_sf_text(aes(label = abbrev_state),
               size = 2.8,
               color = "black")+
  #insight annotation
  annotate("text", 
           x = -75, 
           y = -22, 
           label = "Northern Brazil is \ndominated by the Amazon \nrainforest, where dense \njungle terrain and limited \nroad infrastructure pose \nsignificant challenges to \ndelivery logistics and access \nto remote areas.",
           hjust = 0, 
           size = 3.2, 
           color = "darkred")+
  #gradient of colors
  scale_fill_gradientn(colors = c("forestgreen", "palegreen2", "yellow3",
                                  "lightcoral", "darkred"),
                       name = "",
                       label = label_number(suffix = " days"))+
  #adjusting map location on plot
  coord_sf(xlim = c(-76, -34), ylim = c(-33, 4.5), expand = FALSE)+
  #plot theming
  theme_map()+
  theme(#legend adjustments
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(),
    #Increase title size and adjust alignment
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
  #labels
  labs(title = "Delivery Speed in Brazil",
       subtitle = "Longer delivery times could limit additional revenue",
       caption = "Data is from 1/2017 to 9/2018")

#save plot
ggsave(kpi3_brazil_delivery_map,
       filename = "R/plots/kpi3_brazil_delivery_map.jpg",
       height = 5,
       width = 6)

####################################
####################################

## Average Order Value by State - Choropleth Map
kpi3_brazil_aov_map <- ggplot(tot_state_sales)+
  geom_sf(aes(fill = aov),
          lwd = 0.5,
          color = "white")+
  geom_sf_text(aes(label = abbrev_state),
               size = 2.8,
               color = "black")+
  #gradient of colors
  scale_fill_gradientn(colors = c("lightyellow2", "coral", "coral4"),
                       name = "",
                       label = label_number(prefix = "$"))+
  #insight annotation
  annotate("text", 
           x = -76, 
           y = -22, 
           label = "Average order values \nin rural states \nare on par with urban \nstates, but delivery times \nare slow (> 2 weeks).",
           hjust = 0, 
           size = 4, 
           color = "coral4")+
  #adjusting map location on plot
  coord_sf(xlim = c(-76, -34), ylim = c(-33, 4.5), expand = FALSE)+
  #plot theming
  theme_map()+
  theme(#legend adjustments
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(),
    #Increase title size and adjust alignment
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
  #labels
  labs(title = "Average Order Values in Brazil",
       subtitle = "Spending in undeserved regions show appetite \nfor expanded e-commerce access",
       caption = "Data is from 1/2017 to 9/2018")

#save plot
ggsave(kpi3_brazil_aov_map,
       filename = "R/plots/kpi3_brazil_aov_map.jpg",
       height = 5,
       width = 6)

#####################################
# SUMMARY: Time to upgrade logistics!
#####################################
