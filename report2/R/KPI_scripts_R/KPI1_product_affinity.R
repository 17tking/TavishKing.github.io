###########################
# KPI 1: Product Affinity
##########################
# this script is to be used for analysis and plotting
#
# Packages
library(tidyverse)
library(ggthemes)
library(DBI)
library(RPostgres)


#connect to SQL data base to import tables
con <- dbConnect(Postgres(),
                 dbname = "Brazilian E-Commerce",
                 host = "localhost",
                 port = "5432",
                 user = "postgres",
                 password = "Ecclesiastes4:8")

#######################
# KPI 1 Table Cleaning
######################
# no cleaning necessary. Cancelled orders were removed within the 
# query. All blanks belonged to product categories, but they had no 
# effect on the descriptive analysis because of so few observations.

#################
# Support Values
#################
support_values <- dbReadTable(con, "support_values")

# cumulative %
cum_support <- sum(support_values$support)*100

# top 10 %
top_support <- support_values %>% 
  arrange(desc(support)) %>% 
  slice_head(n=10) %>% 
  summarise(total_support = sum(support)*100) %>% 
  pull(total_support)

#support % variable
support_values <- support_values %>% 
  mutate(support_pct = support*100) %>% 
  mutate(top_category = if_else(rank(-support_pct) <=10, "top", "other"))

# Figure 1-horizontal bar chart
ggplot(support_values, aes(x=reorder(product_category, support_pct), y=support_pct, fill=top_category))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = c("top"="dodgerblue4", "other"="gray40"))+
  labs(x="",
       y="")+
  theme_few()+
  theme(legend.position = "none")
