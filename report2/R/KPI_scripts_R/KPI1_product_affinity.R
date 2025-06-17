###########################
# KPI 1: Product Affinity
##########################
# this script is to be used for analysis, tables, and plotting
#
# Packages
library(tidyverse)  #data manipulation + ggplot2
library(ggthemes)   #extra plot customization
library(scales)     #extra plot customization
library(ggpubr)     #extra plot customization
library(ggimage)      #add images to plot
library(gtsummary)  #tables
library(gt)         #tables
options(scipen=999) #prevents scientific notation


################################
# EXPLORATORY DATA ANALYSIS KPI1
################################
# note: this info comes from SQL and is used to help
#       narrate the product affinity analysis
#
# - 99,439 distinct orders
# - avg items per order = 1.20
# - 785 orders with multiple different product categories

### top 10 highest selling categories
#loading data frame
top_highest_selling <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_10_highest_selling.csv") %>% 
  mutate(salecount = round(salecount/1000, 2))

kpi1_10_highest <- ggplot(top_highest_selling,
       aes(x = reorder(product_category_english, salecount),
           y = salecount))+
  geom_col(fill = "dodgerblue3")+
  geom_text(aes(label = label_number(suffix = "K")(salecount)), 
            hjust = 1.5, 
            size = 3.3,
            color = "white")+
  scale_y_continuous(labels = label_number(suffix = "K"),
                     breaks = seq(0, 12.5, 2.5),
                     limits = c(0,12.5))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"),
        axis.text = element_text(size = 10))+
  labs(title = "Top 10 Highest Selling Categories",
       subtitle = "*Canceled orders were not included",
       x = "",
       y = "")

#saving plot
ggsave(kpi1_10_highest, 
       filename = "R/plots/kpi1_10_highest.jpg",
       height = 5,
       width = 8)

### top 10 lowest selling categories
#loading data frame
top_lowest_selling <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_10_lowest_selling.csv")

kpi1_10_lowest <- ggplot(top_lowest_selling,
       aes(x = reorder(product_category_english, 10:1),
           y = salecount))+
  geom_col(fill = "darkgoldenrod1")+
  geom_text(aes(label = salecount), 
            hjust = 1.5, 
            size = 3.3,
            color = "black")+
  scale_y_continuous(breaks = seq(0, 35, 5),
                     limits = c(0, 35))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"),
        axis.text = element_text(size = 10))+
  labs(title = "Top 10 Lowest Selling Categories",
       subtitle = "*Canceled orders were not included",
       x = "",
       y = "")

#saving plot
ggsave(kpi1_10_lowest,
       filename = "R/plots/kpi1_10_lowest.jpg",
       height = 5,
       width = 8)


### identify orders with multiple distinct product categories
# loading data frames
multiple_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_multiple_categories.csv")

# Table showing proportion of multi-category purchases by product count
pct_variety <- multiple_categories %>%
  count(productcatcount) %>%
  mutate(percentage = n / sum(n) * 100,
         productcatcount = as.factor(productcatcount))

# Horizontal Bar Chart
kpi1_multiple_category_plot <- ggplot(pct_variety,
                                      aes(x=reorder(productcatcount, percentage), 
                                          y=percentage, 
                                          fill=productcatcount))+
  geom_bar(stat="identity",
           width = 0.6)+
  geom_text(label = "Only 2.3% of orders contain 3 \n different product categories",
            hjust = -0.19,
            size = 5)+
  scale_y_continuous(labels = label_number(suffix = "%"),
                     breaks = seq(0, 100, 25),
                     limits = c(0, 100))+
  scale_fill_manual(values = c("gray60", 
                               "red2"))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"))+
  labs(title = "Breakdown of Orders with Multiple Product Categories",
       x = "",
       y = "")

# saving plot
ggsave(plot = kpi1_multiple_category_plot,
       filename = "R/plots/kpi1_multiple_category_plot.jpg",
       height = 5,
       width = 8)


### identify single product categories wit diff product id's
#loading data frame
single_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_single_categories.csv")

# Table of average number of products in purchases with single categories (Top 10 highest)
avg_10_products <- single_categories %>% 
  group_by(product_category_english) %>% 
  summarise(mean_products = round(mean(productid_count),2)) %>% 
  arrange(desc(mean_products)) %>% 
  slice(1:10)

# Horizontal Bar Plot
kpi1_single_category_plot <- ggplot(avg_10_products,
                                    aes(x=reorder(product_category_english, mean_products), 
                                        y=mean_products))+
  geom_bar(stat = "identity",
           fill = "darkslategray4")+
  geom_text(aes(label = round(mean_products, 2)), 
            hjust = 1.5, 
            size = 3.3,
            color = "white")+
  scale_y_continuous(breaks = seq(0, 3, 0.5),
                     limits = c(0, 3))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"))+
  labs(
    title = "Top 10 Categories with Highest Product Presence",
    subtitle = "Average amount of products from category found per order",
    x = "",
    y = "Avg. Unique Products per Order")

# saving plot
ggsave(plot = kpi1_single_category_plot, 
       filename = "R/plots/kpi1_single_category_plot.jpg",
       height = 5,
       width = 8)


### top 10 product category pairings
# loading data frames
paircounts <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_paircounts.csv")
top_cat_pairs <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_top_cat_pairs.csv")

# top 10 highest bought product category pairs
paircounts_top10 <- paircounts %>% 
  mutate(pairlabel = paste(cat1, "&", cat2)) %>% 
  arrange(desc(paircount)) %>% 
  slice(1:10)

# Horizontal Bar plot
kpi1_top_category_pairs <- ggplot(paircounts_top10,
                                  aes(x=reorder(pairlabel, paircount), 
                                      y=paircount))+
  geom_bar(stat="identity",
           fill = "palegreen4")+
  geom_text(aes(label = paircount),
            hjust = 1.5,
            size = 3.5,
            color = "white")+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"))+
  labs(title = "Top Product Category Pairings",
       subtitle = "The 10 most common combinations present in a customer's 'basket'",
       x = "",
       y = "Number of Orders")

# saving plot
ggsave(plot = kpi1_top_category_pairs,
       filename = "R/plots/kpi1_top_category_pairs.jpg",
       height = 5,
       width = 8)

# support and lift values for top 10 pairs
top_10_supportpair_table <- top_cat_pairs %>% 
  arrange(desc(pairtransactioncount)) %>% 
  slice(1:10) %>% 
  mutate(CategoryA = category_a,
         CategoryB = category_b,
         Support = round(supportab*100, 4),
         Lift = lift) %>% 
  select(-c(1:7))


# Heat Table
gt_supportpair_table <- top_10_supportpair_table %>% 
  gt() %>% 
  data_color(
    columns = c(Lift),
    colors = scales::col_numeric(palette = c("seashell1", 
                                             "darkgreen"),
                                 domain = range(top_10_supportpair_table$Lift))) %>% 
  data_color(
    columns = c(Support),
    colors = scales::col_numeric(palette = c("seashell1", 
                                             "darkgreen"),
                                 domain = range(top_10_supportpair_table$Support))) %>% 
  tab_header(
    title = md("**Home Decor dominates paired transactions**"),
    subtitle = md("Support = % of transactions \n 
Lift = pairing strength")) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything()))

#manually saved table for easier formatting...(used screenshot)
gt_supportpair_table


### support values for individual categories
# popularity of single categories
# loading data frame
support_values <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_support_values.csv")

# cumulative %
cum_support <- sum(support_values$support_pct)

# top 10 %
top_support <- support_values %>% 
  arrange(desc(support_pct)) %>% 
  slice_head(n=10) %>% 
  summarise(total_support = sum(support_pct)) %>% 
  pull(total_support)

# Identify top-10 cats by support
top_cats <- support_values %>% 
  arrange(desc(support_pct)) %>% 
  slice_head(n = 10)

# all 65 other categories
other_cats <- support_values %>% 
  filter(!(product_category %in% top_cats$product_category)) %>% 
  summarise(product_category = "all other 65 categories",
            transactioncount = sum(transactioncount),
            support_pct = sum(transactioncount * support_pct) / sum(transactioncount)) #weighted mean

# combine into one table
topsupport_single_categories <- bind_rows(top_cats, other_cats) %>%
  mutate(label = if_else(product_category == "all other 65 categories", 
                         "others", 
                         "top"))


# Figure 1-horizontal bar chart
kpi1_cat_popularity <- ggplot(topsupport_single_categories, 
                              aes(x=reorder(product_category, 11:1), 
                                  y=support_pct,
                                  fill = label))+
  geom_col()+
  geom_text(aes(label = support_pct),
            hjust = 1.5,
            size = 3.5,
            color = "white")+
  geom_bracket(xmin = "bed_bath_table",
               xmax = "toys",
               y.position = 12,
               tip.length = c(0.03, 0.15),
               label = "These categories are found 
    in 62.7% of orders",
               vjust = 4.5,
               hjust = -0.27)+
  coord_flip()+
  scale_fill_manual(values = c("top"="goldenrod3", 
                               "others"="gray50"))+
  scale_y_continuous(labels = label_number(suffix = "%"),
                     breaks = seq(0, 40, 5),
                     limits = c(0,40))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", 
                                  size=14),
        plot.subtitle = element_text(size=10, 
                                     face="italic"))+ 
  labs(title = "Top Individual Categories",
       subtitle = "The 10 most common categories present in a customer's 'basket'",
       x="",
       y="Support",
       caption = "Note: Support is a measure of how frequently \n an item or itemset appears in transactions")

#saving plot
ggsave(kpi1_cat_popularity,
       filename = "R/plots/kpi1_cat_popularity.jpg",
       height = 5,
       width = 8)


#################
# MAIN ANALYSIS
#################
# this table was loaded in the 'pair counts' section :)
# I chose pairings greater than 10 occurrences

# top 10 lift value data frame/table
top10_liftpairs_table <- top_cat_pairs %>% 
  slice_head(n=10) %>% 
  mutate(CategoryA = category_a,
         CategoryB = category_b,
         Frequency = pairtransactioncount,
         Support = round(supportab*100, 2),
         Lift = lift) %>% 
  select(c(8:12))

# Heat Table for top 10 lift values
gt_liftpairs_table <- top10_liftpairs_table %>% 
  gt() %>% 
  data_color(
    columns = c(Lift),
    colors = scales::col_numeric(palette = c("white", 
                                             "dodgerblue4"),
                                 domain = range(top10_liftpairs_table$Lift))) %>%
  data_color(
    columns = c(Frequency),
    colors = scales::col_numeric(palette = c("white", 
                                             "darkgoldenrod2"),
                                 domain = range(top10_liftpairs_table$Frequency))) %>%
  tab_header(
    title = md("**Lift is high, but frequency is low**"),
    subtitle = md("Support = % of transactions \n 
Lift = pairing strength")) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())) %>% 
  tab_footnote("Note: Only pairings with greater than 10 purchases were included 
               for statistical relevance.")

# saving table manually
gt_liftpairs_table

