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

#################################
# MULTIPLE AND SINGLE CATEGORIES
#################################

# loading data frames
multiple_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_multiple_categories.csv")
single_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_single_categories.csv")

# NOTES:
# - 785 unique orders with multiple product category purchases
# - no orders had greater than 3 different categories purchased
# - 2541 orders have multiple different product_id's from the same category (intra-category diversity)


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
  geom_text(label = "Only 2.3% of multi-category orders\n contain 3 product categories",
            hjust = -0.15,
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
       width = 6)


# Table of average number of products in purchases with single categories (Top 15 highest)
avg_15_products <- single_categories %>% 
  group_by(product_category_english) %>% 
  summarise(mean_products = round(mean(productid_count),2)) %>% 
  arrange(desc(mean_products)) %>% 
  slice(1:15) %>% 
  mutate(focus_products = if_else(rank(-mean_products) < 5, "focus", "other"))

# Horizontal Bar Plot
kpi1_single_category_plot <- ggplot(avg_15_products,
       aes(x=reorder(product_category_english, mean_products), 
           y=mean_products, 
           fill = focus_products))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(mean_products, 2)), 
            hjust = 1.5, 
            size = 3.3,
            color = "white")+
  scale_fill_manual(values = c("focus" = "dodgerblue3", 
                               "other" = "gray60"))+
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
    title = "Top 15 Categories with Highest Product Diversity per Order",
    subtitle = "Avg. # of unique products bought per order (single-category orders only)",
    x = "",
    y = "Avg. Unique Products per Order")

# saving plot
ggsave(plot = kpi1_single_category_plot, 
       filename = "R/plots/kpi1_single_category_plot.jpg",
       height = 5,
       width = 8)


# Table of average number of products in orders with single categories (all categories)
avg_all_products <- single_categories %>% 
  group_by(product_category_english) %>% 
  summarise(mean_products = round(mean(productid_count),2)) %>% 
  arrange(desc(mean_products))

# Sum of mean products for all categories
total_variety <- sum(avg_all_products$mean_products)

# Sum of mean products for the top 4 most diverse categories
top4_variety <- avg_all_products %>%
  arrange(desc(mean_products)) %>%
  slice(1:4) %>%
  summarise(top4_sum = sum(mean_products)) %>%
  pull(top4_sum)

# Calculate percent
top4_pct <- (top4_variety / total_variety) * 100

# View result
top4_pct
paste0("These top 4 categories account for ", round(top4_pct, 1), "% of all intra-category variety.")


##############
# PAIR COUNTS
##############

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
  annotate(geom = "text",
           label = "What do the Support and \nLift values tell us?",
           x = 4,
           y = 50,
           size = 4.5)+
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
                                 domain = range(top_10_pair_table$Lift))) %>% 
  data_color(
    columns = c(Support),
    colors = scales::col_numeric(palette = c("seashell1", 
                                             "darkgreen"),
                                 domain = range(top_10_pair_table$Support))) %>% 
  tab_header(
    title = md("**Home Decor dominates paired transactions**"),
    subtitle = md("Support = % of transactions \n 
Lift = pairing strength")) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything()))

#manually saved table for easier formatting...(used screenshot)
gt_supportpair_table


#################
# SUPPORT VALUES
#################

# popularity of single categories
# loading data frame
support_values <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_support_values.csv")

# cumulative %
cum_support <- sum(support_values$support)

# top 10 %
top_support <- support_values %>% 
  arrange(desc(support)) %>% 
  slice_head(n=10) %>% 
  summarise(total_support = sum(support)) %>% 
  pull(total_support)

# Identify top-10 cats by support
top_cats <- support_values %>% 
  arrange(desc(support)) %>% 
  slice_head(n = 10)

# all 65 other categories
other_cats <- support_values %>% 
  filter(!(product_category %in% top_cats$product_category)) %>% 
  summarise(product_category = "all other 65 categories",
            transactioncount = sum(transactioncount),
            support = sum(transactioncount * support) / sum(transactioncount)) #weighted mean

# combine into one table
topsupport_single_categories <- bind_rows(top_cats, other_cats) %>%
  mutate(label = if_else(product_category == "all other 65 categories", 
                         "others", 
                         "top"))


# Figure 1-horizontal bar chart
kpi1_cat_popularity <- ggplot(topsupport_single_categories, 
       aes(x=reorder(product_category, 11:1), 
           y=support,
           fill = label))+
  geom_col()+
  geom_text(aes(label = support),
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
     y="")

#saving plot
ggsave(kpi1_cat_popularity,
       filename = "R/plots/kpi1_cat_popularity.jpg",
       height = 5,
       width = 8)

################
# TOP LIFT PAIRS
################
# this table was loaded in the 'pair counts' section :)
# I chose pairings greater than 1 occurrence

# top 10 lift value data frame/table
top10_liftpairs_table <- top_cat_pairs %>% 
  slice_head(n=10) %>% 
  mutate(CategoryA = category_a,
         CategoryB = category_b,
         Frequency = pairtransactioncount,
         Support = round(supportab*100, 4),
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
    columns = c(Support),
    colors = scales::col_numeric(palette = c("white", 
                                             "dodgerblue4"),
                                 domain = range(top10_liftpairs_table$Support))) %>%
  data_color(
    columns = c(Frequency),
    colors = scales::col_numeric(palette = c("white", 
                                             "dodgerblue4"),
                                 domain = range(top10_liftpairs_table$Frequency))) %>%
  tab_header(
    title = md("**Lift is high, but occurrences are low**"),
    subtitle = md("Support = % of transactions \n 
Lift = pairing strength")) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything()))

# saving table manually
gt_liftpairs_table
