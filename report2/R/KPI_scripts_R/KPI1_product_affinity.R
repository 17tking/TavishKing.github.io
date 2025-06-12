###########################
# KPI 1: Product Affinity
##########################
# this script is to be used for analysis, tables, and plotting
#
# Packages
library(tidyverse)
library(ggthemes)
library(scales)


#################################
# multiple and single categories
#################################
multiple_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_multiple_categories.csv")
single_categories <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_single_categories.csv")
#notes:
# - 785 unique orders with multiple product category purchases
# - no orders had greater than 3 different categories purchased
# - 2541 orders have multiple different product_id's from the same category (intra-category diversity)


# Stacked Bar Chart - show proportion of multiple category purchases
pct_variety <- multiple_categories %>%
  count(productcatcount) %>%
  mutate(percentage = n / sum(n) * 100,
         productcatcount = as.factor(productcatcount))

kpi1_multiple_category_plot <- ggplot(pct_variety,
       aes(x=reorder(productcatcount, percentage), y=percentage, fill=productcatcount))+
  geom_bar(stat="identity",
           width = 0.6)+
  geom_text(label = "Only 2.3% of multi-category orders\n contain 3 product categories",
            hjust = -0.15,
            size = 5)+
  scale_y_continuous(labels = label_number(suffix = "%"),
                     breaks = seq(0, 100, 25),
                     limits = c(0, 100))+
  scale_fill_manual(values = c("gray60", "red2"))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=10, face="italic"))+
  labs(
    title = "Breakdown of Orders with Multiple Product Categories",
    x = "",
    y = "")

ggsave(plot = kpi1_multiple_category_plot,
       filename = "R/plots/kpi1_multiple_category_plot.jpg",
       height = 5,
       width = 6)


# Bar Plot - average number of distinct products per category
avg_15_products <- single_categories %>% 
  group_by(product_category_english) %>% 
  summarise(mean_products = round(mean(productid_count),2)) %>% 
  arrange(desc(mean_products)) %>% 
  slice(1:15) %>% 
  mutate(focus_products = if_else(rank(-mean_products) < 5, "focus", "other"))


kpi1_single_category_plot <- ggplot(avg_products,
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

ggsave(plot = kpi1_single_category_plot, 
       filename = "R/plots/kpi1_single_category_plot.jpg",
       height = 5,
       width = 8)



avg_all_products <- single_categories %>% 
  group_by(product_category_english) %>% 
  summarise(mean_products = round(mean(productid_count),2)) %>% 
  arrange(desc(mean_products))

# Sum of mean products for all categories
total_variety <- sum(avg_all_products$mean_products)

# Sum of mean products for the top 3 most diverse categories
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
# pair counts
##############
paircounts <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_paircounts.csv")



#################
# support values
#################
support_values <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_support_values.csv")
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


################
# top cat pairs
################
top_cat_pairs <- read_csv("SQL/exports_to_R/KPI1_product_affinity/kpi1_top_cat_pairs.csv")
