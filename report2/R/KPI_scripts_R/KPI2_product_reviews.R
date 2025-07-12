###########################
# KPI 2: Product Reviews
##########################
# this script is to be used for analysis, tables, and plotting
#
# Packages
library(tidyverse)  #data manipulation + ggplot2
library(ggthemes)   #extra plot customization
library(ggridges)   #for ridge line plots
library(scales)     #extra plot customization
library(ggpubr)     #extra plot customization
library(gtsummary)  #tables
library(gt)         #tables
library(MASS)       #ordinal logistic regression functions
library(Hmisc)      #data analysis
library(reshape2)   #reshaping data
library(car)        #check multicollinearity
library(brant)      #test of proportional odds
options(scipen=999) #prevents scientific notation



################################
# EXPLORATORY DATA ANALYSIS KPI2
################################


### top 10 highest reviewed product categories
#loading data
top_10_highest_reviewed <- read_csv("SQL/exports_to_R/KPI2_product_reviews/kpi2_10_highest_reviewed.csv")


# horizontal bar chart
kpi2_10_highest_reviewed <- ggplot(top_10_highest_reviewed,
       aes(x = reorder(product_category_english, avg_review_score),
           y = avg_review_score))+
  geom_col(fill = "seagreen4")+
  geom_text(aes(label = label_number(suffix = " \u2605")(avg_review_score)),
            hjust = 1.2,
            size = 4,
            color = "white",
            fontface = "bold")+
  scale_y_continuous(breaks = seq(0, 5, 1),
                     limits = c(0,5))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Top 10 Highest Reviewed Categories",
       subtitle = "These categories are earning an average rating above 4 stars",
       x = "",
       y = "Rating",
       caption = "Note: only products with > 50 reviews were used")

#saving plot
ggsave(kpi2_10_highest_reviewed,
       filename = "R/plots/kpi2_10_highest_reviewed.jpg",
       height = 5,
       width = 8)


### top 10 lowest reviewed product categories
#loading data
top_10_lowest_reviewed <- read_csv("SQL/exports_to_R/KPI2_product_reviews/kpi2_10_lowest_reviewed.csv")

# horizontal bar chart
kpi2_10_lowest_reviewed <- ggplot(top_10_lowest_reviewed,
       aes(x = reorder(product_category_english, 10:1),
           y = avg_review_score))+
  geom_col(fill = "indianred4")+
  geom_text(aes(label = label_number(suffix = " \u2605")(avg_review_score)),
            hjust = 1.2,
            size = 4,
            color = "white",
            fontface = "bold")+
  scale_y_continuous(breaks = seq(0, 5, 1),
                     limits = c(0,5))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Top 10 Lowest Reviewed Categories",
       subtitle = "Not low, but close to being problematic",
       x = "",
       y = "Category Rating",
       caption = "Note: only products with > 50 reviews were used")

#saving plot
ggsave(kpi2_10_lowest_reviewed,
       filename = "R/plots/kpi2_10_lowest_reviewed.jpg",
       height = 5,
       width = 8)


### Distribution of Review Scores
#load data
review_dist <- read_csv("SQL/exports_to_R/KPI2_product_reviews/kpi2_review_dist.csv") %>% 
  mutate(review_score = as.factor(review_score))

# bar graph
kpi2_review_dist <- ggplot(review_dist,
       aes(x = review_score,
           y = review_score_prop,
           fill = review_score))+
  geom_col()+
  geom_text(aes(label = label_number(suffix = "%")(review_score_prop)),
                vjust = -0.5,
                size = 4,
                color = "black",
            fontface = "bold")+
  scale_fill_manual(values = c("darkorange3",
                               "goldenrod",
                               "palegoldenrod",
                               "palegreen2",
                               "palegreen4"))+
  scale_y_continuous(breaks = seq(0, 100, 20),
                     label = label_number(suffix = "%"),
                     limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Share of Product Reviews",
       subtitle = "Over 75% of reviews are greater than 3 stars",
       x = "Rating",
       y = "")
  
#saving plot
ggsave(kpi2_review_dist,
       filename = "R/plots/kpi2_review_dist.jpg",
       height = 5,
       width = 8)


####################################
# Plots for Regression EDA
####################################
#loading data
review_model_analysis <- read_csv("SQL/exports_to_R/KPI2_product_reviews/kpi2_review_model_analysis.csv") %>% 
  mutate(review_score = as.factor(review_score)) %>% 
  drop_na(review_score) %>% 
  group_by(review_score) %>% 
  mutate(tot_price = if_else(is.na(tot_price), median(tot_price, na.rm=TRUE), tot_price),
         payment_value = if_else(is.na(payment_value), median(payment_value, na.rm=TRUE), payment_value),
         tot_price = tot_price/1000,
         payment_value = payment_value/1000,
         avg_review_price = mean(tot_price), #making grand means by review score for plot
         avg_review_delivery_days = mean(delivery_days),
         avg_review_order_size = mean(order_size),
         avg_review_payment_value = mean(payment_value)) %>% 
  ungroup()

### price v review score
kpi2_price_v_score <- ggplot(review_model_analysis,
       aes(x = tot_price,
           y = review_score,
           fill = review_score,
           color = review_score))+
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.5),
                      alpha = 0.95)+
  geom_text(data = review_model_analysis,
            aes(x = avg_review_price,
                y = review_score,
                label = paste0("avg - $", round(avg_review_price*1000, 0))),
            vjust = -2.5,
            hjust = -0.5,
            size = 3.5,
            color = "black")+
   scale_x_continuous(label = label_number(suffix = "K"),
                      breaks = seq(0, 2, 0.5),
                      limits = c(0, 2))+
  scale_fill_manual(values = c("darkorange3",
                               "goldenrod",
                               "palegoldenrod",
                               "palegreen2",
                               "palegreen4",
                               "gray"))+
  scale_color_manual(values = c("darkorange4",
                               "goldenrod4",
                               "goldenrod",
                               "palegreen4",
                               "darkgreen",
                               "gray"))+
  coord_cartesian(xlim = c(0, 1.5))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Review Rating by Total Order Price",
       subtitle = "Customers rate similarly regardless of what they paid",
       y = "Rating",
       x = "Total Order Price (R$)",
       caption = "NOTE: 157 orders cost more than 1.5K")

#saving plot
ggsave(kpi2_price_v_score,
       filename = "R/plots/kpi2_price_v_score.jpg",
       height = 5,
       width = 8)
  

### shipping days v review score
kpi2_delivery_v_score <- ggplot(review_model_analysis,
       aes(x = delivery_days,
           y = review_score,
           fill = review_score,
           color = review_score))+
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.5),
                      alpha = 0.95)+
  geom_text(data = review_model_analysis,
            aes(x = avg_review_delivery_days,
                y = review_score,
                label = paste0("avg - ", round(avg_review_delivery_days, 0), " days")),
            vjust = -2.6,
            hjust = -0.8,
            size = 3.5,
            color = "black")+
  scale_x_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100))+
  scale_fill_manual(values = c("indianred3",
                               "lightcoral",
                               "lightsteelblue",
                               "dodgerblue",
                               "dodgerblue4",
                               "gray"))+
  scale_color_manual(values = c("darkred",
                                "indianred4",
                                "steelblue3",
                                "dodgerblue4",
                                "darkblue",
                                "gray"))+
  coord_cartesian(xlim = c(0, 100))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Review Rating by Delivery Time",
       subtitle = "Review scores tend to drop as delivery time increases",
       y = "Rating",
       x = "Days",
       caption = "NOTE: 67 deliveries took longer than 100 days")


#saving plot
ggsave(kpi2_delivery_v_score,
       filename = "R/plots/kpi2_delivery_v_score.jpg",
       height = 5,
       width = 8)



### order size v review score
kpi2_size_v_score <- ggplot(review_model_analysis,
       aes(x = order_size,
           y = review_score,
           fill = review_score,
           color = review_score))+
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.5),
                      alpha = 0.95)+
  geom_text(data = review_model_analysis,
            aes(x = avg_review_order_size,
                y = review_score,
                label = paste0("avg - ", round(avg_review_order_size, 1), " items")),
            vjust = -2.6,
            hjust = -0.15,
            size = 3.5,
            color = "black")+
  scale_x_continuous(breaks = seq(1, 7, 1),
                     limits = c(0,7))+
  scale_fill_manual(values = c("sienna",
                               "sienna3",
                               "wheat3",
                               "skyblue",
                               "skyblue4",
                               "gray"))+
  scale_color_manual(values = c("brown4",
                                "sienna4",
                                "wheat4",
                                "skyblue3",
                                "steelblue4",
                                "gray"))+
  coord_cartesian(xlim = c(1, 7))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Review Ratings by Order Size",
       subtitle = "Order size doesn't appear to sway a Customer's review rating",
       x = "Items",
       y = "Rating",
       caption = "NOTE: 54 orders had greater than 7 items")

#saving plot
ggsave(kpi2_size_v_score,
       filename = "R/plots/kpi2_size_v_score.jpg",
       height = 5,
       width = 8)
  

### payment value v review score
kpi2_payment_v_score <- ggplot(review_model_analysis,
                             aes(x = payment_value,
                                 y = review_score,
                                 fill = review_score,
                                 color = review_score))+
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.5),
                      alpha = 0.95)+
  geom_text(data = review_model_analysis,
            aes(x = avg_review_payment_value,
                y = review_score,
                label = paste0("avg - $", round(avg_review_payment_value*1000, 0))),
            vjust = -2.5,
            hjust = -0.5,
            size = 3.5,
            color = "black")+
  scale_x_continuous(label = label_number(suffix = "K"),
                     breaks = seq(0, 2, 0.5),
                     limits = c(0, 2))+
  scale_fill_manual(values = c("darkorange3",
                               "goldenrod",
                               "palegoldenrod",
                               "skyblue",
                               "skyblue4",
                               "gray"))+
  scale_color_manual(values = c("darkorange4",
                                "goldenrod4",
                                "goldenrod",
                                "skyblue3",
                                "steelblue4",
                                "gray"))+
  coord_cartesian(xlim = c(0, 1.5))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Review Rating by Final Payment Cost (discounts, shipping, etc.)",
       subtitle = "Customers rate similarly regardless of final payment costs",
       y = "Rating",
       x = "Final Price (R$)",
       caption = "NOTE: 211 orders had a final \n payment cost more than R$1.5K")

#saving plot
ggsave(kpi2_payment_v_score,
       filename = "R/plots/kpi2_payment_v_score.jpg",
       height = 5,
       width = 8)
#######################
# End of KPI2 EDA
#
#
#
#
#################
# MAIN ANALYSIS
#################
#loading in data for ordinal logistic regression + cleaning
review_olm_data <- read_csv("SQL/exports_to_R/KPI2_product_reviews/kpi2_review_model_analysis.csv") %>%
  drop_na(review_score) %>% #dropping NA values in review_score
  mutate(tot_price = if_else(is.na(tot_price), median(tot_price, na.rm=TRUE), tot_price), #imputing tot_price with median
         review_score = as.ordered(review_score)) %>% #ensuring review_score is ordered + factor
  drop_na() #dropping all other NA values.

#sample size
paste0("n = ", nrow(review_olm_data))

review_olm_data %>% 
  count(review_score) #adequate :)

#data transformations
review_olm_data <- review_olm_data %>% 
  mutate(across(2:5, log1p)) #safe log transformation for 0 values (log(x + 1))

#checking distributions for normality
hist(review_olm_data$order_size) #normal enough

hist(review_olm_data$delivery_days) #normal

hist(review_olm_data$tot_price) #normal

hist(review_olm_data$payment_value) #normal


### MODELING ###

#null model
null <- polr(review_score ~ 1,
             data = review_olm_data, 
             Hess = TRUE)

summary(null)

#full model
full <- polr(review_score ~ order_size + delivery_days + tot_price + payment_value,
             data = review_olm_data,
             Hess = TRUE)

summary(full)

#model comparison
anova(null, full)

#check for multicollinearity
vif(full) 
    #price and payment value have a vif of 4.1, but a model comparison (deleted)
    #showed no differences. The value is also less than 5. Good to go!

# test of Proportional Odds assumption
brant(full)
   #we've failed the proportional odds assumption (except payment_value)...
   #my next step is to run a partial proportional odds model in the VGAM package


### Partial Proportional Odds Model ###
library(VGAM)

# ppo model
ppo_model <- vglm(review_score ~ order_size + delivery_days + tot_price + payment_value,
                   family = cumulative(parallel = FALSE ~ order_size + delivery_days + tot_price,
                                       reverse = FALSE), 
                   data = review_olm_data)

summary(ppo_model)

# Extract values
coefs <- coef(summary(ppo_model))
est <- coefs[, "Estimate"]
se <- coefs[, "Std. Error"]

# OR + confidence intervals
lower <- est - 1.96 * se
upper <- est + 1.96 * se

OR <- exp(est)
LCL_95 <- exp(lower)
UCL_95 <- exp(upper)

# ppom table
ppo_df <- data.frame(
  variable = rownames(coefs),
  estimate = est,
  std_error = se,
  OR = round(OR, 3),
  LCL_95 = round(LCL_95, 3),
  UCL_95 = round(UCL_95, 3),
  z_value = coefs[, "z value"],
  p_value = round(coefs[, "Pr(>|z|)"], 4)
) %>% 
  dplyr::select(variable, OR, LCL_95, UCL_95, p_value, estimate, std_error, z_value)

#removing rownames
rownames(ppo_df) <- NULL

###########################
# End of Modeling
#
#
##################################
# LADDER PLOTS for EACH PREDICTOR
##################################

# order size ladder plot
ordersize_df <- ppo_df[5:8, ]

ordersize_df$variable <- c(
  "Rating of 1",
  "Rating of ≤ 2",
  "Rating of ≤ 3",
  "Rating of ≤ 4"
)

#plot
kpi2_OR_ordersize <- ggplot(ordersize_df,
                          aes(x = OR,
                              y = reorder(variable, 4:1),
                              text=paste0("p=" ,p_value)))+
    geom_vline(aes(xintercept = 1), size = .7, linetype = "dashed", color = "gray50")+
    geom_errorbarh(aes(xmax = UCL_95, xmin = LCL_95), size = 0.6, 
                 height = .25, color = "black")+
    geom_point(shape = 20, color="black", fill = "gray10", size = 6)+
    geom_text(aes(label = paste0(round(OR, 1), "x")), 
              hjust = 0.5,
              vjust = 2.5,
              size = 4.5, 
              color = "black") +  # OR label
    scale_x_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0,3))+
    annotate("text", 
             label = "more likely",
             x = 1.27,
             y = 4.2)+
    annotate("text",
             label = "less likely",
             x = 0.7,
             y = 0.85)+
    geom_segment(aes(x=0.9, #less likely line segment
                     y=0.65, 
                     xend=0.3, 
                     yend=0.65), 
                 arrow = arrow(length=unit(.5, 'cm')),
                 lwd = 1,
                 color = "red3")+
    geom_segment(aes(x=1.05, #more likely line segment
                     y=4.4, 
                     xend=1.65, 
                     yend=4.4), 
                 arrow = arrow(length=unit(.5, 'cm')),
                 lwd = 1,
                 color = "green4")+
    theme_classic()+
    theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
    labs(title = "Order Size - Odds Ratios",
       subtitle = "Bigger orders are more prone to lower ratings",
       x = "OR",
       y = "")

# INSIGHTS - Order Size
  # - larger orders may be more difficult to fulfill.
  # - larger orders may stretch fulfillment or shipping capacity, increasing the risk of packaging errors, delivery delays, or partial shipments
  # - customers may be dissatisfied with one product of multiple, bringing down the entire order review

#save plot
ggsave(kpi2_OR_ordersize,
       filename = "R/plots/kpi2_OR_ordersize.jpg",
       height = 5,
       width = 8)



# delivery days ladder plot
deliverydays_df <- ppo_df[9:12, ]

deliverydays_df$variable <- c(
  "Rating of 1",
  "Rating of ≤ 2",
  "Rating of ≤ 3",
  "Rating of ≤ 4"
)

#plot
kpi2_OR_deliverydays <- ggplot(deliverydays_df,
                            aes(x = OR,
                                y = reorder(variable, 4:1),
                                text=paste0("p=" ,p_value)))+
  geom_vline(aes(xintercept = 1), size = .7, linetype = "dashed", color = "gray50")+
  geom_errorbarh(aes(xmax = UCL_95, xmin = LCL_95), size = 0.6, 
                 height = .25, color = "black")+
  geom_point(shape = 20, color="black", fill = "gray10", size = 6)+
  geom_text(aes(label = paste0(round(OR, 1), "x")), 
            hjust = 0.5,
            vjust = 2.5,
            size = 4.5, 
            color = "black") +  # OR label
  scale_x_continuous(breaks = seq(0, 4.5, 0.5),
                     limits = c(0,4.5))+
  annotate("text", 
           label = "more likely",
           x = 1.32,
           y = 4.2)+
  annotate("text",
           label = "less likely",
           x = 0.7,
           y = 0.85)+
  geom_segment(aes(x=0.9, #less likely line segment
                   y=0.65, 
                   xend=0.2, 
                   yend=0.65), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "red3")+
  geom_segment(aes(x=1.05, #more likely line segment
                   y=4.4, 
                   xend=1.8, 
                   yend=4.4), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "green4")+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Delivery Days - Odds Ratios",
       subtitle = "Longer deliveries equate to significantly lower ratings",
       x = "OR",
       y = "")

# INSIGHTS - Delivery Days
# - Delivery time is a huge driver for customer satisfaction
# - Customers expect timely deliveries and short expected delivery times
# - Review scores might be functioning as a diagnostic of delivery performance, to 
#   prepare for this in the future, we should flag inefficiencies in our logistics pipeline 
#   (sourcing, freight, vendors, lead times, inventory, estimated delivery windows)

#save plot
ggsave(kpi2_OR_deliverydays,
       filename = "R/plots/kpi2_OR_deliverydays.jpg",
       height = 5,
       width = 8)



# total order price ladder plot
totprice_df <- ppo_df[13:16, ]

totprice_df$variable <- c(
  "Rating of 1",
  "Rating of ≤ 2",
  "Rating of ≤ 3",
  "Rating of ≤ 4"
)

#plot
kpi2_OR_totprice <- ggplot(totprice_df,
                               aes(x = OR,
                                   y = reorder(variable, 4:1),
                                   text=paste0("p=" ,p_value)))+
  geom_vline(aes(xintercept = 1), size = .7, linetype = "dashed", color = "gray50")+
  geom_errorbarh(aes(xmax = UCL_95, xmin = LCL_95), size = 0.6, 
                 height = .25, color = "black")+
  geom_point(shape = 20, color="black", fill = "gray10", size = 6)+
  geom_text(aes(label = paste0(round(OR, 2), "x")), 
            hjust = -0.3,
            vjust = 2,
            size = 4.5, 
            color = "black") +  # OR label
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2))+
  annotate("text", 
           label = "more likely",
           x = 1.17,
           y = 4.35)+
  annotate("text",
           label = "less likely",
           x = 0.84,
           y = 0.67)+
  geom_segment(aes(x=0.95, #less likely line segment
                   y=0.55, 
                   xend=0.6, 
                   yend=0.55), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "red3")+
  geom_segment(aes(x=1.05, #more likely line segment
                   y=4.45, 
                   xend=1.40, 
                   yend=4.45), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "green4")+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Total Order Price - Odds Ratios",
       subtitle = "Customers don’t seem to penalize or reward based on how much they paid",
       x = "OR",
       y = "")

# INSIGHTS - Total Order Price
# - Customers arent too affected by price. If they're buying it, they must be okay with the price.
# - At the lowest threshold (Rating = 1), higher prices are associated with a slight increase in 
#  low ratings. This might be due to possibly higher expectations of the product(s).
# - For mid-to-high review levels, price has a negligible effect.

#save plot
ggsave(kpi2_OR_totprice,
       filename = "R/plots/kpi2_OR_totprice.jpg",
       height = 5,
       width = 8)



# total order price ladder plot
paymentvalue_df <- ppo_df[17, ]

#plot
kpi2_OR_paymentvalue <- ggplot(paymentvalue_df,
                           aes(x = OR,
                               y = variable,
                               text=paste0("p=" ,p_value)))+
  geom_vline(aes(xintercept = 1), size = .7, linetype = "dashed", color = "gray50")+
  geom_errorbarh(aes(xmax = UCL_95, xmin = LCL_95), size = 0.6, 
                 height = .25, color = "black")+
  geom_point(shape = 20, color="black", fill = "gray10", size = 8)+
  geom_text(aes(label = paste0(round(OR, 2), "x")), 
            hjust = 1.3,
            vjust = 2.4,
            size = 4.5, 
            color = "black") +  # OR label
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2))+
  annotate("text", 
           label = "more likely",
           x = 1.17,
           y = 1.37)+
  annotate("text",
           label = "less likely",
           x = 0.84,
           y = 0.59)+
  geom_segment(aes(x=0.95, #less likely line segment
                   y=0.55, 
                   xend=0.6, 
                   yend=0.55), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "red3")+
  geom_segment(aes(x=1.05, #more likely line segment
                   y=1.4, 
                   xend=1.40, 
                   yend=1.4), 
               arrow = arrow(length=unit(.5, 'cm')),
               lwd = 1,
               color = "green4")+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=12, face="italic"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"))+
  labs(title = "Final Payment Cost - Odds Ratio",
       subtitle = "Higher final costs are less likely to get bad reviews (small effect)",
       x = "OR",
       y = "")

# INSIGHTS - Final Payment Cost
# - Higher priced orders may be more likely to get better reviews due to quality of products within order
# - Although the relationship is statistically significant, the effect size is small, so payment value 
#   is not a strong driver of satisfaction on its own.
# - Since the effect is consistent across all thresholds (1-5 stars), this variable passes the proportional odds 
#   assumption, so it affects the chance of bad vs. good reviews in a stable, uniform way.

#save plot
ggsave(kpi2_OR_paymentvalue,
       filename = "R/plots/kpi2_OR_paymentvalue.jpg",
       height = 5,
       width = 8)

