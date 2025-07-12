########################################
# Cleaning Script for E-Commerce Tables
########################################


# packages
library(tidyverse)


################
# DATA LOADING
###############

# loading in raw tables
# 9 total raw tables
customers = read_csv("Tables/raw/raw_customers.csv")

geolocation = read_csv("Tables/raw/raw_geolocation.csv")

order_items = read_csv("Tables/raw/raw_order_items.csv")

order_payments = read_csv("Tables/raw/raw_order_payments.csv")

order_reviews = read_csv("Tables/raw/raw_order_reviews.csv")

orders = read_csv("Tables/raw/raw_orders.csv")

product_category_name_translation = read_csv("Tables/raw/raw_product_category_name_translation.csv")

products = read_csv("Tables/raw/raw_products.csv")

sellers = read_csv("Tables/raw/raw_sellers.csv")


# creating object of all 9 raw tables to inspect
dfs <- list(
  customers = customers,
  geolocation = geolocation,
  order_items = order_items,
  order_payments = order_payments,
  order_reviews = order_reviews,
  orders = orders,
  product_category_name_translation = product_category_name_translation,
  products = products,
  sellers = sellers)


# looping over each tables for:
# table size, forst 2 rows, column names, and data types
for (name in names(dfs)) {
  df <- dfs[[name]]
  
  cat("\n\n --- for table <<", toupper(name), ">> shape is (", nrow(df), ", ", ncol(df), ") ---\n", sep = "")
  cat("First 2 rows: \n")
  print(head(df, 2))
  
  cat("\nColumn Names:\n")
  print(colnames(df))
  
  cat("\nData Types:\n")
  print(sapply(df, class))
}


# looping through tables for count of NULL values in each column
for (name in names(dfs)) {
  df <- dfs[[name]]
  
  cat("\n", toupper(name), " - NULL values:\n", sep = "")
  print(colSums(is.na(df)))
  cat("\n")
}


# looping through tables to view number of duplicate rows
for (name in names(dfs)) {
  df <- dfs[[name]]
  
  num_duplicates <- sum(duplicated(df))
  cat("\n", toupper(name), " - Duplicates: ", num_duplicates, "\n", sep = "")
}


# dropping duplicates in geolocation
geolocation <- geolocation[!duplicated(geolocation), ]
sum(duplicated(geolocation)) #zero! nice.
    #why remove those duplicates?
    #they were repeated identical entries. Provide no additional info.


#########################
# HANDLING MISSING DATA
#########################

# drop 'review_comment_title' from column from reviews
order_reviews <- order_reviews %>% 
  select(-review_comment_title)

#drop unnecessary columns from 'products' table
products <- products %>% 
  select(-product_name_lenght,
         -product_description_lenght,
         -product_photos_qty)

# imputing NA values with median in 'products' table
products <- products %>%
  mutate(
    product_category_name = replace_na(product_category_name, "unknown"),
    product_weight_g = if_else(is.na(product_weight_g), median(product_weight_g, na.rm = TRUE), product_weight_g),
    product_length_cm = if_else(is.na(product_length_cm), median(product_length_cm, na.rm = TRUE), product_length_cm),
    product_height_cm = if_else(is.na(product_height_cm), median(product_height_cm, na.rm = TRUE), product_height_cm),
    product_width_cm = if_else(is.na(product_width_cm), median(product_width_cm, na.rm = TRUE), product_width_cm)
  )


# finding cancelled and NULL orders
# Create the logical mask
mask <- is.na(orders$order_approved_at) & orders$order_status == "canceled"
# Show the first 3 rows matching the condition
orders %>% 
  filter(mask) %>% 
  head(3)


# count of 'order_status' type for NULL 'orders_approved_at'
orders %>%
  filter(is.na(order_approved_at)) %>%
  count(order_status) %>%
  arrange(desc(n))


# pulling 'order_id's where 'order_approved_at' is missing and 'order_status' says delivered
orders %>%
  filter(is.na(order_approved_at) & order_status == "delivered") %>%
  pull(order_id)


# extractign those order_ids
order_ids <- c(
  'e04abd8149ef81b95221e88f6ed9ab6a',
  '8a9adc69528e1001fc68dd0aaebbb54a',
  '7013bcfc1c97fe719a7b5e05e61c12db',
  '5cf925b116421afa85ee25e99b4c34fb',
  '12a95a3c06dbaec84bcfb0e2da5d228a',
  'c1d4211b3dae76144deccd6c74144a88',
  'd69e5d356402adc8cf17e08b5033acfb',
  'd77031d6a3c8a52f019764e68f211c69',
  '7002a78c79c519ac54022d4f8a65e6e8',
  '2eecb0d85f281280f79fa00f9cec1a95',
  '51eb2eebd5d76a24625b31c33dd41449',
  '88083e8f64d95b932164187484d90212',
  '3c0b8706b065f9919d0505d3b3343881',
  '2babbb4b15e6d2dfe95e2de765c97bce'
)
# examinign from order_payments table
payments_filtered <- order_payments %>%
  filter(order_id %in% order_ids)

payments_filtered


# replace missing order_approved_at timestamps with the 
# corresponding order_purchase_timestamp for orders that 
# have a status of "delivered."
orders <- orders %>%
  mutate(order_approved_at = if_else(
    is.na(order_approved_at) & order_status == "delivered",
    order_purchase_timestamp,
    order_approved_at
  ))


# count the number of missing values in each column of the orders dataset.
colSums(is.na(orders)) #14 ids removed


# count the occurrences of each 'order_status' where 'order_delivered_carrier_date' is missing
orders %>% 
  filter(is.na(order_delivered_carrier_date)) %>% 
  count(order_status) %>% 
  arrange(desc(n))

# count the occurrences of each 'order_status' where 'order_delivered_customer_data' is missing
orders %>% 
  filter(is.na(order_delivered_customer_date)) %>% 
  count(order_status) %>% 
  arrange(desc(n))

# show where 'order_delivered_customer_date' is missing but 'order_status' says delivered
orders %>% 
  filter(is.na(order_delivered_customer_date),
         order_status == "delivered")

# show where 'order_delivered_carrier_date' is missing but 'order_status' says delivered
orders %>% 
  filter(is.na(order_delivered_carrier_date),
         order_status == "delivered")

# removing two order_ids and impute missing delivery and approval timestamps
# with estimated and purchase timestamps
orders <- orders %>%
  filter(!(order_id %in% c("2d858f451373b04fb5c984a1cc2defaf", 
                           "2aa91108853cecb43c84a5dc5b277475"))) %>%
  mutate(
    order_delivered_customer_date = if_else(
      is.na(order_delivered_customer_date),
      order_estimated_delivery_date,
      order_delivered_customer_date),
    order_approved_at = if_else(
      is.na(order_approved_at),
      order_purchase_timestamp,
      order_approved_at))


#############################
# DATA MERGING AND INTEGRITY
#############################

# creating master table for analysis
master_orders <- orders %>%
  left_join(customers %>% 
              select(customer_id, customer_unique_id, customer_city, customer_state, customer_zip_code_prefix),
            by = "customer_id") 
  
# merging product, seller, and payment details into the master_orders dataset 
# using left joins on 'order_id' and 'product_id'
master_orders <- master_orders %>% 
  left_join(order_items %>% 
              select(order_id, product_id, seller_id, price), 
            by = "order_id") %>%
  left_join(products %>% 
              select(product_id, product_category_name), 
            by = "product_id") %>%
  left_join(order_payments %>% 
              select(order_id, payment_type, payment_value, payment_installments), 
            by = "order_id")


# checking missing values
colSums(is.na(master_orders))


# counting the occurrences of each 'order_status' in master_orders where 'payment_type' is missing.
master_orders %>%
  filter(is.na(payment_type)) %>%
  count(order_status) %>%
  arrange(desc(n))


# filtering rows in master_orders where 'payment_type' is missing
master_orders %>%
  filter(is.na(payment_type))


# merging review scores and seller city information into the master_orders dataset 
# using left joins on 'order_id' and 'seller_id'
master_orders <- master_orders %>%
  left_join(order_reviews %>% 
              select(order_id, review_score, review_creation_date), 
            by = "order_id") %>%
  left_join(sellers %>% 
              select(seller_id, seller_city), 
            by = "seller_id")


# calculating the average latitude and longitude for each zip code prefix in geolocation
geo_avg <- geolocation %>%
  group_by(geolocation_zip_code_prefix) %>%
  summarise(
    geolocation_lat = mean(geolocation_lat, na.rm = TRUE),
    geolocation_lng = mean(geolocation_lng, na.rm = TRUE)
  ) %>%
  ungroup()

# merging it into master_orders based on customer zip code prefix
master_orders <- master_orders %>%
  left_join(geo_avg, by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix"))

# check missing values
colSums(is.na(master_orders))


# replacing missing 'payment_type' values in master_orders with "not_defined"
master_orders <- master_orders %>%
  mutate(payment_type = if_else(is.na(payment_type), "not_defined", payment_type))



# mapping Portuguese product categories to English equivalents in master_orders by creating a 
# new column 'product_category_english'. Using custom translation from Sergei Egorov (kaggle)
category_translation <- c(
  "utilidades_domesticas" = "home_utilities",
  "perfumaria" = "perfumery",
  "automotivo" = "automotive",
  "pet_shop" = "pet_shop",
  "papelaria" = "stationery",
  "unknown" = "unknown",
  "moveis_decoracao" = "furniture_decor",
  "moveis_escritorio" = "office_furniture",
  "ferramentas_jardim" = "tools_garden",
  "informatica_acessorios" = "computing_accessories",
  "cama_mesa_banho" = "bed_bath_table",
  "brinquedos" = "toys",
  "construcao_ferramentas_construcao" = "construction_tools_construction",
  "telefonia" = "telephony",
  "beleza_saude" = "beauty_health",
  "eletronicos" = "electronics",
  "bebes" = "baby",
  "cool_stuff" = "cool_stuff",
  "relogios_presentes" = "watches_gifts",
  "climatizacao" = "air_conditioning",
  "esporte_lazer" = "sports_leisure",
  "livros_interesse_geral" = "books_general_interest",
  "eletroportateis" = "small_appliances",
  "alimentos" = "food",
  "malas_acessorios" = "luggage_accessories",
  "fashion_underwear_e_moda_praia" = "fashion_underwear_beachwear",
  "artigos_de_natal" = "christmas_articles",
  "fashion_bolsas_e_acessorios" = "fashion_bags_accessories",
  "instrumentos_musicais" = "musical_instruments",
  "construcao_ferramentas_iluminacao" = "construction_tools_lighting",
  "livros_tecnicos" = "technical_books",
  "construcao_ferramentas_jardim" = "construction_tools_garden",
  "eletrodomesticos" = "home_appliances",
  "market_place" = "market_place",
  "agro_industria_e_comercio" = "agribusiness",
  "artigos_de_festas" = "party_supplies",
  "casa_conforto" = "home_comfort",
  "cds_dvds_musicais" = "cds_dvds_music",
  "industria_comercio_e_negocios" = "industry_commerce_business",
  "consoles_games" = "consoles_games",
  "moveis_quarto" = "bedroom_furniture",
  "construcao_ferramentas_seguranca" = "construction_tools_safety",
  "telefonia_fixa" = "landline_phones",
  "bebidas" = "drinks",
  "moveis_cozinha_area_de_servico_jantar_e_jardim" = "kitchen_service_dining_garden_furniture",
  "fashion_calcados" = "fashion_shoes",
  "casa_construcao" = "home_construction",
  "audio" = "audio",
  "eletrodomesticos_2" = "home_appliances_2",
  "fashion_roupa_masculina" = "fashion_male_clothing",
  "cine_foto" = "cinema_photography",
  "moveis_sala" = "living_room_furniture",
  "artes" = "arts",
  "alimentos_bebidas" = "food_drinks",
  "tablets_impressao_imagem" = "tablets_printing_image",
  "fashion_esporte" = "fashion_sports",
  "portateis_cozinha_e_preparadores_de_alimentos" = "portable_kitchen_food_preparers",
  "la_cuisine" = "la_cuisine",
  "flores" = "flowers",
  "pcs" = "pcs",
  "casa_conforto_2" = "home_comfort_2",
  "portateis_casa_forno_e_cafe" = "portable_home_oven_coffee",
  "dvds_blu_ray" = "dvds_blu_ray",
  "pc_gamer" = "pc_gamer",
  "construcao_ferramentas_ferramentas" = "construction_tools_tools",
  "fashion_roupa_feminina" = "fashion_female_clothing",
  "moveis_colchao_e_estofado" = "mattress_upholstered_furniture",
  "sinalizacao_e_seguranca" = "signaling_safety",
  "fraldas_higiene" = "diapers_hygiene",
  "livros_importados" = "imported_books",
  "fashion_roupa_infanto_juvenil" = "fashion_children_clothing",
  "musica" = "music",
  "artes_e_artesanato" = "arts_crafts",
  "seguros_e_servicos" = "insurance_services"
)

master_orders <- master_orders %>%
  mutate(product_category_english = category_translation[product_category_name])

# filling missing geolocation latitude and longitude in master_orders 
# with the median coordinates per customer_city.
median_coords <- master_orders %>%
  group_by(customer_city) %>%
  summarise(
    median_lat = median(geolocation_lat, na.rm = TRUE),
    median_lng = median(geolocation_lng, na.rm = TRUE)
  )

master_orders <- master_orders %>%
  left_join(median_coords, by = "customer_city") %>%
  mutate(
    geolocation_lat = if_else(is.na(geolocation_lat), median_lat, geolocation_lat),
    geolocation_lng = if_else(is.na(geolocation_lng), median_lng, geolocation_lng)
  ) %>%
  select(-median_lat, -median_lng)


# check missing values
colSums(is.na(master_orders))


# filling remaining missing geolocation latitude and longitude in master_orders with overall median values.
median_lat <- median(master_orders$geolocation_lat, na.rm = TRUE)
median_lng <- median(master_orders$geolocation_lng, na.rm = TRUE)

master_orders <- master_orders %>%
  mutate(
    geolocation_lat = if_else(is.na(geolocation_lat), median_lat, geolocation_lat),
    geolocation_lng = if_else(is.na(geolocation_lng), median_lng, geolocation_lng))


# check missing values
colSums(is.na(master_orders))


# convert specified columns in master_orders to date-time format
date_cols <- c(
  "order_purchase_timestamp", "order_approved_at",
  "order_delivered_carrier_date", "order_delivered_customer_date",
  "order_estimated_delivery_date", "review_creation_date")

master_orders <- master_orders %>%
  mutate(across(all_of(date_cols), as.POSIXct))

# End of cleaning!

##########################################
# SAVING CLEAN TABLES to PROCESSED FOLDER
##########################################
# 10 tables
write.csv(master_orders, "Tables/processed/master_orders.csv", row.names = FALSE, na = "")

write.csv(customers, "Tables/processed/clean_customers.csv", row.names = FALSE)

write.csv(geolocation, "Tables/processed/clean_geolocation.csv", row.names = FALSE)

write.csv(order_items, "Tables/processed/clean_order_items.csv", row.names = FALSE)

write.csv(order_payments, "Tables/processed/clean_order_payments.csv", row.names = FALSE)

write.csv(order_reviews, "Tables/processed/clean_order_reviews.csv", row.names = FALSE)

write.csv(orders, "Tables/processed/clean_orders.csv", row.names = FALSE, na = "")

write.csv(product_category_name_translation, "Tables/processed/clean_product_category_name_translation.csv", row.names = FALSE)

write.csv(products, "Tables/processed/clean_products.csv", row.names = FALSE)

write.csv(sellers, "Tables/processed/clean_sellers.csv", row.names = FALSE)






