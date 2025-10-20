# Feeding the Market: Insights into Pet Food & Treat Trends 
## *Exploring product trends, consumer preferences, and brand performance in pet food and treats.*

### Project Description
This project analyzes pet food and treat sales and insights data to explore product pricing, brand performance, consumer ratings, and category-level trends. The goal is to uncover actionable insights for pet product brands and demonstrate data analysis skills using R and Tableau.

### Motivation
As a pet owner of two beloved dogs, I noticed I’m constantly buying food and treats. This inspired a personal project to explore trends in pet food and treats, uncover insights about products and brands, and practice data analysis using real-world data.

### Dataset
This analysis uses a free Kaggle dataset: [PetSmart Dataset](https://www.kaggle.com/datasets/crawlfeeds/petssmart-dataset)

#### Notes on the dataset:
* Covers a wide range of pet food and treat products.
* Includes product name, brand, price, average rating and review counts.
* Since detailed pet product sales data is generally behind paywalls, this dataset provides a free way to practice analysis.

### Data Cleaning & Preparation
Key cleaning steps include:
* Converting price to numeric and removing missing values
* Filtering only cat and dog food and treat products
* Cleaning HTML entitities in brand and product names
``` r
# Clean HTML entities in brand/ product names
petsmart_food <- petsmart_food %>%
  mutate(brand = str_replace_all(brand, "&amp;", "&"),
    brand = str_replace_all(brand, "&trade;", "™"),
    brand = str_replace_all(brand, "&reg;", "®"),
    name = str_replace_all(name, "&amp;", "&"),
    name = str_replace_all(name, "&trade;", "™"),
    name = str_replace_all(name, "&reg;", "®"))
*Exported a cleaned CSV for use in Tableau
``` r
write.csv(petsmart_food, "petsmart_food_clean.csv", row.names = FALSE)
```

### Analysis Overview
This analysis explores:
* Price distribution of pet products
* Top brands by product count and average rating
* Top products by price and review count
* Category trends comparing food vs. treats
* Scatter plots and correlations between price, rating and reviews

**1. Price Distribution**
``` r
ggplot(petsmart_food, aes(x = price)) +
  geom_histogram(fill = main_color, bins = 30) +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Distribution of Pet Food & Treat Prices",
       x = "Price (log scale)",
       y = "Count") +
  theme_minimal()
```
<img width="1344" height="960" alt="download" src="https://github.com/user-attachments/assets/48f329e7-2b1c-4837-ace9-5ffc2fcafaec" />


**2. Top 10 Brands by Product Count**
``` r
top_brands <- petsmart_food %>%
  count(brand, sort = TRUE) %>%
  top_n(10)

ggplot(top_brands, aes(x = reorder(brand, n), y = n)) +
  geom_col(fill = main_color) +
  coord_flip() +
  labs(title = "Top 10 Brands for Cat & Dog Food/Treats",
       x = "Brand", y = "Number of Products") +
  theme_minimal() +
  theme(axis.text.y = element_markdown())
```
<img width="1344" height="960" alt="download" src="https://github.com/user-attachments/assets/ea752318-c5e1-4832-9122-fc57e903f9cf" />


**3. Price vs. Average Rating**
``` r
ggplot(petsmart_food, aes(x = price, y = avg_rating)) +
  geom_point(alpha = 0.6, color = main_color) +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Price vs. Average Rating for Cat & Dog Food/Treats",
       x = "Price (log scale)", y = "Average Rating") +
  theme_minimal()
```
<img width="1344" height="960" alt="download" src="https://github.com/user-attachments/assets/cdae6abf-ef35-47fa-9c98-e69c901ee694" />


**4. Top 10 Most Expensive Products**
``` r
top_expensive <- petsmart_food %>%
  arrange(desc(price)) %>%
  slice_head(n = 10)

ggplot(top_expensive, aes(x = reorder(name, price), y = price)) +
  geom_col(fill = main_color) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Top 10 Most Expensive Cat & Dog Food/Treats",
       x = "Product", y = "Price") +
  theme_minimal() +
  theme(axis.text.y = element_markdown())
```
<img width="804" height="575" alt="21b5b1a7-3e3b-48e0-b2a5-4c0288b31fb1" src="https://github.com/user-attachments/assets/96f66cd5-4b41-40aa-8759-8a53843b9e0e" />

**5. Top 10 Most Reviewed Products**
``` r
top_reviewed <- petsmart_food %>%
  arrange(desc(reviews_count)) %>%
  slice_head(n = 10)

ggplot(top_reviewed, aes(x = reorder(name, reviews_count), y = reviews_count)) +
  geom_col(fill = main_color) +
  coord_flip() +
  labs(title = "Top 10 Most Reviewed Cat & Dog Food/Treats",
       x = "Product", y = "Number of Reviews") +
  theme_minimal() +
  theme(axis.text.y = element_markdown())
```
<img width="932" height="575" alt="d4cf3c89-8e5a-4179-bfcd-b93b5077243e" src="https://github.com/user-attachments/assets/bdb89f7f-6b78-4c6b-8093-7ef4be127b3c" />

**6. Average Rating by Brand (Top 10 Brands with >=5 Products)**
``` r
brand_ratings <- petsmart_food %>%
  group_by(brand) %>%
  filter(n() >= 5) %>%
  summarise(avg_rating = mean(avg_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 10)

ggplot(brand_ratings, aes(x = reorder(brand, avg_rating), y = avg_rating)) +
  geom_col(fill = main_color) +
  coord_flip() +
  labs(title = "Top 10 Brands by Average Rating",
    x = "Brand",
    y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.y = element_markdown())
```
<img width="536" height="575" alt="99a1c74d-3a80-4e87-84e7-5a4a4eeeb4c6" src="https://github.com/user-attachments/assets/9d02acf0-ad0a-4161-b543-ff14a7f7ed63" />


**7. Category-Level Trends**
``` r
category_summary <- petsmart_food %>%
  mutate(category_type = case_when(
    grepl("treat|chew|snack", tolower(name)) ~ "Treats",
    grepl("food|kibble", tolower(name)) ~ "Food",
    TRUE ~ "Other")) %>%
  group_by(category_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE),
            avg_rating = mean(avg_rating, na.rm = TRUE),
            total_products = n())
```

**Average Price by Category**
``` r
ggplot(category_summary, aes(x = category_type, y = avg_price, fill = category_type)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(main_color, secondary_color, secondary_color)) +
  labs(title = "Average Price by Category Type",
       x = "Category", y = "Average Price ($)") +
  theme_minimal()
```
<img width="536" height="575" alt="9924ca60-a460-4ab1-af8f-12a0be27dde5" src="https://github.com/user-attachments/assets/2e11c359-4297-4d2a-9726-a9f1567a2976" />

**Average Rating by Category**
``` r
ggplot(category_summary, aes(x = category_type, y = avg_rating, fill = category_type)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(main_color, secondary_color, secondary_color)) +
  labs(title = "Average Rating by Category Type",
       x = "Category",
       y = "Average Rating (out of 5)") +
  theme_minimal()
```
<img width="536" height="575" alt="443531c4-0667-4052-a257-5573afaea1dc" src="https://github.com/user-attachments/assets/54cd2736-d608-41a7-8dee-cd76ddbbb6a2" />

**8. Scatter Plot Matrix**
``` r
p1 <- ggplot(petsmart_food, aes(x = price, y = avg_rating)) +
  geom_point(alpha = 0.6, color = main_color) +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Price vs Average Rating",
       x = "Price (log scale)",
       y = "Average Rating") +
  theme_minimal()

p2 <- ggplot(petsmart_food, aes(x = price, y = as.numeric(reviews_count))) +
  geom_point(alpha = 0.6, color = main_color) +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Price vs Number of Reviews",
       x = "Price (log scale)",
       y = "Number of Reviews") +
  theme_minimal()

p3 <- ggplot(petsmart_food, aes(x = as.numeric(reviews_count), y = avg_rating)) +
  geom_point(alpha = 0.6, color = main_color) +
  labs(title = "Reviews Count vs Average Rating",
       x = "Number of Reviews",
       y = "Average Rating") +
  theme_minimal()

combined_plot <- p1 / p2 / p3
combined_plot
```
<img width="536" height="575" alt="5bb1f97e-15df-4a54-8312-ba97643b92a0" src="https://github.com/user-attachments/assets/07b2d261-0309-4116-8bd3-0392c0313afe" />

### Tableau Dashboard
View the interactive dashboard [Feeding the Market: Insights into Pet Food & Treat Trends](https://public.tableau.com/app/profile/sarah.hannah.silverstein/viz/FeedingtheMarketInsightsintoPetFoodTreatTrends/Dashboard1?publish=yes)

### Key Insights
1. Most products are priced $5–$20, showing a strong mid-market segment.
2. A few brands dominate the product offerings, highlighting the importance of brand visibility.
3. Price vs. rating shows higher-priced products tend to have slightly higher ratings, but value options exist across price ranges.
4. Food has a slightly higher average rating than treats, while average prices are similar.
5. High-priced products showcase premium positioning; products with high review counts indicate strong engagement.
6.Brands with ≥5 products maintaining high ratings demonstrate consistent quality.

### Recommendations
* Focus on brand visibility and consistent product quality to compete with top brands.
* Offer mid-market pricing with select premium options.
* Monitor food product ratings to maintain a competitive edge.
* Encourage customer reviews to boost credibility and visibility.
---
#### Link to full R code: 
Access the code [here](https://github.com/shsilv/Pet_Food_Insights/blob/main/R%20Code).
