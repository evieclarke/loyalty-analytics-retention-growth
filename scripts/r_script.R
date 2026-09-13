### LSE Data Analytics Online Career Accelerator 

## DA301:  Advanced Analytics for Organisational Impact

# Evie Clarke


# ==============================================================================
  # Exploratory data analysis
# ==============================================================================

# Set working directory
  
# Installing necessary packages
install.packages('tidyverse')
install.packages('tidyr')
install.packages('ggplot2')

# Load the installed packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# Import data (n.b. this is the version cleaned in python, not the raw)
reviews <- read.csv('reviews.csv', header=T)

# View the head of the data
head(reviews,n=6)

# Create a summary of the data
summary(reviews)

# Distribution of age using a histogram
ggplot(reviews, aes(x = age)) +
  geom_histogram(bins=15, fill = 'palegreen4') + 
  scale_x_continuous(breaks = seq(0, 80, 10),"Age") + 
  scale_y_continuous(breaks = seq(0, 500, 100),"Frequency") +
  labs(title = "Customers by age") +
  theme_classic()

# Distribution of income using a histogram
ggplot(reviews, aes(x = remuneration)) +
  geom_histogram(bins=15, fill = 'palegreen4') + 
  scale_x_continuous(breaks = seq(0, 120, 10),"Remuneration (k£)") + 
  scale_y_continuous(breaks = seq(0, 500, 100),"Frequency") +
  labs(title = "Customers by remuneration (k£)") +
  theme_classic()

# Distribution of spending using a histogram
ggplot(reviews, aes(x = spending_score)) +
  geom_histogram(bins=10, fill = 'palegreen4') + 
  scale_x_continuous(breaks = seq(0, 120, 10),"spending_score") + 
  scale_y_continuous(breaks = seq(0, 500, 100),"Frequency") +
  labs(title = "Customers by spending_score") +
  theme_classic()

# Patterns of customers by gender using a bar chart
ggplot(reviews, aes(x = gender, fill=gender)) +  
  geom_bar(position = 'dodge') +  
  labs(title='Distributions of customers by gender') +
  theme_classic()

# Patterns of customers by education using a bar chart
ggplot(reviews, aes(x = education, fill=education)) +  
  geom_bar(position = 'dodge') +  
  labs(title='Distributions of customers by education') +
  theme_classic()

# Patterns of customers by product using a bar chart
# identify top 10 products
top10_product <- reviews %>%
  count(product, sort = TRUE) %>%
  slice_head(n=10)

# Convert product from numeric to factor
top10_product <- top10_product %>%
  mutate(product = factor(product))

# Visualise table with a column chart
ggplot(top10_product, aes(x = product, y=n, fill=product)) +  
  geom_col() +
  labs(title='Top 10 products',
       x='Product',
       y='Number of customers') +
  theme_classic()

# Distribution of loyalty points using a histogram
ggplot(reviews, aes(x = loyalty_points)) +
  geom_histogram(bins=10, fill = 'royalblue1') + 
  annotate('rect',
           xmin = 2500,
           xmax = Inf, ymin = 0,
           ymax = 200,
           alpha = 0.15,
           fill = 'gray37') +
  annotate(
    'text',
    x = 5600,
    y = 150,
    label = 'High-value customers\n(power users)',
    size = 4.5,
    color='black',
    hjust = 0
  ) +
  scale_x_continuous(breaks = seq(0, 7500, 500),"Loyalty points") + 
  scale_y_continuous(breaks = seq(0, 800, 100),"Frequency") +
  labs(title = paste("Distribution of loyalty points\nA small number",
  "of highly engaged customers account for very high loyalty balances")) +
  theme_classic()

# Relationship between gender and loyalty using a boxplot
ggplot(reviews, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() +
  labs(title = "Loyalty by gender") +  
  theme_minimal()  

# Relationship between education and loyalty using a boxplot
ggplot(reviews, aes(x = education, y = loyalty_points, fill = education)) +
  geom_boxplot() +
  labs(title = "Loyalty by education") +  
  theme_minimal() 

# Relationship between income and loyalty using a scatterplot
ggplot(data=reviews, mapping=aes(x = remuneration, y = loyalty_points)) +
  geom_point(alpha = 1, size = 3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Customer remuneration") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship between income and loyalty")

# Relationship between spending score and loyalty using a scatterplot
ggplot(data=reviews, mapping=aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 1, size = 3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Customer spending score") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship between spending score and loyalty")

# Relationship between age and loyalty using a scatterplot
ggplot(data=reviews, mapping=aes(x = age, y = loyalty_points)) +
  geom_point(alpha = 1, size = 3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Customer age") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship between age and loyalty")

"
Insights observed:
- Exploratory analysis indicates that the core customer base for Turtle games
are primarily ages 30-40 with remuneration clustered around £40-50k, suggesting
a predominantly middle-income demographic. Spending scores are more varied,
with most customers scoring between 40 and 60, indicating engagement is not 
just driven by income. Customer counts are relatively balanced by gender, and
loyalty distributions by gender are very similar suggesting no meaningful 
difference between the two.

Loyalty points are strongly right-skewed with the majority of customers holding
between 500 and 2000 points and a small number accumulating substantially higher
values. These high-loyalty outliers may represent particularly valuable 
customers. Education levels are skewed towards customers with a graduate 
education and above suggesting a highly educated customer base. There is no 
clear relationship between education and loyalty, with customers holding a basic
education having the most varied loyalty points indicating more heterogeneous
behaviour.

Groupings that may be useful for deeper insight:
- Product groupings: there are 200 unique product codes at the moment. Reducing
cardinality here and providing more high-level groupings would help deduce 
meaningful insight into product (e.g. books, board games, video games and toys).

- Spending score grouping: putting these groups into bands such as low, medium
and high spending would help us to better target marketing efforts.

- Loyalty tiers: as above, putting customers into groups would help us to 
explore the data further and to target marketing efforts.

Patterns worth investigating further:
- The group of customers achieving disproportionately high loyalty points.
- The funneling of loyalty after remuneration hits 60, but are in low loyalty.
- Whether grouped product catergories show clear loyalty patterns for us to 
better target marketing campaigns.
"

# ==============================================================================
  # Additional analysis after exploratory analysis
# ==============================================================================
# Add a column to bucket customers into low, medium and high loyalty
reviews <- reviews %>%
  mutate(
    loyalty_bucket = dplyr :: case_when(
      .data$loyalty_points < 500 ~ 'Low',
      .data$loyalty_points >= 500 & loyalty_points < 2000 ~ 'Medium',
      .data$loyalty_points >= 2000 ~ 'High',
      TRUE ~ NA_character_
    )
  )

table(reviews$loyalty_bucket)

# ==============================================================================
  # Insights into high loyalty demographic
# ==============================================================================
# Create a new dataset filtered to just high loyalty
high_loyalty <- reviews %>%
  filter(loyalty_bucket == 'High')

# Patterns of customers by education with high loyalty bar chart
ggplot(high_loyalty, aes(x = education, fill=education)) +  
  geom_bar(position = 'dodge') +  
  scale_y_continuous(breaks = seq(0, 240, 20),"Frequency") +
  labs(title=paste('High loyalty customers are most commonly educated',
  'to graduate level')) +
  theme_classic()

# Patterns of customers by gender with low loyalty and high pay bar chart
ggplot(high_loyalty, aes(x = gender, fill=gender)) +  
  geom_bar(position = 'dodge') +
  scale_y_continuous(breaks = seq(0, 240, 20),"Frequency") +
  labs(title='High loyalty customers are more commonly female than male') +
  theme_classic()

# Patterns of customers by age with high loyalty bar chart
ggplot(high_loyalty, aes(x = age)) +
  geom_histogram(bins=15, fill = 'orange') + 
  scale_x_continuous(breaks = seq(0, 80, 10),"Age") + 
  scale_y_continuous(breaks = seq(0, 160, 20),"Frequency") +
  labs(title = "High-loyalty customers concentrate in the 30-40 age range") +
  theme_classic()

# Relationship between income and loyalty for high loyalty
ggplot(high_loyalty, aes(x = remuneration)) +
  geom_histogram(bins=15, fill = 'darkorchid1') + 
  scale_x_continuous(breaks = seq(0, 150, 10), "Remuneration") + 
  scale_y_continuous(breaks = seq(0,80,10), "Loyalty Points") +
  labs(title = "Higher income is more common among high-loyalty customers") +
  theme_classic()

# Relationship between spending and loyalty for high loyalty
ggplot(high_loyalty, aes(x = spending_score)) +
  geom_histogram(bins=15, fill = 'palegreen4') + 
  scale_x_continuous(breaks = seq(0, 80, 10),"spending_score") + 
  scale_y_continuous(breaks = seq(0, 500, 100),"Frequency") +
  labs(title = "High loyalty customers by spending_score") +
  theme_classic()

# ==============================================================================
  # Insights into low loyalty high pay demographic
# ==============================================================================
# Create a new dataset filtered to just low loyalty and high pay
lowloyalty_highpay <- reviews %>%
  filter(loyalty_bucket == 'Low' &
           remuneration >= 60)

# Patterns of customers by education with low loyalty and high pay bar chart
ggplot(lowloyalty_highpay, aes(x = education, fill=education)) +  
  geom_bar(position = 'dodge') +  
  labs(title=paste('Graduate and postgradute educated customers make up the',
                   '\nmajority of low-loyalty, high-income segment')) +
  theme_classic()

# Patterns of customers by gender with low loyalty and high pay bar chart
ggplot(lowloyalty_highpay, aes(x = gender, fill=gender)) +  
  geom_bar(position = 'dodge') +  
  labs(title=paste('The low-loyalty, high-income segment is dominated by',
                   'male customers')) +
  theme_classic()

# Patterns of customers by age with low loyalty and high pay bar chart
ggplot(lowloyalty_highpay, aes(x = age)) +
  geom_histogram(bins=15, fill = 'orange') + 
  scale_x_continuous(breaks = seq(0, 80, 10),"Age") + 
  scale_y_continuous(breaks = seq(0, 30, 5),"Frequency") +
  labs(title = paste('Low-loyalty, high-income customers spen a wide age', 
                  'range, with greater \nconcentration in their 20s and 30s')) +
  theme_classic()

# Patterns of customers by income with low loyalty and high pay bar chart
ggplot(lowloyalty_highpay, aes(x = remuneration)) +
  geom_histogram(bins=15, fill = 'darkorchid1') + 
  scale_x_continuous(breaks = seq(0, 120, 10),"Remuneration") + 
  scale_y_continuous(breaks = seq(0, 30, 10),"Frequency") +
  labs(title = "Low loyalty but high income customers by income") +
  theme_classic()

# Patterns of customers by spending with low loyalty and high pay bar chart
ggplot(lowloyalty_highpay, aes(x = spending_score)) +
  geom_histogram(bins=15, fill = 'palegreen4') + 
  scale_x_continuous(breaks = seq(0, 20, 5),"spending_score") + 
  scale_y_continuous(breaks = seq(0, 15, 5),"Frequency") +
  labs(title = "Low loyalty but high income customers by spending_score") +
  theme_classic()

"
Insights uncovered:
- Within the high loyalty bracket the demographic of customers is more likely 
to be women with graduate level education aged 30-40.
- Within the low loyalty and high pay bracket, the demographic is likely to be
men with graduate or postgraduate level education, either early 20s or mid 30s.
"

# ==============================================================================
  # Using Multiple Linear Regression to investigate customer behaviour and the 
  # Effectiveness of the loyalty programme
# ==============================================================================
# Compute descriptive statistics.
summary(reviews)

# Determine range
max(reviews$loyalty_points)- min(reviews$loyalty_points) 

# Calculate Inter-Quartile Range (IQR).
IQR(reviews$loyalty_points)  

# Determine the variance and standard deviation.
var(reviews$loyalty_points)  
sd(reviews$loyalty_points)  

# Use the summary() function.
summary(reviews$loyalty_points)
summary(reviews$gender)
summary(reviews$age)
summary(reviews$remuneration)
summary(reviews$spending_score)
summary(reviews$education)
summary(reviews$product)

# Determine normality of data.
# Specify qqnorm function (draw a qqplot).
qqnorm(reviews$loyalty_points)
qqline(reviews$loyalty_points) 

# Shapiro-Wilk test:
shapiro.test(reviews$loyalty_points)

# Skewness and Kurtosis
install.packages('moments') 
library(moments)

skewness(reviews$loyalty_points) 
kurtosis(reviews$loyalty_points)


"
Comments on distributions and patterns in the data:
Looking at loyalty points:
  - The mean (1578) is higher than the median (1276)
    suggesting a right-skewed distribution.
  - Most customers fall in the middle range of loyalty with IQR at 979.25.
  - The maximum loyalty point accumulated is 6847 suggesting there are some 
  outlier customers accumulating very high number of points.This suggests there
  are some customers creating an uneven balance in loyalty and could maybe be
  considered 'power users'.
  - The Q-Q plot confirms this, showing a strong deviation from normality in the
  upper tail.
  - The shapiro-wilk test shows a p-value near 0, suggesting there is strong
  evidence to reject normality.
  - Skewness of 1.46 suggests loyalty is moderate to strongly right skewed.
  - Kurtosis is quite a bit above 3 suggesting there are bigger outliers than 
  expected under normality.
  
Looking at demographics:
  - Age and remuneration show concentrated central distributions.
  - Spending is more dispersed indicating heterogeneous engagement.

Feature selection:
  - Age: as seen in the previous MLR in python, once age is considered alongside 
  spending and remuneration it becomes more statistically significant.
  - Spending_score: this is likely the biggest factor affecting loyalty with 
  it appearing to react more to behavioural factors than demographic.
  - Remuneration: similar to above there appears to be a fairly strong positive
  correlation between loyalty and pay.
  - Gender and Education: i do not anticipate these will be strong predictors
  but considering previous analysis seeing the gender balance shifts with loyalty
  amount and pay amount are considered including them will allow us to test if 
  they matter at all.
  - Product: i will not include this as the category is too cardinal to extract
  meaningful insight.

Potential concerns and corrective action:
  - Loyalty points are skewed: the strong right skew observes in loyalty may lead
  to non-normal and heteroscedastic residuals when fitting linear regression. If
  we apply a log transformation to loyalty points it may help reduce the 
  influence of extreme values and improve model performance.
"

# ==============================================================================
  # Transform loyalty_points
# ==============================================================================
reviews$log_loyalty <- log(reviews$loyalty_points)

# Determine normality of data.
# Specify qqnorm function (draw a qqplot).
qqnorm(reviews$log_loyalty,
       main = 'Normal Q-Q Plot of Log-Transformed Loyalty Points')
qqline(reviews$log_loyalty) 

"
After log-transformation the new Q-Q plot shows values at the right-tail now
fits much better to normality. The values at the lower tail appear to deviate
more at the lower tail however, the overall fit of the plot is an improvement.
"

# ==============================================================================
  # Create a MLR model
# ==============================================================================
modela = lm(log_loyalty~age+spending_score+remuneration+gender+education,
              data=reviews)

summary(modela) 

# Remove education from model
modelb = lm(log_loyalty~age+spending_score+remuneration+gender,
            data=reviews)

summary(modelb) 

# Model without logged loyalty and all features
modelc = lm(loyalty_points~age+spending_score+remuneration+gender+education,
            data=reviews)

summary(modelc) 

# Model without logged loyalty and remove education
modeld = lm(loyalty_points~age+spending_score+remuneration+gender,
            data=reviews)

summary(modeld) 

"
Evaluation of models:
- Modela: The log-transformed models explain over 80% of the variation in 
loyalty, a  strong overall fit. Spending score, remuneration, age, and gender 
all appeared as statistically significant predictors, while education showed 
little additional explanatory power therefore i removed this from the 2nd 
variation of the model.

- Modelb: this was the same model minus education. Here we saw little impact 
to the R-squared value supporting the theory that education held little 
explanatory power and reduced the complexity of the model.

- Modelc/d: i then ran the same models but on the loyalty_points without the
log-transformation applied. In these models, we see the same impact to 
feature selection. However, the difference comes when looking at the residual
standard error; in the transformed models this value is very low (less than 1)
whereas in the untransformed models this is over 500, reflecting the large
indluence of extreme loyalty values. This indicates that the transformed models
provide more stable estimates.

Conclusion: the improved standard errors and better alignment with the models
assumption of normality, modelb is the more reliable model to use when looking
at the relationship between customer characteristics and loyalty accumulation.
"

# ==============================================================================
  # Visualise the model
# ==============================================================================
reviews$predicted_log_loyalty <- fitted(modelb)

# Plot actual vs predicted
plot(
  reviews$log_loyalty,
  reviews$predicted_log_loyalty,
  xlab = 'Observed log(loyalty points)',
  ylab = 'Predicted log(loyalty points',
  main = 'Observed vs predicted loyalty'
)
# Add a reference line
abline(0,1,col='red')


# Plot residuals vs fitted values
plot(
  fitted(modelb),
  resid(modelb),
  xlab = 'Fitted values',
  ylab = 'Residuals',
  main = 'Residuals vs Fitted values'
)
abline(h=0,col='red')

"
Insights observed:
- Observed vs predicted: most points lie close to the red line (perfect 
predictions), indicating that the model predicts loyalty reasonably well.
This suggests a strong overall fit.

- Residuals vs Fitted: Overall the residuals are centered around zero, so 
the model does not consistently over/under predict loyalty. However, there is 
some curvature present, suggesting not all variation is fully caprured by the 
model, and there is still some structure. Though because these are not
extreme the model provides an acceptable representation of the data.

Usefulness of the model
- This model helps us understand which type of customers are more likely to 
accumulate points. This model shows loyalty is strongly linked to spending
and income, meaning we can better identify customers with high potential but
that are not yet fully engaged. We could input the characteristics of a target
group to estimate their loyalty, which would help marketing teams decide who
to focus on and what incentives would be effective.

Potential improvements:
- The model performs well, but there remains some patterns in the residuals 
therefore including additional features such as length of time as a customer, 
more high-level product categories etc could improve the models explanatory
power. 

Alternative approaches:
- Using classification models to be used to predict which loyalty group a 
certain kind of customer would fall into, which might be easier than predicting 
exact loyalty values.
"

# ==============================================================================
  # Demonstration of how to use the model to predict loyalty
# ==============================================================================
# Create a scenario data set based on a desirable demographic:
  # Low loyalty, low spend but high remuneration (male/graduate)
scenario_target <- data.frame(
  age = 35,
  spending_score = 30,
  remuneration = 70,
  gender = 'Male',
  education = 'Graduate'
)

# Predict loyalty for this scenario
predicted_log_loyalty <- predict(modelb, newdata = scenario_target)

# Exponential of the log prediction
exp(predicted_log_loyalty)

"
For a high-income but low-spending customer profile (male, graduate, mid 30s),
the model predicts loyalty of approximatelu 890. This is relatively low to 
high-loyalty customers. This suggests that despite strong earning potential,
these customers show low engagement. This further supports the idea that we can
target these customers with incentives to increase participation.
"

# Visualise spending against predicted loyalty
# Create a sequence of spending scores against a typical customer
spending_range <- data.frame(
  age = 35,
  spending_score = seq(
    min(reviews$spending_score, na.rm = TRUE),
    max(reviews$spending_score, na.rm = TRUE),
    length.out = 100
  ),
  remuneration = 50,
  gender = 'Female',
  education = 'graduate'
)

# Predict loyalty using modelb (final model)
spending_range$predicted_log_loyalty <- predict(
  modelb,
  newdata = spending_range
)

# Convert prediction back to loyalty range
spending_range$predicted_loyalty <- exp(spending_range$predicted_log_loyalty)

# ==============================================================================
  # Visualise the model
# ==============================================================================

# Plot predicted loyalty against spending score
plot(
  spending_range$spending_score,
  spending_range$predicted_loyalty,
  type = 'l',
  lwd = 2,
  col = 'steelblue',
  xlab = 'Spending Score',
  ylab = 'Predicted loyalty points',
  main = paste('Predicted loyalty increases as spending increases for',
  'a typical customer')
)
