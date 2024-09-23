library(ggplot2)
install.packages("plotly")
library(plotly)

df <- read.csv("C:/Users/Dell/Downloads/archive/ICRISAT-District Level Data.csv")

head(df)
tail(df)

str(df)

summary(df)

colSums(is.na(df))

anyDuplicated(df)

missing_values <- colSums(is.na(df))

numeric_columns <- sapply(df, is.numeric)
df[numeric_columns] <- lapply(df[numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

data_types <- sapply(df, class)

df <- df[!duplicated(df), ]

names(df)

missing_rice_production <- any(is.na(df$`RICE PRODUCTION (1000 tons)`))
missing_rice_area <- any(is.na(df$`RICE AREA (1000 ha)`))

if (missing_rice_production | missing_rice_area) {
  cat("Warning: Missing values found in 'RICE PRODUCTION (1000 tons)' or 'RICE AREA (1000 ha)' columns. Please handle missing values before calculating yield per hectare.\n")
}
df$RICE_YIELD_PER_HECTARE <- df$`RICE.PRODUCTION..1000.tons.` / df$`RICE.AREA..1000.ha.`
valid_rows <- !is.na(df$RICE_YIELD_PER_HECTARE)
rice_yield_per_hectare <- df$RICE_YIELD_PER_HECTARE[valid_rows]
colSums(is.na(df))

#Rice Production
rice_state <- aggregate(df$`RICE.PRODUCTION..1000.tons.`, by = list(df$`State.Name`), FUN = sum)
rice_state <- rice_state[order(-rice_state$x), ]
colnames(rice_state) <- c("State Name", "RICE PRODUCTION (1000 tons)")
rice_state <- head(rice_state, 10)
rice_state
library(ggplot2)
colors <- c("#440154", "#404387", "#29788E", "#22A784", "#79D151", "#FDE724", "#FFE44D", "#FDAE61", "#F46D43", "#D73027")
gg <- ggplot(rice_state, aes(x = reorder(`State Name`, -`RICE PRODUCTION (1000 tons)`), 
                             y = `RICE PRODUCTION (1000 tons)`, fill = `State Name`)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust width of bars
  labs(title = "Top 10 rice productions by States",
       x = "State names",
       y = "Rice Production (1000 tons)") +
  scale_fill_manual(values = colors) +  # Use manual color palette
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +  # Rotate labels vertically
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right") +  # Place legend to the right
  guides(fill = guide_legend(title = "State"))  # Add legend title
print(gg)

#Rice Area
rice_area <- aggregate(df$`RICE.AREA..1000.ha.`, by = list(df$`State.Name`), FUN = sum)
rice_area <- rice_area[order(-rice_area$x), ]
colnames(rice_area) <- c("State Name", "RICE AREA (1000 ha)")
rice_area <- head(rice_area, 10)
rice_area
gg <- ggplot(rice_area, aes(x = `State Name`, y = `RICE AREA (1000 ha)`, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 5) +  # Add points
  labs(title = "Top 10 rice production Areas by States",
       x = "State names",
       y = "Rice Production (1000 ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right")  # Move legend to the right side
gg

#By Year
rice_year <- aggregate(df$`RICE.PRODUCTION..1000.tons.`, by = list(df$Year), FUN = sum)
rice_year <- rice_year[order(-rice_year$x), ]
colnames(rice_year) <- c("Year", "RICE PRODUCTION (1000 tons)")
rice_year <- head(rice_year, 10)

rice_year
library(plotly)
fig <- plot_ly(rice_year, labels = ~Year, values = ~`RICE PRODUCTION (1000 tons)`, type = 'pie',
               textposition = 'inside', textinfo = 'percent+label',
               marker = list(colors = rainbow(nrow(rice_year))),
               hole = 0.4)
fig <- fig %>% layout(title = 'Total Rice Production by 10 years')
fig

#Rice Yeild
rice_yield <- aggregate(df$`RICE.YIELD..Kg.per.ha.`, by = list(df$`State.Name`), FUN = sum)
rice_yield <- rice_yield[order(-rice_yield$x), ]
colnames(rice_yield) <- c("State Name", "RICE YIELD (Kg per ha)")
rice_yield <- head(rice_yield, 10)
rice_yield
gg_yield <- ggplot(rice_yield, aes(x = reorder(`State Name`, -`RICE YIELD (Kg per ha)`), 
                                   y = `RICE YIELD (Kg per ha)`, fill = `State Name`)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Rice yield by 10 states",
       x = "State names",
       y = "Rice yield (Kg per ha)") +
  scale_fill_discrete(name = "Rice yield (Kg per ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right")  # Legend at the right side
gg_yield

#STATISTICAL OPERATION ON RICE
linear_model <- lm(`RICE.AREA..1000.ha.` ~ `RICE.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `RICE.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `RICE.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of rice Area vs Production and Year",
       x = "Year",
       y = "rice Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`RICE.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on Rice
install.packages("caret")
library(caret)
threshold <- median(df$`RICE.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`RICE.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `RICE.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)


#Wheat
#By Production
wheat_state <- aggregate(df$`WHEAT.PRODUCTION..1000.tons.`, by = list(df$`State.Name`), FUN = sum)
wheat_state <- wheat_state[order(-wheat_state$x), ]
colnames(wheat_state) <- c("State Name", "WHEAT PRODUCTION (1000 tons)")
wheat_state <- head(wheat_state, 10)
wheat_state
wheat_state$percentage <- (wheat_state$`WHEAT PRODUCTION (1000 tons)` / sum(wheat_state$`WHEAT PRODUCTION (1000 tons)`)) * 100
gg_wheat <- ggplot(wheat_state, aes(x = factor(1), y = `WHEAT PRODUCTION (1000 tons)`, fill = `State Name`)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Total Wheat Production by 10 states",
       fill = "State Name",
       x = NULL,
       y = "Wheat Production (1000 tons)") +
  theme_void() +
  theme(legend.position = "right")  # Legend at the right side
gg_wheat

#By Area
library(ggplot2)

gg_wheat_area <- ggplot(wheat_area, aes(x = reorder(`State Name`, -`WHEAT AREA (1000 ha)`), 
                                        y = `WHEAT AREA (1000 ha)`, fill = `State Name`)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Wheat Area by 10 states",
       x = "State names",
       y = "Wheat Area (1000 ha)") +
  scale_fill_manual(name = "State Name", values = rainbow(length(unique(wheat_area$`State Name`)))) + # You can change the color palette as needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right")  # Legend at the right side

print(gg_wheat_area)


#By Yeild
wheat_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`WHEAT YIELD (Kg per ha)` = sum(`WHEAT.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`WHEAT YIELD (Kg per ha)`)) %>%
  head(10)
gg_wheat_yield <- ggplot(wheat_yield, aes(x = reorder(`State.Name`, -`WHEAT YIELD (Kg per ha)`), 
                                          y = `WHEAT YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Wheat yield by 10 states",
       x = "State names",
       y = "Wheat Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
gg_wheat_yield

#STATISTICAL OPERATION ON WHEAT
linear_model <- lm(`WHEAT.AREA..1000.ha.` ~ `WHEAT.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `WHEAT.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `WHEAT.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of wheat Area vs Production and Year",
       x = "Year",
       y = "wheat Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`WHEAT.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on WHEAT
threshold <- median(df$`WHEAT.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`WHEAT.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `WHEAT.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)

#Barley
#By area
barley_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`BARLEY.AREA..1000.ha.` = sum(`BARLEY.AREA..1000.ha.`)) %>%
  arrange(desc(`BARLEY.AREA..1000.ha.`)) %>%
  head(10)
barley_area
library(plotly)
fig <- plot_ly(barley_area, x = ~`State.Name`, y = ~`BARLEY.AREA..1000.ha.`, type = "bar", 
               color = ~`BARLEY.AREA..1000.ha.`, colors = c("#440154"),
               text = ~paste('Barley Area (1000 ha):', `BARLEY.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total Barley Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'Barley Area'),
         template = "plotly_gridon")
fig

#By Production
library(dplyr)
barley_pro <- df %>%
  group_by(`State.Name`) %>%
  summarise(`BARLEY.PRODUCTION..1000.tons.` = sum(`BARLEY.PRODUCTION..1000.tons.`)) %>%
  arrange(desc(`BARLEY.PRODUCTION..1000.tons.`)) %>%
  head(10)

print(barley_pro)

#Barley Yield
barley_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`BARLEY YIELD (Kg per ha)` = sum(`BARLEY.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`BARLEY YIELD (Kg per ha)`)) %>%
  head(10)
gg_barley_yield <- ggplot(barley_yield, aes(x = reorder(`State.Name`, -`BARLEY YIELD (Kg per ha)`), 
                                            y = `BARLEY YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total barley yield by 10 states",
       x = "State names",
       y = "barley Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
gg_barley_yield

#STATISTICAL OPERATION ON BARLEY
linear_model <- lm(`BARLEY.AREA..1000.ha.` ~ `BARLEY.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `BARLEY.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `BARLEY.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of barley Area vs Production and Year",
       x = "Year",
       y = "barley Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`BARLEY.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on BARLEY
threshold <- median(df$`BARLEY.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`BARLEY.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `BARLEY.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)


#SUNFLOWER AREA
sunflower_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SUNFLOWER.AREA..1000.ha.` = sum(`SUNFLOWER.AREA..1000.ha.`)) %>%
  arrange(desc(`SUNFLOWER.AREA..1000.ha.`)) %>%
  head(10)
sunflower_area
library(plotly)
fig <- plot_ly(sunflower_area, x = ~`State.Name`, y = ~`SUNFLOWER.AREA..1000.ha.`, type = "bar", 
               color = ~`SUNFLOWER.AREA..1000.ha.`, colors = c("blue"),
               text = ~paste('sunflower Area (1000 ha):', `SUNFLOWER.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total sunflower Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'sunflower Area'),
         template = "plotly_gridon")
fig

#SUNFLOWER PRODUCTION
library(dplyr)

# Assuming df is your dataframe containing the data
sunflower_pro <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SUNFLOWER.PRODUCTION..1000.tons.` = sum(`SUNFLOWER.PRODUCTION..1000.tons.`)) %>%
  arrange(desc(`SUNFLOWER.PRODUCTION..1000.tons.`)) %>%
  head(10)

print(sunflower_pro)

#SUNFLOWER YIELD
sunflower_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SUNFLOWER YIELD (Kg per ha)` = sum(`SUNFLOWER.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`SUNFLOWER YIELD (Kg per ha)`)) %>%
  head(10)
gg_sunflower_yield <- ggplot(sunflower_yield, aes(x = reorder(`State.Name`, -`SUNFLOWER YIELD (Kg per ha)`), 
                                                  y = `SUNFLOWER YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total sunflower yield by 10 states",
       x = "State names",
       y = "sunflower Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
gg_sunflower_yield

#STATISTICAL OPERATION ON SUNFLOWER
linear_model <- lm(`SUNFLOWER.AREA..1000.ha.` ~ `SUNFLOWER.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `SUNFLOWER.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `SUNFLOWER.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of sunflower Area vs Production and Year",
       x = "Year",
       y = "sunflower Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`SUNFLOWER.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on SUNFLOWER
threshold <- median(df$`SUNFLOWER.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`SUNFLOWER.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `SUNFLOWER.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)



#SOYABEAN AREA
soyabean_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SOYABEAN.AREA..1000.ha.` = sum(`SOYABEAN.AREA..1000.ha.`)) %>%
  arrange(desc(`SOYABEAN.AREA..1000.ha.`)) %>%
  head(10)
soyabean_area
library(plotly)
fig <- plot_ly(soyabean_area, x = ~`State.Name`, y = ~`SOYABEAN.AREA..1000.ha.`, type = "bar", 
               color = ~`SOYABEAN.AREA..1000.ha.`, colors = c("#1f77b4"),
               text = ~paste('soyabean Area (1000 ha):', `SOYABEAN.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total soyabean Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'sunflower Area'),
         template = "plotly_gridon")
fig

#SOYABEAN PRODUCTION
library(dplyr)
SOYABEAN_pro <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SOYABEAN.PRODUCTION..1000.tons.` = sum(`SOYABEAN.PRODUCTION..1000.tons.`)) %>%
  arrange(desc(`SOYABEAN.PRODUCTION..1000.tons.`)) %>%
  head(10)

print(SOYABEAN_pro)

#SOYABEAN YIELD
SOYABEAN_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`SOYABEAN YIELD (Kg per ha)` = sum(`SOYABEAN.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`SOYABEAN YIELD (Kg per ha)`)) %>%
  head(10)
gg_SOYABEAN_yield <- ggplot(SOYABEAN_yield, aes(x = reorder(`State.Name`, -`SOYABEAN YIELD (Kg per ha)`), 
                                                y = `SOYABEAN YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total soyabean yield by 10 states",
       x = "State names",
       y = "soyabean Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
gg_SOYABEAN_yield

#STATISTICAL OPERATION ON SOYABEAN
linear_model <- lm(`SOYABEAN.AREA..1000.ha.` ~ `SOYABEAN.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `SOYABEAN.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `SOYABEAN.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of soyabean Area vs Production and Year",
       x = "Year",
       y = "soyabean Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`SOYABEAN.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on SOYABEAN
threshold <- median(df$`SOYABEAN.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`SOYABEAN.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `SOYABEAN.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)



#OILSEEDS AREA
OILSEEDS_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`OILSEEDS.AREA..1000.ha.` = sum(`OILSEEDS.AREA..1000.ha.`)) %>%
  arrange(desc(`OILSEEDS.AREA..1000.ha.`)) %>%
  head(10)
OILSEEDS_area
library(plotly)
fig <- plot_ly(OILSEEDS_area, x = ~`State.Name`, y = ~`OILSEEDS.AREA..1000.ha.`, type = "bar", 
               color = ~`OILSEEDS.AREA..1000.ha.`, colors = c("#1f77b4"),
               text = ~paste('soyabean Area (1000 ha):', `OILSEEDS.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total oilseeds Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'oilseeds Area'),
         template = "plotly_gridon")
fig

#OILSEEDS PRODUCTION
library(dplyr)

# Assuming df is your dataframe containing the data
OILSEEDS_pro <- df %>%
  group_by(`State.Name`) %>%
  summarise(`OILSEEDS.PRODUCTION..1000.tons.` = sum(`OILSEEDS.PRODUCTION..1000.tons.`)) %>%
  arrange(desc(`OILSEEDS.PRODUCTION..1000.tons.`)) %>%
  head(10)

print(OILSEEDS_pro)

#OILSEEDS YIELD
OILSEEDS_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`OILSEEDS YIELD (Kg per ha)` = sum(`OILSEEDS.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`OILSEEDS YIELD (Kg per ha)`)) %>%
  head(10)
gg_OILSEEDS_yield <- ggplot(OILSEEDS_yield, aes(x = reorder(`State.Name`, -`OILSEEDS YIELD (Kg per ha)`), 
                                                y = `OILSEEDS YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total OILSEEDS yield by 10 states",
       x = "State names",
       y = "OILSEEDS Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
gg_OILSEEDS_yield

#STATISTICAL OPERATION ON OILSEEDS
linear_model <- lm(`OILSEEDS.AREA..1000.ha.` ~ `OILSEEDS.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `OILSEEDS.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `OILSEEDS.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of oilseeds Area vs Production and Year",
       x = "Year",
       y = "oilseeds Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`OILSEEDS.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on OILSEEDS
threshold <- median(df$`OILSEEDS.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`OILSEEDS.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `OILSEEDS.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)


#COTTON AREA
COTTON_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`COTTON.AREA..1000.ha.` = sum(`COTTON.AREA..1000.ha.`)) %>%
  arrange(desc(`COTTON.AREA..1000.ha.`)) %>%
  head(10)
COTTON_area
library(plotly)
fig <- plot_ly(COTTON_area, x = ~`State.Name`, y = ~`COTTON.AREA..1000.ha.`, type = "bar", 
               color = ~`COTTON.AREA..1000.ha.`, colors = c("pink"),
               text = ~paste('COTTON Area (1000 ha):', `COTTON.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total COTTON Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'COTTON Area'),
         template = "plotly_gridon")
fig

#COTTON PRODUCTION
library(dplyr)

# Assuming df is your dataframe containing the data
COTTON_pro <- df %>%
  group_by(`State.Name`) %>%
  summarise(`COTTON.PRODUCTION..1000.tons.` = sum(`COTTON.PRODUCTION..1000.tons.`)) %>%
  arrange(desc(`COTTON.PRODUCTION..1000.tons.`)) %>%
  head(10)

print(COTTON_pro)

#COTTON YIELD
COTTON_yield <- df %>%
  group_by(`State.Name`) %>%
  summarise(`COTTON YIELD (Kg per ha)` = sum(`COTTON.YIELD..Kg.per.ha.`)) %>%
  arrange(desc(`COTTON YIELD (Kg per ha)`)) %>%
  head(10)

gg_COTTON_yield <- ggplot(COTTON_yield, aes(x = reorder(`State.Name`, -`COTTON YIELD (Kg per ha)`), 
                                            y = `COTTON YIELD (Kg per ha)`)) +
  geom_line() +
  geom_point() +
  labs(title = "Total COTTON yield by 10 states",
       x = "State names",
       y = "COTTON Yield (Kg per ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

print(gg_COTTON_yield)

#STATISTICAL OPERATION ON COTTON
linear_model <- lm(`COTTON.AREA..1000.ha.` ~ `COTTON.PRODUCTION..1000.tons.` + Year, data = df)
summary(linear_model)
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope_production <- coefficients[2]
slope_year <- coefficients[3]
cat("Intercept:", intercept, "\n")
cat("Production Slope:", slope_production, "\n")
cat("Year Slope:", slope_year, "\n")
gg <- ggplot(df, aes(x = Year, y = `COTTON.PRODUCTION..1000.tons.`)) +
  geom_point(aes(color = `COTTON.AREA..1000.ha.`), alpha = 0.7) +
  labs(title = "Scatter plot of cotton Area vs Production and Year",
       x = "Year",
       y = "cotton Area (1000 ha)") +
  geom_abline(intercept = intercept, slope = slope_production, color = "blue") +
  geom_abline(intercept = intercept, slope = slope_year, color = "blue") +
  scale_color_gradient(low = "green", high = "red")
print(gg)
predicted <- predict(linear_model)
residual <- df$`COTTON.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")

# Confusion Matrxi on COTTON
threshold <- median(df$`COTTON.PRODUCTION..1000.tons.`)
df$HighProduction <- ifelse(df$`COTTON.PRODUCTION..1000.tons.` > threshold, 1, 0)
df$HighProduction <- factor(df$HighProduction, levels = c(0, 1))
set.seed(123)
trainIndex <- createDataPartition(df$HighProduction, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
model <- train(HighProduction ~ `COTTON.AREA..1000.ha.` + Year, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
predictions <- factor(predictions, levels = levels(testData$HighProduction))
confusionMatrix(predictions, testData$HighProduction)



#FRUITS AREA
FRUITS_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`FRUITS.AREA..1000.ha.` = sum(`FRUITS.AREA..1000.ha.`)) %>%
  arrange(desc(`FRUITS.AREA..1000.ha.`)) %>%
  head(10)
FRUITS_area
library(plotly)
fig <- plot_ly(FRUITS_area, x = ~`State.Name`, y = ~`FRUITS.AREA..1000.ha.`, type = "bar", 
               color = ~`FRUITS.AREA..1000.ha.`, colors = c("green"),
               text = ~paste('FRUITS Area (1000 ha):', `FRUITS.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total FRUITS Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'FRUITS Area'),
         template = "plotly_gridon")
fig

#STATISTICAL FOR FRUIT
linea_model <- lm(`FRUITS.AREA..1000.ha.` ~ Year, data = df)
summary(linea_model)
coefficient <- coef(linea_model)
intercep <- coefficient[1]
slop <- coefficient[2]
cat("Intercept:", intercep, "\n")
cat("Slope:", slop, "\n")
gg <- ggplot(df, aes(x = Year, y = `FRUITS.AREA..1000.ha.`)) +
  geom_point(color = "red") +
  labs(title = "Scatter plot of fruits Area vs Year",
       x = "Year",
       y = "fruits Area (1000 ha)")
gg <- gg + geom_abline(intercept = intercep, slope = slop, color = "blue") 
print(gg)
predicte <- predict(linea_model)
residual <- df$`FRUITS.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")


#VEGETABLE AREA
VEGETABLE_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`VEGETABLES.AREA..1000.ha.` = sum(`VEGETABLES.AREA..1000.ha.`)) %>%
  arrange(desc(`VEGETABLES.AREA..1000.ha.`)) %>%
  head(10)
VEGETABLE_area
library(plotly)
fig <- plot_ly(VEGETABLE_area, x = ~`State.Name`, y = ~`VEGETABLES.AREA..1000.ha.`, type = "bar", 
               color = ~`VEGETABLES.AREA..1000.ha.`, colors = c("yellow"),
               text = ~paste('VEGETABLES Area (1000 ha):', `VEGETABLES.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total VEGETABLES Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'VEGETABLES Area'),
         template = "plotly_gridon")
fig

#STATISTICAL OPERATION ON VEGETABLE
linea_model <- lm(`VEGETABLES.AREA..1000.ha.` ~ Year, data = df)
summary(linea_model)
coefficient <- coef(linea_model)
intercep <- coefficient[1]
slop <- coefficient[2]
cat("Intercept:", intercep, "\n")
cat("Slope:", slop, "\n")
gg <- ggplot(df, aes(x = Year, y = `VEGETABLES.AREA..1000.ha.`)) +
  geom_point(color = "green") +
  labs(title = "Scatter plot of vegetables Area vs Year",
       x = "Year",
       y = "vegetables Area (1000 ha)")
gg <- gg + geom_abline(intercept = intercep, slope = slop, color = "blue") 
print(gg)
predicte <- predict(linea_model)
residual <- df$`VEGETABLES.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")



#POTATOES AREA
POTATOES_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`POTATOES.AREA..1000.ha.` = sum(`POTATOES.AREA..1000.ha.`)) %>%
  arrange(desc(`POTATOES.AREA..1000.ha.`)) %>%
  head(10)
POTATOES_area
library(plotly)
fig <- plot_ly(POTATOES_area, x = ~`State.Name`, y = ~`POTATOES.AREA..1000.ha.`, type = "bar", 
               color = ~`POTATOES.AREA..1000.ha.`, colors = c("#1f77b4"),
               text = ~paste('POTATOES Area (1000 ha):', `POTATOES.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total POTATOES Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'POTATOES Area'),
         template = "plotly_gridon")
fig

#STATISTICAL OPERATION ON POTATOES
linea_model <- lm(`POTATOES.AREA..1000.ha.` ~ Year, data = df)
summary(linea_model)
coefficient <- coef(linea_model)
intercep <- coefficient[1]
slop <- coefficient[2]
cat("Intercept:", intercep, "\n")
cat("Slope:", slop, "\n")
gg <- ggplot(df, aes(x = Year, y = `POTATOES.AREA..1000.ha.`)) +
  geom_point(color = "green") +
  labs(title = "Scatter plot of potatoes Area vs Year",
       x = "Year",
       y = "potatoes Area (1000 ha)")
gg <- gg + geom_abline(intercept = intercep, slope = slop, color = "blue") 
print(gg)
predicte <- predict(linea_model)
residual <- df$`POTATOES.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")


#ONION AREA
ONION_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`ONION.AREA..1000.ha.` = sum(`ONION.AREA..1000.ha.`)) %>%
  arrange(desc(`ONION.AREA..1000.ha.`)) %>%
  head(10)
ONION_area
library(plotly)
fig <- plot_ly(ONION_area, x = ~`State.Name`, y = ~`ONION.AREA..1000.ha.`, type = "bar", 
               color = ~`ONION.AREA..1000.ha.`, colors = c("red"),
               text = ~paste('ONION Area (1000 ha):', `ONION.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total ONION Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'ONION Area'),
         template = "plotly_gridon")
fig

#STATISTICAL OPERATION ON ONION
linea_model <- lm(`ONION.AREA..1000.ha.` ~ Year, data = df)
summary(linea_model)
coefficient <- coef(linea_model)
intercep <- coefficient[1]
slop <- coefficient[2]
cat("Intercept:", intercep, "\n")
cat("Slope:", slop, "\n")
gg <- ggplot(df, aes(x = Year, y = `ONION.AREA..1000.ha.`)) +
  geom_point(color = "green") +
  labs(title = "Scatter plot of onion Area vs Year",
       x = "Year",
       y = "onion Area (1000 ha)")
gg <- gg + geom_abline(intercept = intercep, slope = slop, color = "blue") 
print(gg)
predicte <- predict(linea_model)
residual <- df$`ONION.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")


#FODDER AREA
FODDER_area <- df %>%
  group_by(`State.Name`) %>%
  summarise(`FODDER.AREA..1000.ha.` = sum(`FODDER.AREA..1000.ha.`)) %>%
  arrange(desc(`FODDER.AREA..1000.ha.`)) %>%
  head(10)
FODDER_area
library(plotly)
fig <- plot_ly(FODDER_area, x = ~`State.Name`, y = ~`FODDER.AREA..1000.ha.`, type = "bar", 
               color = ~`FODDER.AREA..1000.ha.`, colors = c("#22A784"),
               text = ~paste('FODDEER Area (1000 ha):', `FODDER.AREA..1000.ha.`),
               width = 800, height = 600) %>%
  layout(title = 'Total FODDER Area by 10 states',
         xaxis = list(title = 'State names'),
         yaxis = list(title = 'FODDER Area'),
         template = "plotly_gridon")
fig

#Statistical operation on Fodder
linea_model <- lm(`FODDER.AREA..1000.ha.` ~ Year, data = df)
summary(linea_model)
coefficient <- coef(linea_model)
intercep <- coefficient[1]
slop <- coefficient[2]
cat("Intercept:", intercep, "\n")
cat("Slope:", slop, "\n")
gg <- ggplot(df, aes(x = Year, y = `FODDER.AREA..1000.ha.`)) +
  geom_point(color = "green") +
  labs(title = "Scatter plot of fodder Area vs Year",
       x = "Year",
       y = "fodder Area (1000 ha)")
gg <- gg + geom_abline(intercept = intercep, slope = slop, color = "blue") 
print(gg)
predicte <- predict(linea_model)
residual <- df$`FODDER.AREA..1000.ha.` - predicted
ms <- mean(residual^2)
rms <- sqrt(ms)
cat("Root Mean Square Error (RMSE):", rms, "\n")










