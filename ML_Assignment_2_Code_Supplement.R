# TRANSFERRED FROM R MARKDOWN FORMAT - ADD EACH CELL DENOTED BY "###" 
# COPY ACROSS INTO R MARKDOWN FOR MORE LEGIBLE OUTPUTS OTHERWISE THERE 
# WILL BE AN ABSURD NUMBER OF PLOTS 
# SOME DATA PROCESSING OCCURS WHERE COLUMNS ARE CREATED AND THEN IMMEDIATELY 
# REMOVED. THIS IS INTENTIONAL TO ALLOW FOR RUNNING WITH DIFFERENT COMBINATIONS 
# OF COLUMNS BY COMMENTING OUT 

library(dplyr)
library(ggplot2)
library(scales)
library(GGally)
library(corrplot)
library(skimr)
library(knitr)
library(kableExtra)
library(ggparty)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

file = "https://raw.githubusercontent.com/ccscaiado/MLRepo/main/Assignment%202%20Datasets/Pokemon/pokemon.csv"

pokemon_data = read.csv(file, header = TRUE)

seed = 123456789

type_combinations_flag = FALSE

###

# Check for NAs 
colSums(is.na(pokemon_data))

###

# Check column data types 
sapply(pokemon_data, class)

###

# Check unique combinations of type1 and type2 to work out if it is feasible to predict unique combinations as opposed to type1 
pokemon_data %>% 
  dplyr::mutate(type2 = if_else(type1 == type2, "", type2)) %>% 
  group_by(type1, type2) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  dplyr::arrange(desc(count), type1, type2) 

###

pokemon_data_clean = pokemon_data

# Assign height, weight, and type2 values manually scraped from serebii 
# Add Alolan version exists flag 
pokemon_data_clean = pokemon_data_clean %>% 
  dplyr::mutate(height_m = case_when(name == "Rattata" ~ 0.3, 
                                     name == "Raticate" ~ 0.7,
                                     name == "Raichu" ~ 0.8,
                                     name == "Sandshrew" ~ 0.6,
                                     name == "Sandslash" ~ 1,
                                     name == "Vulpix" ~ 0.6,
                                     name == "Ninetales" ~ 1.1,
                                     name == "Diglett" ~ 0.2,
                                     name == "Dugtrio" ~ 0.7,
                                     name == "Meowth" ~ 0.4,
                                     name == "Persian" ~ 1,
                                     name == "Geodude" ~ 0.4,
                                     name == "Graveler" ~ 1,
                                     name == "Golem" ~ 1.4,
                                     name == "Grimer" ~ 0.9,
                                     name == "Muk" ~ 1.2,
                                     name == "Exeggutor" ~ 2,
                                     name == "Marowak" ~ 1,
                                     name == "Hoopa" ~ 0.5,
                                     name == "Lycanroc" ~ 0.8, 
                                     TRUE ~ height_m)) %>% 
  dplyr::mutate(weight_kg = case_when(name == "Rattata" ~ 3.5, 
                                      name == "Raticate" ~ 18.5,
                                      name == "Raichu" ~ 30,
                                      name == "Sandshrew" ~ 12,
                                      name == "Sandslash" ~ 29.5,
                                      name == "Vulpix" ~ 9.9,
                                      name == "Ninetales" ~ 19.9,
                                      name == "Diglett" ~ 0.8,
                                      name == "Dugtrio" ~ 33.3,
                                      name == "Meowth" ~ 4.2,
                                      name == "Persian" ~ 32,
                                      name == "Geodude" ~ 20,
                                      name == "Graveler" ~ 105,
                                      name == "Golem" ~ 300,
                                      name == "Grimer" ~ 30,
                                      name == "Muk" ~ 30,
                                      name == "Exeggutor" ~ 120,
                                      name == "Marowak" ~ 45,
                                      name == "Hoopa" ~ 9,
                                      name == "Lycanroc" ~ 25, 
                                      TRUE ~ weight_kg)) %>% 
  dplyr::mutate(type2 = case_when(name == "Rattata" ~ "", 
                                  name == "Raticate" ~ "",
                                  name == "Raichu" ~ "",
                                  name == "Sandshrew" ~ "",
                                  name == "Sandslash" ~ "",
                                  name == "Vulpix" ~ "",
                                  name == "Ninetales" ~ "",
                                  name == "Diglett" ~ "",
                                  name == "Dugtrio" ~ "",
                                  name == "Meowth" ~ "",
                                  name == "Persian" ~ "",
                                  name == "Geodude" ~ type2,
                                  name == "Graveler" ~ type2,
                                  name == "Golem" ~ type2,
                                  name == "Grimer" ~ "",
                                  name == "Muk" ~ "",
                                  name == "Exeggutor" ~ type2,
                                  name == "Marowak" ~ "",
                                  name == "Hoopa" ~ type2,
                                  name == "Lycanroc" ~ type2, 
                                  TRUE ~ type2)) %>% 
  dplyr::mutate(alolan_form_exists = case_when(name == "Rattata" ~ 1, 
                                               name == "Raticate" ~ 1,
                                               name == "Raichu" ~ 1,
                                               name == "Sandshrew" ~ 1,
                                               name == "Sandslash" ~ 1,
                                               name == "Vulpix" ~ 1,
                                               name == "Ninetales" ~ 1,
                                               name == "Diglett" ~ 1,
                                               name == "Dugtrio" ~ 1,
                                               name == "Meowth" ~ 1,
                                               name == "Persian" ~ 1,
                                               name == "Geodude" ~ 1,
                                               name == "Graveler" ~ 1,
                                               name == "Golem" ~ 1,
                                               name == "Grimer" ~ 1,
                                               name == "Muk" ~ 1,
                                               name == "Exeggutor" ~ 1,
                                               name == "Marowak" ~ 1,
                                               TRUE ~ 0)) %>% 
  dplyr::mutate(alolan_form_exists = as.factor(alolan_form_exists))

# Post Alolan fix 
# Check unique combinations of type1 and type2 to work out if it is feasible to predict unique combinations as opposed to type1 
pokemon_data_clean %>% 
  dplyr::mutate(type2 = if_else(type1 == type2, "", type2)) %>% 
  group_by(type1, type2) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  dplyr::arrange(desc(count), type1, type2) 

# Initial data processing 
if (type_combinations_flag) {
  pokemon_data_clean = pokemon_data_clean %>% 
    dplyr::mutate(type2 = if_else(type1 == type2, "", type2)) %>% 
    dplyr::mutate(type1 = case_when(type1 == "normal" & type2 == "flying" ~ "normal-flying",
                                    type1 == "grass" & type2 == "poison" ~ "grass-poison",
                                    type1 == "bug" & type2 == "flying" ~ "bug-flying",
                                    type1 == "bug" & type2 == "poison" ~ "bug-poison",
                                    TRUE ~ type1)) 
}

###

pokemon_data_clean = pokemon_data_clean %>% 
  dplyr::select(-c(abilities, japanese_name, name, pokedex_number, type2)) %>% # Remove unnecessary columns 
  dplyr::mutate(capture_rate = as.numeric(if_else(capture_rate == "30 (Meteorite)255 (Core)", "142.5", capture_rate))) %>% # Fix character issue 
  # Minior has a differing catch rate dependent on form based on if it is above or below 50% hp and therefore the expected value for catch rate is 
  # assumed to be (30 + 255) / 2 = 142.5 
  dplyr::mutate(generation = as.factor(generation)) %>% # Make generation a factor 
  dplyr::mutate(is_legendary = as.factor(is_legendary)) %>% # Make is_legendary a factor 
  dplyr::mutate(type1 = as.factor(type1)) %>% # Make type1 a factor 
  dplyr::rename(classification = classfication) %>% 
  dplyr::mutate(classification = as.factor(classification)) %>% # Make classification a factor 
  dplyr::select(-c(classification)) # Remove classification based on it having too many categories (prohibitively long run time for random forest) 


###

# Distribution of Pokemon male sex percentage histogram 
ggplot(data = pokemon_data_clean, aes(x = percentage_male)) +
  geom_histogram(aes(y = ..count..), binwidth = 10, colour = "darkblue", fill = "lightblue") +
  scale_x_continuous(breaks = breaks_width(25)) +
  scale_y_continuous(breaks = breaks_width(50)) +
  labs(x = "Percentage Male", y = "Count", title = "Distribution of Pokemon Male Sex Percentage - Generations 1 to 7") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5), axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"))

###

# Create percentage male categorical feature 
pokemon_data_clean = pokemon_data_clean %>% 
  dplyr::mutate(percentage_male_cat = case_when(is.na(percentage_male) ~ "Genderless", 
                                                percentage_male == 50 ~ "50/50",
                                                percentage_male < 50 ~ "< 50",
                                                percentage_male > 50 ~ "> 50")) %>% # Categorise percentage male 
  dplyr::mutate(percentage_male_cat = as.factor(percentage_male_cat)) %>% # Make percentage_male_cat a factor 
  dplyr::select(-percentage_male) # Remove percentage_male column 

###

# Check for any NAs 
any(is.na(pokemon_data_clean))

###

# Frequency of Pokemon types bar chart 
ggplot(data = pokemon_data_clean, aes(x = type1, fill = type1)) +
  geom_bar(colour = "black") +
  geom_hline(yintercept = 0, colour = "black", size = 0.5) +
  scale_y_continuous(breaks = breaks_width(10)) +
  labs(x = "Pokemon Type", y = "Count", title = "Frequency of Pokemon Types - Generation 1 to 7") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5), axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.35, color = "black"), legend.position = "none")

###

# Create subset with only categorical variables 
pokemon_data_clean_categorical = pokemon_data_clean %>% 
  dplyr::select(c(type1, generation, is_legendary, percentage_male_cat, alolan_form_exists))

# Loop through categorical variables and plot 
for (i in 1:ncol(pokemon_data_clean_categorical)) {
  
  print(ggplot(data = pokemon_data_clean_categorical, aes(x = pokemon_data_clean_categorical[[i]], fill = pokemon_data_clean_categorical[[i]])) +
          geom_bar(colour = "black") +
          geom_hline(yintercept = 0, colour = "black", size = 0.5) +
          labs(x = colnames(pokemon_data_clean_categorical)[i], y = "Count", title = "") +
          theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5), axis.text = element_text(color = "black"), 
                axis.text.x = element_text(angle = 90, vjust = 0.35, color = "black"), legend.position = "none"))
  
}

###

# Create subset with only numeric variables 
pokemon_data_clean_numeric = pokemon_data_clean %>% 
  dplyr::select(-c(type1, generation, is_legendary, percentage_male_cat, alolan_form_exists))

# Loop through numeric variables and plot 
for (i in 1:ncol(pokemon_data_clean_numeric)) {
  
  print(ggplot(data = pokemon_data_clean_numeric, aes(x = pokemon_data_clean_numeric[[i]])) +
          geom_histogram(aes(y = ..count..), colour = "darkblue", fill = "lightblue", bins = 16) +
          labs(x = colnames(pokemon_data_clean_numeric)[i], y = "Count", title = "") +
          theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5), axis.text = element_text(color = "black")))
  
}

###

# Distribution of Normal-type damage multipliers histogram 
ggplot(data = pokemon_data_clean, aes(x = against_normal)) +
  geom_histogram(aes(y = ..count..), colour = "darkblue", fill = "lightblue", bins = 16) +
  scale_x_continuous(breaks = breaks_width(0.25)) +
  scale_y_continuous(breaks = breaks_width(50)) +
  labs(x = "Damage Multipliers Against Normal-type Damage", y = "Count", title = "Distribution of Normal-type Damage Multipliers - Generations 1 to 7") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5), axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"))

###

# Remove categorical variables 
pokemon_data_pairs = pokemon_data_clean %>% 
  dplyr::select(-c(type1, generation, is_legendary, percentage_male_cat, alolan_form_exists))

# Prepare data 
# Use Spearman's Rank Correlation Coefficient because the data is not normally distributed 
pokemon_data_pairs_matrix = cor(pokemon_data_pairs, method = "spearman")

# Create correlation heatmap 
corrplot(pokemon_data_pairs_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 90)

###

# Create correlation heatmap with coefficients 
corrplot(pokemon_data_pairs_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 90, addCoef.col = "black", number.cex = 0.5)

###

# Summarise dataset 
summary(pokemon_data_clean)

###

# Summarise dataset 
skim(pokemon_data_clean)

###

# Create subset with only numeric variables 
pokemon_data_clean_numeric = pokemon_data_clean %>% 
  dplyr::select(-c(type1, generation, is_legendary, percentage_male_cat, alolan_form_exists))

# Create numeric variables summary table 
kable(summary(pokemon_data_clean_numeric)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

###

# Create subset with only categorical variables 
pokemon_data_clean_categorical = pokemon_data_clean %>% 
  dplyr::select(c(type1, generation, is_legendary, percentage_male_cat, alolan_form_exists))

# Create categorical variables summary table 
kable(summary(pokemon_data_clean_categorical)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

###

# Set seed 
set.seed(seed)

# Proportion of data to partition into test split 
test_split = 0.25

# Sample indices based on the test split proportion using stratified random sampling at the type1 level 
# This ensures the test dataset includes a representative proportion of each type1 class from the full dataset 
indices = createDataPartition(pokemon_data_clean$type1, p = test_split, list = FALSE, times = 1)

# Create test set 
test_data = pokemon_data_clean[indices,]
# Create training set 
training_data = pokemon_data_clean[-indices,]

###

# Define cp values to run for tuning 
tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.001))

# Specify 5-fold cross-validation 
fitControl = trainControl(method = "cv", 
                          number = 5)

# Set seed 
set.seed(seed)

# Perform 5-fold cross-validation 
model_cart = train(type1 ~ .,
                   data = training_data,
                   method = "rpart",
                   tuneGrid = tuneGrid,
                   trControl = fitControl)

plot(as.party(model_cart$finalModel))

print(model_cart)

###

# Predict type1 values for test data 
predictions = predict(model_cart, newdata = test_data)
# Print confusion matrix 
confusionMatrix(predictions, test_data$type1)

###

# Define mtry values to run for tuning 
tuneGrid = expand.grid(mtry = c(1:((ncol(pokemon_data_clean) / 2) + 1)))

# Specify 5-fold cross-validation 
fitControl = trainControl(method = "cv", 
                          number = 5)

# Set seed 
set.seed(seed)

# Perform 5-fold cross-validation and mtry tuning 
model_rf = train(type1 ~ ., 
                 data = training_data, 
                 method = "rf", 
                 tuneGrid = tuneGrid, 
                 trControl = fitControl, 
                 metric = "Accuracy")

print(model_rf)

###

# Predict type1 values for test data 
predictions = predict(model_rf, newdata = test_data)
# Print confusion matrix 
confusionMatrix(predictions, test_data$type1)

###

# Manually tuned ntrees as specified in report 

###





