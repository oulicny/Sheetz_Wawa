##### Import Data #####

poll_responses <- read.delim("Survey Responses.txt") # read in poll response data
poll_weights <- read.delim("urvey Weights.txt") # read in poll weight data

library(tidyverse) # load package for cleaning/transforming data that I like to use

as_tibble(poll_responses) # provides a quick look at the table (note: I prefer looking at the table in the next tab)
as_tibble(poll_weights)

##### Clean/Check Data #####
str(poll_responses) # see the structure of the dataset to make sure variables are correctly classified (e.g. numerical variables are not characters)
str(poll_weights) # both have variables correctly classified

table(poll_responses$Q2_Store_Preference) # see "Lean Sheetz" not properly classified
poll_responses$Q2_Store_Preference[which(poll_responses$Q2_Store_Preference == ":ean Sheetz")] <- "Lean Sheetz"

table(poll_responses$Q3_Wawa_chance) 
table(poll_responses$Q4_Sheetz_chance) # both don't have the same problem
 
###### Exploration/Transformation #####
#see breakdown of Wawa vs. Sheetz by county
table(poll_responses$County)
table(poll_responses$Media.Market) # too much variance in media market, doesn't show the differences within each media region

# decide to group by area to capture spillover effects of Wawa popularity and more realistic for biz decisions
# seeing county by county is too granular

poll_responses2 <- poll_responses %>% 
 mutate(area = case_when(
   County %in% c("bucks", "delaware", "montgomery", "chester") ~ "SE PA",
   County == "philadelphia" ~ "Philadelphia",
   County == "allegheny" ~ "Allegheny",
   County %in% c("carbon", "lackawanna", "lehigh", "luzerne", "monroe", "northampton",
                 "pike", "schuylkill", "susquehanna", "wayne", "wyoming") ~ "NE PA",
   County %in% c("adams", "berks", "cumberland", "dauphin", "franklin", "lancaster",
                 "lebanon", "perry", "york") ~ "Dutch",
   County %in% c("armstrong", "bedford", "cameron", "centre", "clarion", "clearfield",
                 "clinton", "columbia", "elk", "forest", "fulton", "huntingdon",
                 "indiana", "jefferson", "juniata", "lycoming", "mckean", "mifflin",
                 "montour", "northumberland", "potter", "snyder", "sullivan", "tioga",
                 "union", "venango", "warren") ~ "Central",
   County %in% c("beaver", "butler", "cambria", "crawford", "erie", "fayette",
                 "greene", "lawrence", "mercer", "somerset", "washington", "westmoreland") ~ "West"
   )) # breakdown by area

# Note: idea for regions comes from WaPo's "Seven States of Pennsylvania"
# https://www.washingtonpost.com/graphics/2020/politics/pennsylvania-political-geography/

poll_responses2 %>% 
  group_by(area, Q2_Store_Preference) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(area, -n), y = n, fill = Q2_Store_Preference)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(x = "Area", y = "Preferences", fill = 'Preferences') +
  theme_bw() # graph showing breakdown of preferences by area with largest area at bottom

# with this graph, you can see Wawa is preferred brand in almost every 
# region except West, Central; Northeast and Dutch less clear

# we narrow down the areas that we are focused on -- West, Central, Dutch, NE PA
poll_responses_total %>% 
group_by(area, Q2_Store_Preference) %>% 
  filter(area %in% c("West", "NE PA", "Dutch", "Central")) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(area, -n), y = n, fill = Q2_Store_Preference)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(x = "Area", y = "Preferences", fill = 'Preferences') +
  theme_bw() 

##### Strategy Analysis #####
## we can examine this data in two ways to see potential for growth:
# 1) % undecided/lean and see the wiggle room there
# 2) number of people in a given score bracket (depends on accuracy of model)
# for both, we also need to see how many people are in a given area according to weighted results

# first, need to see which regions are the most populous
poll_responses_total %>% 
  filter(area %in% c("West", "NE PA", "Dutch", "Central")) %>% 
  group_by(area) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) # Dutch is the most populous of four by response count, followed by West, NE PA, Central

poll_responses_total %>% 
  filter(area %in% c("West", "NE PA", "Dutch", "Central")) %>% 
  group_by(area) %>% 
  count(area, wt = Weights) # see oversampling for each of these regions, so ranking still same

# method 1: pct breakdown of responses
poll_responses_total %>% 
  filter(area == "West") %>% 
  group_by(area, Q2_Store_Preference) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = round((n/sum(n)*100), 2)) %>% 
  arrange(desc(prop))

poll_responses_total %>% 
  filter(area == "NE PA") %>% 
  group_by(area, Q2_Store_Preference) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = round((n/sum(n)*100), 2)) %>% 
  arrange(desc(prop))

poll_responses_total %>% 
  filter(area == "Dutch") %>% 
  group_by(area, Q2_Store_Preference) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = round((n/sum(n)*100), 2)) %>% 
  arrange(desc(prop))

poll_responses_total %>% 
  filter(area == "Central") %>% 
  group_by(area, Q2_Store_Preference) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = round((n/sum(n)*100), 2)) %>% 
  arrange(desc(prop))

# note how polarizing the Wawa/Sheetz choice preferences are (very few undecided)
# see Wawa struggled in survey with respondents in West, Central

# can make a decision from this data that Wawa should not pursue activity in Central and West regions 
# while remaining competitive in the populous Dutch, NE PA regions
# and try to devote resources from West+Central to boost sales in Dutch/NE PA

## Method 2: predictive modeling scores
# want to measure how well these scores predict who will go for Wawa or Sheetz
# know from the prompt that it's a two-way score
# i.e. 60 ~ 60% Wawa, 40% Sheetz

## can create prediction tables to find good cutoff score and see who is incorrectly classified
## for these , I try to find upper and lower bounds of the scores to find the universe of undecided/leaner customers

# 1) find lowest bound without sacrificing the accuracy of model predictions

# cutoff at 50.1 (i.e. 50.1 and above are Wawa shoppers, else Sheetz)
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 50.1), 1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table

(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) # 86.7% accuracy

str(poll_responses_total$Q2_Store_Preference) # structure of response variable is character

poll_responses_total$preference_fct <- factor(poll_responses_total$Q2_Store_Preference,
       levels = c("Wawa", "Lean Wawa", "Sheetz", "Lean Sheetz", "Other", "Undecided")) # recode to factor

ggplot(poll_responses_total, aes(x = Wawa_preference_score_December)) +
  geom_freqpoly(aes(color = preference_fct), binwidth = 10) +
  labs(x = "Wawa Preference Score", y = "Count", color = 'Preferences', title = "Frequency Distribution of Likely PA Shoppers") # want to see where there's room to peel off Sheetz people and pick up undecided/leaners

## cutoff at 45
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 45.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table

(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) 
# 86.8% accuracy

## cutoff at 40
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 40.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #86.9% accuracy

## cutoff at 35
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 35.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #87.0% accuracy

## cutoff at 30
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 30.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #86.9% accuracy

## cutoff at 25
poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 25.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #86.7% accuracy

# decision: lower bound of predicted score is 30 for potential Wawa customers

# 2) find upper bound of scores

poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 75.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #86.7% accuracy

poll_responses_total$wawa_predict <- ifelse((poll_responses_total$Wawa_preference_score_December >= 75.0), 
                                            1, 0) # create binary to group the two
poll_responses_total$wawa_predict <- factor(poll_responses_total$wawa_predict,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$wawa_predict, poll_responses_total$Q2_Store_Preference)
predict_table
(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) #86.7% accuracy

# call the upper bound of potential Wawa customers at 75 

## so our universe of potential Wawa customers have a predicted score in [30, 75]

# go back to our list of four persuadable regions - West, NE PA, Dutch, Central

# West
poll_responses_total %>% 
  filter(area == "West" & Wawa_preference_score_December >= 75.0) %>% 
  summarise(n = n())

poll_responses_total %>% 
  filter(area == "West") %>% 
  filter(Wawa_preference_score_December >= 30.0 & Wawa_preference_score_December <= 75.0) %>% 
  summarise(n = n()) # 18% growth potential (25 people)

# NE PA
poll_responses_total %>% 
  filter(area == "NE PA" & Wawa_preference_score_December >= 75.0) %>% 
  summarise(n = n())

poll_responses_total %>% 
  filter(area == "NE PA") %>% 
  filter(Wawa_preference_score_December >= 30.0 & Wawa_preference_score_December <= 75.0) %>% 
  summarise(n = n()) # 22% growth potential (40 people)

# Dutch
poll_responses_total %>% 
  filter(area == "Dutch" & Wawa_preference_score_December >= 75.0) %>% 
  summarise(n = n())

poll_responses_total %>% 
  filter(area == "Dutch") %>% 
  filter(Wawa_preference_score_December >= 30.0 & Wawa_preference_score_December <= 75.0) %>% 
  summarise(n = n()) # 17% growth potential (34 people)

# Central
poll_responses_total %>% 
  filter(area == "Central" & Wawa_preference_score_December >= 75.0) %>% 
  summarise(n = n())

poll_responses_total %>% 
  filter(area == "Central") %>% 
  filter(Wawa_preference_score_December >= 30.0 & Wawa_preference_score_December <= 75.0) %>% 
  summarise(n = n()) # 23% growth potential (21 people)

# SE PA
poll_responses_total %>% 
  filter(area == "SE PA" & Wawa_preference_score_December >= 75.0) %>% 
  summarise(n = n())

poll_responses_total %>% 
  filter(area == "SE PA") %>% 
  filter(Wawa_preference_score_December >= 30.0 & Wawa_preference_score_December <= 75.0) %>% 
  summarise(n = n()) # 14.1% growth potential (45 people)

##### Model Analysis ####
# transform data
str(poll_responses_total)

table(poll_responses_total$Income.bucket) # turn income bracket into factor variable

poll_responses_total$income_fct <- factor(poll_responses_total$Income.bucket, levels = c(
  "00-30k", "030k-50k", "050k-75k", "075k-125k", "125k+"
))

table(poll_responses_total$Gender)
poll_responses_total$gender_fct <- factor(poll_responses_total$Gender, levels = c("Female", "Male"))

table(poll_responses_total$Education)
poll_responses_total$educ_fct <- factor(poll_responses_total$Education)

table(poll_responses_total$Q2_Store_Preference)

poll_responses_total$predict_bin <- ifelse((poll_responses_total$Q2_Store_Preference %in% c("Wawa", "Lean Wawa")), 1, 0)

poll_responses_total$predict_bin <- factor(poll_responses_total$predict_bin,
                                           levels = c(0,1), labels = c("Sheetz", "Wawa"))

m1 <- glm(predict_bin ~ gender_fct + income_fct + educ_fct, family = binomial("logit"), data = poll_responses_total, weights = Weights)
summary(m1)


m2 <- glm(predict_bin ~ gender_fct + income_fct + educ_fct + Race + Age, family = binomial("logit"), data = poll_responses_total, weights = Weights)
summary(m2)
1-logLik(m2)/logLik(m2)

m3 <- glm(predict_bin ~ gender_fct + income_fct + educ_fct + Race + Age + area, family = binomial("logit"), data = poll_responses_total, weights = Weights)
summary(m3)

m4 <- glm(predict_bin ~ Race + income_fct + gender_fct + area, family = binomial("logit"),
          data = poll_responses_total, weights = Weights)
summary(m4)


poll_responses_total$predict_scores_april21 <- predict.glm(m3, newdata = poll_responses_total, type = "response")

# find scores
poll_responses_total$predict_apr21 <- ifelse((poll_responses_total$predict_scores_april21 >= .501), 1, 0) # create binary to group the two
poll_responses_total$predict_apr21 <- factor(poll_responses_total$predict_apr21,
                                            levels = c(0, 1), labels = c("Sheetz", "Wawa"))

predict_table <- table(poll_responses_total$predict_apr21, poll_responses_total$Q2_Store_Preference)
predict_table

(predict_table_acc <- ((predict_table[1,4] + predict_table[2,6])/sum(predict_table))) # 61.0% accurate


##### Notes #####
## Books/Resources used

# R for Data Science textbook (https://r4ds.had.co.nz/) - tidyverse, ggplot
# Marketing Data Science textbook (https://www.amazon.com/Marketing-Data-Science-Techniques-Predictive-ebook/dp/B00XANZZ4A) - predictive accuracy tables (I've had the kindle version of this book for a while and previously used for another proejct)


