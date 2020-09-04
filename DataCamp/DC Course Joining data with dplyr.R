# DataCamp Course: Joining Data with dplyr
# https://learn.datacamp.com/courses/joining-data-with-dplyr
# 2020-07-27
rm(list = ls())
library(tidyverse)

sets <- readRDS("data/sets.rds")
themes <- readRDS("data/themes.rds")
parts <- readRDS("data/parts.rds")
part_categories <- readRDS("data/part_categories.rds")
inventories <- readRDS("data/inventories.rds")
inventory_parts <- readRDS("data/inventory_parts.rds")
colors <- readRDS("data/colors.rds")

#----- Ch1 Joining tables -----
parts %>% 
  inner_join(part_categories, by = c("part_cat_id" = "id"))

# nicer naming for cols with same name in both tables
parts %>% 
  inner_join(part_categories, by = c("part_cat_id" = "id"), suffix = c("_part", "_category"))


parts %>% 
  inner_join(inventory_parts, by = ("part_num"))

#sets (set_num) inventories (id/inventory_id) inventory_parts (color_id/id) colors
sets %>%
  inner_join(inventories, by = "set_num") %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  inner_join(colors, by = c("color_id" = "id"), suffix = c("_set", "_color")) %>% 
  count(name_color) %>% 
  arrange

#----- Ch 2 Left and Right Joins -----
inventory_parts_joined <- inventories %>% 
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  select(-id, -version) %>% 
  arrange(desc(quantity))

millennium_falcon <- inventory_parts_joined %>%  filter(set_num == "7965-1")
star_destroyer <- inventory_parts_joined %>% filter(set_num == "75190-1")

millennium_falcon %>% 
  inner_join(star_destroyer, by = c("part_num", "color_id"), suffix = c("_falcon", "_star_destroyer"))

millennium_falcon_colors <- millennium_falcon %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))
star_destroyer_colors <- star_destroyer %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))
millennium_falcon_colors %>% 
  left_join(star_destroyer_colors, by = "color_id", suffix = c("_falcon", "_star_destroyer"))

# find sets with no version 1
inventory_version_1 <- inventories %>%
  filter(version == 1)
sets %>%
  left_join(inventory_version_1, by = "set_num") %>%
  # Filter for where version is na
  filter(is.na(version))


parts %>% 
  count(part_cat_id) %>% 
  right_join(part_categories, by = c("part_cat_id" = "id"))
  
parts %>% 
  count(part_cat_id) %>% 
  right_join(part_categories, by = c("part_cat_id" = "id")) %>% 
  # Filter for NA
  #filter(is.na(n))
  replace_na(list(n = 0))  #list(col = 0)

themes %>% 
  inner_join(themes, by = c("id" = "parent_id"), suffix = c("_parent", "_child")) %>% 
  filter(name_parent == "Harry Potter")

# Join themes to itself again to find the grandchild relationships
themes %>% 
  inner_join(themes, by = c("id" = "parent_id"), suffix = c("_parent", "_child")) %>%
  inner_join(themes, by = c("id_child" = "parent_id"), suffix = c("_parent", "_grandchild")) 

themes %>% 
  # Left join the themes table to its own children
  left_join(themes, by = c("id" = "parent_id"), suffix = c("_parent", "_child")) %>%
  # Filter for themes that have no child themes
  filter(is.na(id_child))

#----- Ch 3 Full, Semi, and Anti Joins -----
inventory_parts_joined <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  arrange(desc(quantity)) %>%
  select(-id, -version)

#sets (set_num) inventories (id/inventory_id) inventory_parts (color_id/id) colors



inventory_parts_joined %>% 
  inner_join(sets, by = "set_num") %>% 
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))


#aggregating each theme
inventory_sets_themes <- inventory_parts_joined %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

batman <- inventory_sets_themes %>%
  filter(name_theme == "Batman")

star_wars <- inventory_sets_themes %>%
  filter(name_theme == "Star Wars")

batman_parts <- batman %>%
  count(part_num, color_id, wt = quantity)

star_wars_parts <- star_wars %>%
  count(part_num, color_id, wt = quantity)

batman_parts %>%
  # Combine the star_wars_parts table 
  full_join(star_wars_parts, by = c("part_num", "color_id"), suffix = c("_batman", "_star_wars")) %>% 
  # Replace NAs with 0s in t
  replace_na(list(n_batman = 0, n_star_wars = 0))

parts_joined <- batman_parts %>%
  full_join(star_wars_parts, by = c("part_num", "color_id"), suffix = c("_batman", "_star_wars")) %>%
  replace_na(list(n_batman = 0, n_star_wars = 0))

parts_joined %>% 
  arrange(desc(n_star_wars)) %>% 
  # Join the colors table to the parts_joined table
  inner_join(colors, by = c("color_id" = "id"), suffix = c("_color", "_part")) %>% 
  # Join the parts table to the previous join 
  inner_join(parts, by = "part_num", suffix = c("_color", "_part"))

# filtering joins: anti_join, semi_join
# semi: what in A is also in B?
# anti: what in A is NOT in B?

batmobile <- inventory_parts_joined %>%
  filter(set_num == "7784-1") %>%
  select(-set_num)

batwing <- inventory_parts_joined %>%
  filter(set_num == "70916-1") %>%
  select(-set_num)

# Filter the batwing set for parts that are also in the batmobile set
batwing %>%
  semi_join(batmobile, by = c("part_num"))
# Filter the batwing set for parts that aren't in the batmobile set
batwing %>%
  anti_join(batmobile, by = c("part_num"))

# Use inventory_parts to find colors included in at least one set
colors %>%
  semi_join(inventory_parts, by = c("id" = "color_id"))  

# Use filter() to extract version 1 
version_1_inventories <- inventories %>%
  filter(version == 1)
# Use anti_join() to find which set is missing a version 1
sets %>%
  anti_join(version_1_inventories, by = "set_num")

# themes color comparison
inventory_parts_themes <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  arrange(desc(quantity)) %>%
  select(-id, -version) %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))


batman_colors <- inventory_parts_themes %>%
  # Filter the inventory_parts_themes table for the Batman theme
  filter(name_theme == "Batman") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a percent column of the total divided by the sum of the total 
  mutate(percent = total / sum(total))
star_wars_colors <- inventory_parts_themes %>%
  filter(name_theme == "Star Wars") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  mutate(percent = total / sum(total))

colors_joined  <- batman_colors %>%
  # Join the Batman and Star Wars colors (all rows from both)
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
  # Replace NAs in the total_batman and total_star_wars columns
  replace_na(list(total_star_wars = 0, total_batman = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>% 
  mutate(difference = percent_batman - percent_star_wars,
         total = total_batman + total_star_wars) %>% 
  filter(total > 200) %>% 
  mutate(name = fct_reorder(name, difference))
  
color_palette <- c("#05131D","#0055BF","#C91A09","#F2CD37","#FFFFFF","#E4CD9E","#958A73","#C91A09","#F5CD2F","#582A12","#A0A5A9","#6C6E68","#CC702A","#898788","#A0BCAC")
names(color_palette) = c("Black","Blue","Red","Yellow","White","Tan","Dark Tan","Trans-Red","Trans-Yellow","Reddish Brown","Light Bluish Gray","Dark Bluish Gray","Medium Dark Flesh","Flat Silver","Sand Green")

ggplot(colors_joined, aes(name, difference, fill = name)) +
geom_col() +
coord_flip() +
scale_fill_manual(values = color_palette, guide = FALSE) +
labs(y = "Difference: Batman - Star Wars")


  
#----- Ch 4 Case Study: Joins on Stack Overflow Data -----
rm(list = ls())
library(tidyverse)
library(lubridate)
questions <- readRDS("data/questions.rds")
tags <- readRDS("data/tags.rds")
question_tags <- readRDS("data/question_tags.rds")
answers <- readRDS("data/answers.rds")

# Join the questions and question_tags tables
questions_with_tags <- questions %>%
  left_join(question_tags, by = c("id" = "question_id")) %>% 
  left_join(tags, by = c("tag_id" = "id")) %>%
  replace_na(list(tag_name = "only-r"))

questions_with_tags %>% 
  group_by(tag_name) %>% 
  summarize(score = mean(score),
            num_questions = n()) %>% 
  arrange(desc(num_questions))

# find tags not used
tags %>% 
  anti_join(question_tags, by = c("id" = "tag_id"))

questions %>%
  # Inner join questions and answers with proper suffixes
  inner_join(answers, by = c("id" = "question_id"), suffix = c("_question", "_answer")) %>% 
  # Subtract creation_date_question from creation_date_answer to create gap
  mutate(gap = as.integer(creation_date_answer - creation_date_question))


# Count and sort the question id column in the answers table
answer_counts <- answers %>%
  count(question_id, sort = TRUE)

# Combine the answer_counts and questions tables
questions %>%
  left_join(answer_counts, by = c("id" = "question_id")) %>%
  # Replace the NAs in the n column
  replace_na(list(n = 0))

# Count and sort the question id column in the answers table
answer_counts <- answers %>%
  count(question_id, sort = TRUE)

question_answer_counts <- questions %>%
  left_join(answer_counts, by = c("id" = "question_id")) %>%
  replace_na(list(n = 0))

question_answer_counts %>%
  # Join the question_tags tables
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  # Join the tags table
  inner_join(tags, by = c("tag_id" = "id"))

tagged_answers <- question_answer_counts %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))

tagged_answers %>%
  # Aggregate by tag_name
  group_by(tag_name) %>%
  # Summarize questions and average_answers
  summarize(questions = n(),
            average_answers = mean(n)) %>%
  # Sort the questions in descending order
  arrange(desc(questions))
#---
questions_with_tags <- questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))
answers_with_tags <- answers %>%
  inner_join(question_tags, by = "question_id") %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Combine the two tables into posts_with_tags
posts_with_tags <- bind_rows(questions_with_tags %>% mutate(type = "question"),
                             answers_with_tags %>% mutate(type = "answer"))

# Add a year column, then aggregate by type, year, and tag_name
posts_with_tags %>%
  mutate(year = lubridate::year(creation_date)) %>%
  count(type, year, tag_name)

by_type_year_tag <- posts_with_tags %>%
  mutate(year = year(creation_date)) %>%
  count(type, year, tag_name)

# Filter for the dplyr and ggplot2 tag names 
by_type_year_tag_filtered <- by_type_year_tag %>%
  filter(tag_name %in% c("dplyr", "ggplot2"))

# Create a line plot faceted by the tag name 
ggplot(by_type_year_tag_filtered, aes(year, n, color = type)) +
  geom_line() +
  facet_wrap(~ tag_name)
