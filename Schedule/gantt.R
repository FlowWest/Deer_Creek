library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(stringr)
library()

sched <- read_csv("U:/Active Projects/Deer Creek/Project_Management\Schedule\schedule_revised/updated_schedule_data.csv")


sched %>%
  mutate(Start = mdy(Start), End = Start + days(`Duration (days)`)) %>%
  gather(type, date, -ID:-`Duration (days)`, -Predecessors) %>% 
  mutate(date = as_date(as.numeric(date)),
         task = str_detect(`Task Name`, 'Task')) %>% 
  arrange(desc(ID)) %>% 
  ggplot(aes(x = date, y = fct_inorder(`Task Name`), color = task)) +
  geom_line(size = 3) +
  theme_minimal() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', position = 'top') +
  scale_color_manual(values = c('#756bb1', '#3f007d')) +
  labs(y = '', x = '') +
  theme(legend.position = 'none',
        text = element_text(size = 12))

s2 <- sched %>%
  mutate(Start = mdy(Start), Finish = Start + days(`Duration (days)`)) %>%
  arrange(desc(ID)) %>% 
  select(ID, `Task Name`, Duration = `Duration (days)`, Start, Finish, Predecessors) 

write_csv(s2, "U:/Active Projects/Deer Creek/Project_Management\Schedule\schedule_revised/gantt_sched.csv")


t4 <- read_csv("U:/Active Projects/Deer Creek/Project_Management\Schedule\schedule_revised/task4_data.csv")

t4 %>%
  mutate(Start = mdy(Start), End = Start + days(`Duration (days)`)) %>%
  gather(type, date, -ID:-`Duration (days)`, -Predecessors) %>% 
  mutate(date = as_date(as.numeric(date)),
         task = str_detect(`Task Name`, 'Task')) %>% 
  arrange(desc(ID)) %>% 
  ggplot(aes(x = date, y = fct_inorder(`Task Name`), color = task)) +
  geom_line(size = 3) +
  theme_minimal() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', position = 'top') +
  scale_color_manual(values = c('#756bb1', '#3f007d')) +
  labs(y = '', x = '') +
  theme(legend.position = 'none',
        text = element_text(size = 12))

t4_table <- t4 %>%
  mutate(Start = mdy(Start), Finish = Start + days(`Duration (days)`)) %>%
  arrange(desc(ID)) %>% 
  select(ID, `Task Name`, Duration = `Duration (days)`, Start, Finish, Predecessors) 

write_csv(t4_table, 'U:/Active Projects/Deer Creek/Project_Management\Schedule\schedule_revised/t4_table.csv')



