install.packages('rJava')
install.packages('ReporteRs')
install.packages('WordR')

library(WordR)
library(ReporteRs)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(stringr)
library(rJava)

setwd("U:/Active Projects/Deer Creek/Project_Management/Schedule/schedule_revised")
getwd()

#read in csv of task data
t4 <- read_csv("task4_data.csv")

#create Gantt chart
t4Gantt <- t4 %>%
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

t4Gantt

#Code not working yet. having a java installation error.
addPlots(
  "U:/Active Projects/Deer Creek/Deliverables/BaselineMonitoring/BaselineMonitoring_20171012.docx", 
  "U:/Active Projects/Deer Creek/Deliverables/BaselineMonitoring/BaselineMonitoring_20171012.docx", 
  t4Gantt, height=4, bookmark = 'p1')
