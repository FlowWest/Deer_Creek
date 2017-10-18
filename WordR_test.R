#for installation of rJava to work, need to install most recent version of
#Java from here http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
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
t4raw <- read_csv("task4_data.csv")

#organize csv data for Gantt chart
t4 <- t4raw %>%
  mutate(Start = mdy(Start), End = Start + days(`Duration (days)`)) %>%
  gather(type, date, -ID:-`Duration (days)`, -Predecessors) %>% 
  mutate(date = as_date(as.numeric(date)),
         task = str_detect(`Task Name`, 'Task')) %>% 
  arrange(desc(ID)) 

#gantt chart plot
t4gantt <-  ggplot(t4, aes(x = date, y = fct_inorder(`Task Name`), color = task)) +
  geom_line(size = 3) +
  theme_minimal() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', position = 'top') +
  scale_color_manual(values = c('#756bb1', '#3f007d')) +
  labs(y = '', x = '') +
  theme(legend.position = 'none',
        text = element_text(size = 12))

mtcars1 <- ggplot(mtcars, aes(x = wt, y = hp, color = cyl)) +
  geom_point(size = 2)
mtcars1

#Interesting syntax here for assigning plots to word bookmarks
#Plot gets inserted, but not looking like it should (clipped). 
#Need to figure out formatting.
Plots <- list(t4gantt = function() print(t4gantt), mtcars1 = function() print(mtcars1))
  
addPlots(
  "U:/Active Projects/Deer Creek/Deliverables/BaselineMonitoring/20171012_BaselineMonitoring_Draft-WordRtest.docx", 
  "U:/Active Projects/Deer Creek/Deliverables/BaselineMonitoring/20171016_BaselineMonitoring.docx", Plots)
