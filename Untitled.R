setwd('/Users/angiepoon/Desktop/Temp/')
getwd() #get working directory
if(!require(pacman))install.packages("pacman")
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
library(ggplot2) #for plotting 
library(tidyverse) #for data cleaning

df <- read.csv("Student Survey English Language Study Tour 2025 - Copy of raw data-2.csv")
#rename column
df <- df %>% 
  rename(number = X , Q1 = X1...Not.at.all..2...A.little.bit..3...OK..4...Quite..5...A.lot)

#select 
df_selected_Q1 <- df %>% 
  select(Q1)

#changing data type
df_selected_Q1 <- df_selected_Q1%>%
  mutate(Q1 = as.factor(Q1))

#remove first row 
df_selected_Q1 <- df_selected_Q1%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q1, aes(Q1)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Do you think the tour is the right length? ")+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q1.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

#rename column
df <- df %>% 
  rename(Q2 = X.1)

#select 
df_selected_Q2 <- df %>% 
  select(Q2)

#changing data type
df_selected_Q2 <- df_selected_Q2%>%
  mutate(Q2 = as.factor(Q2))

#remove first row 
df_selected_Q2 <- df_selected_Q2%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q2, aes(Q2)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Are you more interested in English and English Literature?", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q2.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")


#rename column
df <- df %>% 
  rename(Q3 = X.2)

#select 
df_selected_Q3 <- df %>% 
  select(Q3)

#changing data type
df_selected_Q3 <- df_selected_Q3%>%
  mutate(Q3 = as.factor(Q3))

#remove first row 
df_selected_Q3 <- df_selected_Q3%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q3, aes(Q3)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Do you feel more confident in speaking English?", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q3.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")



#column extract workflow
df <- df %>% 
  rename(Q4 = X.3)
df_selected_Q4 <- df %>% 
  select(Q4)
df_selected_Q4 <- df_selected_Q4%>%
  mutate(Q4 = as.factor(Q4))
df_selected_Q4 <- df_selected_Q4%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q4, aes(Q4)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Did your tutor provide enough help with your learning during the tour? ", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q4.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

#column extract workflow
df <- df %>% 
  rename(Q5 = X.4)
df_selected_Q5 <- df %>% 
  select(Q5)
df_selected_Q5 <- df_selected_Q5%>%
  mutate(Q5 = as.factor(Q5))
df_selected_Q5 <- df_selected_Q5%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q5, aes(Q5)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Did you enjoy the tour? ", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q5.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")


#column extract workflow
df <- df %>% 
  rename(Q7 = X1...Not.at.all..2...A.little.bit..3...OK..4...Quite..5...A.lot.1)
df_selected_Q7 <- df %>% 
  select(Q7)
df_selected_Q7 <- df_selected_Q7%>%
  mutate(Q7 = as.factor(Q7))
df_selected_Q7 <- df_selected_Q7%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q7, aes(Q7)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "English Speaking Improvement", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q7.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

#column extract workflow
df <- df %>% 
  rename(Q8 = X.7)
df_selected_Q8 <- df %>% 
  select(Q8)
df_selected_Q8 <- df_selected_Q8%>%
  mutate(Q8 = as.factor(Q8))
df_selected_Q8 <- df_selected_Q8%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q8, aes(Q8)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "English Reading Improvement", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q8.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

#column extract workflow
df <- df %>% 
  rename(Q9 = X.8)
df_selected_Q9 <- df %>% 
  select(Q9)
df_selected_Q9 <- df_selected_Q9%>%
  mutate(Q9 = as.factor(Q9))
df_selected_Q9 <- df_selected_Q9%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q9, aes(Q9)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "English Vocabulary Improvement", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q9.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

#column extract workflow
df <- df %>% 
  rename(Q10 = X.9)
df_selected_Q10 <- df %>% 
  select(Q10)
df_selected_Q10 <- df_selected_Q10%>%
  mutate(Q10 = as.factor(Q10))
df_selected_Q10 <- df_selected_Q10%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(df_selected_Q10, aes(Q10)) +geom_bar(fill="#1380A1") + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "English Writing Improvement", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("my_histogram_Q10.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

df <- df %>% 
  rename(Q15 = X.15, Q16 = X.16, Q17 = X.17)
temp<- readline(prompt = "Enter the column name: ")
temp_selected <- df %>% 
  select(temp)
temp_selected <- temp_selected%>%
  slice(-1)

#Histogram #Export parameter, 1772, 1092
d <- ggplot(temp_selected, aes(Q17))+
              geom_bar(fill="#1380A1") +
              scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels = c("Not at all", "A little bit", "OK", "Quite", "A lot")) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  labs(title = "Embracing changes", )+
  bbc_style()+
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10)) 
d
ggsave("my_histogram_Q17.png", plot = d, device = "png", width = 5.51, height = 4.73, units = "in")

  