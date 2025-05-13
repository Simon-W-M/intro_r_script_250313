# plot script

# load in tidyverse
library(tidyverse)

library(readr)
capacity_ae <- read_csv("capacity_ae.csv")

ggplot(data = capacity_ae)  +
  geom_point(aes(x = dcubicles,
                 y = dwait,
                 colour = 'red',
                 size = dwait,
                 alpha = 0.5)
             ) +
  geom_smooth(aes(x = dcubicles,
                  y = dwait),
              method = 'lm') 

summary(lm(dwait ~ dcubicles, data = capacity_ae))


ggplot(data = capacity_ae)  +
  aes(x = dcubicles,
      y = dwait,
      colour = staff_increase) +
  geom_point() +
  facet_wrap(~ staff_increase)


capacity_ae$newrow <- sample(5, size = nrow(capacity_ae), 
                             replace = TRUE)


ggplot(data = capacity_ae)  +
  aes(x = dcubicles,
      y = dwait,
      colour = staff_increase) +
  geom_point() +
  facet_wrap(~ newrow,
             ncol = 2)


# histogram
ggplot(data = capacity_ae) +
  geom_histogram(aes(x = dwait),
                 binwidth = 5)

# bar plot
ggplot(data = capacity_ae) +
  geom_col(aes(x = site,
               y = attendance2018))


# reorder by attendance
ggplot(data = capacity_ae) +
  geom_col(aes(x = reorder(site, attendance2018),
               y = attendance2018))


# reorder by attendance
ggplot(data = capacity_ae) +
  geom_col(aes(x = attendance2018,
               y = reorder(site, attendance2018),
               fill = -attendance2018)) + 
  theme_minimal()

# box plot

ggplot(data = capacity_ae) +
  geom_boxplot(aes(x = staff_increase,
                   y = dwait)) +
  labs(title = 'Change in waits by staff increase groups',
       x = 'Staff increase',
       y = 'Change in waiting time',
       caption = 'Data taken from study x') +
  theme_minimal()


ggplot(data = capacity_ae) +
  geom_violin(aes(x = staff_increase,
                   y = dwait)) +
  geom_boxplot(aes(x = staff_increase,
                  y = dwait,
                  alpha = 0.5,
                  fill = staff_increase)) +
  labs(title = 'Change in waits by staff increase groups',
       x = 'Staff increase',
       y = 'Change in waiting time',
       caption = paste('Data downloaded:', Sys.Date())) +
  theme_minimal() 

