library(ggplot2)
#A1
patients <- read.csv("patient_data.csv") 

#A2, A3, A4
ggplot(patients, mapping = aes(BMI, Weight, col = Height)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#A5, A6
ggplot(patients, mapping = aes(Smokes, Score, col = Sex)) + 
  geom_boxplot()

#A7
ggplot(patients, mapping = aes(x = BMI)) + 
  geom_histogram(fill = "blue", binwidth = 1)

#A8, A9
ggplot(patients, mapping = aes(x = BMI, col = Sex)) + 
  geom_density()

#B1
ggplot(patients, mapping = aes(BMI, Weight, col = Height)) + 
  geom_point() + 
  facet_grid(Smokes ~ Sex)

#B2
ggplot(patients, mapping = aes(Smokes, BMI, col = Sex)) +
  geom_boxplot() + 
  facet_grid(~ Age)

#B3
ggplot(patients, mapping = aes(Sex, BMI, col = factor(Age))) +
  geom_boxplot() +
  facet_grid(~Smokes)
  
#C1
ggplot(patients, aes(BMI, Weight)) +
  geom_point()
  
#C2
ggplot(patients, aes(BMI, Weight)) +
  geom_point() + 
  scale_x_continuous(
    limits = c(20, 40), 
    breaks = seq(20, 40, by = 10)
  ) + 
  scale_y_continuous(
    limits = c(60, 100), 
    breaks = seq(60, 100, by = 5),
    name = "Weight in Kilograms"
  )
  
#C3
ggplot(patients, aes(BMI, Age, fill = factor(Age))) +
  geom_violin() + 
  scale_fill_brewer(
    palette = "Oranges"
  )
  
#C4
ggplot(patients, aes(BMI, Weight, col = Height)) + 
  geom_point() +
  scale_colour_gradient2(
    low = "green", 
    high = "red", 
    mid = "grey",
    midpoint = mean(patients$Height)
  )

#C5
ggplot(patients, aes(BMI, Weight, col = factor(Age))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#C6
ggplot(patients, aes(BMI, Weight, col = factor(Age))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme(
    legend.key = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#C7
Final05 <- ggplot(patients, aes(BMI, Weight, col = factor(Age))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "BMI Scores by Weight and Age") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("Final05.png",
       plot = Final05, 
       width = 5, 
       height = 5, 
       units = "in"
)
  