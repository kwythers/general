
data_path<-"/Users/kirkw/Desktop/soil_efulx.csv"
soil <- read.table(data_path, sep = ",", header = TRUE)

tapply(soil$eFlux, soil$Temperature, summary)
tapply(soil$eFlux, soil$Temperature, sd)

# Plots of CO2 flux 
# Boxplot rs vs temp      
ggplot(soil, aes(factor(Temperature), eFlux, color = factor(Treatment))) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Soil Respiration") +
  xlab("Temperature, C??") +
  ylab(expression("Respiration, ??mols "*CO[2]*s^-1)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) + 
  scale_x_discrete(breaks = seq(0, 40, 5))

# Points rs vs temp
ggplot(soil, aes(y = eFlux, x = Temperature, color = factor(Treatment))) + 
  geom_point(aes(shape = factor(Treatment))) + 
  scale_shape(solid = FALSE) + 
  stat_smooth(method = "lm", se = TRUE, fill = NA, formula = y ~ poly(x, 3, raw = TRUE)) + 
  theme_bw() + 
  labs(title = "Soil Respiration") + 
  xlab("Temperature, C??") + 
  ylab(expression("Respiration, ??mols "*CO[2]*s^-1)) + 
  scale_y_continuous(breaks = seq(0, 30, 5)) + 
  scale_x_continuous(breaks = seq(0, 40, 5))



