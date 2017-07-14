# Data Analysis


# Regressing total forage with hay purchase
# Just checking to make sure this relationship makes sense. 
forage1 <- lm(total.forage ~ cost.adpt + herd + yr, data = sim)
summary(forage1)
