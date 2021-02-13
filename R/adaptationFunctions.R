

# Drought Adaptation Functions --------------------------------------------

calculateAdaptationIntensity <- function(forage.production, drought.adaptation.cost.factor = 1) {
  " Description: Takes forage potential and an adaptation intensity factor to 
  provide a scalar of drought action. If forage potential is above 1 (no drought), 
  then this variable goes to 0 (no adaptation). 
  Inputs: 
  adpt.intensity.factor (parameter that scales adaptation actions to reflect 
  actual adaptation behavior. Currently defaults to 1 which assumes a 
  one-to-one ratio of drops in forage percentage to need for forage 
  replacement.)
  forage.production (the percentage of average forage produced in a year 
  based on rainfall. See forage potential functions.)
  Output: intens.adj (scales action to account for forage potential's 
  deviation from the norm.)
  Assumptions: The variable has a maximum of 1, which assumes that drought 
  actions are parameterized at full forage replacement for the full herd.
  "
  intens.adj <- ifelse(forage.production >= 1, 0, (1 - forage.production) * drought.adaptation.cost.factor)
  intens.adj <- ifelse(intens.adj > 1, 1, intens.adj)  # putting a ceiling of this variable at 1 (no more than 100% of drought action)
  intens.adj
}

