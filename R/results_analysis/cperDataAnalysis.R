# CPER Data Analysis
# Write to allow reuse of code for MTurk

# Packages
require(plm)
require(lmtest) 

r <- cperGameOutputs # renaming results dataframe r for easier typing

#### Analyzing Final Net Worth ####
# List of final net worth of each mTUrk ID. 2008 is the final year, so we filter by selecting only that year. 
finalNetWorth <-  r %>% select(mTurkID,yr,net.wrth, insurance)                                                               %>% filter(yr == 2008)

nw <- lm(net.wrth ~ insurance, data = finalNetWorth)
summary(nw)

r %>% 
  filter(insurance == 1) %>%
  filter(mTurkID == 1983352) %>%
  select(rev.ins, cost.ins) %>%
  summarize(sum(rev.ins), sum(cost.ins)) -> ins

wealthGainIns <- ins$`sum(rev.ins)` - ins$`sum(cost.ins)`  # Total wealth transfer from insurance
nw$coefficients[2] - wealthGainIns  # Increase in net wealth over and above total wealth transfer

finalNetWorth$netWorthWithoutTransfer <- ifelse(finalNetWorth$insurance == 1, 
                                                finalNetWorth$net.wrth - wealthGainIns,
                                                finalNetWorth$net.wrth)
nw2 <- lm(netWorthWithoutTransfer ~ insurance, data = finalNetWorth)
summary(nw2)


#### Analyzing Herd Size ####
# Due to some people selling off the herd in the last two years, we don't see a 
# significant difference in herd size at end. 

finalHerd <- r %>% 
  select(mTurkID, yr, herd, insurance) %>% 
  filter(yr == 2005)

h <- lm(herd ~ insurance, data = finalHerd)
summary(h)

h1 <- lm(herd ~ insurance, data = finalHerd)
summary(h1)

## Clustered errors method from http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
# fit pooled OLS
m1 <- lm(herd ~ insurance, data = r)
# fit same model with plm (needed for clustering)
pm1 <- plm(herd ~ insurance, data = r, model = "pooling")

# compute Stata like df-adjustment
G <- length(unique(as.numeric(as.character(r$mTurkID))))
N <- length(r$mTurkID)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
id_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = id_c_vcov)

