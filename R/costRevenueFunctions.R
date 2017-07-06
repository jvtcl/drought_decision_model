# Costs and Revenues ------------------------------------------------------
# Baseline Costs and Revenues
calculateExpSales <- function(herd, wn.succ, calf.sell, wn.wt, p.wn) {
  "
  Function: calculateExpSales
  Description: Calculates expected calf revenues for non-drought year
  
  Inputs:
  herd = Size of herd (head of cows, does not include calves)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn = Expected sale price of calves ($/pound)
  wn.succ = Vector of reproductive success percentages that measures how many
  cows in the herd successfully get pregnant, give birth, and wean their calves
  
  Outputs:
  base.sales = Expected revenues from calf sales for a non-drought year
  "
  if(is.na(herd)){
    base.sales <- calf.sell * wn.wt * p.wn
  }else{
    base.sales <- (herd * wn.succ * calf.sell) * wn.wt * p.wn  
  }
  return(base.sales)
}

# Option 1: Buy feed
calculateFeedCost <- function(kHayLbs, kOthLbs, p.hay,p.oth, days.feed, herd, intens.adj) {
  "
  Function: calculateFeedCost
  Description: Calculates the costs of purchasing additional feed
  
  Inputs:
  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
  p.hay = Price of hay ($/ton). User input.
  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
  days.feed = Number of days additional feed is needed. Generally, equal to days.act. (days)
  herd = Size of herd (head of cows, does not include calves)
  
  Outputs:
  cost.feed = Additional costs to feed the herd over the remainder of the season ($/year)
  "
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd * intens.adj
  return(feed.cost)
}

# Option 2: Rent Pasture
CalculateRentPastCost <- function(n.miles, truck.cost, past.rent, oth.cost, days.rent, max.wt, cow.wt, calf.wt, herd) {
  "
  Function: CalculatePastureRentCost
  Description: Calculates the costs of renting pasture and trucking pairs
  NOTE: Not including loan interest costs. This needs to be resolved across all options.
  
  Inputs:
  n.miles = Distance to rented pasture (miles)
  truck.cost = Trucking cost per loaded mile ($/mile/truck)
  past.rent = Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($/pair/month)
  days.rent = Days on rented pasture. Generally, equal to days.act. (days)
  oth.cost = All other non-rental, non-trucking costs ($)
  max.wt = Maximum weight per truck (pounds)
  cow.wt = Average cow weight (pounds)
  calf.wt = Average 'current' weight of calves before trucking to rented pasture (pounds)
  herd = Size of herd (head of cows, does not include calves)
  loan.int = interest rate for borrowed money (%/year)
  
  Outputs:
  cost.rentpast = Total costs of using renting pasture including transport costs ($/year)
  "
  # Calculating number of trucks needed to truck pairs to pasture and to truck cows home (without calves)
  n.trucks.past <- ceiling(herd / ceiling(max.wt / (cow.wt + calf.wt)))
  n.trucks.home <- ceiling(herd / ceiling(max.wt / cow.wt))
  
  # Cost per herd of trucking pairs to pasture and trucking cows back home
  tot.truck.cost <- n.miles * truck.cost * n.trucks.past + n.miles * truck.cost * n.trucks.home
  
  # Cost of renting pasture
  tot.past.rent <- past.rent / 30 * days.rent * herd
  
  # Total costs including transport, rent, and other costs
  cost.rentpast.woint <- ifelse(days.rent > 0, tot.truck.cost + tot.past.rent + oth.cost, 0)
  cost.rentpast <- ifelse(days.rent > 0, cost.rentpast.woint * (1 + loan.int / 365 * days.rent), 0)  # I think we should not include interest here unless it is also included in other adaptation costs
  
  return(cost.rentpast)
}

getAdaptCost <- function(adpt_choice, pars, days.act, current_herd, intens.adj){
  "
  Function: getAdaptCost
  Description: Calculates the cost of adaptation based on strategy, intensity needed, days, and herd size
  
  Inputs:
  adpt_choice = adpatation strategy either 'feed' or 'rentpast'
  pars = list of state variables simRuns from Master
  days.act = number of days adaptation action is needed for
  current_herd = current number of cows
  intens.adj = intensity of adaptation needed from 0 - 1

  Outputs:
  adpt_cost = Cost of engaging in adaptation
  "
  
  adpt_cost <- 0
  
  ## Adaptation cost when feeding
  if(adpt_choice == "feed"){
    adpt_cost <- with(pars, calculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, 180, current_herd, intens.adj))
  
  ## Adaptation cost when renting pasture  
  }else if(adpt_choice == "rentpast"){
    adpt_cost <- with(pars, CalculateRentPastCost(n.miles = n.miles,
                                                         truck.cost = truck.cost,
                                                         past.rent = past.rent,
                                                         days.rent = days.act,
                                                         oth.cost = oth.cost,
                                                         max.wt = max.wt,
                                                         cow.wt = cow.wt,
                                                         calf.wt = calf.wt,
                                                         herd = current_herd))
  }
  return(adpt_cost)
}