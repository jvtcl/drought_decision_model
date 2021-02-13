# Drought Decision Model: Simulation Version

## Model Workflow

All functions are run within `master.R` and call additional scripts from the `R` folder.

* In `master.R`:
  * **SETUP**
    * Get the **station gauge attributes**, and assign them to a sub-environment (`getStationGauge`).
      * `getStationGauge` branches depending upon whether the target location is the Central Plains Experimental Range (CPER) or another site.
        * If **CPER** (default):
          * __Zone Weights__ `zonewt` are read from the *Excel model* which is based on [xx] drought calculator state forage potential weights that we cannot reproduce. We are missing spatial reference information necessary to assign each target location to a state zone.
          * __Weights Zone__ `stzone` which corresponds to the state weights zone in which CPER resides (Colorado Zone 3).
          * __Station Gauge__ `stgg`, historical precipitation totals dating back to 1948, which are also read in from the Excel model. Precip totals are collected at CPER itself and do not rely on precip data from COOP sites.
          *__Target Grid Cell__ `tgrid`, for reading in PRF index values at a given point in time. `tgrid` is assigned to the PRF grid cell where we assume CPER exists, 25002.
        * If **COOP site** (user-specified):
          * __Zone Weights__ `zonewt` are based upon the Major Land Resource Area in which the COOP site resides. The MLRA forage potential is an average of plant growth curves calculated for a series of ecological site surveys (ESS) performed for that MLRA (using functions `COOP_in_MRLA` `getMLRAWeights`). Our decision to use an average plant growth curve is a placeholder that could be replaced, for example, by a regression framework like the one used to calculate the state weights. Alternatively, we could contact the authors of the [xx] state weights and use these instead of MLRAs (since these are likely more accurate than the weights used here).
          * __Weights Zone__ which in this case corresponds to the MLRA site within which the target COOP site resides.
          * __Station Gauge__ `stgg`, which is read from the historical precipitation data `precip` attached to the target COOP site list `target.coop`.
          * __Target Grid Cell__ which belongs to the list of COOP site attributes `target.coop`. This variable is computed by converting the coordinates of the COOP site to a SpatialPoint object and finding the underlying PRF grid cell.
      * Upon completion of the function `getStationGauge`, a new sub-environment `station.gauge` is generated, which contains `zonewt`, `stzone`, `stgg`, and `tgrd` based on the target location.
    * Get the **model constant variables** by calling `getConstantVars` from the script `R/vars.R`. `getConstantVars` is a wrapper function that assigns variables from an external file, `data/constant_vars.csv`. The file `constant_vars.csv` may be altered to include/omit variables and change their assignments. (*The file `constant_vars.csv` can be thought of as a placeholder i.e. for user input fields in a Shiny app*.)

      * **Constant variables include**:

        * __Drought Action Start Year__ (`act.st.yr`)
          * *Default: 1* (We assume for now that drought action begins immediately.)
        * Drought Action Start Month (`act.st.m`)
          * *Default: 6*
        * Drought Action End Year (`act.end.yr`)
          * *Default: 1*
        * Drought Action End Month (`act.end.m`)
          * *Default: 12*
        * Number of pounds of additional hay needed for each cow each day (pounds/head/day; `khaylbs`)
          *  *Default: 22*
        * Number of pounds of additional other feed needed for each cow each day (pounds/head/day; `kOthlbs`).
          *  *Default: 0*
        * Price per ton of Hay (USD, `p.hay`)
          * *Default: 100*
        * Price per ton of Other Feed (USD, `p.oth`)
          * *Default: 0*
        * Herd Size (head of cows, does not include calves; `herd`)
          * *Default: 600*
        * Distance to Rented Pasture (miles, `n.miles`)
          * *Default: 300*
        * Trucking cost per loaded mile ($USD/mile/truck; `truck.cost`)
          * *Default: 4*
        * Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($USD/pair/month) (`past.rent`)
          * *Default: 16.49*
        * All other non-rental, non-trucking costs ($USD; `oth.cost`)
          * *Default: 300*
        * Maximum weight per truck (pounds; `max.wt`)
          * *Default: 40000*
        * Average cow weight (pounds; `cow.wt`)
          * *Default: 1200*
        * Average 'current' weight of calves (pounds; `calf.wt`)
          * *Default: 375*
        * Expected weight of calves at weaning without drought (pounds; `normal.wn.wt`)
          * *Default: 600*
          * This variable is used as a placeholder weight for years 2-5 when `use.forage` is set to `FALSE` in `getSimVars.R`
        * Average percentage of calves sold (%; `calf.sell`)
          * *Default: 0.75*
        * Average percentage of cows that successfully wean calves (%; `wn.succ`)
          * *Default: 0.94*
        * Calf sale price at weaning ($USD/lb; `p.calf.t0`)
          * *Default: 1.45*
        * Cow sale price ($USD, `p.cow`)
          * *Default: 850*
        * Expenditure per cow ($USD, `cow.cost`)
          * *Default: 500*
          * This variable is used to compute the baseline operating cost per year.
        * Dummy varible for whether insurance is purchased, a value of one indicates insurance presence (`purchase.insurance`)
          * *Default: 1*
        * Interest rate on investments (`invst.int`)
          * *Default: 0.0125*
        * Capital Tax Rate (`cap.tax.rate`)
          * *Default: 0.15*
        * Number of Years in the Model (`t`)
          * *Default: 5*
        * Number of cows culled in a normal year (`cull.num`)
          * *Default: 15*
          * Used only if assets increase from the previous year, see `CalcCapSalesPurch`
        * Interest rate for borrowed money (%/year; `loan.int`)
          * *Default: 0.065*
        * Change in operating costs in year 1 per cow ($/cow/year). Negative value represents reduced costs ($USD/cow/year; `op.cost.adj`)
          * *Default: -100*
        * Operating costs incurred without a herd ($USD/year, `herdless.op.cost`)
          * *Default: 5000*
        * Selling cost per cow ($USD; `sell.cost`)
          * *Default: 20*
          * NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
        * Cost of replacement per cow ($USD; `replc.cost`)
          * *Default: 850*
        * Expected weight of calves at weaning (pounds; `calf.wt`)
          * *Default: 600*
          * **REDUNDANT WITH `normal.wn.wt`- FIND WHERE THIS IS USED AND REPLACE IT!!**

  * **GENERATE MODEL INPUTS**
    * `generateRunParams` is a wrapper that calls the function `getSimVars` in `xx.R` to populate another environment `simvars` with model inputs. We will call this *n* times where *n* is the number of simulation runs.
      * Currently simulated variables:
        * **Starting Year**, from a uniform distribution.
        * **Acreage**, from a normal distribution with mean=3000 and sd=xx, currently not enabled (?)
        * **Productivity Factor**, from a uniform distribution
        * **Forage Potential**, default `FALSE`. Not really a simulated variable, but should be set to `TRUE` if using a custom location.
  * **PARALLELIZE SIMULATION RUNS**
    * This enables us to perform many model runs faster.
    * Load required packages for parallelization using `snowfall`.
      * These *should not* be loaded into the global environment because `snowfall` will not recognize them.
    * Perform the simulations:
      1. Save each model run's outputs to a list `parouts`
      2. Convert `parouts` to a data frame object for summary, plotting, etc.
    * Save simulation inputs/results:
      * Input data (`simruns` to `output/simulation_inputs_baseline.RData`)
      * All results (`parouts` to `simulation_results_baseline.RData`)
  * **SUMMARY,**
    * **FILL THIS IN!**
