# COVID-19 branching process models for the beginning of outbreaks. 

```diff
- This repository is under active development and is subject to change as the analysis evolves
```

This repositoty contains code for running branching process models of COVID-19.
The code has diverged quite a lot from the original (repo)[https://github.com/epiforecasts/ringbp] and we are not trying to keep the two code bases in sync.

The main functionality added here is a model of testing that is used alongside the contat tracing.


## Usage

### Set up

Set your working directory to the home directory of this project (or use the provided Rstudio project). Install the analysis and all dependencies with: 

```r
remotes::install_github("timcdlucas/ringbp", dependencies = TRUE)
```

### Run a single scenario

Run a single scenario for a 100 simulations.

```r
library(ringbp)
library(ggplot2)

res <- ringbp::scenario_sim(n.sim = 10, num.initial.cases = 1,prop.asym=0,
                     prop.ascertain = 0.2, cap_cases = 4500, cap_max_days = 350,
                     r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1, delay_shape = 1.651524,
                     delay_scale = 4.287786,k = 0, quarantine = FALSE)

# Plot of weekly cases
ggplot2::ggplot(data=res, ggplot2::aes(x=week, y=cumulative, col = as.factor(sim))) +
  ggplot2::geom_line(show.legend = FALSE, alpha=0.3) +
  ggplot2::scale_y_continuous(name="Number of cases") + 
  ggplot2::theme_bw()

ringbp::extinct_prob(res,cap_cases = 4500)
```



# Scripts

There are a number of scripts in `inst/scripts` that run the actual analyses.
I'll get round to adding descriptions of them at somepoint.


# Papers

The original repo was used for the analysis that underlies [Feasibility of controlling COVID-19 outbreaks by isolation of cases and contacts](https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30074-7/fulltext)

Our additions have been used for the paper [An imperfect tool: COVID-19 'test & trace' success relies on minimising the impact of false negatives and continuation of physical distancing.](https://www.medrxiv.org/content/10.1101/2020.06.09.20124008v2).
This paper used the code up to [df7dc743](https://github.com/timcdlucas/ringbp/tree/df7dc743aa84bba2be6f1807c30822e0b7fd247f).
I don't think I can make a release as this repo is a fork. 
