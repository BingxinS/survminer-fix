
### ggsurvplot_facet_risktable()

This is a workaround function with survminer package to facet survival plots and risk tables.
The workaround function is available at:
https://github.com/BingxinS/survminer-fix/blob/master/ggsurvplot_facet_risktable.R

### Example 1
With strata factor: sex, and 
1 facet factor: ph.ecog.
```{r}
library(survminer)
require("survival")
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet.risktable.R")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data <- lung
fit <- survfit(Surv(time, status) ~ sex + ph.ecog, data)
ggsurvplot_facet_risktable(fit, data, risktable.ylab="Number at Risk")
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/issue-fig/ggsurvplot_facet_risktable_example1.png)

### Example 2:
With strata factor: sex, and 
2 facet factors: ph.ecog, ph.karno.
```{r}
library(survminer)
require("survival")
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet.risktable.R")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data <- lung
fit <- survfit(Surv(time, status) ~  sex + ph.ecog + ph.karno, data)
ggsurvplot_facet_risktable(fit, data)
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/issue-fig/ggsurvplot_facet_risktable_example2.png)



