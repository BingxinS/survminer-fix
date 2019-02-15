
### ggsurvplot_facet_table_confint()

This is a workaround function with survminer package to facet survival plots, risk tables, and confidence intervals.
The workaround function is available at:
https://github.com/BingxinS/survminer-fix/blob/master/ggsurvplot_facet_table_confint.R

### Example 1
Facet the plots and risk tables, with strata factor: **sex**, and 1 facet factor: **ph.ecog**.
```{r}
library(survminer)
require("survival")
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet_table_confint.R")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data <- lung
fit <- survfit(Surv(time, status) ~ sex + ph.ecog , data)
ggsurvplot_facet_table_confint(fit, data, risktable=TRUE)
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/issue-fig/ggsurvplot_facet_risktable_example1.png)

### Example 2:
Facet the plots and confidence intervals, with strata factor: **sex**, and 1 facet factor: **ph.karno**.
```{r}
library(survminer)
require("survival")
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet.risktable.R")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data <- lung
fit <- survfit(Surv(time, status) ~ sex + ph.karno, data)
ggsurvplot_facet_table_confint(fit, data, conf.int = TRUE)
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/issue-fig/ggsurvplot_facet_table_confint_example2.png)

### Example 3:
Facet the plots, risk tables and confidence intervals, with strata factor: **sex**, and 2 facet factors: **ph.ecog, ph.karno**.
```{r}
library(survminer)
require("survival")
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet.risktable.R")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data <- lung
fit <- survfit(Surv(time, status) ~ sex + ph.ecog + ph.karno, data)
ggsurvplot_facet_table_confint(fit, data, risktable=TRUE, conf.int = TRUE)
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/issue-fig/ggsurvplot_facet_table_confint_example3.png)


### Issue
https://github.com/kassambara/survminer/issues/371
