Actually, the main survminer package's author had just merged my fix pull request.
[https://github.com/kassambara/survminer/pull/363 ](url)
You could install the latest developmental version from CRAN.

```{r}
# survminer-fix
# download the file "ggsurvplot_facet_fix_12072018.R" 

##### example ####
library(survminer)
require("survival")

# fix the issue in ggsurvplot_facet()           
# all subplot/facited plot connected to origin         

# example
fit <- survfit(Surv(time, status) ~  sex, lung)

# not connected to origin
ggsurvplot_facet(fit, lung, facet.by=c("ph.ecog", "ph.karno"))

# fixed subplot connected to origin
source("/mnt/survminer-issues/ggsurvplot_facet_fix_12072018.R") # change to your own directory
ggsurvplot_facet_fix(fit, lung, facet.by=c("ph.ecog", "ph.karno"))
```

Same issue reported in https://github.com/kassambara/survminer/issues/254.

Below is my proposed fix:

```{r LoadPackages}
library(survminer)
require("survival")
```

### Test case 1

```{r, echo=FALSE}
# testing dataframe
lungTest <- data.frame ("inst" = c(3, 3, 3, 5, 1, 12, 7, 11, 1, 7), 
                          "time" = c(306, 455, 1010, 210, 883, 1022, 310, 361, 218, 166),
                          "status" = c(2, 2, 1, 2, 2, 1, 2, 2, 2, 2),
                          "age" = c(74, 68, 56, 57, 60, 74, 68, 71, 53, 61),
                          "sex" = c(1, 2, 2, 1, 1, 1, 2, 2, 1, 1),
                          "ph.ecog" = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
                          "ph.karno" = c(90, 90, 90, 90, 100, 50, 70, 60, 70, 70),
                          "pat.karno" = c(100, 90, 90, 60, 90, 80, 60, 80, 80, 70),
                          "meal.cal" = c(1175, 1225, NA, 1150, NA, 513, 384, 538, 825, 271), 
                          "wt.loss" = c(NA, 15, 15, 11, 0, 0, 10, 1, 16, 34)
)
```
which is actural the first 10 data points in lung:
```{r, echo=FALSE}
# testing dataframe
temprcd<-10
lungTest<-lung[1:temprcd,]
```

#### Plot with issues

One can facet the <span style="color:#2980B9">sex=1</span> and <span style="color:#2980B9">sex=2</span> plots by <span style="color:#2980B9">ph.ecog</span> values:
```{r}
survfit(Surv(time, status) ~ sex, lungTest) %>%
    ggsurvplot_facet(lungTest, facet.by="ph.ecog")
```

![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test1plot-issue.png)

Here comes the problem. The function ggsurvplot_facet() missed the first intervals for each survival plot. It was caused by the returning dataframe .$data, where the starting point was not set correctly, shown in the table below. Pay attention to the first 4 rows and the last 2 columns, sex and ph.ecog values are not set correctly, compare to the column strata values.
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test1table-issue.png)

#### A fix

The function, that causes the problem of missing the first interval in a survival plot, was function <span style="color:#2980B9">.connect2origin()</span>, which was called by <span style="color:#2980B9">ggsurvplot_df()</span>, which was called by <span style="color:#2980B9">ggsurvplot_core()</span>, which was called by <span style="color:#2980B9">ggsurvplot_facet()</span>. 

The orginial span <span style="color:#2980B9">.connect2origin()</span> is as follows:

```{r}
.connect2origin <- function (d, fit, data = NULL) 
{
    base <- d[1, , drop = FALSE]
    base[intersect(c("time", "n.censor", "std.err", "n.event"), 
        colnames(base))] <- 0
    base[c("surv", "upper", "lower")] <- 1
    n.strata <- length(levels(d$strata))
    if (n.strata > 1) {
        strata <- levels(d$strata)
        base <- base[rep(1, n.strata), , drop = FALSE]
        row.names(base) <- 1:nrow(base)
        base$strata <- strata
        base$strata <- factor(strata, levels = strata)
        if (!missing(fit)) {
            if (!inherits(fit, "survfit.cox")) {
                variables <- .get_variables(base$strata, fit, 
                  data)
                for (variable in variables) base[[variable]] <- .get_variable_value(variable, 
                  base$strata, fit, data)
            }
        }
    }
    d <- rbind(base, d)
    d
}
```
The code basically created and copy the origin for each subplot/faceted plot from the very first one. 

I have added a block of code to modify the origin per strata per facet as follows as <span style="color:#2980B9">.connect2origin_fix()</span>:

```{r}
.connect2origin_fix <- function(d, fit, data = NULL){

  base <- d[1, , drop = FALSE]
  base[intersect(c('time', 'n.censor', 'std.err', "n.event"), colnames(base))] <- 0
  base[c('surv', 'upper', 'lower')] <- 1.0
  n.strata <- length(levels(d$strata))
  
  # Connect each group to the origin
  if (n.strata > 1) {
    strata <- levels(d$strata)
    base <- base[rep(1, n.strata),, drop = FALSE]
    row.names(base) <- 1:nrow(base)
    base$strata <- strata
    base$strata <- factor(strata, levels = strata)
    
    #############################################################
    ##   added to modify values for each strata and facet      ##
    indStrata <- grep("strata",names(base))
    indMax    <- length(names(base))
    if(indStrata<indMax & n.strata>0){
      for (indRow in c(1:n.strata)) {
        tmpStrata = as.list(strsplit(strata[indRow], '\\,'))[[1]]
        tmpFacet  = as.numeric(gsub("\\D", "", tmpStrata)) 
        base[indRow, c((indStrata+1):indMax)] = tmpFacet
      }
    }
    ##                ending of the modification               ##
    #############################################################
    
    # update variable values
    if(!missing(fit)){
      if(!inherits(fit, "survfit.cox")){
        variables <- .get_variables(base$strata,  fit, data)
        for(variable in variables) base[[variable]] <- .get_variable_value(variable, base$strata, fit, data)
      }
    }
  }
  d <- rbind(base, d)
  d
}
```

Now the fixed table is as:
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test1table-fix.png)
The fixed survival plot is as:
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test1plot-fix.png)

### Test case 2
This case is with more choices of strata and facets. Note that there is no data points for certain strata and facet.

```{r}
require("survival")
temprcd<-100
lungTest<-lung[1:temprcd,]

# original ggsurvplot_facet missed the first intervals for each survival plot
survfit(Surv(time, status) ~  sex, lungTest) %>%
  ggsurvplot_facet(lungTest, facet.by=c("ph.ecog", "ph.karno"))
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test2plot-issue.png)

```{r}
# fixed plot
survfit(Surv(time, status) ~  sex, lungTest) %>%
  ggsurvplot_facet_fix(lungTest, facet.by=c("ph.ecog", "ph.karno"))
```
![Image of Yaktocat](https://raw.githubusercontent.com/BingxinS/survminer-fix/master/test2plot-fix.png)
