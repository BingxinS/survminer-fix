#' A workaround function to combine faceting, risk tables, and confidence intervals
#' in addition to survminer package
#' by Bingxin Shen
#' 
#' @examples
#' library(survminer)
#' require("survival")
#' source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet_table_confint.R")
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' data <- lung
#' fit <- survfit(Surv(time, status) ~ sex + ph.karno, data)
#' ggsurvplot_facet_table_confint(fit, data, risktable=TRUE, risktable.ylab="Number at Risk")
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' data <- lung
#' fit <- survfit(Surv(time, status) ~  sex + ph.ecog + ph.karno, data)
#' ggsurvplot_facet_table_confint(fit, data, conf.int = TRUE)
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' data <- lung
#' fit <- survfit(Surv(time, status) ~ sex + ph.ecog + ph.karno, data)
#' ggsurvplot_facet_table_confint(fit, data, risktable=TRUE, conf.int = TRUE)

# use the fixed faceted plot along this workaround function, or use the most recent experimental version of survminer
source("https://raw.githubusercontent.com/BingxinS/survminer-fix/master/ggsurvplot_facet_fix_12072018.R")
# report available at https://github.com/BingxinS/survminer-fix/blob/master/README.md

ggsurvplot_facet_table_confint <- function (fit, data = NULL, 
                                        risktable = FALSE,
                                        risktable.title = NULL,
                                        risktable.ylab = NULL,
                                        conf.int = FALSE,
                                        text.size = 10, ...) 
{
  # get strata.by and facet.by from fit$call
  fit.str <- sub('.*~', '', as.character(fit$call)[2])
  # facet.by = 
  fit.factor <-  strsplit(fit.str, " ")
  fit.factor.length <- length(fit.factor[[1]])/2
  if (fit.factor.length > 3) 
    stop("facet.by has more than 2 factors. Please use 1 or 2 faceting factors.")
  if (fit.factor.length <= 1) 
    stop("facet.by missing. Please use 1 or 2 faceting factors.")
  strata.by <- fit.factor[[1]][2]
  # Generate risk table for each facet plot item
  if(fit.factor.length == 2) {
    facet.by <- fit.factor[[1]][4]
    #================get the plot  ================
    plt_fct <- ggsurvplot_facet_fix(fit, data, 
                                    facet.by = facet.by)
    if (conf.int == TRUE){
    #================get the conf.int================
      ggsurv <- ggsurvplot(fit, data,
                           conf.int = conf.int, 
                           ggtheme = theme_bw())
      data.survplot <- .connect2origin_fix(ggsurv$data.survplot) 
      data.survplot <- .apply_surv_func(data.survplot)
      data.survplot.rect <- survdata2rect(data.survplot)
      # add conf.int to each facet plot
        facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
        plt_fct <- plt_fct +
          #geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha=0.3) +
          geom_rect(data = data.survplot.rect,
                    aes(xmin = time, xmax = timemax, ymin = lower, ymax = upper, fill = sex),
                    alpha = 0.3) +
          facet_wrap(facet.formula, labeller = label_both) + 
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_rect(fill="white", size = 1),
                axis.text.x = element_text(color="black", size = text.size+2),
                axis.text.y = element_text(color="black", size = text.size),
                axis.title = element_text(color="black", size = text.size+4),
                strip.text = element_text(color="black", size = text.size))
    }
  }
  if(fit.factor.length == 3) {
    facet.by <- fit.factor[[1]][c(4,6)]
    #================get the plot  ================
    plt_fct <- ggsurvplot_facet_fix(fit, data, 
                                    facet.by = facet.by)
    if (conf.int == TRUE){
    #================get the conf.int================
      ggsurv <- ggsurvplot(fit, data,
                           conf.int = conf.int, 
                           ggtheme = theme_bw())
      data.survplot <- .connect2origin_fix(ggsurv$data.survplot) 
      data.survplot <- .apply_surv_func(data.survplot)
      data.survplot.rect <- survdata2rect(data.survplot)
      # add conf.int to each facet plot
        facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
        plt_fct <- plt_fct +
          # geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha=0.3) +
          geom_rect(data = data.survplot.rect,
                    aes(xmin = time, xmax = timemax, ymin = lower, ymax = upper, fill = sex),
                    alpha = 0.3) +
          facet_grid(facet.formula, labeller = label_both) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_rect(fill="white", size = 1),
                axis.text.x = element_text(color="black", size = text.size+2),
                axis.text.y = element_text(color="black", size = text.size),
                axis.title = element_text(color="black", size = text.size+4),
                strip.text = element_text(color="black", size = text.size))
    }
  }
  if (risktable == FALSE){
    plt_fct
  } else {
  #================get the risk table================
    if(fit.factor.length == 2) {
      ggsurv <- ggsurvplot(fit, data,
                           risk.table = TRUE, 
                           ggtheme = theme_bw())
      tbl_fct <- 
        ggplot(ggsurv$table$data, ggplot2::aes_string("time", strata.by)) + 
        geom_text(aes(label = n.risk), size = max (2, (text.size-6)) ) +
        facet_wrap(facet.formula, labeller = label_both) + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill="white", size = 1),
              axis.text.x = element_text(color="black", size = text.size+2),
              axis.text.y = element_text(color="black", size = text.size),
              axis.title = element_text(color="black", size = text.size+4),
              strip.text = element_text(color="black", size = text.size))+
        ggtitle(risktable.title) +
        xlab("Time") + ylab(risktable.ylab) 
    }
    if(fit.factor.length == 3) {   
      ggsurv <- ggsurvplot(fit, data,
                           risk.table = TRUE, 
                           ggtheme = theme_bw())
      tbl_fct <- 
        ggplot(ggsurv$table$data, ggplot2::aes_string("time", strata.by)) + 
        geom_text(aes(label = n.risk), size = max (2, (text.size-6)) ) +
        facet_grid(facet.formula, labeller = label_both) + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill="white", size = 1),
              axis.text.x = element_text(color="black", size = text.size+2),
              axis.text.y = element_text(color="black", size = text.size),
              axis.title = element_text(color="black", size = text.size+4),
              strip.text = element_text(color="black", size = text.size))+
        ggtitle(risktable.title) +
        xlab("Time") + ylab(risktable.ylab) 
    }
    #========== combine plot and risk tables ===========
    g_plt_fct <- ggplotGrob(plt_fct)
    g_tbl_fct <- ggplotGrob(tbl_fct)
    min_ncol <- min(ncol(g_plt_fct), ncol(g_tbl_fct))
    g_plt_tbl <- gridExtra::gtable_rbind(g_plt_fct[, 1:min_ncol], g_tbl_fct[, 1:min_ncol], size="last")
    g_plt_tbl$widths <- grid::unit.pmax(g_plt_fct$widths, g_tbl_fct$widths)
    grid::grid.newpage() 
    grid::grid.draw(g_plt_tbl)
  }
}

survdata2rect <- function(data.survplot){
  data.survplot.rect = data.survplot[-nrow(data.survplot), ]
  data.survplot.rect$timemax = data.survplot$time[-1]
  strata.number <- length(levels(data.survplot$strata))
    ind <- which( (data.survplot.rect$timemax - data.survplot.rect$time)<0 )
  data.survplot.rect$timemax[ind] <- data.survplot.rect$time[ind]
  data.survplot.rect$timemax[c(1:strata.number)] <- data.survplot.rect$time[c(strata.number+1,ind+1)]
  data.survplot.rect
}
