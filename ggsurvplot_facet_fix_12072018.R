ggsurvplot_fix <- function (fit, data = NULL, fun = NULL, color = NULL, palette = NULL, 
                            linetype = 1, conf.int = FALSE, pval = FALSE, pval.method = FALSE, 
                            test.for.trend = FALSE, surv.median.line = "none", risk.table = FALSE, 
                            cumevents = FALSE, cumcensor = FALSE, tables.height = 0.25, 
                            group.by = NULL, facet.by = NULL, add.all = FALSE, combine = FALSE, 
                            ggtheme = theme_survminer(), tables.theme = ggtheme, ...) 
{
  if (length(group.by) > 2) 
    stop("group.by should be of length 1 or 2.")
  opts_df <- list(fit = fit, fun = fun, color = color, palette = palette, 
                  linetype = linetype, conf.int = conf.int, ggtheme = ggtheme, 
                  ...)
  opts <- list(fit = fit, data = data, fun = fun, color = color, 
               palette = palette, linetype = linetype, conf.int = conf.int, 
               pval = pval, pval.method = pval.method, test.for.trend = test.for.trend, 
               surv.median.line = surv.median.line, risk.table = risk.table, 
               cumevents = cumevents, cumcensor = cumcensor, tables.height = tables.height, 
               ggtheme = ggtheme, tables.theme = tables.theme, ...)
  if (.is_list(fit)) {
    if (combine) 
      ggsurv <- do.call(ggsurvplot_combine, opts)
    else ggsurv <- do.call(ggsurvplot_list, opts)
  }
  else if (is.data.frame(fit)) 
    ggsurv <- do.call(ggsurvplot_df, opts_df)
  else if (.is_survfit(fit)) {
    if (!is.null(group.by)) {
      opts$group.by <- group.by
      ggsurv <- do.call(ggsurvplot_group_by, opts)
    }
    else if (!is.null(facet.by)) {
      opts$facet.by <- facet.by
      ggsurv <- do.call(ggsurvplot_facet_fix, opts)
    }
    else if (add.all) {
      ggsurv <- do.call(ggsurvplot_add_all, opts)
    }
    else {
      if (is.null(fit$strata)) {
        if (missing(conf.int)) {
          opts$conf.int = TRUE
          opts$conf.int.fill = "strata"
        }
      }
      ggsurv <- do.call(ggsurvplot_core_fix, opts)
    }
  }
  else if (inherits(fit, "flexsurvreg")) 
    ggsurv <- do.call(ggflexsurvplot, opts)
  return(ggsurv)
}
#<bytecode: 0xaaa57b8>
#  <environment: namespace:survminer>

ggsurvplot_facet_fix <- function (fit, data, facet.by, color = NULL, palette = NULL, 
                                  legend.labs = NULL, pval = FALSE, pval.method = FALSE, pval.coord = NULL, 
                                  pval.method.coord = NULL, nrow = NULL, ncol = NULL, scales = "fixed", 
                                  short.panel.labs = FALSE, panel.labs = NULL, panel.labs.background = list(color = NULL, 
                                                                                                            fill = NULL), panel.labs.font = list(face = NULL, color = NULL, 
                                                                                                                                                 size = NULL, angle = NULL), panel.labs.font.x = panel.labs.font, 
                                  panel.labs.font.y = panel.labs.font, ...) 
{
  if (length(facet.by) > 2) 
    stop("facet.by should be of length 1 or 2.")
  if (!is.null(panel.labs) & !is.list(panel.labs)) # changed from .is_list() to is.list()
    stop("Argument panel.labs should be a list. Read the documentation.")
  . <- NULL
  .labeller <- "label_value"
  if (short.panel.labs) 
    .labeller <- label_both
  .dots <- list(...)
  fit.ext <- .extract.survfit(fit, data)
  .formula <- fit.ext$formula
  surv.obj <- fit.ext$surv
  surv.vars <- fit.ext$variables
  all.variables <- c(surv.vars, facet.by) %>% unique()
  vars.notin.groupby <- setdiff(all.variables, facet.by)
  data <- fit.ext$data.all
  if (!is.null(panel.labs)) {
    for (.grouping.var in facet.by) {
      if (!is.null(panel.labs[[.grouping.var]])) {
        if (!is.factor(data[, .grouping.var])) 
          data[, .grouping.var] <- as.factor(data[, .grouping.var])
        levels(data[, .grouping.var]) <- panel.labs[[.grouping.var]]
      }
    }
  }
  if (!all(facet.by %in% surv.vars) | !is.null(panel.labs)) {
    fit <- .build_formula(surv.obj, all.variables) %>% surv_fit(., 
                                                                data = data)
  }
  if (length(vars.notin.groupby) == 1) {
    if (is.null(color)) 
      color <- vars.notin.groupby
    .survformula <- .build_formula(surv.obj, vars.notin.groupby)
  }
  else {
    new.strata <- .create_strata(data, vars.notin.groupby, 
                                 sep = "; ")
    strata.levels <- levels(new.strata)
    data <- data %>% dplyr::mutate(.strata. = new.strata)
    new.surv.vars <- paste(c(".strata.", facet.by), collapse = " + ")
    .new.formula <- .build_formula(surv.obj, new.surv.vars)
    fit <- surv_fit(formula = .new.formula, data = data)
    if (is.null(color)) 
      color <- ".strata."
    if (is.null(legend.labs)) 
      legend.labs <- gsub(";", ",", strata.levels)
    .survformula <- .build_formula(surv.obj, ".strata.")
  }
  ################# calling the ggsurvplot_fix function in this file  ####################
  #################    instead of ggsurvplot function in survminer    ####################
  ggsurv <- ggsurvplot_core_fix(fit, data = data, color = color, 
                                palette = palette, legend.labs = legend.labs, ...)
  p <- .facet(ggsurv$plot, facet.by, nrow = nrow, ncol = ncol, 
              scales = scales, short.panel.labs = short.panel.labs, 
              panel.labs.background = panel.labs.background, panel.labs.font = panel.labs.font, 
              panel.labs.font.x = panel.labs.font.x, panel.labs.font.y = panel.labs.font.y)
  if (pval) {
    grouped.d <- surv_group_by(data, grouping.vars = facet.by)
    sf <- surv_fit(.survformula, grouped.d$data, ...)
    grouped.d <- grouped.d %>% mutate(fit = sf)
    pvalue <- surv_pvalue(grouped.d$fit, grouped.d$data, 
                          pval.coord = pval.coord, pval.method.coord = pval.method.coord, 
                          ...) %>% dplyr::bind_rows() %>% tibble::as.tibble()
    pvals.df <- grouped.d %>% dplyr::select_(.dots = facet.by) %>% 
      dplyr::bind_cols(pvalue)
    pval.x <- pval.y <- pval.txt <- method.x <- method.y <- method <- NULL
    p <- p + geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, 
                                            label = pval.txt), hjust = 0)
    if (pval.method) 
      p <- p + geom_text(data = pvals.df, aes(x = method.x, 
                                              y = method.y, label = method), hjust = 0)
  }
  p
}
#<bytecode: 0xd96f2f0>
#  <environment: namespace:survminer>

#' @include utilities.R surv_summary.R ggsurvplot_df.R surv_pvalue.R ggsurvtable.R
#
# Core function to plot survival curves using ggplot2.
# Accepts only one survfit object. Internally called by the other \code{ggsurvplot_*()} family functions.
# The documentation of arguments are described at ?ggsurvplot
################# calling the ggsurvplot_core_fix function in this file  ####################
#################    instead of ggsurvplot_core function in survminer    ####################
ggsurvplot_core_fix <- function(fit, data = NULL, fun = NULL,
                                color = NULL, palette = NULL, linetype = 1,
                                break.x.by = NULL, break.y.by = NULL,  break.time.by = NULL,
                                surv.scale = c("default", "percent"), xscale = 1,
                                conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                                conf.int.alpha = 0.3,
                                censor = TRUE, censor.shape = "+", censor.size = 4.5,
                                pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                                test.for.trend = FALSE,
                                pval.method = FALSE, pval.method.size = pval.size, pval.method.coord = c(NULL, NULL),
                                log.rank.weights = c("survdiff", "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1"),
                                title = NULL,  xlab = "Time", ylab = "Survival probability",
                                xlim = NULL, ylim = NULL, axes.offset = TRUE,
                                legend = c("top", "bottom", "left", "right", "none"),
                                legend.title = "Strata", legend.labs = NULL,
                                fontsize = 4.5, font.family = "",
                                tables.height = 0.25, tables.y.text = TRUE, tables.col = "black",
                                tables.y.text.col = TRUE,
                                risk.table = FALSE, risk.table.pos = c("out", "in"), risk.table.title = NULL,
                                risk.table.col = tables.col, risk.table.fontsize = fontsize,
                                risk.table.y.text = tables.y.text,
                                risk.table.y.text.col = tables.y.text.col,
                                risk.table.height = tables.height, surv.plot.height = 0.75,
                                ncensor.plot.height = tables.height, cumevents.height = tables.height,
                                cumcensor.height = tables.height,
                                ncensor.plot = FALSE,
                                ncensor.plot.title = NULL,
                                cumevents = FALSE, cumevents.col = tables.col, cumevents.title = NULL,
                                cumevents.y.text = tables.y.text, cumevents.y.text.col = tables.y.text.col,
                                cumcensor = FALSE, cumcensor.col = tables.col, cumcensor.title = NULL,
                                cumcensor.y.text = tables.y.text, cumcensor.y.text.col = tables.y.text.col,
                                surv.median.line = c("none", "hv", "h", "v"),
                                ggtheme = theme_survminer(),
                                tables.theme = ggtheme,
                                ...
){
  
  if(!inherits(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  surv.median.line <- match.arg(surv.median.line)
  stopifnot(log.rank.weights %in% c("survdiff", "1", "n", "sqrtN", "S1", "S2","FH_p=1_q=1"))
  log.rank.weights <- match.arg(log.rank.weights)
  
  # Make sure that user can do either ncensor.plot or cumcensor
  # But not both
  if(ncensor.plot & cumcensor){
    warning("Both ncensor.plot and cumsensor are TRUE.",
            "In this case, we consider only cumcensor.", call. = FALSE)
    ncensor.plot <- FALSE
  }
  if(cumcensor) ncensor.plot.height <- cumcensor.height
  if(is.null(ncensor.plot.title))
    ncensor.plot.title <- "Number of censoring"
  if(is.null(cumcensor.title))
    cumcensor.title <- "Cumulative number of censoring"
  if(is.null(cumevents.title))
    cumevents.title <- "Cumulative number of events"
  
  # risk.table argument
  risk.table.pos <- match.arg(risk.table.pos)
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type
  extra.params <- list(...)
  
  # Axes offset
  .expand <- ggplot2::waiver()
  if(!axes.offset)
    .expand <- c(0, 0)
  
  
  # Data
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # data used to compute survfit
  data <- .get_data(fit, data = data, complain = FALSE)
  # Data for survival plot
  d <- surv_summary(fit, data = data)
  if(!is.null(fit$start.time)) d <- subset(d, d$time >= fit$start.time )
  
  # Axis limits
  xmin <- ifelse(.is_cloglog(fun), min(c(1, d$time)), 0)
  if(!is.null(fit$start.time)) xmin <- fit$start.time
  xmax <- .get_default_breaks(d$time, .log = .is_cloglog(fun)) %>% max()
  if(is.null(xlim)) xlim <- c(xmin, xmax)
  
  # Main survival curves
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ################# calling the ggsurvplot_df_fix function in this file  ####################
  #################    instead of ggsurvplot_df function in survminer    ####################
  p <- ggsurvplot_df_fix(d, fun = fun,
                         color = color, palette = palette, linetype = linetype,
                         break.x.by = break.x.by, break.time.by = break.time.by, break.y.by = break.y.by,
                         surv.scale = surv.scale, xscale = xscale,
                         conf.int = conf.int, conf.int.fill = conf.int.fill, conf.int.style = conf.int.style,
                         conf.int.alpha = conf.int.alpha,
                         censor = censor, censor.shape = censor.shape, censor.size = censor.size,
                         title = title,  xlab = xlab, ylab = ylab,
                         xlim = xlim, ylim = ylim, axes.offset = axes.offset,
                         legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                         ggtheme = ggtheme, ...)
  
  
  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  color <- surv.color <- pms$color
  
  # Add pvalue
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Compute pvalue or parse it if provided by the user
  pval <- surv_pvalue(fit, method = log.rank.weights, data = data,
                      pval = pval, pval.coord = pval.coord,
                      pval.method.coord = pval.method.coord,
                      test.for.trend = test.for.trend)
  
  if(pval$pval.txt != ""){
    p <- p + ggplot2::annotate("text", x = pval$pval.x, y = pval$pval.y,
                               label = pval$pval.txt, size = pval.size, hjust = 0,
                               family = font.family)
    if(pval.method)
      p <- p + ggplot2::annotate("text", x = pval$method.x, y = pval$method.y,
                                 label = pval$method, size = pval.method.size, hjust = 0,
                                 family = font.family)
  }
  
  
  # Drawing a horizontal line at 50% survival
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #if(surv.scale == "percent") fun <- "pct"
  if(surv.median.line %in% c("hv", "h", "v"))
    p <- .add_surv_median(p, fit, type = surv.median.line, fun = fun, data = data)
  
  res <- list(plot = p)
  
  # Extract strata colors used in survival curves
  # Will be used to color the y.text of risk table and cumevents table
  if(risk.table | cumevents | cumcensor | ncensor.plot){
    scurve_cols <- .extract_ggplot_colors (p, grp.levels = pms$legend.labs)
  }
  
  
  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  surv.color <- pms$color
  
  pms$fit <- fit
  pms$data <- data
  pms$risk.table.type <- risk.table.type
  pms$risk.table.title <- risk.table.title
  pms$cumevents.title <- cumevents.title
  pms$cumcensor.title <- cumcensor.title
  pms$fontsize <- fontsize
  pms$ggtheme <- ggtheme
  pms$ylab <- pms$legend.title
  pms$tables.theme <- tables.theme
  pms$y.text <- tables.y.text
  pms$color <- tables.col
  pms$font.family <- font.family
  pms$axes.offset <- axes.offset
  
  
  # Add risk table
  if(risk.table){
    if(risk.table.pos == "in") risk.table.col = surv.color
    pms$color <- risk.table.col
    pms$title <- risk.table.title
    pms$y.text <- risk.table.y.text
    pms$y.text.col <- risk.table.y.text.col
    pms$fontsize <- risk.table.fontsize
    pms$survtable <- "risk.table"
    # color risk.table ticks by strata
    if(risk.table.y.text.col) pms$y.text.col <- scurve_cols
    res$table <- risktable <- do.call(ggsurvtable_debug, pms)
  }
  
  # Add the cumulative number of events
  if(cumevents){
    pms$color <- cumevents.col
    pms$title <- cumevents.title
    pms$y.text <- cumevents.y.text
    if(cumevents.y.text.col) pms$y.text.col <- scurve_cols
    pms$fontsize <- fontsize
    pms$survtable <- "cumevents"
    res$cumevents <- do.call(ggsurvtable_debug, pms)
  }
  
  # Add ncensor.plot or cumcensor plot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(ncensor.plot){
    ncensor_plot <- ggplot(d, aes_string("time", "n.censor")) +
      ggpubr::geom_exec(geom_bar, d, color = surv.color, fill = surv.color,
                        stat = "identity", position = "dodge")+
      coord_cartesian(xlim = xlim)+
      scale_y_continuous(breaks = sort(unique(d$n.censor))) +
      ggtheme
    
    ncensor_plot <- ggpubr::ggpar(ncensor_plot, palette = pms$palette)
    ncensor_plot <- ncensor_plot + ggplot2::labs(color = pms$legend.title, fill = pms$legend.title,
                                                 x = xlab, y = "n.censor", title = ncensor.plot.title)
    
    # For backward compatibility
    ncensor_plot <-  .set_general_gpar(ncensor_plot,  ...) # general graphical parameters
    ncensor_plot <- .set_ncensorplot_gpar(ncensor_plot,  ...) # specific graphical params
    ncensor_plot <- ncensor_plot + tables.theme
    
    if(!pms$xlog) ncensor_plot <- ncensor_plot + scale_x_continuous(breaks = pms$time.breaks,
                                                                    labels = pms$xticklabels, expand = .expand)
    else ncensor_plot <- ncensor_plot + ggplot2::scale_x_continuous(breaks = pms$time.breaks, trans = "log10", labels = pms$xticklabels)
    
  }
  else if(cumcensor){
    pms$color <- cumcensor.col
    pms$title <- cumcensor.title
    if(cumcensor.y.text.col) pms$y.text.col <- scurve_cols
    #pms$y.text.col <- cumcensor.y.text.col
    pms$fontsize <- fontsize
    pms$survtable <- "cumcensor"
    ncensor_plot  <- do.call(ggsurvtable_debug, pms)
  }
  if(ncensor.plot | cumcensor)
    res$ncensor.plot <- ncensor_plot
  
  
  # Defining attributs for ggsurvplot
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  heights <- list(
    plot =  surv.plot.height,
    table =  ifelse(risk.table, risk.table.height, 0),
    ncensor.plot = ifelse(ncensor.plot | cumcensor, ncensor.plot.height, 0),
    cumevents = ifelse(cumevents, cumevents.height, 0)
  )
  y.text <- list(
    table =  risk.table.y.text,
    cumevents = cumevents.y.text,
    cumcensor = cumcensor.y.text
  )
  y.text.col <- list(
    table =  risk.table.y.text.col,
    cumevents = cumevents.y.text.col,
    cumcensor = cumcensor.y.text.col
  )
  
  # Returning the data used to generate the survival plots
  res$data.survplot <- d
  res$data.survtable <- .get_timepoints_survsummary(fit, data, pms$time.breaks)
  
  class(res) <- c("ggsurvplot", "ggsurv", "list")
  attr(res, "heights") <- heights
  attr(res, "y.text") <- y.text
  attr(res, "y.text.col") <- y.text.col
  attr(res, "legend.position") <- legend
  attr(res, "legend.labs") <- legend.labs
  attr(res, "cumcensor") <- cumcensor
  attr(res, "risk.table.pos") <- risk.table.pos
  attr(res, "axes.offset") <- axes.offset
  res
}

# Build ggsurvplot for printing
.build_ggsurvplot <- function(x, surv.plot.height = NULL,
                              risk.table.height = NULL, ncensor.plot.height = NULL,
                              cumevents.height = NULL, ...)
{
  if(!inherits(x, "ggsurvplot"))
    stop("An object of class ggsurvplot is required.")
  heights <- attr(x, "heights")
  y.text <- attr(x, "y.text")
  y.text.col <- attr(x, "y.text.col")
  cumcensor <- attr(x, "cumcensor")
  axes.offset <- attr(x, "axes.offset")
  
  
  risk.table.pos <- attr(x, "risk.table.pos")
  if(risk.table.pos == "in") x <- .put_risktable_in_survplot(x, axes.offset = axes.offset)
  
  nplot <- .count_ggplots(x)
  # Removing data components from the list and keep only plot objects
  x$data.survplot <- x$data.survtable <-  NULL
  # Extract legend from the survival plot
  legend.position <- attr(x, "legend.position")[1]
  legend.grob <- .get_legend(x$plot)
  
  # Update heights
  if(!is.null(surv.plot.height))  heights$plot <- surv.plot.height
  if(!is.null(risk.table.height))  heights$table <- risk.table.height
  if(!is.null(ncensor.plot.height))  heights$ncensor.plot <- ncensor.plot.height
  if(!is.null(cumevents.height))  heights$cumevents <- cumevents.height
  heights$plot <- 1 - heights$table - heights$ncensor.plot - heights$cumevents
  
  # Extract strata colors for survival curves
  legend.labs <- attr(x, "legend.labs")
  if(!is.null(x$table) | !is.null(x$ncensor.plot) | !is.null(x$cumevents)){
    g <- ggplot_build(x$plot)
    cols <- unlist(unique(g$data[[1]]["colour"]))
    if(length(cols)==1) cols <- rep(cols, length(legend.labs))
    names(cols) <- legend.labs # Give every color an appropriate name
  }
  
  if(nplot > 1 & legend.position %in% c("left", "right", "bottom")) x$plot <- .hide_legend(x$plot)
  
  if(!is.null(x$table)){
    x$table <- .hide_legend(x$table)
    if(!y.text$table) x$table <- .set_large_dash_as_ytext(x$table)
    # Make sure that risk.table.y.text.col will be the same as the plot legend colors
    # if(y.text.col$table)
    #    x$table <- x$table + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }
  
  if(!is.null(x$cumevents)){
    x$cumevents <- .hide_legend(x$cumevents)
    if(!y.text$cumevents) x$cumevents <- .set_large_dash_as_ytext(x$cumevents)
    # Make sure that y.text.col will be the same as the plot legend colors
    #if(y.text.col$cumevents)
    #  x$cumevents <- x$cumevents + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }
  
  
  if(!is.null(x$ncensor.plot)){
    x$ncensor.plot <- x$ncensor.plot + theme (legend.position = "none")
    if(cumcensor){
      if(!y.text$cumcensor) x$ncensor.plot <- .set_large_dash_as_ytext(x$ncensor.plot)
      #if(y.text.col$cumcensor)
      # x$ncensor.plot <- x$ncensor.plot + theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
    }
  }
  
  if(is.null(x$table) & is.null(x$ncensor.plot) & is.null(x$cumevents)) return(ggplotGrob(x$plot))
  
  heights <- unlist(heights)[names(x)] # get the height of each component in x
  plots <- x
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    if(is.ggplot(plots[[i]])){
      grobs[[i]] <- ggplotGrob(plots[[i]])
      widths[[i]] <- grobs[[i]]$widths[2:5]
    }
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  
  
  ggsurv <- gridExtra::arrangeGrob(grobs = grobs, nrow = nplot, heights = unlist(heights))
  
  # Set legend
  if(nplot > 1 & legend.position %in% c("left", "right", "bottom") & !is.null(legend.grob)){
    ggsurv <- switch(legend.position,
                     bottom = gridExtra::arrangeGrob(grobs = list(ggsurv, legend.grob), nrow = 2, heights = c(0.9, 0.1)),
                     top = gridExtra::arrangeGrob(grobs = list(legend.grob, ggsurv), nrow = 2, heights = c(0.1, 0.9)),
                     right = gridExtra::arrangeGrob(grobs = list(ggsurv, legend.grob), ncol = 2, widths = c(0.75, 0.25)),
                     left = gridExtra::arrangeGrob(grobs = list(legend.grob, ggsurv), ncol = 2, widths = c(0.25, 0.75)),
                     ggsurv
    )
  }
  
  return(ggsurv)
}

.hide_legend <- function(p){
  p <- p + theme(legend.position = "none")
}



# Parse risk.table argument
#%%%%%%%%%%%%%%%%%%%%%%%
# risk.table a logical value (TRUE/FALSE) or a string ("absolute", "percentage", "abs_pct")
.parse_risk_table_arg <- function(risk.table){
  res <- list(display = risk.table, type = "absolute")
  if(inherits(risk.table, "character") ){
    if(risk.table %in% c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents") )
      res <- list(display = TRUE, type = risk.table)
    else stop("Allowed values for risk.table are: TRUE, FALSE, 'absolute', 'percentage', 'nrisk_cumcensor', 'nrisk_cumevents' ")
  }
  res
}

# Drawing horizontal line at 50% median survival
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.add_surv_median <-function(p, fit, type = "hv", fun = NULL, data = NULL){
  x1 <- x2 <- y1 <- y2 <- NULL
  
  draw_lines <- TRUE
  med_y = 0.5
  
  if(is.null(fun)) draw_lines <- TRUE
  else if(fun %in% c("cumhaz", "cloglog")){
    warning("Adding survival median lines is not allowed when fun is: ", fun)
    draw_lines <- FALSE
  }
  else if(fun == "pct") med_y <- 50
  
  if(draw_lines){
    if(!is.null(fit$strata) | is.matrix(fit$surv)) .table <- as.data.frame(summary(fit)$table)
    else{
      .table <- t(as.data.frame(summary(fit)$table))
      rownames(.table) <- "All"
    }
    surv_median <- as.vector(.table[,"median"])
    df <- data.frame(x1 = surv_median, x2 = surv_median,
                     y1 = rep(0, length(surv_median)),
                     y2 = rep(med_y, length(surv_median)),
                     strata = .clean_strata(rownames(.table)))
    if(!is.null(fit$strata)){
      variables <- .get_variables(df$strata, fit, data)
      for(variable in variables) df[[variable]] <- .get_variable_value(variable, df$strata, fit, data)
    }
    df <- stats::na.omit(df)
    
    if(nrow(df)>0){
      if(type %in% c("hv", "h"))
        p <- p +
          geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),
                       data = df, linetype = "dashed", size = 0.5) # horizontal segment
      
      if(type %in% c("hv", "v"))
        p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df,
                              linetype = "dashed", size = 0.5) # vertical segments
    }
    else warning("Median survival not reached.")
  }
  
  p
}



# Put risk table inside main plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.put_risktable_in_survplot <- function(ggsurv, axes.offset = TRUE){
  
  if(is.null(ggsurv$table)) return(ggsurv)
  
  if(is.null(ggsurv$table))
    stop("You can't put risk table inside the main plot because risk.table = FALSE. Use risk.table = TRUE")
  
  # Create a transparent theme object
  theme_transparent<- function() {
    theme(
      title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      plot.margin=unit(c(0,0,0,0),"mm"),
      panel.border = element_blank(),
      legend.position = "none")
  }
  
  survplot <- ggsurv$plot
  risktable <- ggsurv$table + theme_transparent()
  nstrata <- length(levels(survplot$data$strata))
  .time <- survplot$data$time
  ymax <- nstrata*0.05
  ymin <- -0.05
  xmin <- -max(.time)/20
  
  if(!axes.offset){
    ymin <- -0.02
    xmin <- -max(.time)/50
  }
  risktable_grob = ggplotGrob(risktable)
  survplot <- survplot + annotation_custom(grob = risktable_grob, xmin = xmin,
                                           ymin = ymin, ymax = ymax)
  ggsurv$plot <- survplot
  ggsurv$table <- NULL
  ggsurv
}

# Check if fun is cloglog
.is_cloglog <- function(fun){
  res <- FALSE
  if(is.character(fun)){
    res <- fun == "cloglog"
  }
  res
}

  ################# calling the ggsurvplot_df_fix function in this file  ####################
  #################    instead of ggsurvplot_df function in survminer    ####################
ggsurvplot_df_fix <- function (fit, fun = NULL, color = NULL, palette = NULL, linetype = 1, 
                               break.x.by = NULL, break.time.by = NULL, break.y.by = NULL, 
                               surv.scale = c("default", "percent"), surv.geom = geom_step, 
                               xscale = 1, conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon", 
                               conf.int.alpha = 0.3, censor = TRUE, censor.shape = "+", 
                               censor.size = 4.5, title = NULL, xlab = "Time", ylab = "Survival probability", 
                               xlim = NULL, ylim = NULL, axes.offset = TRUE, legend = c("top", 
                                                                                        "bottom", "left", "right", "none"), legend.title = "Strata", 
                               legend.labs = NULL, ggtheme = theme_survminer(), ...) 
{
  .dots <- list(...)
  if (!inherits(fit, "data.frame")) 
    stop("fit should be a data frame.")
  df <- fit
  size <- ifelse(is.null(list(...)$size), 1, list(...)$size)
  if (!is(legend, "numeric")) 
    legend <- match.arg(legend)
  if (!is.numeric(xscale) & !(xscale %in% c("d_m", "d_y", "m_d", 
                                            "m_y", "y_d", "y_m"))) 
    stop("xscale should be numeric or one of c(\"d_m\", \"d_y\", \"m_d\", \"m_y\", \"y_d\", \"y_m\").")
  ylab <- .check_ylab(ylab, fun)
  lty <- .get_lty(linetype)
  linetype <- lty$lty
  linetype.manual <- lty$lty.manual
  if (is.null(df$strata)) {
    df$strata <- as.factor(rep("All", nrow(df)))
    if (missing(conf.int)) {
      conf.int = TRUE
      conf.int.fill = "strata"
    }
  }
  if (is.null(color)) 
    color <- .strata.var <- "strata"
  else if (color %in% colnames(df)) {
    .strata.var <- color
  }
  else {
    warning("Now, to change color palette, use the argument palette= '", 
            color, "' ", "instead of color = '", color, "'", 
            call. = FALSE)
    palette <- color
    .strata.var <- "strata"
  }
  .strata <- df[, .strata.var]
  strata_names <- .levels(.strata)
  n.strata <- length(strata_names)
  if (!is.null(legend.labs)) {
    if (n.strata != length(legend.labs)) 
      stop("The length of legend.labs should be ", n.strata)
  }
  else if (is.null(legend.labs)) 
    legend.labs <- strata_names ##### this is correct! 2 for 2 sex#####
  if (is.null(legend.title)) 
    legend.title <- .strata.var
  if (!.is_cloglog(fun)) 
  ################# calling the .connect2origin_fix function in this file  ####################
  #################    instead of .connect2origin function in survminer    ####################
  df <- .connect2origin_fix(df) 
  df <- .apply_surv_func(df, fun = fun)
  surv.scale <- match.arg(surv.scale)
  scale_labels <- ggplot2::waiver()
  if (surv.scale == "percent") 
    scale_labels <- scales::percent
  xlog <- .is_cloglog(fun)
  y.breaks <- ggplot2::waiver()
  if (!is.null(break.y.by)) 
    y.breaks <- seq(0, 1, by = break.y.by)
  xmin <- ifelse(.is_cloglog(fun), min(c(1, df$time)), 0)
  if (is.null(xlim)) 
    xlim <- c(xmin, max(df$time))
  if (is.null(ylim) & is.null(fun)) 
    ylim <- c(0, 1)
  .expand <- ggplot2::waiver()
  if (!axes.offset) 
    .expand <- c(0, 0)
  df[, .strata.var] <- factor(df[, .strata.var], levels = .levels(.strata), 
                              labels = legend.labs)
  p <- ggplot2::ggplot(df, ggplot2::aes_string("time", "surv")) + 
    ggpubr::geom_exec(surv.geom, data = df, size = size, 
                      color = color, linetype = linetype, ...) + ggplot2::scale_y_continuous(breaks = y.breaks, 
                                                                                             labels = scale_labels, limits = ylim, expand = .expand) + 
    ggplot2::coord_cartesian(xlim = xlim) + ggtheme
  p <- ggpubr::ggpar(p, palette = palette, ...)
  if (!is.null(break.x.by)) 
    break.time.by <- break.x.by
  times <- .get_default_breaks(df$time, .log = xlog)
  if (!is.null(break.time.by) & !xlog) 
    times <- seq(0, max(c(df$time, xlim)), by = break.time.by)
  xticklabels <- .format_xticklabels(labels = times, xscale = xscale)
  if (!.is_cloglog(fun)) {
    p <- p + ggplot2::scale_x_continuous(breaks = times, 
                                         labels = xticklabels, expand = .expand) + ggplot2::expand_limits(x = 0, 
                                                                                                          y = 0)
  }
  else p <- p + ggplot2::scale_x_continuous(breaks = times, 
                                            trans = "log10", labels = xticklabels)
  if (conf.int) {
    if (missing(conf.int.fill)) 
      conf.int.fill <- color
    else if (length(conf.int.fill) == 1 & conf.int.fill[1] == 
             "gray" & n.strata > 1) 
      conf.int.fill <- color
    if (conf.int.style == "ribbon") {
      p <- p + ggpubr::geom_exec(.geom_confint, data = df, 
                                 ymin = "lower", ymax = "upper", fill = conf.int.fill, 
                                 alpha = conf.int.alpha, na.rm = TRUE)
    }
    else if (conf.int.style == "step") {
      p <- p + ggpubr::geom_exec(surv.geom, data = df, 
                                 y = "lower", linetype = "dashed", color = color, 
                                 na.rm = TRUE) + ggpubr::geom_exec(surv.geom, 
                                                                   data = df, y = "upper", linetype = "dashed", 
                                                                   color = color, na.rm = TRUE)
    }
  }
  if (censor & any(df$n.censor >= 1)) {
    p <- p + ggpubr::geom_exec(ggplot2::geom_point, data = df[df$n.censor > 
                                                                0, , drop = FALSE], colour = color, size = censor.size, 
                               shape = censor.shape)
  }
  lty.leg.title <- ifelse(linetype == "strata", legend.title, 
                          linetype)
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = title, 
                         color = legend.title, fill = legend.title, linetype = lty.leg.title)
  p <- .set_general_gpar(p, legend = legend, ...)
  if (!is.null(linetype.manual)) 
    p <- p + scale_linetype_manual(values = linetype.manual)
  pms <- list(data = df, color = color, palette = palette, 
              break.time.by = break.time.by, xlim = xlim, legend = legend, 
              legend.title = legend.title, legend.labs = legend.labs, 
              xlog = xlog, time.breaks = times, xlab = xlab, ylab = ylab, 
              xscale = xscale, xticklabels = xticklabels)
  attr(p, "parameters") <- pms
  p
}
#<bytecode: 0x5bf20c0>
#  <environment: namespace:survminer>
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Adapt ylab according to the value of the argument fun
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.check_ylab <- function(ylab, fun){
  if(!is.null(fun) & is.character(fun)){
    if(ylab == "Survival probability"){
      ylab <- switch(fun, log = "log(Survival probability)",
                     event = "Cumulative event",
                     cumhaz = "Cumulative hazard",
                     pct = "Survival probability (%)",
                     identity = "Survival probability",
                     cloglog = "log(-log(S(t)))",
                     "Survival probability")
    }
  }
  ylab
}

# Check user defined legend labels
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.check_legend_labs <- function(fit, legend.labs = NULL){
  
  if(!is.null(legend.labs) & !inherits(fit, "survfit.cox")){
    
    if(!is.null(fit$strata)){
      if(length(fit$strata) != length(legend.labs))
        stop("The length of legend.labs should be ", length(fit$strata) )
    }
    
    else{
      if(length(legend.labs) != 1)
        stop("The length of legend.labs should be 1")
    }
    
  }
}



# Connect survival data to the origine
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
######################## the real fixed function ########################
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
        tmpFacet  = gsub(".*=", "", tmpStrata)
        tmpFacet  = gsub(" ", "", tmpFacet, fixed = TRUE)
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



# Adjust linetype manually
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.get_lty <- function(linetype){
  linetype.manual = NULL
  nlty <- length(linetype)
  if(is.numeric(linetype)){
    if(nlty > 1) {
      linetype.manual <-linetype
      linetype <- "strata"
    }
  }
  else (is.character(linetype))
  {
    base_lty <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    is_base_lty <- all(linetype %in% base_lty)
    if(is_base_lty & nlty > 1){
      linetype.manual <-linetype
      linetype <- "strata"
    }
  }
  list(lty = linetype, lty.manual = linetype.manual)
}



# Graphical parameters
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# general graphical parameters to be applied to
# survival curves, risk.table, ncensor.plot
.set_general_gpar <- function(p, legend = "top", ...){
  extra.params <- list(...)
  ggpubr::ggpar(p = p, font.main = extra.params$font.main, font.x = extra.params$font.x,
                font.y = extra.params$font.y, font.submain = extra.params$font.submain,
                font.caption = extra.params$font.caption,
                font.tickslab = extra.params$font.tickslab,
                legend = legend, font.legend = extra.params$font.legend)
}


# Specific graphical params to ncensor_plot
.set_ncensorplot_gpar <- function(p, legend = "none", ...){
  extra.params <- list(...)
  ggpubr::ggpar(p,
                subtitle = extra.params$ncensor.plot.subtitle,
                caption = extra.params$ncensor.plot.caption,
                font.main = extra.params$font.ncensor.plot.title,
                font.submain = extra.params$font.ncensor.plot.subtitle,
                font.caption = extra.params$font.ncensor.plot.caption,
                font.tickslab = extra.params$font.ncensor.plot.tickslab,
                font.x = extra.params$font.ncensor.plot.x,
                font.y = extra.params$font.ncensor.plot.y,
                legend = legend)
  
}

# Helper function for faceting
.facet <- function(p,  facet.by, nrow = NULL, ncol = NULL,
                   scales = "fixed", short.panel.labs = FALSE,
                   panel.labs.background = list(color = NULL, fill = NULL),
                   panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                   panel.labs.font.x = panel.labs.font,
                   panel.labs.font.y = panel.labs.font
)
{
  
  panel.labs.background <- .compact(panel.labs.background)
  panel.labs.font.x <- .compact(panel.labs.font.x)
  panel.labs.font.y <- .compact(panel.labs.font.y)
  
  .labeller <- "label_value"
  if(!short.panel.labs) .labeller <- label_both
  
  if(length(facet.by) == 1){
    facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
    p <- p + facet_wrap(facet.formula, nrow = nrow, ncol = ncol, scales = scales, labeller = .labeller)
  }
  else if(length(facet.by) == 2){
    facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
    p <- p + facet_grid(facet.formula, scales = scales, labeller = .labeller)
  }
  
  if(!.is_empty(panel.labs.background))
    p <- p + theme(strip.background = do.call(element_rect, panel.labs.background))
  if(!.is_empty(panel.labs.font.x))
    p <- p + theme(strip.text.x = do.call(element_text, panel.labs.font.x))
  if(!.is_empty(panel.labs.font.y))
    p <- p + theme(strip.text.y = do.call(element_text, panel.labs.font.y))
  
  p
}

#' @import ggplot2
#' @import ggpubr
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom survival survdiff
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom methods is
#' @importFrom stats pchisq
#' @importFrom survMisc ten comp
#' @importFrom utils capture.output


# Count the number of ggplots in a list
.count_ggplots <- function(list.objects){
  nplot <- 0
  for(i in 1:length(list.objects)){
    if(is.ggplot(list.objects[[i]])) nplot <- nplot +1
  }
  nplot
}

# Extract legend from a ggplot
.get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]] # if legend
  else leg <- NULL
  return(leg)
}

# Connect observations by stairs.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Connect observations by stairs.
#
# mapping the aesthetic mapping
# data a layer specific dataset
# stat the statistical transformation to use on the data for this layer
# position the position adjustment to use for overlapping points on this layer
# na.rm logical frag whether silently remove missing values
#  ... other arguments passed to methods
.geom_confint <- function (mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, ...) {
  ggplot2::layer(mapping = mapping,
                 data = data,
                 stat = stat,
                 geom = GeomConfint,
                 position = position,
                 params = list(na.rm = na.rm, ...))
}
GeomConfint <- ggplot2::ggproto('GeomConfint', ggplot2::GeomRibbon,
                                required_aes = c("x", "ymin", "ymax"),
                                draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
                                  if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
                                  data <- data[order(data$group, data$x), ]
                                  data <- self$stairstep_confint(data)
                                  ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
                                },
                                stairstep_confint = function (data) {
                                  data <- as.data.frame(data)[order(data$x), ]
                                  n <- nrow(data)
                                  ys <- rep(1:n, each = 2)[-2 * n]
                                  xs <- c(1, rep(2:n, each = 2))
                                  data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
                                             data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
                                }
)

GeomConfint_old <- ggplot2::ggproto('GeomConfint_old', ggplot2::GeomRibbon,
                                    required_aes = c("x", "ymin", "ymax"),
                                    draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                                      if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
                                      data <- rbind(data, data)
                                      data <- data[order(data$x), ]
                                      data$x <- c(data$x[2:nrow(data)], NA)
                                      data <- data[complete.cases(data["x"]), ]
                                      GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
                                    }
                                    #                                 draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
                                    #                                   if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
                                    #                                   data <- data[order(data$group, data$x), ]
                                    #                                   data <- self$stairstep_confint(data)
                                    #                                   ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
                                    #                                 },
                                    #                                 stairstep_confint = function (data) {
                                    #                                   data <- as.data.frame(data)[order(data$x), ]
                                    #                                   n <- nrow(data)
                                    #                                   ys <- rep(1:n, each = 2)[-2 * n]
                                    #                                   xs <- c(1, rep(2:n, each = 2))
                                    #                                   data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
                                    #                                              data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
                                    #                                 }
)


# Remove NULL items in a vector or list
#
# x a vector or list
.compact <- function(x){Filter(Negate(is.null), x)}

# remove white space at the head and the tail of a string
.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}

# Take a data frame and return a flatten value
.flat <- function(x){
  x <- as.data.frame(x)
  x <- tidyr::gather_(x,
                      key_col = "key", value_col = "value",
                      gather_cols = colnames(x))
  x$value
}

# extract dataset if not provided
.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}

# Compute default axis breaks as ggplot2
#-------------------------------------
# Return a vector of axis labels
.get_default_breaks <- function(x, .log = FALSE){
  if(!.log) scales::extended_breaks()(x)
  else scales::log_breaks()(x)
}


# Get survival summary for a specified time points
#------------------------------------------------
# fit: survfit object
# data: data used for survfit
# times: a vector of timepoints
#
# Return a data frame with the following components:
#   - strata: stratification of curve estimation
#   - time: the timepoints on the curve
#   - n.risk: the number of subjects at risk at time t-0
#   - n.event: the cumulative number of events that have occurred since the last time listed until time t+0
#   - n.censor: number of censored subjects
#   - strata_size: number of subject in the strata
.get_timepoints_survsummary <- function(fit, data, times, decimal.place = 0)
{
  survsummary <- summary(fit, times = times, extend = TRUE)
  
  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(survsummary$time)))
    strata_names <- "All"
    strata_size <- rep(fit$n, length(.strata))
  }
  else {
    .strata <- factor(survsummary$strata)
    strata_names <- names(fit$strata)
    nstrata <- length(strata_names)
    strata_size <- rep(fit$n, each = length(.strata)/nstrata)
  }
  
  strata <- .clean_strata(.strata)
  res <- data.frame(
    strata = strata,
    time = survsummary$time,
    n.risk = round(survsummary$n.risk, digits = decimal.place),
    pct.risk = round(survsummary$n.risk*100/strata_size),
    n.event = round(survsummary$n.event, digits = decimal.place),
    cum.n.event = unlist(by(survsummary$n.event, strata, cumsum)),
    n.censor = round(survsummary$n.censor, digits = decimal.place),
    cum.n.censor = unlist(by(survsummary$n.censor, strata, cumsum)),
    strata_size = strata_size
  )
  
  if(!is.null(fit$strata)){
    variables <- .get_variables(res$strata, fit, data)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, fit, data)
  }
  rownames(res) <- 1:nrow(res)
  res
}

# Get variable names in strata
# -----------------------------------------
# strata: a vector
# fit: survfit object
# data: data used to fit survival curves
.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

# levels of a given variable used in survfit formula
# ----------------------------
# variable: variable name
.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    index <- grep(paste0("^", variable, "$"), x)
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}


# remove dollar sign ($) in strata
# ---------------------------------
# remove dollar sign ($) in strata, in the situation, where
# the user uses data$variable to fit survival curves
.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}


# Set large dash as y tick labels when ytext = FALSE
# Each dash corresponds to a strata
# This is used for tables under the main survival plots
#
.set_large_dash_as_ytext <- function(ggp){
  ggp + theme(axis.text.y = element_text(size = 50, vjust = 0.35),
              axis.ticks.y = element_blank())
}


# Transform x-axis labels according to the scale see ggsurvplot().
#
# labels: numeric vector (x-axis labels)
#xscale: numeric or character values (see
# gsurvplot). If numeric, the value is used to divide the labels on the x axis.
# For example, a value of 365.25 will give labels in years instead of the
# original days. If character, allowed options include one of c("d_m", "d_y",
# "m_d", "m_y", "y_d", "y_m"), where d = days, m = months and y = years. For
# example, xscale = "d_m" will transform labels from days to months; xscale =
# "m_y", will transform labels from months to years.
.format_xticklabels <- function(labels, xscale){
  
  # 1 year = 365.25 days
  # 1 month = 365.25/12 = 30.4375 days
  if(is.numeric(xscale)) xtrans <- 1/xscale
  else
    xtrans <- switch(xscale,
                     d_m = 12/365.25,
                     d_y = 1/365.25,
                     m_d = 365.25/12,
                     m_y = 1/12,
                     y_d = 365.25,
                     y_m = 12,
                     1
    )
  round(labels*xtrans,2)
}


# Extract strata colors used in survival curves
# Will be used to color the y.text of risk table and cumevents table
.extract_ggplot_colors <- function(p, grp.levels){
  g <- ggplot_build(p)
  .cols <- unlist(unique(g$data[[1]]["colour"]))
  if(!is.null(grp.levels)){
    if(length(.cols)==1) .cols <- rep(.cols, length(grp.levels))
    names(.cols) <- grp.levels
  }
  .cols
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions for survival curves
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.is_survfit <- function(fit){
  inherits(fit, "survfit")
}

.is_grouped_data <- function(data){
  inherits(data, c("surv_group_by"))
}

.get_fit_formula <- function(fit){
  fit$call$formula %>%
    stats::as.formula()
}

.build_formula <- function(surv.obj, variables){
  . <- NULL
  paste(variables, collapse  = " + ") %>%
    paste0(surv.obj, " ~ ", .) %>%
    stats::as.formula()
}

# Function defining a transformation of the survival curve
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# see ?survival::plot.survfit
# d: data frame containing the column surv, upper and lower
# fun the function
.apply_surv_func <- function(d, fun = NULL){
  
  if (!is.null(fun)) {
    if (is.character(fun)) {
      fun <- switch(fun, log = function(y) log(y),
                    event = function(y) 1 - y,
                    cumhaz = function(y) -log(y),
                    cloglog = function(y) log(-log(y)),
                    pct = function(y) y * 100,
                    logpct = function(y) 100 * y,
                    identity = function(y) y,
                    stop("Unrecognized survival function argument"))
    }
    else if (!is.function(fun)) {
      stop("Invalid 'fun' argument")
    }
    cnames <- colnames(d)
    if("surv" %in% cnames) d$surv <- fun(d$surv)
    if("upper" %in% cnames) d$upper <- fun(d$upper)
    if("lower" %in% cnames) d$lower <- fun(d$lower)
  }
  return(d)
}


# Get the names of formulas
#.........................................................................
# If formulas is a named lists, returns the list names if available.
# If formula names are not available, collapse the variables in the formula, and use this as the formula name
# If formula is a formula object, returns collapsed variable names using "+"
.get_formula_names <- function(formula){
  
  # helper function to return collapsed variable names for one formula.
  .fname <- function(formula){
    res <- attr(stats::terms(formula), "term.labels")
    if(.is_empty(res)) res <- "null_model"
    else res <- .collapse(res, sep = " + ")
    res
  }
  
  if(.is_list(formula)){
    fnames <- names(formula)
    if(is.null(fnames)) fnames <- purrr::map(formula, .fname) %>%
        unlist()
  }
  
  else fnames <- .fname(formula)
  fnames
}



# Get the names of fit
# If fit is a named lists, returns the list names if available.
# If fit names are not available, collapse the variables in the formula, and use this as the fit name
#.........................................................................
.get_fit_names <- function(fit){
  
  if(.is_list(fit)){
    fnames <- names(fit)
    if(is.null(fnames)) {
      fnames <- purrr::map(fit, .get_fit_formula) %>%
        .get_formula_names()
    }
  }
  else fnames <- .get_fit_formula(fit) %>%
      .get_formula_names()
  fnames
}


# Extract survfit components
#.................................................................................
# Return a list: list(formula, surv, variables, data.all, data.formula)
#       - formula: survival formula
#       - surv: surv object
#       - variables: vector of variable names
#       - data.all: the dataset used in survfit
#       - data.formula: data off all variables in the formula including time and status
.extract.survfit <- function(fit, data = NULL){
  
  if(inherits(fit, "survfit.cox"))
    return(list())
  
  .formula <- fit$call$formula %>%
    stats::as.formula()
  surv.obj <- deparse(.formula[[2]])
  surv.vars <- attr(stats::terms(.formula), "term.labels")
  data.all <- data <- .get_data(fit, data = data, complain = FALSE)
  # data of variables used in formula
  data.formula <- stats::get_all_vars(.formula, data = data) #%>%
  #na.omit()
  
  list(formula = .formula, surv = surv.obj,
       variables = surv.vars,
       data.all = data.all,
       data.formula = data.formula)
}


# Create strata from variable names
#.................................................................................
# Returns a factor
# Example:
# library(survival)
# .create_strata(colon, c("sex", "rx"))
.create_strata <- function(data, var.names, sep = ", "){
  
  # Strata
  .strata <- data[, var.names, drop = FALSE] %>%
    survival::strata(sep = sep)
  .strata.levels <- levels(.strata)
  # Replace  "=" by ".
  .strata <- gsub("=", ":", .strata) %>% .trim()
  .strata.levels <- gsub("=", ":", .strata.levels) %>% .trim()
  factor(.strata, levels = .strata.levels)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# General helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Returns the levels of a factor variable
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.levels <- function(x){
  if(!is.factor(x)) x <- as.factor(x)
  levels(x)
}

# Check if is a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_list <- function(x){
  inherits(x, "list")
}

# Collapse one or two vectors
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.collapse <- function(x, y = NULL, sep = "."){
  if(missing(y))
    paste(x, collapse = sep)
  else if(is.null(x) & is.null(y))
    return(NULL)
  else if(is.null(x))
    return (as.character(y))
  else if(is.null(y))
    return(as.character(x))
  else
    paste0(x, sep, y)
}

# Check if en object is empty
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_empty <- function(x){
  length(x) == 0
}


# Pasting the column name to each value of a dataframe
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.paste_colnames <- function(data, sep = "."){
  
  data <- as.data.frame(data)
  
  if(ncol(data) == 1){
    
    res <- paste0(colnames(data), ".", data[[1]])
    res <- data.frame(x = res, stringsAsFactors = FALSE)
    colnames(res) <- colnames(data)
    return(res)
  }
  
  res <- apply(data, 1,
               function(row, cname){paste(cname, row, sep = sep)},
               colnames(data)
  ) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(res) <- colnames(data)
  res
}


# Bind data list by rows
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# add id columns for a named data list
.rbind_data_list <- function(df.list){
  
  .names <- names(df.list)
  
  if(!is.null(.names)){
    df.list <- purrr::map2(df.list, .names, function(df, .name){
      dplyr::mutate(df, id = .name)
    })
  }
  
  id <- NULL
  res <- dplyr::bind_rows(df.list) %>%
    dplyr::select(id, dplyr::everything())
  res
}

#########debug##########
ggsurvtable_debug <- function (fit, data = NULL, survtable = c("cumevents", "cumcensor", 
                                                               "risk.table"), risk.table.type = c("absolute", "percentage", 
                                                                                                  "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"), title = NULL, 
                               risk.table.title = NULL, cumevents.title = title, cumcensor.title = title, 
                               color = "black", palette = NULL, break.time.by = NULL, xlim = NULL, 
                               xscale = 1, xlab = "Time", ylab = "Strata", xlog = FALSE, 
                               legend = "top", legend.title = "Strata", legend.labs = NULL, 
                               y.text = TRUE, y.text.col = TRUE, fontsize = 4.5, font.family = "", 
                               axes.offset = TRUE, ggtheme = theme_survminer(), tables.theme = ggtheme, 
                               ...) 
{
  if (is.data.frame(fit)) {
  }
  else if (.is_list(fit)) {
    if (!all(c("time", "table") %in% names(fit))) 
      stop("fit should contain the following component: time and table")
  }
  else if (!.is_survfit(fit)) 
    stop("Can't handle an object of class ", class(fit))
  xmin <- ifelse(xlog, min(c(1, fit$time)), 0)
  if (is.null(xlim)) 
    xlim <- c(xmin, max(fit$time))
  times <- .get_default_breaks(fit$time, .log = xlog)
  if (!is.null(break.time.by) & !xlog) 
    times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)
  if (.is_survfit(fit)) {
    data <- .get_data(fit, data = data)
    survsummary <- .get_timepoints_survsummary(fit, data, 
                                               times)
  }
  else if (.is_list(fit)) {
    survsummary <- fit$table
  }
  else if (inherits(fit, "data.frame")) {
    survsummary <- fit
  }
  opts <- list(survsummary = survsummary, times = times, survtable = survtable, 
               risk.table.type = risk.table.type, color = color, palette = palette, 
               xlim = xlim, xscale = xscale, title = title, xlab = xlab, 
               ylab = ylab, xlog = xlog, legend = legend, legend.title = legend.title, 
               legend.labs = legend.labs, y.text = y.text, y.text.col = y.text.col, 
               fontsize = fontsize, font.family = font.family, axes.offset = axes.offset, 
               ggtheme = ggtheme, tables.theme = tables.theme, ...)
  res <- list()
  time <- strata <- label <- n.event <- cum.n.event <- NULL
  if ("cumevents" %in% survtable) {
    opts$survtable = "cumevents"
    opts$title <- ifelse(is.null(cumevents.title), "Cumulative number of events", 
                         cumevents.title)
    res$cumevents <- do.call(.plot_survtable_debug, opts)
  }
  if ("cumcensor" %in% survtable) {
    opts$survtable = "cumcensor"
    opts$title <- ifelse(is.null(cumcensor.title), "Cumulative number of events", 
                         cumcensor.title)
    res$cumcensor <- do.call(.plot_survtable_debug, opts)
  }
  if ("risk.table" %in% survtable) {
    opts$survtable = "risk.table"
    if (is.null(risk.table.title)) 
      opts$title <- NULL
    else opts$title <- risk.table.title
    res$risk.table <- do.call(.plot_survtable_debug, opts)
  }
  if (length(res) == 1) 
    res <- res[[1]]
  res
}

##debug############
.plot_survtable_debug <- function (survsummary, times, survtable = c("cumevents", "risk.table", 
                                                                     "cumcensor"), risk.table.type = c("absolute", "percentage", 
                                                                                                       "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"), color = "black", 
                                   palette = NULL, xlim = NULL, xscale = 1, title = NULL, xlab = "Time", 
                                   ylab = "Strata", xlog = FALSE, legend = "top", legend.title = "Strata", 
                                   legend.labs = NULL, y.text = TRUE, y.text.col = TRUE, fontsize = 4.5, 
                                   font.family = "", axes.offset = TRUE, ggtheme = theme_survminer(), 
                                   tables.theme = ggtheme, ...) 
{
  survtable <- match.arg(survtable)
  risk.table.type <- match.arg(risk.table.type)
  if (is.null(title)) {
    if (survtable == "risk.table") {
      risk.table.type <- match.arg(risk.table.type)
      title <- switch(risk.table.type, absolute = "Number at risk", 
                      percentage = "Percentage at risk", abs_pct = "Number at risk: n (%)", 
                      nrisk_cumcensor = "Number at risk (number censored)", 
                      nrisk_cumevents = "Number at risk (number of events)", 
                      "Number at risk")
    }
    else title <- switch(survtable, cumevents = "Cumulative number of events", 
                         cumcensor = "Number of censored subjects")
  }
  if (is.null(color)) 
    color <- .strata.var <- "strata"
  else if (color %in% colnames(survsummary)) 
    .strata.var <- color
  else .strata.var <- "strata"
  
  ##########################################
  ##       original code block            ##
  
  .strata <- survsummary[, .strata.var]
  strata_names <- .levels(.strata)
  n.strata <- length(strata_names)
  #   legend.labs <- strata_names
  #####################################################  
  ###########     start of modification     ###########
  #ind.strata_size <- grep("strata_size",names(survsummary))
  #.strata <- survsummary[, (ind.strata_size+1)]
  #strata_names <- .levels(.strata)
  #n.strata <- length(strata_names)
  #.strata.var <- names(survsummary)[(ind.strata_size+1)]
  ###########        end of modification    ###########
  #####################################################
  #  if (!is.null(legend.labs)) {
  #    if (n.strata != length(legend.labs)) 
  #      warning("The length of legend.labs should be ", n.strata)
  #    else survsummary$strata <- factor(survsummary$strata, 
  #                                      labels = legend.labs) 
  #    #else survsummary$strata <- factor(subset(survsummary, select=.strata.var), 
  #    #                                  labels = legend.labs) ##########
  #  }
  # else if (is.null(legend.labs)) 
  legend.labs <- strata_names
  yticklabs <- rev(levels(survsummary$strata))
  n_strata <- length(levels(survsummary$strata))
  if (!y.text) 
    yticklabs <- rep("-", n_strata)
  time <- strata <- label <- n.event <- cum.n.event <- cum.n.censor <- NULL
  if (survtable == "cumevents") {
    mapping <- aes(x = time, y = rev(strata), label = cum.n.event, 
                   shape = rev(strata))
  }
  else if (survtable == "cumcensor") {
    mapping <- aes(x = time, y = rev(strata), label = cum.n.censor, 
                   shape = rev(strata))
  }
  else if (survtable == "risk.table") {
    pct.risk <- abs_pct.risk <- n.risk <- NULL
    llabels <- switch(risk.table.type, percentage = round(survsummary$n.risk * 
                                                            100/survsummary$strata_size), abs_pct = paste0(survsummary$n.risk, 
                                                                                                           " (", survsummary$pct.risk, ")"), nrisk_cumcensor = paste0(survsummary$n.risk, 
                                                                                                                                                                      " (", survsummary$cum.n.censor, ")"), nrisk_cumevents = paste0(survsummary$n.risk, 
                                                                                                                                                                                                                                     " (", survsummary$cum.n.event, ")"), survsummary$n.risk)
    survsummary$llabels <- llabels
    mapping <- aes(x = time, y = rev(strata), label = llabels, 
                   shape = rev(strata))
  }
  .expand <- ggplot2::waiver()
  if (!axes.offset) {
    .expand <- c(0, 0)
    offset <- max(xlim)/30
    survsummary <- survsummary %>% dplyr::mutate(time = ifelse(time == 
                                                                 0, offset, time))
  }
  p <- ggplot(survsummary, mapping) + scale_shape_manual(values = 1:length(levels(survsummary$strata))) + 
    ggpubr::geom_exec(geom_text, data = survsummary, size = fontsize, 
                      color = color, family = font.family) + ggtheme + 
    scale_y_discrete(breaks = as.character(levels(survsummary$strata)), 
                     labels = yticklabs) + coord_cartesian(xlim = xlim) + 
    labs(title = title, x = xlab, y = ylab, color = legend.title, 
         shape = legend.title)
  if (survtable == "risk.table") 
    p <- survminer:::.set_risktable_gpar(p, ...) ############call internal func########
  p <- ggpubr::ggpar(p, legend = legend, palette = palette, 
                     ...)
  xticklabels <- .format_xticklabels(labels = times, xscale = xscale)
  if (!xlog) 
    p <- p + ggplot2::scale_x_continuous(breaks = times, 
                                         labels = xticklabels, expand = .expand)
  else p <- p + ggplot2::scale_x_continuous(breaks = times, 
                                            trans = "log10", labels = xticklabels)
  p <- p + tables.theme
  if (!y.text) {
    p <- .set_large_dash_as_ytext(p)
  }
  if (is.logical(y.text.col) & y.text.col[1] == TRUE) {
    cols <- .extract_ggplot_colors(p, grp.levels = legend.labs)
    p <- p + theme(axis.text.y = element_text(colour = rev(cols)))
  }
  else if (is.character(y.text.col)) 
    p <- p + theme(axis.text.y = element_text(colour = rev(y.text.col)))
  p
}
