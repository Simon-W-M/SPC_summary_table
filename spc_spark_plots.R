library(ggrepel)
library(tidyverse)
library(NHSRdatasets)
library(NHSRplotthedots)
library (gt)
library(gtExtras)
library(purrr)
library(glue)

# create dataset

# note for this example I am using the org_codes to be a metric type

# Filter the data a bit, add a target and rename attendance to value
# can specify target to 9999 to be no target
dat <- ae_attendances |>
  filter(org_code %in% c('R1K', 'RJ2'),
         period > '2017-10-01',
         type %in% c('1')) |>
  mutate(target = if_else(org_code == 'R1K', 9999, 20000),
         imp = 'increase',
         perc = FALSE) |>
  select (period,
          metric = org_code,
          value = attendances,
          imp,
          target,
          perc)

# have made some with percentages just for fun
dat2 <- ae_attendances |>
  filter(org_code %in% c('RF4', 'RQM'),
         period > '2017-10-01',
         type %in% c('1')) |>
  mutate(target = if_else(org_code == 'RF4', 9999, 0.05),
         imp = 'increase',
         value = (breaches / attendances) * 100,
         perc = TRUE) |>
  select (period,
          metric = org_code,
          value,
          imp,
          target,
          perc)

dat <- rbind(dat, dat2)

# target and improvement direction are arbitrary in this example.  
# This kind of meta data could be saved in a separate dataframe and added
# in a separate process


# create a sparkline SPC plot
plot_spc_spark <- function (df, met) {
#  @function plot_spc_spark
#  This function takes a dataframe (df) and a metric name (met) as 
#  input and creates a sparkline SPC plot.
#  The sparkline SPC plot displays the control limits (UCL and LCL) for 
#  the metric, as well as the target value and the most recent data points.
#  @param {data.frame} df The dataframe containing the data for the SPC plot.
#  @param {string} met The name of the metric to plot.
#  @returns {ggplot} A ggplot object containing the sparkline SPC plot. 

# filter the data to the metric - this assumes each metric has unique name
# may need to concatenate metric and team/ward/icb if running same metric 
# on different levels
  dat <- df |>
    filter (metric == met)
  
  # identify a single value for target
  targ <- dat$target[1]  
  
  # if 9999 entered as target don't plot plot target
  tg <- if (targ == 9999) {ptd_target()}  else {ptd_target(targ) }
  
  # check if percentage measure
  perc <- dat$perc[1]
  
  # run spc rules over data and save as dataframe
  spc_dat <- ptd_spc(dat,
                     value_field = value,
                     date_field = period,
                     target = tg,
                     improvement_direction = unique(dat$imp))
  
  # create the basic SPC plot
  p <-ptd_create_ggplot(spc_dat,
                        icons_position = 'none',
                        point_size = 8,
                        percentage_y_axis = perc)
 
  # find the latest value of metric
  curr <- dat$value[dat$period == max(dat$period)] 
  
  # find current month of value
  curr_per <- dat$period[dat$period == max(dat$period)] 
  
  # min value of metric
  min_s <- min(dat$value) 
  
  # max value of metric
  max_s <- max(dat$value)
  
  # mean of metric
  mean_s <- spc_dat$mean[1]

  # find the upl, lpl and mean
  cl_mean <- c(spc_dat$upl[1], 
               spc_dat$lpl[1], 
               spc_dat$mean[1])
  
  # give them lables
  cl_lab <- c('UCL', 'LCL', 'Mean')
  
  # add them into dataframe
  cl_dat <- data.frame(cl_mean, cl_lab)
  
  # make a position to to the labels to the left and right 
  # of the chart, their positions determined by dates
  # assumes monthly data!
  min_d <- as.POSIXct.Date(min(dat$period) %m-% months(4))
  max_d <- as.POSIXct.Date(curr_per %m+% months(16))
  
  # set the target position and label
  targpos <- if_else(targ == 9999, NA, targ)
  targlab <- if_else(targ == 9999, NA, 'Target:')
  
  # put the current, min , max and target into vector
  curr_min_max <- (c(curr, 
                     min_s, 
                     mean_s,
                     max_s,
                     targpos))
  
  # remove target from the vector if no target
  if (targ == 9999) {curr_min_max <- curr_min_max[1:4]}
  
  # create a vector of all the positions
  curr_min_max_pos <- (c(curr, 
                     min_s, 
                     max_s,
                     targpos,
                     spc_dat$upl[1],
                     spc_dat$lpl[1],
                     targpos))
  
  # finds the max and min of the positions
  minpos <- min(curr_min_max_pos, na.rm = T)
  maxpos <- max(curr_min_max_pos, na.rm = T)
  
  # create a new sequence to spread the label positions 
  #at intervals between the min and max
  curr_min_max_pos <-seq(minpos, maxpos, length.out = 7)[2:6]
  if (targ == 9999) {curr_min_max_pos <- seq(minpos, maxpos, length.out = 6)[2:5]}

  
  # create vector of labels,
  mix <- c(paste0(format(curr_per, "%b %y"),':'), 
           'Min:', 
           'Mean:',
           'Max:', 
           targlab)
  
  if (targ == 9999) {mix <- mix[1:4]}
  
  # create dataframe for right hand details
  rhd <- data.frame(curr_min_max, mix)
  
  text_size <- 16
  
  # format the plot
  plot <- p  + 
    geom_text(data = rhd, 
                    aes(x = as.POSIXct(curr_per %m+% months(16)), 
                    y = curr_min_max_pos), 
                    label = paste0(mix,
                                   ' ',
                                   prettyNum(curr_min_max, 
                                             format = 'f',
                                             big.mark = ",", 
                                             digits = 2),
                                   if_else (perc==TRUE, '%', '')), 
                    size = text_size,
                    hjust = 'right',
                    box.padding	= 0.4) +
    geom_text_repel(data = cl_dat, 
                    aes(x = as.POSIXct(min(dat$period) %m-% months(6)), 
                        y = cl_mean), 
                        label = cl_lab, 
                        size = text_size) +
    theme_void() + 
    theme(legend.position="none") + 
    theme(title=element_blank()) +
    theme(axis.title.x = element_text(size = text_size *3)) + 
    xlab('Months                    ') +
    coord_cartesian(xlim = c(min_d, 
                             max_d),
                    expand = TRUE) 
  
  plot
}

#plot_spc_spark(dat,'RF4')



spc_icons <- function (df, met, assu_or_var) {
  #' @function spc_icons
  #' This function determines the appropriate SPC icon to display 
  #' for a given metric, based on either assurance or variation logic.
  #' @param {data.frame} df - The dataframe containing the data for the SPC plot.
  #' @param {string} met - The name of the metric to consider.
  #' @param {string} assu_or_var - The type of icon to generate: 
  #'                               either "assurance" or "variation".
  #' @returns {string} The code for the appropriate SPC icon. 
  #' Available icons include:
  #' - SIH: Special Cause Improvement High
  #' - SCL: Special Cause Concern Low
  #' - SIL: Special Cause Improvement Low
  #' - SCH: Special Cause Concern High
  #' - CCV: Common Cause Variation
  #' - CF: Consistent Fail
  #' - CP: Consistent Pass
  #' - CV: Common Variation
  #' - BLANK: A blank icon

  # filter data to just the metric
  dat <- df |>
    filter (metric == met)
  
  # find the target and replace with null if 9999
  targ <- dat$target[1]  
  tg <- if (targ == 9999) {ptd_target()}  else {ptd_target(targ)}
  
  # pull the improvement direction
  imp <- dat$imp[1]
  
  # run the spc dataframe
  spc_dat <- ptd_spc(dat,
                     value_field = value,
                     date_field = period,
                     target = tg,
                     improvement_direction = 'increase')
  
  # find the latest value, upl, lpl, and latest special point type
  latest_val <- spc_dat$y[spc_dat$x == max(spc_dat$x, na.rm = TRUE)]
  upl <- spc_dat$upl[1]
  lpl <- spc_dat$lpl[1]
  latest_pt <- spc_dat$point_type[spc_dat$x == max(spc_dat$x, na.rm = TRUE)]
  
  # calulate which icon to use
  
  # variation icons
  icon_var <- case_when(latest_pt == 'special_cause_improvement' & 
                          imp == 'increase' ~ 'SIH',  # spec imp high
                        latest_pt == 'special_cause_concern' & 
                          imp == 'increase' ~ 'SCL',  # spec con low
                        latest_pt == 'special_cause_improvement' & 
                          imp == 'decrease' ~ 'SIL',  # spec imp low
                        latest_pt == 'special_cause_concern' & 
                          imp == 'decrease' ~ 'SCH',  # spec con high
                        .default = 'CCV') # common case
  
  # assurance icons
  icon_assu <- case_when(#targ == 9999 ~ 'BLANK', # no target
                         upl <= tg & imp == 'increase' ~ 'CF', # consist fail
                         lpl >= tg & imp == 'decrease' ~ 'CF', # consist fail
                         upl <= tg & imp == 'decrease' ~ 'CP', # consist pass
                         lpl >= tg & imp == 'increase' ~ 'CP', # consist pass
                         .default = 'CV') # common variation
  
  if (targ == 9999) {icon_assu <- 'BLANK'}
  
  # commentary
  narr_assur <-case_when (icon_assu == "CV" ~ "This process will not consistently achieve or fail the target.", 
                          icon_assu == "CF" ~ "This process is not capable, it will consistently fail without a redesign or change.", 
                          icon_assu == "CP" ~ "The process is capable and will consistently achieve the target.",
                          icon_assu == "BLANK" ~ "This process has no target",
                          .default = "Error - please check")  
  
  narr_varr = case_when (icon_var == "CCV" ~ "The measure is within common cause variation, with no significant change.", 
                         icon_var == "SCH" ~ "There is evidence of special cause variation of a concerning nature.", 
                         icon_var == "SCL" ~ "There is evidence of special cause variation of a concerning nature.",
                         icon_var == "SIH" ~ "There is evidence of special cause variation of a improving nature.", 
                         icon_var == "SIL" ~ "There is evidence of special cause variation of a improving nature.",
                         icon_var == "BLANK" ~ " ",
                         .default =  "Error - please check")
  
  # return the icon or narrative
  res <- case_when (assu_or_var == 'assurance' ~ icon_assu, 
                    assu_or_var == 'variation' ~ icon_var,
                    .default = glue('<span style="font-size:0.7em;">{narr_assur} <br/> {narr_varr}</span>'))
  
  res
}

#spc_icons(dat, 'RQM', 'assurance')
#spc_icons(dat, 'RQM','variation')

# thats the functions set up, now want to run functions across each of the metrics

# make a list of the metrics (as char as they were factors)
metrics_list <- as.character(unique(dat$metric))

# make spc spark plots for each of the metrics
spark_plots <- map(.x = metrics_list, 
                          .f = ~plot_spc_spark(df = dat, 
                                               met = .x))

# calculate assurance icon for each metric
spc_ic_ass  <- map(.x = metrics_list, 
                   .f = ~spc_icons(df = dat, 
                                   met = .x,
                                   assu_or_var = 'assurance'))

# calculate variation icon for each metric
spc_ic_var  <- map(.x = metrics_list, 
                   .f = ~spc_icons(df = dat, 
                                   met = .x,
                                   assu_or_var = 'variation'))

# calculate commentary for each metric
spc_comm <- map(.x = metrics_list, 
                .f = ~spc_icons(df = dat, 
                                met = .x,
                                assu_or_var = 'commentary'))

# create a dataframe of just the latest result
dat_f <- dat |>
  filter (period == max(period)) |>
  mutate(period = format(period, '%b %Y'),
         plots = spark_plots,
         ic_assu = spc_ic_ass,
         ic_var = spc_ic_var,
         commentary = spc_comm,
         value = if_else(perc == TRUE, paste0(round(value,1), '%'), paste(value))) |>
  select(period, 
         metric, 
         value, 
         plots, 
         ic_assu, 
         ic_var, 
         commentary) 

tb <- dat_f |>
  gt()

tb <- tb %>%
  text_transform(
    locations = cells_body(columns = plots),
    fn = function(x) {
      purrr::map(
        dat_f$plots, ggplot_image,
        height = px(80), aspect_ratio = 2.5
      )
    }
  ) |>
   text_transform(
     locations = cells_body(c(ic_assu, ic_var)),
     fn = function(x) {
       # loop over the elements of the column
       map_chr(x, ~ local_image(
         filename = paste0(.x, ".png"),
         height = 40
       ))
     }
   ) |>
  cols_align(
    align = "left",
    columns = commentary) |>
  fmt_markdown(
    columns = commentary) |>
  cols_label(
    period = "Period",
    metric = "Metric Name",
    value = "Value",
    plots = "Sparky mc line!!!!",
    ic_assu = "Assurance",
    ic_var = "Variation",
    commentary = "Commentary"
  ) 

tb
