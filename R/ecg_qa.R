#' A convenience function for plotting ECG time series and QRS detection
#'
#' @param ecg_df a data.frame containing ECG data. Must have at least the columns 'time' and 'ecg',
#'    which are used for x and y axes, respectively.
#' @param beat_df an optional data.frame containing beat events to plot against the trace
#' @param freq the frequency of the data in Hz
#' @param max_rows the number of rows on each page of the PDF
#' @param spike_annotations which QRS algorithm columns to plot as points on the stripplot from beat_df
#' @param ibi_splines which QRS algorithm RR columns to plot as splines against the ECG data
#' @param spi How many seconds of data per inch to plot on the page. Default is 2, which allows one to
#'   resolve each beat by eye. Higher values (e.g., 10) may be useful for a summary view and to look for artifacts.
#' @param width the width of the PDF output in inches. Default: 12
#' @param height the height of the PDF output in inches. Default: 10
#' @param out_pdf the filename (and optionally, path) of the PDF to be created
#'
#' @importFrom checkmate assert_data_frame
#' @importFrom dplyr filter mutate select
#' @importFrom ggplot2 ggplot stat_smooth geom_line geom_point facet_wrap
ecg_stripplot <- function(ecg_df, beat_df=NULL, freq=1000, max_rows=5,
                          spike_annotations=c("gqrs"),
                          ibi_splines=c("gqrs"),
                          spi=2, #seconds per inch
                          width=12, height=10, out_pdf="ecg_stripplot.pdf") {

  checkmate::assert_data_frame(ecg_df)
  checkmate::assert_data_frame(beat_df, null.ok = TRUE)
  checkmate::assert_subset(spike_annotations, c("gqrs", "sqrs", "wqrs", "ecgpuwave"))
  checkmate::assert_subset(ibi_splines, c("gqrs", "sqrs", "wqrs", "ecgpuwave"))
  checkmate::assert_data_frame(ecg_df)

  ecg_df[, "time" := time/60] #convert to minutes for ease of display
  beat_df[, "time" := time/60]

  #can realistically fit about 20 seconds of data in a 10 inch space (2 seconds/inch)
  per_strip <- freq*spi*width
  nplots <- ceiling(nrow(ecg_df)/max_rows/per_strip)

  mlo <- min(ecg_df$time); mhi <- max(ecg_df$time)
  delta_page <- (mhi - mlo)/nplots; delta_panel <- (mhi - mlo)/(nplots*max_rows)
  page_cuts <- seq(from=mlo, to=mhi, by=delta_page)
  panel_cuts <- seq(from=mlo, to=mhi, by=delta_panel)

  ecg_df[,"page" := cut(time, page_cuts, include.lowest = TRUE)]
  ecg_df[,"panel" := cut(time, panel_cuts, include.lowest = TRUE)]

  if (!is.null(spike_annotations)) {
    spike_df <- beat_df %>% filter(annotator %in% spike_annotations) %>%
      mutate(page=cut(time, breaks=page_cuts, include.lowest=TRUE),
             panel=cut(time, breaks=panel_cuts, include.lowest=TRUE)) %>%
        left_join(ecg_df %>% dplyr::select(time, ecg), by="time") #pull the ECG trace back in at the annotation points for display
  }

  if (!is.null(ibi_splines)) {
    ibi_df <-  beat_df %>% filter(annotator %in% ibi_splines) %>%
      mutate(page=cut(time, breaks=page_cuts, include.lowest=TRUE),
             panel=cut(time, breaks=panel_cuts, include.lowest=TRUE)) %>%
      left_join(ecg_df %>% dplyr::select(time, ecg), by="time") %>% #pull the ECG trace back in at the annotation points for display
      mutate(RR=scales::rescale(RR, to=quantile(ecg_df$ecg, c(.1, 1.0))))
  }

  # beat_df <- ecg_df %>% filter(gqrs==1 | gqrs_shift==1) %>%
  #   select(time, ecg, gqrs, gqrs_shift, page, panel) %>%
  #   mutate(detector=case_when(gqrs==1 ~ "gqrs", gqrs_shift==1 ~ "gqrs_shift", TRUE ~ "")) %>%
  #   select(page, panel, time, ecg, detector)

  # ibi_df <- ecg_df %>% mutate(RR=scales::rescale(RR, to=quantile(ecg, c(.1, 1.0)))) %>%
  #   filter(gqrs==1) %>% select(page, panel, time, ecg, RR)

  pdf(out_pdf, width=width, height=height)
  for (pp in levels(ecg_df$page)) {
    sub_ecg <- ecg_df %>% filter(page==pp)
    sub_spike <- spike_df %>% filter(page==pp)
    sub_ibi <- ibi_df %>% filter(page==pp)
    g <- ggplot(sub_ecg, aes(x=time, y=ecg)) + theme_bw(base_size=13) +
      geom_line() +
      geom_point(data=sub_spike, mapping=aes(y=ecg, color=annotator)) +
      stat_smooth(data=sub_ibi, mapping=aes(y=RR, color=NULL), color="blue", method=lm, formula=y ~ splines::ns(x, 8)) +
      facet_wrap(~panel, ncol=1, scales="free_x") + xlab("Time (min)")
    plot(g)
  }
  dev.off()

}


#' @title Conduct basic QA checks on IBI time series of ecg signal.
#' @description After converting importing acq file into the experiment.pipeline structure, this function converts the ECG signal to an IBI ts and conducts basic QA checks on the IBI ts.
#' @details
#'   This function uses the detect_rpeaks function from rsleep to create an interbeat interval time series of the ECG signal. Future versions of this function will likely additionally integrate wfdb as a QRS detection method.
#'   This function then converts the IBI ts into a HRV data set using RHRV. By converting the IBI ts into RHRV, we then interpolate the IBI ts onto a time grid.
#'   Finally, this funciton employs the tidyverse package to wrangle that data and provide basic QA and automatizes outlier checks.
#'   By default, outliers are defined as 2IQRs above the 75th percentile of IBI differences or below the 25th percentile. Alternative outlier definitons can be provided as an argument into the function.
#'   Part of these outlier checks include provide plots of the ecg signal where it is believed there is to be an outleir.
#'   For the purpose verifying these QA checks (i.e., to ascertain if there's a problem with the QRS detection algorithm), this function also returnes the IBI time series plotted over the ecg signal as both as a single plot (for the entire time series) and as a list of plots binned by time_bin increments (default is 100s). N.B. For long time series, this total plot will be less useful and it is recommend to interrogate the plot list obejct instead.
#' @param ecg_ts a vector containing the ECG signal. In experiment.pipeline objects, this vector can be found under <acq_data>$raw$<ecg signal name>
#' @param time_ts a vector containing the time corresponding to the ECG signal. In experiment.pipeline objects, this vector can be found under <acq_data>$raw$<time series name>
#' @param Hz the sampling rate (in Hz) of the raw ECG signal. Defaults to 1000 Hz
#' @param outlier_arg the argument for how outliers are defined. Defaults to 2*IQR above and below the 25th and 75th percentiles of ibi differences.
#' @param time_bin for the list of plots, how many seconds of ecg signal should be presented per plot. Default is 100 seconds
#' @importFrom rsleep detect_rpeaks
#' @importFrom stats IQR
#' @importFrom dplyr filter mutate if_else case_when na_if summarise
#' @importFrom ggplot2 ggplot
#' @importFrom RHRV CreateHRVData SetVerbose LoadBeatVector BuildNIHR InterpolateNIHR
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom magrittr `%>%`
#' @author Alison Schreiber
#' @export
ibi_qa <- function(ecg_ts, time_ts, Hz = 1000, outlier_arg = "ibi_diff >  upper_quartile_ibi_diff + 2*iqr_ibi_diff | ibi_diff < lower_quartile_ibi_diff - 2*iqr_ibi_diff", time_bin = 100) {

  ecg_df <- data.frame(ecg = ecg_ts, time = time_ts)
  ibis <- rsleep::detect_rpeaks(ecg_ts, sRate = Hz)
  hrv.data  = CreateHRVData()
  hrv.data = SetVerbose(hrv.data, TRUE )
  #hrv.data = LoadBeatWFDB(hrv.data, RecordName = "ecg_027", RecordPath = "~/ics/ecg_qa/")
  hrv.data = LoadBeatVector(hrv.data, ibis, scale = 1)
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)
  ibi_plot <- ggplot(hrv.data$Beat, aes(x = Time, y = RR)) + geom_line()
  ibi_dist <- ggplot(hrv.data$Beat, aes(x = RR)) + geom_density()
  ibi_df <- hrv.data$Beat %>% mutate(ibi_diff = RR - lag(RR))
  ibi_diff_dist <- ggplot(ibi_df, aes(x = ibi_diff)) + geom_density()
  ibi_summary_df <- ibi_df %>%
    summarise(mean_ibi = mean(RR, na.rm = TRUE),
              sd_ibi = sd(RR, na.rm = TRUE),
              min_ibi = min(RR, na.rm = TRUE),
              max_ibi = max(RR,na.rm = TRUE ),
              mean_ibi_diff = mean(abs(ibi_diff), na.rm = TRUE),
              iqr_ibi_diff = IQR(ibi_diff, na.rm = TRUE),
              upper_quartile_ibi_diff = quantile(ibi_diff, .75, na.rm = TRUE),
              lower_quartile_ibi_diff = quantile(ibi_diff, .25, na.rm = TRUE),
              sd_ibi_diff = sd(abs(ibi_diff), na.rm = TRUE),
              min_ibi_diff = min(abs(ibi_diff), na.rm = TRUE),
              max_ibi_diff = max(abs(ibi_diff),na.rm = TRUE ))
  upper_quartile_ibi_diff <- ibi_summary_df$upper_quartile_ibi_diff
  lower_quartile_ibi_diff <- ibi_summary_df$lower_quartile_ibi_diff
  iqr_ibi_diff <- ibi_summary_df$iqr_ibi_diff
  ibi_outliers_df <- dplyr::filter_(ibi_df, outlier_arg)
  ibi_outliers_plot_list <- list()

  if(nrow(ibi_outliers_df) > 0) {
  for(i in 1:nrow(ibi_outliers_df)) {
    tmp_df <- ibi_outliers_df[i, ]
    t_outlier = ibi_outliers_df[[i, "Time"]]
    if (ibi_outliers_df[[i, "Time"]] - 2 < min(ibi_outliers_df$Time)) {
      tstart = min(ibi_outliers_df$Time)
    } else {
      tstart = ibi_outliers_df[[i, "Time"]] - 2
    }
    if (ibi_outliers_df[[i, "Time"]] + 2 > max(ibi_outliers_df$Time)) {
      tend = max(ibi_outliers_df$Time)
    } else {
      tend = ibi_outliers_df[[i, "Time"]] + 2
    }
    subdf <- ecg_df %>% dplyr::filter(time > tstart, time < tend) %>% mutate(prepost = if_else(time > t_outlier, "post", "pre"))
    g <- ggplot(subdf, aes(x = time, y = ecg, color = prepost)) + geom_line(aes(color = prepost))
    ibi_outliers_plot_list[[paste0("T", tmp_df$Time, "_rr", tmp_df$RR, "_ibidiff", tmp_df$ibi_diff)]] <- g
  }
  }

  ecg_df <- mutate(ecg_df, ibi_time = if_else(time %in% ibi_df$Time, 1, 0))%>% dplyr::mutate(ibi_time = na_if(ibi_time, 0))
  if (length(ecg_df$ecg) < (time_bin*Hz)) {
    ecg_ibi_overplotted <- ggplot(ecg_df, aes(x = time, y = ecg, color = ibi_time)) + geom_line(aes(x = time, y = ecg)) + geom_point(aes(x = time, y = ibi_time, color = ibi_time))
    plot_list <- list()
    plot_list[["time_bin_1"]] <- ecg_ibi_overplotted
  } else {
    ecg_df <- ecg_df %>% dplyr::mutate(time_bin = c(sort(rep(seq(1, floor(max(ecg_df$time)/time_bin)), time_bin*Hz)), rep(floor(max(ecg_df$time)/time_bin) + 1, length(ecg_df$time) - length(rep(seq(1,floor(max(ecg_df$time)/time_bin)),time_bin*Hz)))))
    ecg_ibi_overplotted <- ggplot(ecg_df, aes(x = time, y = ecg, color = ibi_time)) + geom_line(aes(x = time, y = ecg)) + geom_point(aes(x = time, y = ibi_time, color = ibi_time))
    np = length(unique(ecg_df$time_bin))
    plot_list <- list()

    for(i in 1:np) {
      tmp_plot <- ggplot(dplyr::filter(ecg_df,time_bin == i), aes(x = time, y = ecg, color = ibi_time)) + geom_line(aes(x = time, y = ecg)) + geom_point(aes(x = time, y = ibi_time, color = ibi_time)) + ggtitle(paste0("time bin: ", i))
      plot_list[[paste0("time_bin_", i)]] <- tmp_plot
    }

  }


  return(list(ibi_summary = ibi_summary_df, ibi_ts = ibis, hrv_obj = hrv.data, ibi_plot = ibi_plot, ibi_dist =ibi_dist, ibi_diff_dist = ibi_diff_dist, ibi_outliers_df = ibi_outliers_df, ibi_outliers_plot_list = ibi_outliers_plot_list, ecg_overplotted = ecg_ibi_overplotted, plot_list = plot_list ))
}
