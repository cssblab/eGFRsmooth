#' Smooths eGFR trajectories and outputs plots for each subject, and a table
#' summarizing slope and percentage change for each trajectory
#' 
#' @param data A data.frame. Input data with subject ID, numeric date and eGFR  
#' @param k.width A numeric value. Smoothing bandwidth parameter
#' @param w.width A numeric value. Width of window (in years). Defaults to 
#'   2 years
#' @param min.width A numeric value. Minimal width (in years) for a trajectory 
#'   to be smoothed. Defaults to 1 year
#' @param thres.acute A numeric value. This argument defines the minimum width 
#'   a data point must be away from both of it's neighbours to be kept. 
#'   Otherwise, the data point is trimmed. This removes dense clusters of points
#'   usually characterized by large variance. Defaults to 30 days
#' @param max.range A numeric value. The maximum range (in years) from the
#'   baseline date to be analyzed. Defaults to NA (unrestricted range)
#' @param sort.param A character vector. Controls output plot name. One of 
#'   Slope, Percentage.Drop, eGFR.Window.Start, eGFR.Window.End, 
#'   eGFR.Baseline, Num.Observations, Time.Range.Followup, Num.Pts, Lead.Time.
#'   Defaults to Percentage.Drop
#' @param decreasing A boolean value. Controls sorting
#' @param output.dir A character value. Output directory for plots and 
#'   summary table. Defaults to NULL
#' 
#' @export
#' @return A data.frame. Summarizes the slope, percentage change and other 
#'  statistics for each trajectory. Outputs a plot (pdf) and table (txt) to 
#'  user defined working directory or the present working directory
#'  
eGFRsmooth <- function(data, k.width, w.width = 2, min.width = 1, 
                       thres.acute = 30, max.range = NA, output.dir = NULL,
                       sort.param = 'Percentage.Drop', decreasing = FALSE) {
  print('Generating unsorted eGFR statistics and plots...')
  result <- drawPlots(data = data,
                      k.width = k.width, w.width = w.width, min.width = min.width,
                      thres.acute = thres.acute, max.range = max.range,
                      output.dir = output.dir)
  print('Generating sorted plots...')
  drawPlotsSorted(data = data, result = result,
                  k.width = k.width, w.width = w.width, min.width = min.width,
                  thres.acute = thres.acute, max.range = max.range,
                  sort.param = sort.param, decreasing = decreasing,
                  output.dir = output.dir)
}

#' @importFrom dplyr as_tibble '%>%' filter arrange group_by summarise mutate nth
#' @importFrom tidyr unnest pivot_wider
#' @importFrom rlang .data
#' @importFrom grDevices dev.off pdf
#' @importFrom  utils write.table
drawPlots = function(data, k.width, w.width, min.width, 
                     thres.acute, max.range, 
                     table = TRUE, sorted = FALSE, output.dir) {
  if(is.null(output.dir)){
    output.dir <- getwd()
  }
  output.dir <- paste0(output.dir, '/output')
  if(!dir.exists(output.dir)){
    dir.create(output.dir, recursive = TRUE)
  }
  
  w.width <- 365 * w.width
  min.width <- 365 * min.width
  max.range <- 365 * max.range
  
  if (sorted) {
    pdf(paste0(output.dir, '/sorted.pdf'), height = 8, width = 7)
  } else {
    pdf(paste0(output.dir, '/unsorted.pdf'), height = 8, width = 7)
  }
  par(mfrow=c(3,3))
  
  result <- as_tibble(data) %>%
    dplyr::filter(!is.na(.data$eGFR)) %>%
    dplyr::arrange(.data$Subject, .data$Date) %>%
    dplyr::group_by(.data$Subject) %>%
    dplyr::summarise(args=trimValues(.data$Date, .data$eGFR, thres.acute), 
                     .groups='drop') %>%
    dplyr::group_by(.data$Subject) %>%
    dplyr::summarise(res=plotSmooth(x = unlist(nth(args, 1)),
                             y = unlist(nth(args, 2)),
                             rx = unlist(nth(args, 3)),
                             ry = unlist(nth(args, 4)),
                             eGFR.range = range(data$eGFR),
                             k.width = k.width,
                             w.width = w.width,
                             min.width = min.width,
                             max.range = max.range,
                             name = unique(.data$Subject)),
                    .groups='drop') %>%
    tidyr::unnest(.data, .data$res) %>%
    dplyr::mutate(ID=rep(c('Slope', 
                    'Percentage.Drop', 
                    'eGFR.Window.Start',
                    'eGFR.Window.End',
                    'eGFR.Baseline',
                    'Num.Observations',
                    'Time.Range.Followup',
                    'Num.Pts',
                    'Lead.Time'), length(unique(.data$Subject)))) %>%
    tidyr::pivot_wider(.data, names_from = .data$ID, values_from = .data$res)
  dev.off()
  if (table) {
    print('Generating results table...')
    write.table(result, paste0(output.dir, '/table.txt'), 
                quote = FALSE, sep = "\t", row.names = FALSE)
    return(result)
  }
}

#' @importFrom dplyr '%>%' filter arrange
#' @importFrom rlang .data
drawPlotsSorted = function(data, result, k.width, w.width, min.width, 
                           thres.acute, max.range, sort.param, 
                           decreasing, output.dir) {
  sort.id <- grep(sort.param, names(result), ignore.case = TRUE)
  if (length(sort.id) == 0) {
    stop('Invalid sorting parameter. See ?eGFRsmooth for valid arguments')
  }
  
  result <- result %>% 
    dplyr::filter(!is.na(.data$Slope)) %>%
    dplyr::arrange(.data, .data[[sort.id]])
    
  subject.order <- as.factor(result$Subject)
  if (decreasing) {
    subject.order <- rev(subject.order)
  }
  
  ## Sort data
  data[,1] = factor(data[,1], levels = subject.order)
  data = data[order(data[,1]),]

  ## Call drawPlots, sorted = T for pdf name
  drawPlots(data, k.width, w.width, min.width, thres.acute, max.range, 
            table = FALSE, sorted = TRUE, output.dir)
}
