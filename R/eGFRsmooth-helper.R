# Helper function - calculate average slope
avgSlope = function(y, w) {
    avg = 0
    ## Calculate avg slope in the window, each interval is 10 days
    for (i in 2:(w+1)) {
        avg = avg + (y[i] - y[i-1]) / 10
    }
    return(avg/w * 365)
}

# Helper function - trim values for regions with dense data points
trimValues <- function(x, y, thres.acute) {
    z1 = x[-1] - x[-length(x)]
    z2 = z1[-1]
    z1 = z1[-length(z1)]
    rid = which(z1 < thres.acute & z2 < thres.acute)
    if (length(rid) != 0) {
        nx = x[-(rid+1)]
        ny = y[-(rid+1)]
        rx = x[(rid+1)]
        ry = y[(rid+1)]
        rx = rx - x[1]
        nx = nx - x[1]
    } else {
        nx = x - x[1]
        ny = y
        rx = NULL
        ry = NULL
    }
    return(list(nx, ny, rx, ry))
}

# Plotting function - smooth single trajectory
#' @importFrom graphics axis points abline par title 
#' @importFrom stats dnorm
plotSmooth = function(x, y, rx, ry, eGFR.range,
                      k.width, w.width, min.width, max.range, name = NULL) {
    ## Initialize variables
    min = NA
    perc.drop = NA
    s.egfr = NA
    e.egfr = NA
    s.time = NA
    e.time = NA
    numpts = NA

    ## Read input, plot the points
    id = which(!is.na(x) & !is.na(y))
    x = x[id]  ## Time point of non-missing eGFR measurements
    y = y[id]  ## Corresponding eGFR values
    rg.x = range(x)
    plot(0, type="b", pch= 19, xlab="Time", ylab="eGFR",
         xlim=rg.x, ylim= eGFR.range, yaxt = "n",
         cex=.5, lty = 3, col = "white")
    axis(2, at = seq(0, round(eGFR.range[2],-1), by = 20),
         labels = as.character(seq(0, round(eGFR.range[2], -1), by = 20)),
         cex.axis = 0.7, las = 1)
    points(rx, ry, col = "gray70", pch = 19, cex = .5)
    points(x, y, pch = 19, cex = 0.5, lty = 3, type = "b")
    
    ## Set the time points - split one year into approx 36 intervals
    xx = seq(rg.x[1], rg.x[2], by = 10)
    yest = rep(NA, length(xx))
    n = length(xx)
    
    ## If the range is greater than a min.width, smooth it
    if (length(x) >= 4 & diff(range(x)) > min.width) {
        for(j in 1:n) {
            ## Gets distribution of weights
            ## Smoothing window size as st.dev
            ww = dnorm(x, xx[j], k.width)  
            ## Weighted eGFR value
            yest[j] = sum(ww * y) / sum(ww)
        }
        
        ## For large intervals in time, draw a straight line
        for(k in 1:(length(x)-1)) {
            tmp.d = x[k+1] - x[k]
            if(tmp.d > 365) {
                wid = which(xx >= x[k] & xx <= x[k+1])
                y1 = yest[xx == min(xx[wid])]
                y2 = yest[xx == max(xx[wid])]
                
                dst1 = abs(xx - x[k])
                dst2 = abs(xx - x[k+1])
                
                new.yest = y1 * (dst2 / (dst1 + dst2)) + y2 * (dst1 / (dst1 + dst2))
                yest[wid] = new.yest[wid]
                
            }
        }
        points(xx, yest, type = "l")
        
        ## Sliding window - variable sliding window - perform only if range is greater than window width
        if (diff(range(x)) > w.width) {
            ## Round window to the nearest 10
            w.width = round(w.width, -1)
            w = w.width / 10
            min = 1000 ## Arbitrary large number
            start = 1 ## In the case that the first window is the window of interest
            for (i in 1:(length(yest) - w)) {
                ## We break the loop once the end point of the range we're 
                ## calculating is greater than max range
                if (!is.na(max.range)) {
                    if (xx[i+w] > max.range) {
                        break
                    }
                }
                tmp = avgSlope(yest[i:(i+w)], w)
                if (tmp < min) {
                    min = tmp
                    start = i
                }
            }
            ## Highlight in red the range with the steepest avg slope
            points(xx[start:(start+w)], yest[start:(start+w)], type = "l", col = 2, lwd = 1.5)
            if (!is.na(max.range)) {
                abline(v = max.range, lwd = 1.5, col = 4, lty = 3)
            }
            
            ## Record the starting eGFR and ending eGFR of the window used
            s.egfr = yest[start]
            e.egfr = yest[start+w]
            s.time = xx[start]
            e.time = xx[start+w]
            numpts = sum(x >= xx[start] & x <= xx[start+w])
            ## Calculate the % drop
            perc.drop = (e.egfr - s.egfr) / s.egfr * 100
            ## Add info to plot
            par(cex.main = 0.8)
            title(main = paste(name, "\n", "Steepest Avg Slope = ", round(min,1), 
                               "\n", "Percentage Change = ", round(perc.drop,1), "%"))
        }
        ## If the range is smaller than window width but greater than a minimum 
        ## width (say, 1 year), calculate using the whole range of the data
        else if (diff(range(x)) > min.width) {
            min = avgSlope(yest, length(xx)-1)
            s.egfr = yest[1]
            e.egfr = yest[length(yest)]
            s.time = xx[1]
            e.time = xx[length(xx)]
            numpts = length(x)
            ## Calculate the % drop
            perc.drop = (e.egfr - s.egfr) / s.egfr * 100
            ## Add info to plot
            par(cex.main = 0.8)
            title(main = paste(name, "\n", "Avg Slope = ", round(min,1),
                               "\n", "Percentage Change = ", round(perc.drop,1), "%"))
        }
        ## If the range is smaller than window width and smaller than minimum width, don't calculate anything
        else {
            title(main = name)
        }
    }
    ## Less than 4 points OR range less than a year - not smoothed
    else {
        title(main = name)
    }
    
    ## Return the slope, percentage change, starting  and ending egfr in the window - NA if not calculated
    ## And the baseline eGFR readings, number of observations, and the time period of the follow up
    list(Slope = min, Percentage.Drop = perc.drop, eGFR.Baseline = y[1],
         eGFR.Window.Start = s.egfr, eGFR.Window.End = e.egfr,
         Num.Observations = length(x), Num.Pts = numpts,
         Time.Range.Followup = diff(range(x)), 
         Time.Window.Start = s.time, Time.Window.End = e.time)
}
