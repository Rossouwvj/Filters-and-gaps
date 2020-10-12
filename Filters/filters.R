# Load required packages 
# ================================================================================================================

if (!require("pacman")) install.packages("pacman")
p_load(devtools,forecast,ggplot2,dplyr,zoo)


library(databotr)

#=================================================================================================================
# Load in the gdp data from sparta data
#=================================================================================================================

sparta_series <- spartaload("SARB.ERD","NOWCAST","1.0",nameby="CODE")
gdp <- sparta_series$GDPMP6$observations


#=================================================================================================================
# Load in code for various filters
#=================================================================================================================

devtools::install_github("KevinKotze/tsm")
install.packages("mFilter", repos = "https://cran.rstudio.com/", 
                 dependencies = TRUE)
library(tsm)
library(mFilter)


#=================================================================================================================
# START the filter stuff
#=================================================================================================================

gdp <- ts(gdp$OBS_VALUE, start = c(1960, 1), frequency = 4)
plot(gdp)

# Linear time trend------------------------------
lin.mod <- lm(gdp ~ time(gdp))
lin.trend <- lin.mod$fitted.values  # fitted values pertain to time trend
linear <- ts(lin.trend, start = c(1960, 1), frequency = 4)  # create a time series variable for trend
lin.cycle <- gdp - linear  # cycle is the difference between the data and linear trend


par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins
plot.ts(gdp, ylab = "")  # first plot time series
lines(linear, col = "red")  # include lines over the plot
legend("topleft", legend = c("data", "trend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(lin.cycle, ylab = "")  # second plot for cycle
legend("topright", legend = c("cycle    "), lty = 1, col = c("black"), 
       bty = "n")



# HP filter------------------------------------------
hp.decom <- hpfilter(log(gdp)*100, freq = 1600, type = "lambda")

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(log(gdp), ylab = "")  # plot time series
lines(hp.decom$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decom$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")


# BK filter---------------------------------------

bp.decom <- bkfilter(gdp, pl = 6, pu = 32)

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")
lines(bp.decom$trend, col = "red")
legend("topleft", legend = c("data", "BPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bp.decom$cycle, ylab = "")
legend("topleft", legend = c("BPcycle"), lty = 1, col = c("black"), 
       bty = "n")


# Christiano-Fitzgerald-------------------------


cf.decom <- cffilter(gdp, pl = 6, pu = 32, root = TRUE)

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")
lines(cf.decom$trend, col = "red")
legend("topleft", legend = c("data", "CFtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(cf.decom$cycle, ylab = "")
legend("topleft", legend = c("CFcycle"), lty = 1, col = c("black"), 
       bty = "n")

#  Beveridge-Nelson decomposition-------------------

bn.decomp <- bnd(gdp, nlag = 8)  # apply the BN decomposition that creates dataframe

bn.trend <- ts(bn.decomp[, 1], start = c(1960, 1), frequency = 4)  # first column contains trend
bn.cycle <- ts(bn.decomp[, 2], start = c(1960, 1), frequency = 4)  # second column contains cycle

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(bn.trend, ylab = "",col = "red")
lines(gdp, col = "black")
legend("topleft", legend = c("BNtrend","data"), lty = 1, 
       col = c("red","black"), bty = "n")
plot.ts(bn.cycle, ylab = "")
legend("topleft", legend = c("BNcycle"), lty = 1, col = c("black"), 
       bty = "n")


# Combination of all ---------------------------------

comb <- ts.union(lin.cycle, hp.decom$cycle, bp.decom$cycle, 
                 cf.decom$cycle, bn.cycle)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot.ts(comb, ylab = "", plot.type = "single", col = c("blue", 
                                                       "red", "darkgrey", "sienna", "darkgreen"))
legend("topleft", legend = c("linear", "hp-filter", "bp-filter", 
                             "cf-filter", "bn-decomp"), lty = 1, col = c("blue", 
                                                                         "red", "darkgrey", "sienna", "darkgreen"), bty = "n")

#=================================================================================================================
# Compare filter results to sarb output 
#=================================================================================================================


library(readxl)
output_gap_official <- read_excel("output gap official.xlsx", col_types = c("text", "numeric"))

gap_official <- ts(output_gap_official$`Output gap`, start = c(1990, 1), frequency = 4)

plot(gap_official)

# Linear

plot(window(log(lin.cycle), start = c(1990,1)),ylab ="")
lines(gap_official, col = "red")

# HP 
plot.ts(hp.decom$cycle, ylab = "")



