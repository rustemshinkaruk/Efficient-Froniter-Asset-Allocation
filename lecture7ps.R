#ps6
library(magrittr)
library(xts)
library(data.table)
library(xlsx)

# Load Data
# https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
msft <- read.xlsx("./lecture6p.xlsx", sheetName = "MSFT") %>% as.data.table
gc()
intc <- read.xlsx("./lecture6p.xlsx", sheetName = "INTC") %>% as.data.table
gc()
luv	 <- read.xlsx("./lecture6p.xlsx", sheetName = "LUV")  %>% as.data.table
gc()
mcd	 <- read.xlsx("./lecture6p.xlsx", sheetName = "MCD")  %>% as.data.table
gc()
jnj	 <- read.xlsx("./lecture6p.xlsx", sheetName = "JNJ")  %>% as.data.table
gc()
rf <- read.xlsx("./lecture6p.xlsx", sheetName = "F-F_Research_Data_Factors_daily", startRow = 5) %>% as.data.table
names(rf)[1] <- "Date"
rf[, names(rf)[2:4] := list(NULL, NULL, NULL)]
# convert to weekly risk-free rate
rf[, RF := ((1 + RF / 100) ^ 5 - 1)]
rf[, Date := as.Date(as.character(Date), format = "%Y%m%d")]

msft <-msft[,.(Date, Adj.Close)]
intc<- intc[,.(Date, Adj.Close)]
luv	<- luv[,.(Date, Adj.Close)]
mcd	<- mcd[,.(Date, Adj.Close)]
jnj	<- jnj[,.(Date, Adj.Close)]

colnames(msft)[2] <- "msft" 
colnames(intc)[2]<- "intc"
colnames(luv)[2]<- "luv"	
colnames(mcd)[2]<- "mcd"	
colnames(jnj)[2]<- "jnj"	

setkey(msft, Date)
setkey(intc, Date)
setkey(luv, Date)
setkey(mcd, Date)
setkey(jnj, Date)
setkey(rf, Date)

# merge data
d <-rf[msft[intc[luv[mcd[jnj]]]]]

# weekly data
ep <- endpoints(d[["Date"]], on = "weeks")
d <- d[ep,]
# calculate reutrns
d[, msft := msft / shift(msft) - 1]
d[, intc := intc / shift(intc) - 1]
d[, luv := luv / shift(luv) - 1]
d[, mcd := mcd / shift(mcd) - 1]
d[, jnj := jnj / shift(jnj) - 1]
d[, RF := shift(RF)]
d <- d[-1,]

# estimate means and variance/covariance matrix
z <- d[, colMeans(.SD), .SDcols = names(d)[-c(1,2)]]
S <- d[, var(.SD), .SDcols = names(d)[-c(1, 2)]]

# for the riskless asset one choice is the average rate over the sample period
r <- mean(d[["RF"]])

# 1
# Construct the mean-variance frontier for the Intel-Microsoft combination.
# Indicate the minimum-variance portfolio and the efficient frontier.
stocks <- c("msft", "intc")
n <- length(stocks)
mu <- matrix(z[stocks], nrow = n)
sig <- S[stocks, stocks]
ones <- matrix(rep(1,n), nrow = n)
sigi <- solve(sig)
A <- (t(ones) %*% sigi %*% ones) %>% as.vector
B <- (t(ones) %*% sigi %*% mu) %>% as.vector
C <- (t(mu) %*% sigi %*% mu) %>% as.vector
D <- A * C - B ^ 2
# minimum variance set
er <- seq(from=0.002,to=.007, by = 0.00001)
stdev <- sqrt((A * er ^ 2 - 2 * B * er + C) / D)
# minimum variance portfolio
wg <- sigi %*% ones / A
erg <- t(wg) %*% mu %>% as.numeric
stdevg <- sqrt(t(wg) %*% sig %*% wg) %>% as.numeric
# tangency portfolio (for q3)
wt1 <- sigi %*% (mu - r*ones) / (B - A * r)
ert1 <- t(wt1) %*% mu %>% as.numeric
stdevt1 <- sqrt(t(wt1) %*% sig %*% wt1) %>% as.numeric
# plot
#
plot(stdev, er, type = 'n', xlim = c(0.02, 0.07), ylim = c(0.002, 0.0055), ylab = "Weekly Expected Return", xlab = "Weekly Standard Deviation")
effidx <- er >= erg
lines(stdev[effidx], er[effidx])
lines(stdev[!effidx], er[!effidx], lty = 2)
points(sqrt(diag(sig)), mu, pch = 21, bg = rainbow(n))
points(stdevg, erg, pch = 21, bg = "grey")
legend("bottomright", inset = .05,
       c("Intel", "Microsoft", "Minimum Variance Portfolio", "Efficient Frontier"),
       pt.bg = c(rainbow(n), "grey", "NA"), lwd = c(NA, NA, NA, 1), lty = c(NA, NA, NA, 1),
       pch = c(21, 21, 21, NA), bg = "white", box.col = "white", cex = .8)
#
#
effidx1 <- effidx
er1 <- er
stdev1 <- stdev

# 2
stocks <- names(z)
n <- length(stocks)
mu <- matrix(z[stocks], nrow = n)
sig <- S[stocks, stocks]
ones <- matrix(rep(1, n), nrow = n)
sigi <- solve(sig)
A <- (t(ones) %*% sigi %*% ones) %>% as.vector
B <- (t(ones) %*% sigi %*% mu) %>% as.vector
C <- (t(mu) %*% sigi %*% mu) %>% as.vector
D <- A * C - B ^ 2
# minimum variance set
er <- seq(from = 0.00005, to = .007, by = 0.00001)
stdev <- sqrt((A * er ^ 2 - 2 * B * er + C) / D)
# minimum variance portfolio
wg <- sigi %*% ones / A
erg <- t(wg) %*% mu %>% as.numeric
stdevg <- sqrt(t(wg) %*% sig %*% wg) %>% as.numeric
# tangency portfolio (for q3)
wt2 <- sigi %*% (mu - r * ones) / (B - A * r)
ert2 <- t(wt2) %*% mu %>% as.numeric
stdevt2 <- sqrt(t(wt2) %*% sig %*% wt2) %>% as.numeric
# plot
#
plot(stdev, er, type = 'n', xlim = c(0.02, 0.07), ylim = c(0.002, 0.0055), ylab = "Weekly Expected Return", xlab = "Weekly Standard Deviation")
lines(stdev1[effidx1], er1[effidx1])
lines(stdev1[!effidx1], er1[!effidx1], lty = 2)
effidx <- er >= erg
lines(stdev[effidx], er[effidx])
lines(stdev[!effidx], er[!effidx], lty = 2)
points(sqrt(diag(sig)), mu, pch = 21, bg = rainbow(n))
points(stdevg, erg, pch = 21, bg = "grey")
legend("bottomright", inset = .01,
       c(stocks, "Minimum Variance Portfolio", "Efficient Frontier"),
       pt.bg = c(rainbow(n), "grey", "NA"), lwd = c(rep(NA, n+1), 1), lty = c(rep(NA, n+1), 1),
       pch = c(rep(21, n+1), NA), bg = "white", box.col = "white", cex = .8)
#
# 3
# plot
#
plot(stdev, er, type = 'n', xlim = c(0, 0.07), ylim = c(0.0, 0.0055), ylab = "Weekly Expected Return", xlab = "Weekly Standard Deviation")
lines(stdev1[effidx1], er1[effidx1])
lines(stdev1[!effidx1], er1[!effidx1], lty = 2)
effidx <- er >= erg
lines(stdev[effidx], er[effidx])
lines(stdev[!effidx], er[!effidx], lty = 2)
#points(sqrt(diag(sig)), mu, pch = 21, bg = rainbow(n))
abline(a = r, b = (ert1 - r) / stdevt1)
abline(a = r, b = (ert2 - r) / stdevt2)
points(stdevt1, ert1, pch = 21, bg = "blue")
points(stdevt2, ert2, pch = 21, bg = "gold")
points(0, r, pch = 21, bg = "grey")
legend("bottomright", inset = .01,
       c("Tangency Portfolio - MSFT/INTC", "Tangency Portfolio - Full Set", "Risk Free Asset"),
       pt.bg = c("blue", "gold", "grey"),
       pch = c(rep(21, 3)), bg = "white", box.col = "white", cex = .8)
#

# 4
riskA <- 5
w_star = (ert2 - r) / (riskA * stdevt2^2)
U <- w_star * (ert2 - r) + r - riskA / 2 * w_star ^ 2 * stdevt2 ^ 2
stdevrU <- seq(0, 0.08, 0.001)
erU <- U + riskA / 2 * stdevrU ^ 2
# plot
#
plot(stdev, er, type = 'n', xlim = c(0, 0.07), ylim = c(0.0, 0.0055), ylab = "Weekly Expected Return", xlab = "Weekly Standard Deviation")
lines(stdev1[effidx1], er1[effidx1])
lines(stdev1[!effidx1], er1[!effidx1], lty = 2)
effidx <- er >= erg
lines(stdev[effidx], er[effidx])
lines(stdev[!effidx], er[!effidx], lty = 2)
#points(sqrt(diag(sig)), mu, pch = 21, bg = rainbow(n))
abline(a = r, b = (ert1 - r) / stdevt1)
abline(a = r, b = (ert2 - r) / stdevt2)
lines(stdevrU, erU, col = "grey")
points(stdevt1, ert1, pch = 21, bg = "blue")
points(stdevt2, ert2, pch = 21, bg = "gold")
points(0, r, pch = 21, bg = "grey")
legend("bottomright", inset = .01,
       c("Tangency Portfolio - MSFT/INTC", "Tangency Portfolio - Full Set", "Risk Free Asset", "Indifference Curve"),
       pt.bg = c("blue", "gold", "grey", NA),
       col = c(NA, NA, NA, "grey"),
       lty = c(NA, NA, NA, 1),
       pch = c(rep(21, 3), NA), bg = "white", box.col = "white", cex = .8)
