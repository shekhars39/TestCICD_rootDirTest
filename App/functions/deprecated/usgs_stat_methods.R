# not used


# # see https://www.sciencebase.gov/catalog/item/5e1738e7e4b0ecf25c59f819
# # and chapter 10 of https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf
# # but not the same as the EPA method anyway!
# 
# # senth   written 10/05/2016 by Edward J. Gilroy.
# # senth(x,y, conf = 95)
# # Computes intercept,slope,confidence interval on the Theil slope
# # and pvalue for the associated Kendall's tau
# # y = response variable
# # x = explanatory variable
# # conf = two-sided confidence coeff for the CI on the Theil slope
# # changed to Black and White by Dennis Helsel, Dec 2016.
# # changed to provide padding to the axis min and max values by Robert Hirsch, Jan 2017
# # added custom x and y labels.  Dennis Helsel  Feb 2020
# 
# senth <- function(x, y, conf = 95, printTitle = TRUE, Xlab = NULL, Ylab = NULL) {
#   {
#     n <- length(x)
#     medx <- median(x)
#     medy <- median(y)
#   }
#   
#   test = 0
#   {
#     slope <- rep(c(NA), n * (n - 1) / 2)
#   }
#   { 
#     k <- 0
#     for (j in 1:(n - 1)) {
#       {
#         for(i in (j + 1):n) {
#           k <- k + 1
#           dx <- (x[c(i)] - x[c(j)])
#           dy <- (y[c(i)] - y[c(j)])
#           if(abs(dx) > 0.0) {
#             slope[c(k)]<-dy/dx
#           }
#         }
#       }
#     }
#   }
#   slope <- as.numeric(na.omit(slope))
#   M <- length(slope)
#   N <- n * (n - 1) / 2
#   SS <- sum(sign(slope))
#   xname <- deparse(substitute(x))
#   yname <- deparse(substitute(y))
#   if (is.null(Xlab)) Xlab = xname
#   if (is.null(Ylab)) Ylab = yname
#   cat("   ", "\n")
#   cat( "      Theil-Sen line", "\n")
#   corout1  <- cor.test(x, y, alternative = "two.sided", method = "kendall", 
#                        continuity = TRUE)
#   sortslope <- sort(slope)
#   S <- sum(sign(slope), na.rm = TRUE)
#   POS <- sum(slope > 0)
#   MINUS <- S - POS
#   Z <- corout1$statistic
#   varS <- ((abs(SS) - 1) / Z) ^ 2
#   if (Z == POS) {
#     varS <- n * (n - 1) * (2 * n + 5) / 18
#   }
#   alpha_2 <- (1.0 + conf / 100.0) / 2
#   zalpha_2 <- qnorm(c(alpha_2), mean = 0, sd = 1, lower.tail = TRUE)
#   # Get rank of CI endpoints
#   Calpha <- zalpha_2 * sqrt(varS)
#   M1 <- (M - Calpha) / 2
#   M2 <- (M + Calpha) / 2
#   if (M1 < 1) {
#     test <- 1
#   }
#   if (M2 + 1 > M) {
#     test <- 1
#   }
#   if (test == 0) {
#     UCL1 <- sortslope[M1]
#     UCL2 <- sortslope[M2 + 2]
#   }
#   tau <- S / (n * (n - 1) / 2)
#   {
#     medslop <- median(slope, na.rm = TRUE)
#     int <- medy - medslop * medx
#     cat("   ", "\n")
#     if (medslop < 0.0) { 
#       cat(yname, "=", int, medslop, "*", xname , "\n", "\n")
#     } else {
#       cat(yname, "=", int, "+", medslop, "*", xname, "\n", "\n")
#     }
#     
#     if (test == 0) {
#       cat("      ", conf, "% Confidence interval on the slope", "\n")
#       cat("LCL = ", round(UCL1, 3), " Theil slope = ", round(medslop,3), 
#           " UCL = ", round(UCL2, 3), "\n", "\n")
#     }
#     
#     if (test == 1) {
#       cat("Too few observations to compute the requested confidence interval on the slope",
#            "\n")
#     }
#     
#     print(corout1)
#     title <- if(printTitle) "Theil-Sen is solid. Regression is dashed" else ""
#     # need to make sure that the axes extend beyond the data
#     
#     xrange <- max(x) - min(x)
#     xpad <- xrange * 0.1
#     xlim <- c(min(x) - xpad, max(x) + xpad)
#     yrange <- max(y) - min(y)
#     ypad <- yrange * 0.1
#     ymin <- if (min(y) > 0) {
#       0 
#     } else { 
#       min(y) - ypad
#     }
#     ylim <- c(ymin, max(y) + ypad)
#     par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
#     plot(y ~ x, main = title, pch = 20, 
#          cex = 1.4, xlab = Xlab, ylab = Ylab, xlim = xlim, ylim = ylim, cex.axis = 1.2, cex.lab = 1.2)
#     abline(int, medslop, lwd = 2)
#     abline(lsfit(x, y), lty = 3, lwd = 2)
#   }
# }
