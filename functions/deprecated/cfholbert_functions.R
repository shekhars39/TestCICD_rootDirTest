# not used

# # theil sen from https://www.cfholbert.com/blog/nonparametric-trend-analysis/
# # The approach used by USEPA (2009) fits the line through the point (median of x, median of y) and then solves for the intercept. 
# 
# # Function to compute Theil-Sen regression estimator
# theilsen <- function(x,y){ 
#   ord <- order(x) 
#   xs <- x[ord] 
#   ys <- y[ord] 
#   vec1 <- outer(ys, ys, "-") 
#   vec2 <- outer(xs, xs, "-") 
#   v1 <- vec1[vec2 > 0] 
#   v2 <- vec2[vec2 > 0] 
#   slope <- median(v1/v2)
#   coef <- median(y) - slope * median(x)
#   c(coef, slope) 
# }
