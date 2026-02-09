# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)
## library(testthat)


library(vegtable)
data(Kenya_veg)
x <- as(Kenya_veg@coverconvert, "list")$br_bl
y <- as(Kenya_veg@coverconvert, "list")
z <- Kenya_veg@coverconvert


cov <- df2coverconvert(y)

y2 <- y
names(y2[[2]])[2] <- "bottom_value"
df2coverconvert(y2)


df2coverconvert(x)
df2coverconvert(x, "br_bl")

names(z)

z$b_bbds

z$cover1 <- z
z$cover2 <- z[2]


## Example for class

#'
#' @examples
#' showClass("coverconvert")
#'
#' ## Add a custom scale
#' Scale <- new("coverconvert")
#' Scale$my_scale <- data.frame(
#'   value = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
#'   bottom = c(0, 50, 75),
#'   top = c(50, 75, 100)
#' )
#' summary(Scale)
