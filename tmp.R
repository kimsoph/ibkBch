df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)


lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# Create two groups on which you want to facet
df$group <- c(rep(1:2,50))

# Create the equation labels for the two groups
eq <- plyr::ddply(df, .$group, lm_eqn)

# And plot

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()


library(ggpubr)

ggplot(mtcars,aes(x = wt, y = hp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  ggpubr::stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 350, aes(label = ..rr.label..))
