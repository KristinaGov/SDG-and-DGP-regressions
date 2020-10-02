library(readxl)
library(ggplot2)
library(ggpubr)
library(stats)
library(tidyr)

####################################################
# Set working diectory
wd<-getwd()
setwd(wd)
####################################################
#                     X - X - X
####################################################
# Load and clean the data
df<-as.data.frame(read_xlsx("GlobalIndexResults.xlsx", sheet = "Correlations", range = "A1:R147"))
#lapply(df, class)
#str(df)
# REPLACE 0 with NA!!
df[df==0]<-NA
####################################################
#                     X - X - X
####################################################
y_goal<-as.numeric(df$`Goal 1 Score`)
x_gdp<-as.numeric(df$LnGDPpcpt)
####################################################
#                     X - X - X
####################################################
# Test polynom regression function and plot
lm( y_goal ~ poly(x_gdp, 3, raw = TRUE))
coef(lm( y_goal ~ poly(x_gdp, 3, raw = TRUE)))
summary(lm( y_goal ~ poly(x_gdp, 3, raw = TRUE)))$r.squared
summary(lm( y_goal ~ poly(x_gdp, 3, raw = TRUE)))$coefficients[, 1]
lm_coef<-coef(lm( y_goal ~ poly(x_gdp, 3, raw = TRUE)))
mod <- lm(  y_goal ~ poly(x_gdp, 3, raw = TRUE))
plot(x_gdp, y_goal)
lines(x_gdp[order(x_gdp)], predict(mod, list(x_gdp = x_gdp[order(x_gdp)])))

# Test exponent regression function and plot
mod <- nls(y_goal ~ exp(a + b * x_gdp), start = list(a = 0, b = 0))
plot(x_gdp, y_goal)
lines(x_gdp[order(x_gdp)], predict(mod, list(x_gdp = x_gdp[order(x_gdp)])))
####################################################
#                     X - X - X
####################################################
# Build single graphs if needed for a SDG / GDP pair
my.data<-df[,c('Country','LnGDPpcpt','Goal 15 Score','Income')]
formula <- y ~ poly(x, 3, raw = TRUE)  # polynom
#formula <- y ~ poly(x, 1, raw = TRUE)  # line

p <- ggplot(my.data, aes(x=LnGDPpcpt, y=`Goal 15 Score`, color=Income)) +
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  theme_bw()
ggpar(p, palette = "jco")
#ggsave('regressiong15.pdf', dpi=320, width = 20, height = 20, units = 'cm')
####################################################
#                     X - X - X
####################################################
# Graph of all goals' vs GDP regressions
df.long<-tidyr::gather(df, goalname, goalvalue, `Goal 1 Score`:`Goal 15 Score`, factor_key=T)

p <- ggplot(df.long, aes(y=goalvalue, x=LnGDPpcpt)) +
  geom_point(aes(color=Income)) +
  stat_smooth(method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula,
    label.x.npc = "left",
    label.y.npc = "bottom"#, size = 2
  ) +
  facet_wrap(vars(goalname), nrow = 5) +
  theme_bw()
ggpar(p, palette = "jco")
#ggsave('regression.pdf', dpi=320, width = 40, height = 40, units = 'cm')
####################################################
#                     X - X - X
####################################################
# Make a report abour R2 of the regressions and coefficients
report.df<-data.frame(goals=colnames(df[,3:17]), rsq=0, x0=0, x1=0, x2=0, x3=0)
formula<-y_f ~ poly(x_f, 3, raw = TRUE) 
x_f<-as.numeric(df$LnGDPpcpt)
for (i in 3:17) {
  y_f<-df[,i]
  report.df[(i-2),2:6]=cbind( rsq=(summary(lm(formula))$r.squared), t(summary(lm(formula))$coefficients[, 1]) )
}
write.table(report.df, "report_regressions.txt")
####################################################

