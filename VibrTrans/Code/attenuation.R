library(nlme)
test <- lme(RI..dB. ~ Distance..mm.*Substrate, data = att, random = ~1 | Group)
anova(test)