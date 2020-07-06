summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # 计算长度
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # 以 groupvars 为组,计算每组的长度,均值,以及标准差
  # ddply 就是 dplyr 中的 group_by + summarise
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # 重命名  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  # 计算标准偏差
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  # 计算置信区间
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



tgc <- summarySE(RMS, measurevar = "RMS..dB.", groupvars = c("Substrate","Distance..mm."))

ggplot(tgc, aes(x=Distance..mm., y=RMS..dB., colour=Substrate)) + 
  geom_errorbar(aes(ymin=RMS..dB.-se, ymax=RMS..dB.+se), width=.2) +
  geom_line(size=2) +
  geom_point() +
  ylab("Relative dB") + xlab("Distance(mm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.line = element_line(size=1, colour = "black", arrow = arrow(length = unit(0.5, 'cm'))),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))