#### This currently produces box plots based on ecoregions provided by gagesII_sept10_2011_conterm.xlsx
#### this step may require we perform on Thermal_metrics.csv a select by location analysis using climate regions shp. 

#install.packages('ggpubr')
library(ggpubr)
library(here)
library(ggplot2)
library(magrittr)

start_time <- Sys.time()

results <- here("results/boxplots")
if(!file.exists(results)){
  dir.create(results, recursive = TRUE)}

df <- read.csv(file.path(here('results'),'Thermal_Metrics.csv'))

keeps <- c("AGGECOREGI", "pstP90", "pstP90Cnt", "pstP50", 
           "pstP10", "pstP10Cnt", "pstTmax", "pstTmin", 
           "pstTmean", "pstTmaxT","pstTminT", "pstCoolRt", 
           "pstWarmRt", "pstCffcntVrtn", "pstP25cnt",
           "pstP75cnt", "Lag_TmaxT", "Lag_TminT", "Ref")

df2 <- df[,keeps,drop=TRUE]

df2$Ref[df2$Ref==0] <- 'Dam Site'
df2$Ref[df2$Ref==1] <- 'Reference Site'

for (min_timing in df$pstTminT){
 if (min_timing > 188){
   df$pstTminT[x] <- min_timing - 366
 }
}

coln <- colnames(df2[2:18])

for (metric in coln){
  tryCatch(
    expr = {
      ggboxplot(df2, x = "AGGECOREGI", y = metric,
                color = "Ref",
                add = "jitter") + 
        scale_color_grey(start=0.2, end=0.8) + 
        theme_classic() +
        stat_compare_means(aes(group = df$Ref, label = paste0(..method.., "\n", "p=", ..p.format.., "\n", ..p.signif..)), size = 2.5) +
        ggtitle(metric) +
        xlab("" ) + 
        ylab("")+
        theme(axis.text.x = element_text(color = "grey20",size = 10, angle = 15,hjust = .5, vjust = .5,face = "plain"), axis.title.x = element_text(angle = 15))
      
      fn <- paste0(metric, ".png")
      ggsave(fn, path = file.path(here('results/boxplots')), device = "png")
    },
    error = function(cond){
      message(paste0(metric))
      return(NA)})}

end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")