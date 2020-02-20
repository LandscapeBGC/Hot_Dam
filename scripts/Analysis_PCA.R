library(factoextra)
library(devtools)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(FactoMineR)
library(ggthemes)
library(here)

#### I need help figuring out how to transfer outputs to results/pca ####

start_time <- Sys.time()

#### Create output location for PCA Results ####

results <- here("results/pca")
if(!file.exists(results)){
  dir.create(results, recursive = TRUE)}


#######################fixing mistake in data set##########
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

##############################subset for thermal metrics of dammed rivers only#########################
data.met <- subset(df2, Ref == "Dam Site")
#data.met <- data.frame(data.met, row.names = 1)
data.met <- data.met[,-17:-18]
head(data.met)
#since they are on different scales, must standardize
data.met.s <- scale(data.met[,2:16], center=T, scale=T)
#Plot Standardized scores. look at relationships
plot(pairs(data.met.s, labels=c("pstP90", "pstP90Cnt", "pstP50",	"pstP10",	"pstP10Cnt",
                           "pstTmax",	"pstTmin",	"pstTmean",	"pstTmaxT",	"pstTminT",
                           "pstCoolRt",	"pstWarmRt",	"pstCffcntV",	"pstP25cnt",
                           "pstP75cnt"),
      panel=function(x,y){
        panel.smooth(x,y)
        abline(lsfit(x,y), lty=2)
      }))

fn <- paste0("pairs", ".png")
ggsave(fn, plot = pairs, path = file.path(here('results/pca')), device = "png")

data.met.s.pc <- prcomp(data.met.s)
data.met.s.pc$sdev #remember that most have tiny variances
summary(data.met.s.pc)
data.met.s.pc$rotation[,-4:-15] #only look at factor loadings for first 3 PCs
s <- var(data.met.s.pc$x)
#view incfluence of variables on PC axes
fviz_pca_var(data.met.s.pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )    # Avoid text overlapping

#my poor attempt to work up data in R... Trying to get
#PCA results into a dataset to plot
p <- data.met.s.pc$x[,-3:-15]
head(p)
data.mett <- subset(df2, Ref == "Dam Site")
d <- data.mett[,-2:-16]
c <- cbind(p, d)
c <- c[,-3]
head(c)
##Plot PCA results
x <- c$PC1
y <- c$PC2
g <- ggplot(c, aes(PC1, PC2, color = 'AGGECOREGI')) +
  geom_point(aes(shape='AGGECOREGI', color='AGGECOREGI'), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18, 19, 20, 15, 16, 17))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9',
                              "#00AFBB", "#D16103", "#CC79A7",
                              "#52854C", "#4E84C4", "#293352"))+
  theme_classic(base_size=16) + ggtitle("Climate Region PCA")+
  xlab("PCA Axis 1") + ylab("PCA Axis 2")
g + stat_ellipse(size=2, geom="polygon",level=0.8,alpha=0.1)

end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")