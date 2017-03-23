
# vizualization of the whole dataset
library(rgl)
library(reshape2)
A <- dcast(mortality_table %>% select(Age,Year,qx),Age~Year)
persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(),col="lightblue", axes = F,
        xlab = 'age', ylab = 'year', zlab = 'deaht probability (qx)')
axes3d(c("x", "y"))
axis3d("z", at=pretty(A %>% select(-Age) %>% as.matrix() %>% log()), labels=10^pretty(A %>% select(-Age) %>% as.matrix() %>% log()))

# residuals plots
LCres <- residuals(LCfit)
plot(LCres, type = "colourmap")
plot(LCres, type = "scatter")


