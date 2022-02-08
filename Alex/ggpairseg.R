#Now a matrix plot with the dependent; but want to control each type of plot
#control density in the diagonal by creating a new function
library(ggplot2)
require(GGally)
require(ggmosaic)
modified_density = function(data, mapping, ...) {
  ggally_densityDiag(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))+
  theme(text = element_text(size=8))
  }
#control scatter in the lower triangle  by creating a new function
mod_points=function(data,mapping,...) {
  ggally_points(data, mapping,pch=20, ...) + scale_colour_manual(values = c("red", "blue"))+
    theme(text = element_text(size=8))
}  
#control histogram in the lower triangle for each level of good by creating a new function
mod_bihist=function(data,mapping,...) {
  ggally_facethist(data, mapping, ...) + scale_fill_manual(values = c("red", "blue")) +
theme(text = element_text(size=7))  +
    scale_x_continuous(breaks=c(floor(min(data[,mapping_string(mapping$x)])),round((min(data[,mapping_string(mapping$x)])+max(data[,mapping_string(mapping$x)]))/2),ceiling(max(data[,mapping_string(mapping$x)])))) +
    scale_y_continuous(breaks=c(0,20,50,60)) 
}
#control box plot
mod_box=function(data,mapping,...) {
  
  ggally_box_no_facet(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))
}
mod_bar=function(data,mapping,...) {
  #control bar chart in diagonal  
  ggally_barDiag(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))
}
#control correlation
mod_cor=function(data,mapping,...) {
  ggally_cor(data, mapping,size=3,alignPercent=0.9) + scale_colour_manual(values = c("red", "blue")) + theme(panel.grid.major = element_blank(),...)
  
}

my_fn <- function(data, mapping, ...){ 
mapping$fill <- mapping$y
ya <- data[,mapping_string(mapping$y)]
xa <- data[,mapping_string(mapping$x)]
print(mapping)
mapping$y <- NULL
print(mapping)
#xx <- product(xa,ya)
#mapping$x <- xx
#print(xx)
print(mapping)
  ggplot(data = data,mapping) +
    geom_mosaic(mapping) 
}
   
#Then plot ggpairs(df, lower=list(continuous=my_fn))

attach(czechloan)

#Now put it all together using ggpairs and using our predefined functions.
ggpairs(czechloan[,c(2,9,10,21,11,12,22,13,14,23,27)],mapping=aes(color=good,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))

ggpairs(czechloan[,c(5,15,16,24,17,18,25,19,20,26,27)],mapping=aes(color=good,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))
mod_bihist=function(data,mapping,...) {
  ggally_facethist(data, mapping, ...) + scale_fill_manual(values = c("red", "blue")) +
    theme(text = element_text(size=7))  +
    scale_y_continuous(breaks=c(0,20,50,60)) 
}
ggpairs(czechloan[,c(2:8,27)],mapping=aes(color=good,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist,discrete=my_fn),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box,discrete=mod_bar))

ggplot(data = czechloan) +
  geom_mosaic(mapping=aes(x=product(sex,good),fill=good))+
  labs(x="good",y="sex")


