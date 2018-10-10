library(hgutils)
load_packages("stringr","magrittr")

cols = list(orange="orange",blue="blue",red="red")
col = cols$blue
packages = c("metafor","shiny","ggplot2","mice","rms","magrittr","dplyr","hgutils")

txts = sapply(packages, function(x) paste0(x,"-",str_replace_all(packageDescription(x)$Version,"-","--")))
urls = paste0("https://img.shields.io/badge/",txts,"-",col,".png")
for(i in 1:length(urls)) {
  download.file(urls[i],paste0("tools/output/",packages[i],".png"),mode="wb")
}
