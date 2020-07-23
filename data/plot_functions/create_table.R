library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the files
g50 = t(read.csv("../mazda/tables/hypervolumes/50gmazdamean.csv"))
g100 = t(read.csv("../mazda/tables/hypervolumes/100gmazdamean.csv"))
g200 = t(read.csv("../mazda/tables/hypervolumes/200gmazdamean.csv"))
labels = t(read.csv("../mazda/tables/hypervolumes/labels.csv"))

# Creating the labels
labels = gsub("^.*?200g_","",labels)
labels = gsub("_"," ",labels)
labels = gsub("sr","stochastic ranking",labels)

# Some custom colors
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"

# Data frame creation
data = data.frame(labels[2,],Generation50 = g50[2,], Generation100 = g100[2,], Generation200 = g200[2,])
colnames(data) = c("CHT","Generation 50","Generation 100","Generation 200")

# Creating Table
formattable(data,
            align =c("l","c","c","c"),list(
            `CHT` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
            `Generation 50`= color_tile(customGreen0, customGreen),
            `Generation 100`= color_tile(customGreen0, customGreen),
            `Generation 200`= color_tile(customGreen0, customGreen)
            ))
