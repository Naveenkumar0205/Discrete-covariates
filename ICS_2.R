#Read data
rent_index_data <- read.csv("rent_index_99.csv")

# Creating Rent per square meter column for our analysis
rent_index_data$rentpsqm <- rent_index_data$net.rent/rent_index_data$living.area

#Check for null values
is.null(rent_index_data)

# Summary
summary(rent_index_data)
#Summary statistics - Count, mean, std
library(dplyr)
group_by(rent_index_data, quality.of.location) %>%
  summarise(
    count = n(),
    mean = mean(rentpsqm, na.rm = TRUE),
    sd = sd(rentpsqm, na.rm = TRUE),
    iqr = IQR(rentpsqm, na.rm = TRUE)
  )

#summary stat for variable bathroom
rent_index_data$bathroom <- ordered(rent_index_data$bathroom,
                                    levels = c(0,1))
standard <- rent_index_data[which(rent_index_data$bathroom=='0'),]
summary(standard$rentpsqm)
premium <- rent_index_data[which(rent_index_data$bathroom=='1'),]
summary(premium$rentpsqm)

#summary stat for variable kitchen
rent_index_data$kitchen <- ordered(rent_index_data$kitchen,
                                   levels = c(0,1))
standard_kit <- rent_index_data[which(rent_index_data$kitchen=='0'),]
summary(standard_kit$rentpsqm)
premium_kit <- rent_index_data[which(rent_index_data$kitchen=='1'),]
summary(premium_kit$rentpsqm)


#summary stat for central heating
rent_index_data$central.heating <- ordered(rent_index_data$central.heating,
                                           levels = c(0,1))
yes <- rent_index_data[which(rent_index_data$central.heating=='1'),]
summary(yes$rentpsqm)
no <- rent_index_data[which(rent_index_data$central.heating=='0'),]
summary(no$rentpsqm)

# groups count bases on quality of location
table(rent_index_data$quality.of.location) # Imbalance data
rent_index_data$quality.of.location <- ordered(rent_index_data$quality.of.location,
                         levels = c(1, 2, 3))


#Variance comparison
# install.packages("ggpubr")
library("ggpubr")
tiff("box-plot.tiff", units="in", width=5, height=5, res=300)
ggboxplot(rent_index_data, x = "quality.of.location", y = "rentpsqm", 
          color = "quality.of.location", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c(1,2,3),
          ylab = "Rent per square meter", xlab = "Quality of location")
dev.off()
# how to make a QQ plot in R
# x = rnorm(100, 50, 25)
# y = rnorm(100, 50, 25)

# qqplot function in r package
# qqplot(x, y, xlab = "test x", ylab = "test y", main = "Q-Q Plot")

#QQPlot for normality assessment
library(ggplot2)
for (i in rent_index_data$quality.of.location){
  
  new_df <- rent_index_data[rent_index_data$quality.of.location == i,]
  
  p <- ggplot(data = new_df, mapping = aes(sample = rentpsqm)) + 
    geom_qq(colour = "darkcyan") + 
    geom_qq_line(size = 1) +
    labs(x = "Theoretical quantiles", y = paste(c("quality of location =", i), collapse = " "))
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
  
  assign(paste0("qq_",i,"_plot"),p)
}

# install.packages("gridExtra")
library(gridExtra)
tiff("qq-plot-1.tiff", units="in", width=5, height=5, res=300)
qq_1_plot # Quality of location 1
dev.off()

tiff("qq-plot-2.tiff", units="in", width=5, height=5, res=300)
qq_2_plot # Quality of location 2
dev.off()

tiff("qq-plot-3.tiff", units="in", width=5, height=5, res=300)
qq_3_plot # Quality of location 3
dev.off()

tiff("qq-plot-grid.tiff", units="in", width=15, height=5, res=300)
grid.arrange(qq_1_plot, qq_2_plot, qq_3_plot, ncol = 3, nrow = 1)
dev.off()

# Task 1 - Kruskal-Wallis test
res <- kruskal.test(rentpsqm ~ quality.of.location, data = rent_index_data)
# Summary of the analysis
res


# Pairwise Wilcox test without adjustment
pairwise.wilcox.test(x = rent_index_data$rentpsqm, g = rent_index_data$quality.of.location,
                     p.adjust.method = "none", paired = FALSE)


# Pairwise Test with Bonferroni adjustment
pairwise.wilcox.test(x = rent_index_data$rentpsqm, g = rent_index_data$quality.of.location,
                p.adjust.method = "bonferroni", paired = FALSE) #, pool.sd = TRUE

