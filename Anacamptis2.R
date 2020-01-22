##################################################################################################

##### DATA PREPARATION AND EXPLORATORY ANALYSIS #####

# Build a dataset for PCA on floral traits
CL2014.FloralPCA <- CL2014[, c(1:2, 12:30)]
CL2014.FloralPCA <- CL2014.FloralPCA[complete.cases(CL2014.FloralPCA), ] # keep only rows withous NAs
CL2014.FloralPCA <- subset(CL2014.FloralPCA, !CL2014.FloralPCA$Ploidy=="3n") # remove 3n; ! is the operator for NOT
summary(CL2014.FloralPCA)
FloralPCA <- princomp(CL2014.FloralPCA[,3:21])
summary(FloralPCA)
CL2014.FloralPCA <- cbind(CL2014.FloralPCA, FloralPCA$scores[,1:3]) # bind PCA scores of the first three PCs to the original dataset

# Plot the first two or three components distinguishing between 4n and 2n 
library(ggplot2)
PC1vsPC2 <- qplot(x = Comp.1, y = Comp.2, colour = Ploidy, data = CL2014.FloralPCA) + 
  stat_ellipse(data=NULL, geom = "polygon", alpha = 0.15) + 
  scale_color_manual(values=c( "light sky blue", "orange")) + 
  ggtitle("Flower traits PC1 vs PC2")
PC1vsPC2
ggsave(plot = PC1vsPC2, filename = "PC1vsPC2.pdf", width = 14, height = 8)

library(plotly)
palette <- c("orange", "light sky blue", "green")
palette <- setNames(palette, c("4n", "2n", "3n"))
PC1vsPC2vsPC3 <- plot_ly(x = ~Comp.1, y = ~Comp.2, z = ~ Comp.3, color = ~Ploidy, mode = "markers", colors = palette, data = CL2014.FloralPCA) %>%
  add_markers(size = 1, sizes = c(2))
PC1vsPC2vsPC3
# export directly from device

# Coefficient of Variation for all traits 
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Height), "Height"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Height), "Height"])
write.table(CoV.4n, file = "CoV.4nTest.txt")
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$n.flowers), "n.flowers"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$n.flowers), "n.flowers"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Fauc), "X_Sp_Fauc"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Fauc), "X_Sp_Fauc"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Met), "X_Sp_Met"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Met), "X_Sp_Met"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Ap), "X_Sp_Ap"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Sp_Ap), "X_Sp_Ap"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_Sp), "Y_Sp"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_Sp), "Y_Sp"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Lab_Max), "X_Lab_Max"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_Lab_Max), "X_Lab_Max"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_LobLat), "X_LobLat"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_LobLat), "X_LobLat"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_Lab), "Y_Lab"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_Lab), "Y_Lab"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_LobMed), "X_LobMed"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_LobMed), "X_LobMed"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_LobMed), "Y_LobMed"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_LobMed), "Y_LobMed"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepDx), "X_SepDx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepDx), "X_SepDx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepSx), "X_SepSx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepSx), "X_SepSx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepCen), "X_SepCen"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_SepCen), "X_SepCen"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepDx), "Y_SepDx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepDx), "Y_SepDx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepSx), "Y_SepSx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepSx), "Y_SepSx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepCen), "Y_SepCen"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_SepCen), "Y_SepCen"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_PetDx), "X_PetDx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_PetDx), "X_PetDx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_PetSx), "X_PetSx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$X_PetSx), "X_PetSx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_PetDx), "Y_PetDx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_PetDx), "Y_PetDx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_PetSx), "Y_PetSx"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Y_PetSx), "Y_PetSx"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Peak.Day), "Peak.Day"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Peak.Day), "Peak.Day"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Init.Day), "Init.Day"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Init.Day), "Init.Day"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$End.Day), "End.Day"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$End.Day), "End.Day"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
CoV.4n <- sd(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Tot.Days), "Tot.Days"])/mean(CL2014.Phen.4n[complete.cases(CL2014.Phen.4n$Tot.Days), "Tot.Days"])
write.table(CoV.4n, file = "CoV.4nTest.txt", append = T)
remove(CoV.4n)

CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Height), "Height"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Height), "Height"])
write.table(CoV.2n, file = "CoV.2nTest.txt")
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$n.flowers), "n.flowers"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$n.flowers), "n.flowers"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Fauc), "X_Sp_Fauc"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Fauc), "X_Sp_Fauc"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Met), "X_Sp_Met"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Met), "X_Sp_Met"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Ap), "X_Sp_Ap"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Sp_Ap), "X_Sp_Ap"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_Sp), "Y_Sp"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_Sp), "Y_Sp"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Lab_Max), "X_Lab_Max"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_Lab_Max), "X_Lab_Max"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_LobLat), "X_LobLat"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_LobLat), "X_LobLat"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_Lab), "Y_Lab"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_Lab), "Y_Lab"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_LobMed), "X_LobMed"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_LobMed), "X_LobMed"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_LobMed), "Y_LobMed"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_LobMed), "Y_LobMed"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepDx), "X_SepDx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepDx), "X_SepDx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepSx), "X_SepSx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepSx), "X_SepSx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepCen), "X_SepCen"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_SepCen), "X_SepCen"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepDx), "Y_SepDx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepDx), "Y_SepDx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepSx), "Y_SepSx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepSx), "Y_SepSx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepCen), "Y_SepCen"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_SepCen), "Y_SepCen"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_PetDx), "X_PetDx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_PetDx), "X_PetDx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_PetSx), "X_PetSx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$X_PetSx), "X_PetSx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_PetDx), "Y_PetDx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_PetDx), "Y_PetDx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_PetSx), "Y_PetSx"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Y_PetSx), "Y_PetSx"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Peak.Day), "Peak.Day"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Peak.Day), "Peak.Day"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Init.Day), "Init.Day"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Init.Day), "Init.Day"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$End.Day), "End.Day"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$End.Day), "End.Day"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
CoV.2n <- sd(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Tot.Days), "Tot.Days"])/mean(CL2014.Phen.2n[complete.cases(CL2014.Phen.2n$Tot.Days), "Tot.Days"])
write.table(CoV.2n, file = "CoV.2nTest.txt", append = T)
remove(CoV.2n)

library(plotly)
Boxplot.CoV <- plot_ly(y = ~CoV.2n, name = "2n", x= ~Type, type="box", data = CoV) %>%
  add_trace(y = ~CoV.4n, name = "4n") %>% 
  layout(boxmode = "group", yaxis = list(title = "CoV"), xaxis = list(title = " "), legend = list(x = 0.725, y = 0.825))
Boxplot.CoV
plotly_POST(Boxplot.CoV, filename = "Anacamptis 2/Boxplot.CoV")
dev.copy2pdf(file = "Boxplot_CoV.pdf")

CL2014.Phen <- read.csv(file="CL2014.csv")
names(CL2014.Phen)[names(CL2014.Phen)=="Max.n.open.flowers"] <- "maxFlowers" # rename long-named variables...
CL2014.Phen <- subset(CL2014.Phen, !CL2014.Phen$Ploidy=="3n") # remove 3n; ! is the operator for NOT
# Separate 4n and 2n
CL2014.Phen.4n <- subset(CL2014.Phen, Ploidy=="4n")
CL2014.Phen.2n <- subset(CL2014.Phen, Ploidy=="2n")

# Differences between traits in 4n and 2n: Mann-Whitney U-tests
U <- wilcox.test(CL2014.Phen.4n$Height, CL2014.Phen.2n$Height)
write.table(U[c(1,3,7)], file = "UTest.txt")
U <- wilcox.test(CL2014.Phen.4n$n.flowers, CL2014.Phen.2n$n.flowers)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_Sp_Fauc, CL2014.Phen.2n$X_Sp_Fauc)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_Sp_Met, CL2014.Phen.2n$X_Sp_Met)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_Sp_Ap, CL2014.Phen.2n$X_Sp_Ap)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_Sp, CL2014.Phen.2n$Y_Sp)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_Lab_Max, CL2014.Phen.2n$X_Lab_Max)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_LobLat, CL2014.Phen.2n$X_LobLat)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_Lab, CL2014.Phen.2n$Y_Lab)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_LobMed, CL2014.Phen.2n$X_LobMed)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_LobMed, CL2014.Phen.2n$Y_LobMed)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_SepDx, CL2014.Phen.2n$X_SepDx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_SepSx, CL2014.Phen.2n$X_SepSx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_SepCen, CL2014.Phen.2n$X_SepCen)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_SepDx, CL2014.Phen.2n$Y_SepDx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_SepSx, CL2014.Phen.2n$Y_SepSx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_SepCen, CL2014.Phen.2n$Y_SepCen)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_PetDx, CL2014.Phen.2n$X_PetDx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$X_PetSx, CL2014.Phen.2n$X_PetSx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_PetDx, CL2014.Phen.2n$Y_PetDx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Y_PetSx, CL2014.Phen.2n$Y_PetSx)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Peak.Day, CL2014.Phen.2n$Peak.Day)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Init.Day, CL2014.Phen.2n$Init.Day)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$End.Day, CL2014.Phen.2n$End.Day)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
U <- wilcox.test(CL2014.Phen.4n$Tot.Days, CL2014.Phen.2n$Tot.Days)
write.table(U[c(1,3,7)], file = "UTest.txt", append = T)
remove(U)

# Check if there are differences between symmetrical floral pieces separately in 4n and 2n: run one-way ANOVAs
# First prepare the tables; I should learn how to use *apply() family functions to do this, or at least reshape2 or plyr packages...
aov4nSpur <- read.csv(file="aov4nSpur.csv")
aov4nXPet <- read.csv(file="aov4nXPet.csv")
aov4nXSep <- read.csv(file="aov4nXSep.csv")
aov4nYPet <- read.csv(file="aov4nYPet.csv")
aov4nYSep <- read.csv(file="aov4nYSep.csv")
SepalsLength.4n <- aov(X0.52 ~ Y_SepSx, data = aov4nYSep)
SepalsWidth.4n <- aov(X0.19 ~ X_SepSx, data = aov4nXSep)
PetalsLenght.4n <- aov(X0.52 ~ Y_PetSx, data = aov4nYPet)
PetalsWidth.4n <- aov(X0.1 ~ X_PetSx, data = aov4nXPet)
SpurAperture.4n <- aov(X0.09 ~ X_Sp_Ap, data = aov4nSpur)
summary(SepalsLength.4n)
summary(SepalsWidth.4n)
summary(PetalsLenght.4n)
summary(PetalsWidth.4n)
summary(SpurAperture.4n)

aov2nSpur <- read.csv(file="aov2nSpur.csv")
aov2nXPet <- read.csv(file="aov2nXPet.csv")
aov2nXSep <- read.csv(file="aov2nXSep.csv")
aov2nYPet <- read.csv(file="aov2nYPet.csv")
aov2nYSep <- read.csv(file="aov2nYSep.csv")
SepalsLength.2n <- aov(X0.5 ~ Y_SepSx, data = aov2nYSep)
SepalsWidth.2n <- aov(X0.19 ~ X_SepSx, data = aov2nXSep)
PetalsLenght.2n <- aov(X0.61 ~ Y_PetSx, data = aov2nYPet)
PetalsWidth.2n <- aov(X0.2 ~ X_PetSx, data = aov2nXPet)
SpurAperture.2n <- aov(X0.1 ~ X_Sp_Ap, data = aov2nSpur)
summary(SepalsLength.2n)
summary(SepalsWidth.2n)
summary(PetalsLenght.2n)
summary(PetalsWidth.2n)
summary(SpurAperture.2n)

# Average symmetrical floral pieces
CL2014.Phen.4n$X_Sep <- ((CL2014.Phen.4n$X_SepSx + CL2014.Phen.4n$X_SepDx + CL2014.Phen.4n$X_SepCen)/3)
CL2014.Phen.4n$Y_Sep <- ((CL2014.Phen.4n$Y_SepSx + CL2014.Phen.4n$Y_SepDx + CL2014.Phen.4n$Y_SepCen)/3)
CL2014.Phen.4n$X_Pet <- ((CL2014.Phen.4n$X_PetSx + CL2014.Phen.4n$X_PetDx)/2)
CL2014.Phen.4n$Y_Pet <- ((CL2014.Phen.4n$Y_PetSx + CL2014.Phen.4n$Y_PetDx)/2)
CL2014.Phen.4n$X_Sp <- ((CL2014.Phen.4n$X_Sp_Ap + CL2014.Phen.4n$X_Sp_Met + CL2014.Phen.4n$X_Sp_Fauc)/3)

CL2014.Phen.2n$X_Sep <- ((CL2014.Phen.2n$X_SepSx + CL2014.Phen.2n$X_SepDx + CL2014.Phen.2n$X_SepCen)/3)
CL2014.Phen.2n$Y_Sep <- ((CL2014.Phen.2n$Y_SepSx + CL2014.Phen.2n$Y_SepDx + CL2014.Phen.2n$Y_SepCen)/3)
CL2014.Phen.2n$X_Pet <- ((CL2014.Phen.2n$X_PetSx + CL2014.Phen.2n$X_PetDx)/2)
CL2014.Phen.2n$Y_Pet <- ((CL2014.Phen.2n$Y_PetSx + CL2014.Phen.2n$Y_PetDx)/2)
CL2014.Phen.2n$X_Sp <- ((CL2014.Phen.2n$X_Sp_Ap + CL2014.Phen.2n$X_Sp_Met + CL2014.Phen.2n$X_Sp_Fauc)/3)

# Scatterplot matrix; WARNING: computationally intensive!
library(ggplot2)
library(GGally)
ggscatmat(CL2014.Phen, columns=c(3:5, 7:10, 12:30), color="Ploidy", alpha=0.76) + 
  scale_color_manual(values=c("light sky blue", "orange"))
# export directly from device

##### CORRELATION ANALYSIS  #####

# Compute Z-scores for all the traits
Z.Phen.4n <- scale(CL2014.Phen.4n[, c(3:35)], scale = T, center = T)
Z.Phen.4n <- cbind(CL2014.Phen.4n[, c("ID","Ploidy")], Z.Phen.4n)
summary(Z.Phen.4n)

Z.Phen.2n <- scale(CL2014.Phen.2n[, c(3:35)], scale = T, center = T)
Z.Phen.2n <- cbind(CL2014.Phen.2n[, c("ID","Ploidy")], Z.Phen.2n)
summary(Z.Phen.2n)

# Evaluate correlation among variables
library(Hmisc) 
PhenCorr.4n <- rcorr(as.matrix(Z.Phen.4n[, c(3:10, 12:30)], type=c("spearman")))
print(PhenCorr.4n)
write.table(PhenCorr.4n$r, file = "PhenCorr.4n.csv", sep = ";", qmethod = "double")
write.table(PhenCorr.4n$P, file = "PhenCorr.4n.csv", sep = ";", qmethod = "double", append=T)

source("http://www.sthda.com/upload/rquery_cormat.r") # function to compute and display correlation
PhenCorr.4n.upper <- rquery.cormat(Z.Phen.4n[, c(3:10, 12:30)], type='upper', graph=F)
write.table(PhenCorr.4n.upper$r, file = "PhenCorr.4n.upper.csv", sep = ";", qmethod = "double")
write.table(PhenCorr.4n.upper$p, file = "PhenCorr.4n.upper.csv", sep = ";", qmethod = "double", append=T)

PhenCorr.2n <- rcorr(as.matrix(Z.Phen.2n[, c(3:10, 12:30)]), type=c("spearman"))
print(PhenCorr.2n)
write.table(PhenCorr.2n$r, file = "PhenCorr.2n.csv", sep = ";", qmethod = "double")
write.table(PhenCorr.2n$P, file = "PhenCorr.2n.csv", sep = ";", qmethod = "double", append=T)

PhenCorr.2n.upper <- rquery.cormat(Z.Phen.2n[, c(3:10, 12:30)], type='upper', graph=F)
write.table(PhenCorr.2n.upper$r, file = "PhenCorr.2n.upper.csv", sep = ";", qmethod = "double")
write.table(PhenCorr.2n.upper$p, file = "PhenCorr.2n.upper.csv", sep = ";", qmethod = "double", append=T)

# Correlogram with p-values numbers: 
library(corrplot)
# in order to change coefficients font size change font scaling (cex) value; first save the original values (default is 1)
par(cex = 0.8) # value for titles and legend
Corrplot4n.Corr <- corrplot(PhenCorr.4n$r, method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1)
par(cex = 0.5) # value for p-values font size
Corrplot4n.Pnums <- corrplot(PhenCorr.4n$P, add=T, type="lower", order="original", method="number", tl.pos="n", cl.pos="n", addgrid.col=NA, diag=F, col="grey", tl.cex=par("cex"))
par(cex = 0.8) # value for titles and legend
Corrplot2n.Corr <- corrplot(PhenCorr.2n$r, method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1)
par(cex = 0.5) # value for p-values font size
Corrplot2n.Pnums <- corrplot(PhenCorr.2n$P, add=T, type="lower", order="original", method="number", tl.pos="n", cl.pos="n", addgrid.col=NA, diag=F, col="grey", tl.cex=par("cex"))
par(cex = 1) # restore original cex value

# Significance stars instead of p-value numbers
library(corrplot)
# Tetraploids: graphical P-values (aka "significance stars") from p-value matrix: 
pstars4n <- symnum(PhenCorr.4n$P, corr = F, cutpoints = c(0,.001,.01,.05,.1,1), symbols = c("***","**","*","."," "), na=F)
pstars4n
par(cex = 0.8) # value for titles and legend
Corrplot4n.Corr <- corrplot(PhenCorr.4n$r, method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1)
# write in the plot white (on white background) p-values as dummies; cex still controls font size
par(cex = 1) # value for overlay strings
Corrplot4n.PnumsDummy <- corrplot(PhenCorr.4n$P, add=T, type="lower", order="original", method="number", tl.pos="n", cl.pos="n", addgrid.col=NA, diag=F, col="white", tl.cex=par("cex"))
pos4n <- expand.grid(1:ncol(pstars4n), ncol(pstars4n):1) # get the positions (in the plot) of the p-values; saves as vector
text(pos4n, pstars4n, col="gray") # overlay string using the position vector `pos4n' and the string vector `pstars4n' 
par(cex = 0.8) # must be the same as the first plot
Corrplot4n.Pstars <- corrplot(PhenCorr.4n$r, title="Tetraploids", mar=c(0,0,30,0), method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1, add=T) # re-overlay upper part of plot to hide unwanted labels 
# Diploids: graphical P-values (aka "significance stars") from p-value matrix: 
pstars2n <- symnum(PhenCorr.2n$P, corr = F, cutpoints = c(0,.001,.01,.05,.1,1), symbols = c("***","**","*","."," "), na=F)
pstars2n
par(cex = 0.8) # value for titles and legend
Corrplot2n.Corr <- corrplot(PhenCorr.2n$r, method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1)
# write in the plot white (on white background) p-values as dummies; cex still controls font size
par(cex = 1) # value for overlay strings
Corrplot2n.PnumsDummy <- corrplot(PhenCorr.2n$P, add=T, type="lower", order="original", method="number", tl.pos="n", cl.pos="n", addgrid.col=NA, diag=F, col="white", tl.cex=par("cex"))
pos2n <- expand.grid(1:ncol(pstars2n), ncol(pstars2n):1) # get the positions (in the plot) of the p-values; saves as vector
text(pos2n, pstars2n, col="gray") # overlay string using the position vector `pos2n' and the string vector `pstars2n' 
par(cex = 0.8) # must be the same as the first plot
Corrplot2n.Pstars <- corrplot(PhenCorr.2n$r, title="Diploids", mar=c(0,0,30,0), method="circle", type="upper", is.corr=T, addgrid.col=NA, order="original", tl.pos="lt", tl.col="black", tl.srt=45, tl.cex=1, add=T) # re-overlay upper part of plot to hide unwanted labels 

##### EVOLUTIONARY TRADE-OFFs  - lm() #####

# Evaluate trade-offs between phenology and morphological traits and among each set (remember: positive correlation between phenology and morphology means trade-off in this case)
MorphoPhenTetr1 <- lm(Peak.Day ~ Init.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoPhenTetr2 <- lm(Init.Day ~ Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoPhenTetr3 <- lm(End.Day ~ Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoPhenTetr4a <- lm(Tot.Days ~ Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoPhenTetr4b <- lm(Tot.Days ~ Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoTetr1 <- lm(Height ~ n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
MorphoTetr2 <- lm(n.flowers ~ X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.4n)
summary(MorphoPhenTetr1)
summary(MorphoPhenTetr2)
summary(MorphoPhenTetr3)
summary(MorphoPhenTetr4a)
summary(MorphoPhenTetr4b)
summary(MorphoTetr1)
summary(MorphoTetr2)
library(perturb) # evaluate colinearity
colldiag(MorphoPhenTetr1, scale=T, center=T) 
colldiag(MorphoPhenTetr2, scale=T, center=T)
colldiag(MorphoPhenTetr3, scale=T, center=T)
colldiag(MorphoPhenTetr4a, scale=T, center=T)
colldiag(MorphoPhenTetr4b, scale=T, center=T)
colldiag(MorphoTetr1, scale=T, center=T) 
colldiag(MorphoTetr2, scale=T, center=T) 

# Evaluate trade-offs between phenology and morphological traits and among each set (remember: positive correlation between phenology and morphology means trade-off in this case)
MorphoPhenDipl1 <- lm(Peak.Day ~ Init.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoPhenDipl2 <- lm(Init.Day ~ Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoPhenDipl3 <- lm(End.Day ~ Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoPhenDipl4a <- lm(Tot.Days ~ Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoPhenDipl4b <- lm(Tot.Days ~ Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoDipl1 <- lm(Height ~ n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
MorphoDipl2 <- lm(n.flowers ~ X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.Phen.2n)
summary(MorphoPhenDipl1)
summary(MorphoPhenDipl1)
summary(MorphoPhenDipl2)
summary(MorphoPhenDipl3)
summary(MorphoPhenDipl4a)
summary(MorphoPhenDipl4b)
summary(MorphoDipl1)
summary(MorphoDipl2)
library(perturb) # evaluate colinearity
colldiag(MorphoPhenDipl1, scale=T, center=T) 
colldiag(MorphoPhenDipl2, scale=T, center=T)
colldiag(MorphoPhenDipl3, scale=T, center=T)
colldiag(MorphoPhenDipl4a, scale=T, center=T)
colldiag(MorphoPhenDipl4b, scale=T, center=T)
colldiag(MorphoDipl1, scale=T, center=T) 
colldiag(MorphoDipl2, scale=T, center=T) 

##### EVOLUTIONARY TRADE-OFFs  - ploidy as response #####

MorphoPhen_Peak.Day <- lm(Peak.Day ~ Ploidy + Init.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + Init.Day:Ploidy + End.Day:Ploidy + Height:Ploidy + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(MorphoPhen_Peak.Day)
plot(MorphoPhen_Peak.Day)

MorphoPhen_Init.Day <- lm(Init.Day ~ Ploidy + Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + Peak.Day:Ploidy + End.Day:Ploidy + Height:Ploidy + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(MorphoPhen_Init.Day)
plot(MorphoPhen_Init.Day)

MorphoPhen_End.Day <- lm(End.Day ~ Ploidy + Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + Peak.Day:Ploidy + Init.Day:Ploidy + Height:Ploidy + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(MorphoPhen_End.Day)
plot(MorphoPhen_End.Day)

MorphoPhen_Tot.DaysA <- lm(Tot.Days ~ Ploidy + Peak.Day + Init.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + Peak.Day:Ploidy + Init.Day:Ploidy + Height:Ploidy + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(MorphoPhen_Tot.DaysA)
plot(MorphoPhen_Tot.DaysA)

MorphoPhen_Tot.DaysB <- lm(Tot.Days ~ Ploidy + Peak.Day + End.Day + Height + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + Peak.Day:Ploidy + End.Day:Ploidy + Height:Ploidy + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(MorphoPhen_Tot.DaysB)
plot(MorphoPhen_Tot.DaysB)

Morpho_Height <- lm(Height ~ Ploidy + n.flowers + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + n.flowers:Ploidy + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(Morpho_Height)
plot(Morpho_Height)

Morpho_n.flowers <- lm(n.flowers ~ Ploidy + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet + X_Sp:Ploidy + Y_Sp:Ploidy + X_Lab_Max:Ploidy + X_LobLat:Ploidy + Y_Lab:Ploidy + X_LobMed:Ploidy + Y_LobMed:Ploidy + X_Sep:Ploidy + Y_Sep:Ploidy + X_Pet:Ploidy + Y_Pet:Ploidy, data = rbind(Z.Phen.2n, Z.Phen.4n))
summary(Morpho_n.flowers)
plot(Morpho_n.flowers)


#####  SELECTION ANALYSIS  #####

# Build a dataset for trait-based selection analyses
CL2014.traits <- cbind(CL2014[1:10], CL2014[12:30])

# Separate 4n and 2n
CL2014.traits.4n <- subset(CL2014.traits, Ploidy=="4n")
CL2014.traits.2n <- subset(CL2014.traits, Ploidy=="2n")
write.csv(CL2014.traits.4n, file="CL2014.traits.4n.csv")
write.csv(CL2014.traits.2n, file="CL2014.traits.2n.csv")

# Average symmetric floral traits
CL2014.traits.4n$X_Sep <- ((CL2014.traits.4n$X_SepSx + CL2014.traits.4n$X_SepDx + CL2014.traits.4n$X_SepCen)/3) # width of sepals is not significant to fitness, apparently
CL2014.traits.4n$Y_Sep <- ((CL2014.traits.4n$Y_SepSx + CL2014.traits.4n$Y_SepDx + CL2014.traits.4n$Y_SepCen)/3)
CL2014.traits.4n$X_Pet <- ((CL2014.traits.4n$X_PetSx + CL2014.traits.4n$X_PetDx)/2)
CL2014.traits.4n$Y_Pet <- ((CL2014.traits.4n$Y_PetSx + CL2014.traits.4n$Y_PetDx)/2)
CL2014.traits.4n$X_Sp <- ((CL2014.traits.4n$X_Sp_Ap + CL2014.traits.4n$X_Sp_Met + CL2014.traits.4n$X_Sp_Fauc)/3) # spur aperture is only important at mouth, signal not overrode by averaging (?) 

CL2014.traits.2n$X_Sep <- ((CL2014.traits.2n$X_SepSx + CL2014.traits.2n$X_SepDx + CL2014.traits.2n$X_SepCen)/3) # width of sepals is not significant to fitness, apparently
CL2014.traits.2n$Y_Sep <- ((CL2014.traits.2n$Y_SepSx + CL2014.traits.2n$Y_SepDx + CL2014.traits.2n$Y_SepCen)/3)
CL2014.traits.2n$X_Pet <- ((CL2014.traits.2n$X_PetSx + CL2014.traits.2n$X_PetDx)/2)
CL2014.traits.2n$Y_Pet <- ((CL2014.traits.2n$Y_PetSx + CL2014.traits.2n$Y_PetDx)/2)
CL2014.traits.2n$X_Sp <- ((CL2014.traits.2n$X_Sp_Ap + CL2014.traits.2n$X_Sp_Met + CL2014.traits.2n$X_Sp_Fauc)/3) # spur aperture is only important at mouth, signal not overrode by averaging (?) 

# Calculate Z-Scores and relative fitness and relative n.fruits
Z.traits.4n <- scale(CL2014.traits.4n[, c(3:4,7:34)], scale = T, center = T)
Z.traits.4n <- cbind(CL2014.traits.4n[, c("ID","Ploidy")], Z.traits.4n)
Z.traits.4n$RelFruits <- CL2014.traits.4n$n.fruits/mean(CL2014.traits.4n$n.fruits, na.rm = T)
Z.traits.4n$RelFitness <- CL2014.traits.4n$Fitness/mean(CL2014.traits.4n$Fitness, na.rm = T)
summary(Z.traits.4n)

Z.traits.2n <- scale(CL2014.traits.2n[, c(3:4,7:34)], scale = T, center = T)
Z.traits.2n <- cbind(CL2014.traits.2n[, c("ID","Ploidy")], Z.traits.2n)
Z.traits.2n$RelFruits <- CL2014.traits.2n$n.fruits/mean(CL2014.traits.2n$n.fruits, na.rm = T)
Z.traits.2n$RelFitness <- CL2014.traits.2n$Fitness/mean(CL2014.traits.2n$Fitness, na.rm = T)
summary(Z.traits.2n)

library(perturb) # condition indexes and variance decomposition porportions for colinearity test
# Fit linear models: directional selection
βTetr1 <- lm(RelFitness ~ Height + n.flowers + Peak.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.4n)
βTetr2 <- lm(RelFitness ~ Height + n.flowers + Init.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.4n)
summary(βTetr1)
summary(βTetr2)
colldiag(βTetr1, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
colldiag(βTetr2, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
# Since the response variable RelFitness contains also n.flowers, fit a lm() with RelFruits as response
βTetr3 <- lm(RelFruits ~ Height + n.flowers + Peak.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.4n)
βTetr4 <- lm(RelFruits ~ Height + n.flowers + Init.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.4n)
summary(βTetr3)
summary(βTetr4)
colldiag(βTetr3, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
colldiag(βTetr4, center=T, scale=T) # evaluate colinearity problems; largest index should be <30

βDipl1 <- lm(RelFitness ~ Height + n.flowers + Peak.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.2n)
βDipl2 <- lm(RelFitness ~ Height + n.flowers + Init.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.2n)
summary(βDipl1)
summary(βDipl2)
colldiag(βDipl1, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
colldiag(βDipl2, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
# Since the response variable RelFitness contains also n.flowers, fit a lm() with RelFruits as response
βDipl3 <- lm(RelFruits ~ Height + n.flowers + Peak.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.2n)
βDipl4 <- lm(RelFruits ~ Height + n.flowers + Init.Day + X_Sp + Y_Sp + X_Lab_Max + X_LobLat + Y_Lab + X_LobMed + Y_LobMed + X_Sep + Y_Sep + X_Pet + Y_Pet, data = Z.traits.2n)
summary(βDipl3)
summary(βDipl4)
colldiag(βDipl3, center=T, scale=T) # evaluate colinearity problems; largest index should be <30
colldiag(βDipl4, center=T, scale=T) # evaluate colinearity problems; largest index should be <30

# Fit quadratic models: stabilizing or disruptive selection
Z.traits.4n.cc <- Z.traits.4n[complete.cases(Z.traits.4n),] # NAs are not allowed in polynomial regressions
γTetr1 <- lm(RelFitness ~ poly(Height, degree=2, raw=T) + poly(n.flowers, degree=2, raw=T) + poly(Peak.Day, degree=2, raw=T) + poly(X_Sp, degree=2, raw=T) + poly(Y_Sp, degree=2, raw=T) + poly(X_Lab_Max, degree=2, raw=T) + poly(X_LobLat, degree=2, raw=T) + poly(Y_Lab, degree=2, raw=T) + poly(X_LobMed, degree=2, raw=T) + poly(Y_LobMed, degree=2, raw=T) + poly(X_Sep, degree=2, raw=T) + poly(Y_Sep, degree=2, raw=T) + poly(X_Pet, degree=2, raw=T) + poly(Y_Pet, degree=2, raw=T), data = Z.traits.4n.cc)
summary(γTetr1)
γTetr2 <- lm(RelFitness ~ poly(Height, degree=2, raw=T) + poly(n.flowers, degree=2, raw=T) + poly(Init.Day, degree=2, raw=T) + poly(X_Sp, degree=2, raw=T) + poly(Y_Sp, degree=2, raw=T) + poly(X_Lab_Max, degree=2, raw=T) + poly(X_LobLat, degree=2, raw=T) + poly(Y_Lab, degree=2, raw=T) + poly(X_LobMed, degree=2, raw=T) + poly(Y_LobMed, degree=2, raw=T) + poly(X_Sep, degree=2, raw=T) + poly(Y_Sep, degree=2, raw=T) + poly(X_Pet, degree=2, raw=T) + poly(Y_Pet, degree=2, raw=T), data = Z.traits.4n.cc)
summary(γTetr2)

Z.traits.2n.cc <- Z.traits.2n[complete.cases(Z.traits.2n),] # NAs are not allowed in polynomial regressions
γDipl1 <- lm(RelFitness ~ poly(Height, degree=2, raw=T) + poly(n.flowers, degree=2, raw=T) + poly(Peak.Day, degree=2, raw=T) + poly(X_Sp, degree=2, raw=T) + poly(Y_Sp, degree=2, raw=T) + poly(X_Lab_Max, degree=2, raw=T) + poly(X_LobLat, degree=2, raw=T) + poly(Y_Lab, degree=2, raw=T) + poly(X_LobMed, degree=2, raw=T) + poly(Y_LobMed, degree=2, raw=T) + poly(X_Sep, degree=2, raw=T) + poly(Y_Sep, degree=2, raw=T) + poly(X_Pet, degree=2, raw=T) + poly(Y_Pet, degree=2, raw=T), data = Z.traits.2n.cc)
summary(γDipl1)
γDipl2 <- lm(RelFitness ~ poly(Height, degree=2, raw=T) + poly(n.flowers, degree=2, raw=T) + poly(Init.Day, degree=2, raw=T) + poly(X_Sp, degree=2, raw=T) + poly(Y_Sp, degree=2, raw=T) + poly(X_Lab_Max, degree=2, raw=T) + poly(X_LobLat, degree=2, raw=T) + poly(Y_Lab, degree=2, raw=T) + poly(X_LobMed, degree=2, raw=T) + poly(Y_LobMed, degree=2, raw=T) + poly(X_Sep, degree=2, raw=T) + poly(Y_Sep, degree=2, raw=T) + poly(X_Pet, degree=2, raw=T) + poly(Y_Pet, degree=2, raw=T), data = Z.traits.2n.cc)
summary(γDipl2)

# Selection coefficients: unstandardized directional selection
sTetr1 <- cov(Z.traits.4n.cc$RelFitness, Z.traits.4n.cc[, 3:27])
sTetr2 <- cov(Z.traits.4n.cc$RelFruits, Z.traits.4n.cc[, 3:27])
sDipl1 <- cov(Z.traits.2n.cc$RelFitness, Z.traits.2n.cc[, 3:27])
sDipl2 <- cov(Z.traits.2n.cc$RelFruits, Z.traits.2n.cc[, 3:27])
write.table(sTetr1, file = "sSelectionCoefficients.Tetr.txt")
write.table(sTetr2, file = "sSelectionCoefficients.Tetr.txt", append = T)
write.table(sDipl1, file = "sSelectionCoefficients.Dipl.txt")
write.table(sDipl2, file = "sSelectionCoefficients.Dipl.txt", append = T)

##################################################################################################