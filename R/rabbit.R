raa=c(1.53,1.61,3.75,2.89,3.26)
rab = c(3.89,3.68,5.70,5.62,5.79)
rac = c(8.18,5.64,7.36,8.82,7.10)
rabbit <- data.frame(raa,rab,rac)
st <- c(1.53,1.61,3.75,2.89,3.26,
        3.89,3.68,5.70,5.62,5.79,
        8.18,5.64,7.36,8.82,7.10)
gr <- factor(rep(c("A", "B", "C"), c(5,5,5)))
data_rabbit <- data.frame(st,gr)
colnames(data_rabbit) <- c("Treatment", "Stimulant")


data(data_rabbit)
