library(data.table)
data <- fread("list_language_orig.txt")

total_listeners <- sum(data$count)

x <- data$language

data$number <- (nchar(x)-nchar(gsub(',','',x)))+1 # Adds a new column to the data frame 
sort(unique(data$number))

data1 <- subset(data,data$number==1)
data2 <- subset(data,data$number==2)
data3 <- subset(data,data$number==3)
data4 <- subset(data,data$number==4)
data5 <- subset(data,data$number==5)
data6 <- subset(data,data$number==6)
data7 <- subset(data,data$number==7)
data8 <- subset(data,data$number==8)
data9 <- subset(data,data$number==9)
data10 <- subset(data,data$number==10)
data11 <- subset(data,data$number==11)
data12 <- subset(data,data$number==12)
data13 <- subset(data,data$number==13)
data16 <- subset(data,data$number==16)
data18 <- subset(data,data$number==18)
data19 <- subset(data,data$number==19)
data20 <- subset(data,data$number==20)
data21 <- subset(data,data$number==21)
data24 <- subset(data,data$number==24)
data25 <- subset(data,data$number==25)
data26 <- subset(data,data$number==26)
data29 <- subset(data,data$number==29)
data32 <- subset(data,data$number==32)
data35 <- subset(data,data$number==35)
data36 <- subset(data,data$number==36)
data59 <- subset(data,data$number==59)
data80 <- subset(data,data$number==80)
data97 <- subset(data,data$number==97)
data123 <- subset(data,data$number==123)
data134 <- subset(data,data$number==134)
data135 <- subset(data,data$number==135)


d1 <- sum(data1$count)
d2 <- sum(data2$count)
d3 <- sum(data3$count)
d4 <- sum(data4$count)
d5 <- sum(data5$count)
d6 <- sum(data6$count)
d7 <- sum(data7$count)
d8 <- sum(data8$count)
d9 <- sum(data9$count)
d10 <- sum(data10$count)
d11 <- sum(data11$count)
d12 <- sum(data12$count)
d13 <- sum(data13$count)
d16 <- sum(data16$count)
d18 <- sum(data18$count)
d19 <- sum(data19$count)
d20 <- sum(data20$count)
d21 <- sum(data21$count)
d24 <- sum(data24$count)
d25 <- sum(data25$count)
d26 <- sum(data26$count)
d29 <- sum(data29$count)
d32 <- sum(data32$count)
d35 <- sum(data35$count)
d36 <- sum(data36$count)
d59 <- sum(data59$count)
d80 <- sum(data80$count)
d97 <- sum(data97$count)
d123 <- sum(data123$count)
d134 <- sum(data134$count)
d135 <- sum(data135$count)

numlis <- c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d16,d18,d19,d20,d21,d24,d25,d26,d29,d32,d35,d36,d59,d80,d97,d123,d134,d135)
numlang <- sort(unique(data$number))

final_data <- cbind(numlis,numlang)
final_data <- as.data.frame(final_data)
final_data$numlang <- as.factor(final_data$numlang)

fd <- final_data[1:5,]
library(ggplot2)
g <- ggplot(finaldata,aes(numlang,numlis,fill=numlang))+geom_bar(stat="identity",color="black",width=0.4)+theme_bw()+labs(x="Number of languages",y="Number of Listeners",title="Total number of languages spoken by Listeners")
g+scale_fill_brewer(palette="Spectral")+theme(legend.position="none")+geom_text(aes(label=c(70.5,22.5,5.1,1.3,0.3,0.3),vjust=-0.3,size=1.5))


colnames(ab) <- c("numlis","numlang")
finaldata <- rbind(final_data,ab)

str(finaldata)

finaldata <- finaldata[-6:-31,]
row.names(finaldata) <- NULL
