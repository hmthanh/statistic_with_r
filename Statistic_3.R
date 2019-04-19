library(ggplot2)
library(ggalt)
library(Lock5withR)
library(plyr)
library(ggthemes)
library(ggfortify)
race = as.data.frame(table(SpeedDating$RaceF))
decisionF = as.data.frame(table(SpeedDating$DecisionFemale))
decisionM = as.data.frame(table(SpeedDating$DecisionMale))
age = as.data.frame(table(SpeedDating$AgeF))
like = as.data.frame(table(SpeedDating$LikeM))

head(race)
mean(race$Freq,na.rm = TRUE)
median(race$Freq,na.rm = TRUE)
sd(race$Freq,na.rm = TRUE)
var(race$Freq,na.rm = TRUE)
ggplot(race, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity")+
  labs(x = NULL, y = NULL, fill = "Chủng tộc", title = "Chủng tộc của người phụ nữ")

head(decisionF)
mean(decisionF$Freq,na.rm = TRUE)
median(decisionF$Freq,na.rm = TRUE)
sd(decisionF$Freq,na.rm = TRUE)
var(decisionF$Freq,na.rm = TRUE)
ggplot(decisionF, aes(x ="", y = Freq, fill = Var1)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5))+
labs(x = NULL, y = NULL, fill = "Lựa chọn", title = "Người phụ nữ muốn tiếp tục hẹn hò")


head(decisionM)
mean(decisionM$Freq, na.rm = TRUE)
median(decisionM$Freq,na.rm = TRUE)
sd(decisionM$Freq,na.rm = TRUE)
var(decisionM$Freq,na.rm = TRUE)
ggplot(decisionM, aes(x ="", y = Freq, fill = Var1)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5))+
  labs(x = NULL, y = NULL, fill = "Lựa chọn", title = "Người nam giới muốn tiếp tục hẹn hò")

head(SpeedDating$AgeF)
mean(SpeedDating$AgeF, na.rm = TRUE)
median(SpeedDating$AgeF, na.rm = TRUE)
sd(SpeedDating$AgeF, na.rm = TRUE)
var(SpeedDating$AgeF, na.rm = TRUE)
ggplot(age, aes(x =Var1, y = Freq, fill=Var1)) +
  geom_bar(stat="identity", width=0.8)+
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),axis.ticks = element_blank())+
  labs(x = "Tuổi", y = "Số lượng", fill = "Độ tuổi", title = "Độ tuổi của nữ giới")


head(SpeedDating$LikeM)
mean(SpeedDating$LikeM, na.rm = TRUE)
median(SpeedDating$LikeM, na.rm = TRUE)
sd(SpeedDating$LikeM, na.rm = TRUE)
var(SpeedDating$LikeM, na.rm = TRUE)
ggplot(like, aes(x =Var1, y = Freq, fill=Var1)) +
  geom_bar(stat="identity", width=0.8)+
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),axis.ticks = element_blank())+
  labs(x = "Thang điểm", y = "Số người", fill = "Thang điểm", title = "Mức độ yêu thích của nam giới với đối phương")


#1 Khao sat 2 thuoc tinh DecitionM va DecisionF
ggplot(SpeedDating, aes(x = DecisionMale)) +
  geom_bar(aes(fill = DecisionFemale), position = "dodge")+
  labs(x = "Quyết định của nam giới", y = "Số lượng", fill = "Quyết định của nữ giới", title ="Biểu đồ thể hiện việc tiếp tục hẹn hò của nam và nữ")

ggplot(SpeedDating, aes(x = DecisionFemale)) +
  geom_bar(aes(fill = DecisionMale), position = "dodge")+
  labs(x = "Quyết định của nữ giới", y = "Số lượng", fill = "Quyết định của nam giới", title ="Biểu đồ thể hiện việc tiếp tục hẹn hò của nam và nữ")
#2 dinh tinh 1 dinh luong
ggplot(SpeedDating, aes(x=RaceF, y = LikeM))+
  geom_count(aes(color = ..n.., size = ..n..),na.rm=TRUE)+
  scale_y_continuous(breaks=seq(1, 10, 1))+
 guides(color = 'legend')+
  labs(x = "Chủng tộc", y = "Độ yêu thích", fill = "Số lượng", title ="Biểu đồ thể hiện sư tương quan giữa chủng tộc người nữ và mức độ thích của nam")
#3
likeCircle <- SpeedDating[SpeedDating$AgeF > 20 & 
                            SpeedDating$AgeF < 30 & 
                              SpeedDating$LikeM >= 6 & 
                              SpeedDating$LikeM < 10, ]

oldCircle <- SpeedDating[SpeedDating$AgeF > 32 & 
                            SpeedDating$AgeF < 35 & 
                            SpeedDating$LikeM >= 6 & 
                            SpeedDating$LikeM < 10, ]

ggplot(SpeedDating, aes(x=AgeF, y = LikeM))+
  geom_count(aes(color = ..n.., size = ..n..),na.rm=TRUE)+
  geom_encircle(aes(x=AgeF, y=LikeM), data=likeCircle, color="red", size=2, s_shape=0.5,na.rm=TRUE)+
  geom_encircle(aes(x=AgeF, y=LikeM), data=oldCircle, color="blue", size=2, s_shape=0.5,na.rm=TRUE)+
  scale_y_continuous(breaks=seq(1, 10, 1))+
  scale_x_continuous(breaks=seq(0, 60, 2))+
  guides(color = 'legend')+
  labs(x = "Độ tuổi", y = "Độ yêu thích", fill = "Số lượng", title ="Biểu đồ thể hiện sư tương quan giữa độ tuổi của nữ và mức độ thích của nam")
  
#4
ggplot(SpeedDating, aes(x = AgeF, y = LikeM)) +
  geom_point(aes(color=RaceF, fill=RaceF, group=RaceF), na.rm = TRUE)+
labs(x = "Độ tuổi", y = "Độ yêu thích", fill = "Chủng tộc", color="Chủng tộc", title ="Biểu đồ thể hiện sư tương quan giữa độ tuổi của nữ và mức độ thích của nam theo chủng tộc")

ggplot(SpeedDating, aes(x=AgeF, y = LikeM))+
  geom_count(aes(color = ..n.., size = ..n..),na.rm=TRUE)+
  scale_y_continuous(breaks=seq(1, 10, 2))+
  scale_x_continuous(breaks=seq(0, 60, 5))+
facet_wrap(~RaceF, ncol=2)+
labs(x = "Độ tuổi", y = "Độ yêu thích", fill = "Chủng tộc", color=NULL, title ="Biểu đồ thể hiện sư tương quan giữa độ tuổi của nữ và mức độ thích của nam theo chủng tộc")

ggplot(SpeedDating, aes(x = AgeF, y = LikeM, color=LikeM)) +
  geom_point(na.rm=TRUE)+
  scale_y_continuous(breaks=seq(1, 10, 2))+
  scale_x_continuous(breaks=seq(0, 60, 5))+
  facet_wrap(~RaceF)+
  guides(color = 'legend')+
  labs(x = "Độ tuổi", y = "Độ yêu thích", fill = "Độ yêu thích", color="Độ yêu thích", title ="Biểu đồ thể hiện sư tương quan giữa độ tuổi của nữ và mức độ thích của nam theo chủng tộc")
