#Make taiji.png
#x-axis, y-axis of the big circle(no fill)
bigcirclex<-seq(from=-1000,to=1000,length=10000)
bigcirclex<-c(bigcirclex,rev(bigcirclex))
bigcircley<-c(sqrt(1000000-bigcirclex^2))

#Black circle
upcirclex<-bigcirclex[1:10000]
upcircley<-bigcircley[1:10000]

#White circle
downcirclex<-bigcirclex[1:10000]
downcircley<--bigcircley[1:10000]

#small white circle
leftcirlex<-seq(from=-1000,to=0,length=1000)
leftcirley<-sqrt(250000-(leftcirlex+500)^2)

#small black circle
rightcirclex<-seq(from=0,to=1000,length=1000)
rightcircley<-sqrt(250000-(rightcirclex-500)^2)

#Save as png
CairoPNG(file="taiji.png",width=500,height=500, bg = "transparent")

#plot
ggplot(data=NULL)+
  geom_area(aes(upcirclex,upcircley),fill="white",col=NA)+
  geom_area(aes(downcirclex,downcircley),fill="black",col=NA)+
  geom_area(aes(leftcirlex,-leftcirley),fill="white",col=NA)+
  geom_area(aes(rightcirclex,rightcircley),fill="black",col=NA)+
  coord_flip()+
  annotate("text",x=-500,y=0,label="●", color= "black", size=60)+
  annotate("text",x=500,y=0,label="●", color= "white", size=60)+
  geom_path(aes(bigcirclex,bigcircley),col="grey60")+
  theme_nothing()
dev.off()

# ##draw the chess board
# bg = readJPEG("bg.jpg")
# x=c(1:n)
# y=c(1:n)
# temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
# ggplot(data =NULL,color = "black") + 
#   annotation_custom(rasterGrob(bg, 
#                                width = unit(1,"npc"), 
#                                height = unit(1,"npc")), 
#                     -Inf, Inf, -Inf, Inf)+
#   geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
#   geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
#   geom_point(aes(rep(temp, 3), rep(temp, each = 3), size = 6/sqrt(n)), shape = 18, show.legend = F)+
#   theme_void()
# 
# 
# 
# ##scene_1
# taiji = readPNG("taiji.png")
# windowsFonts(JP1 = windowsFont("Pristina"),
#              JP2 = windowsFont("MS Gothic"),
#              JP3 = windowsFont("Arial Unicode MS"))
# bg = readJPEG("bg.jpg")
# colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
# colfunc1 = colorRampPalette(c("black","gray90"))
# n = 100
# x=c(1:n)
# y=c(1:n)
# ggplot(data =NULL,color = "black") + 
#   geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
#   geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
#   annotation_custom(rasterGrob(bg, 
#                                width = unit(1,"npc"), 
#                                height = unit(1,"npc")), 
#                     -Inf, Inf, -Inf, Inf)+
#   annotate("text", x = seq(33,67, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
#            size = 14, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1")+
#   annotate("text", x = seq(39,61, length.out = 6), y = rep(28,6), col = colfunc1(6), label = unlist(strsplit("BATTLE",NULL)), family = "JP1", size = 10)+
#   annotate("text",x = seq(33,67, length.out = 8), y = rep(13,8), col = colfunc1(8), family = "JP1", size = 10, label = unlist(strsplit("COMPUTER", NULL)))+
#   annotation_custom(rasterGrob(taiji),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
#   theme_void()
# 
# ##scene_2
# taiji = readPNG("taiji.png")
# windowsFonts(JP1 = windowsFont("Pristina"),
#              JP2 = windowsFont("MS Gothic"),
#              JP3 = windowsFont("Arial Unicode MS"))
# bg = readJPEG("bg.jpg")
# colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
# colfunc1 = colorRampPalette(c("black","gray90"))
# n = 100
# x=c(1:n)
# y=c(1:n)
# ggplot(data =NULL,color = "black") + 
#   geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
#   geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
#   annotation_custom(rasterGrob(bg, 
#                                width = unit(1,"npc"), 
#                                height = unit(1,"npc")), 
#                     -Inf, Inf, -Inf, Inf)+
#   annotate("text", x = seq(26,74, length.out = 8), y=rep(8*n/9,8), col = colfunc(8),
#            size = 14, label = unlist(strsplit("COMPUTER", NULL)), family = "JP1")+
#   annotate("text", x = 50, y = 28, label = "BLACK", col = "black", family = "JP1", size = 10)+
#   annotate("text", x = 50, y = 13, label = "WHITE", col = "white", size = 10, family = "JP1")+
#   annotation_custom(rasterGrob(taiji),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
#   theme_void()
# 
# ##gameover
# result = "WHITE WINS!"
# len = str_count(result)
# n = 100
# x=c(1:n)
# y=c(1:n)
# ggplot(data =NULL,color = "black") + 
#   geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
#   geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
#   annotation_custom(rasterGrob(bg, 
#                                width = unit(1,"npc"), 
#                                height = unit(1,"npc")), 
#                     -Inf, Inf, -Inf, Inf)+
#   annotate("text", x = 50, y = 8*n/9, label = result, size = 14, col = "white", family = "JP1")+
#   annotate("text", x = 50, y = 28, label = "PLAY AGAIN", col = "black", family = "JP1", size = 10)+
#   annotate("text", x = 50, y = 13, label = "QUIT", col = "white", size = 10, family = "JP1")+
#   annotation_custom(rasterGrob(img),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
#   theme_void()




