library(data.table)
library(dplyr)
library(igraph)
newyork_sci <- fread("newyork_sci_group.csv")
groups <- fread("groups.csv")
members <- fread("members.csv")
groups <- groups[city=="New York"]
members2 <- members[group_id%in%groups$group_id]
#fwrite(members,"newyork_sci_members.csv")
length(unique(members$group_id))
affi <- members[,c(1,14)]

########################### internal edgelist of sci-fiction groups ################################

edgelist <- left_join(affi,affi,by="member_id")
edgelist <- filter(edgelist,group_id.x<group_id.y)
edgelist <- as.data.table(edgelist)
edgelist[,weight:=length(member_id),by=c("group_id.x","group_id.y")]
edge_list <- unique(edgelist[,c(2:4)])

length(unique(union(edge_list$group_id.x,edge_list$group_id.y)))

length(unique(edge_list$group_id.x))

coordinate <- data.frame(group_id=numeric(28))
coordinate$group_id<-unique(union(edge_list$group_id.x,edge_list$group_id.y))
coordinate$LineX<-runif(28, min = 0, max = 100)
coordinate$LineY<-runif(28, min = 0, max = 100)
edge_list1 <- left_join(edge_list,coordinate,by=c("group_id.x"="group_id"))
#fwrite(edge_list1,"edge_list1.csv")
edge_list2 <- left_join(edge_list,coordinate,by=c("group_id.y"="group_id"))
#fwrite(edge_list2,"edge_list2.csv")

edge_list1$group_id.x <- as.character(edge_list1$group_id.x)
edge_list1$group_id.y <- as.character(edge_list1$group_id.y)
g <- graph_from_edgelist(as.matrix(edge_list1[,c(1:2)]))
E(g)$weight <- edge_list1$weight


coordinate$degree <- degree(g)
coordinate$coreness <- coreness(g)

coordinate$group_id <- as.character(coordinate$group_id)

edge_list2$group_id.x <- as.character(edge_list2$group_id.x)
edge_list2$group_id.y <- as.character(edge_list2$group_id.y)

edge_list11 <- left_join(edge_list1,coordinate[,c(1,4,5)],by=c("group_id.x"="group_id"))
fwrite(edge_list11,"edge_list11.csv")
edge_list22 <- left_join(edge_list2,coordinate[,c(1,4,5)],by=c("group_id.y"="group_id"))
fwrite(edge_list22,"edge_list22.csv")


newyork_sci$group_id <- as.character(newyork_sci$group_id)
sci_group_info <- left_join(coordinate,newyork_sci[,c("group_id","group_name","created","description","link","rating")],by=c("group_id"="group_id"))
sci_group_info <- sci_group_info[,c(1,4:10)]
fwrite(sci_group_info,"sci_group_info.csv")


########################### regression for all groups in NYC ################################

coordinate <- data.frame(group_id=numeric(6458))
coordinate$group_id<-unique(union(edge_list$group_id.x,edge_list$group_id.y))

edge_list1 <- left_join(edge_list,coordinate,by=c("group_id.x"="group_id"))
#fwrite(edge_list1,"edge_list1.csv")


edge_list1$group_id.x <- as.character(edge_list1$group_id.x)
edge_list1$group_id.y <- as.character(edge_list1$group_id.y)
g <- graph_from_edgelist(as.matrix(edge_list1[,c(1:2)]))
E(g)$weight <- edge_list1$weight
get.data.frame(g)
degree(g)
sort(degree(g))
sort(eigen_centrality(g)$vector)

coordinate$degree <- degree(g)
coordinate$eigen <- eigen_centrality(g)$vector
coordinate$betweenness <- betweenness(g)
coordinate$coreness <- coreness(g)

coordinate$group_id <- as.character(coordinate$group_id)
groups$group_id <- as.character(groups$group_id)
coordinate2 <- left_join(coordinate,groups)

edge_list2$group_id.x <- as.character(edge_list2$group_id.x)
edge_list2$group_id.y <- as.character(edge_list2$group_id.y)

edge_list11 <- left_join(edge_list1,coordinate[,c(1,4,5)],by=c("group_id.x"="group_id"))
fwrite(edge_list11,"edge_list11.csv")
edge_list22 <- left_join(edge_list2,coordinate[,c(1,4,5)],by=c("group_id.y"="group_id"))
fwrite(edge_list22,"edge_list22.csv")


newyork_sci$group_id <- as.character(newyork_sci$group_id)
sci_group_info <- left_join(coordinate,newyork_sci[,c("group_id","group_name","created","description","link","rating")],by=c("group_id"="group_id"))
sci_group_info <- sci_group_info[,c(1,4:10)]

play <- lm(rating~degree+eigen+betweenness+coreness,coordinate2)
summary(play)


########################### count other interests than sci-fiction ################################

members2 <- members[group_id%in%newyork_sci$group_id]
member <- members[group_id==8940,member_id]

cat <- data.table(group_id=0,category_id=0)
for (mid in member){
  group <- as.data.frame(members2[member_id==mid,group_id])
  colnames(group)="group_id"
  group2 <- left_join(group,groups[,c(1,2)])
  group3 <- as.data.table(group2)[category_id!=29]
  cat <- rbind(cat,group3)
}
cat <- cat[group_id!=0]

#table(cat$category_id)
catcom <- as.data.frame(table(cat$category_id))
catcom$group_id=8940
names(catcom)=c("other_category","freq","group_id")


for (gid in c(137667, 147811, 3797562, 18524790,22488816)){
  member <- members[group_id==gid,member_id]
  cat <- data.table(group_id=0,category_id=0)
  for (mid in member){
    group <- as.data.frame(members2[member_id==mid,group_id])
    colnames(group)="group_id"
    group2 <- left_join(group,groups[,c(1,2)])
    group3 <- as.data.table(group2)[category_id!=29]
    cat <- rbind(cat,group3)
  }
  cat <- cat[group_id!=0]
  
  #table(cat$category_id)
  catcomb <- as.data.frame(table(cat$category_id))
  catcomb$group_id=gid
  names(catcomb)=c("other_category","freq","group_id")
  catcom <- rbind(catcom,catcomb)
}


fwrite(catcom,"otherinterest.csv")
