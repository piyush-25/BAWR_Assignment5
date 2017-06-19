
data_cars=mtcars
View(data_cars)
# Question-1

hp_rank=data_cars[order(data_cars$hp, decreasing=TRUE),]
hp_rank$rank=c(1:32)
hp_rank

wt_rank=data_cars[order(data_cars$wt, decreasing=TRUE),]
wt_rank$rank=c(1:32)
wt_rank

disp_rank=data_cars[order(data_cars$disp, decreasing=TRUE),]
disp_rank$rank=c(1:32)
disp_rank

drat_rank=data_cars[order(data_cars$drat, decreasing=TRUE),]
drat_rank$rank=c(1:32)
drat_rank

mpg_rank=data_cars[order(data_cars$mpg, decreasing=TRUE),]
mpg_rank$rank=c(1:32)
mpg_rank

# Question-2
data_cars %>%
  group_by(gear) %>%
  summarise(avg_mpg=mean(mpg)) %>%
  mutate(rank=rank(avg_mpg)) %>%
  arrange(desc(rank))

# Question-3
uniq=sort(unique(data_cars$gear))
parameter=c("hp","wt","mpg","disp")
for(y in parameter)
{
  name=paste(y,"_gear",sep="")
  for(x in uniq )
  {
    if(x==3)
    {
      geardata=data_cars[data_cars$gear==x,]
      assign=geardata[which.max(geardata[,y]),]
    }
    else
    {
      a=x-2
      geardata=data_cars[data_cars$gear==x,]
      assign[a,]=geardata[which.max(geardata[,y]),]
    }
  }
  assign(name,assign)
}
print(hp_gear)
print(wt_gear)
print(mpg_gear)
print(disp_gear)
