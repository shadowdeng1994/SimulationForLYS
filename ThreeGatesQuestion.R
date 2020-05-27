#### Three gates with only one prize.
#### A player chooses one gate. Then the host show an empty gate.
#### Q: Should the player choose the other door?

library(tidyverse)
library(parallel)

fun.ThreeGateQuestion <- function(iii){
	library(tidyverse)
	cat(iii,sep="\n")
	# Generate three gates and three prizes
	data.frame("Gate"=1:3,"Prize"=sample(c(0,0,1))) %>% 
	# Choose one gate by player
	mutate(FstChoose=sample(c(0,0,1))) %>% 
	# Exclude player's choose and the gate with price
	group_by(AAA=(Prize==0)*(FstChoose==0)) %>% group_by(AAA) %>% 
	# Choose by presenter
	mutate(Presenter=((AAA==1)*min_rank(runif(n()))==1) %>% as.numeric) %>% 
	# Mark the last gate
	group_by %>% select(-AAA) %>% mutate(Change=(FstChoose==0)*(Presenter==0)) %>% 
	# Announce the winner!
	gather(Winner,VVV,-Gate,-Prize) %>% group_by(Winner) %>% filter(Prize*VVV==1) %>% group_by
}

cl <- makeCluster(100)
1:100000 %>% parLapply(cl,.,fun.ThreeGateQuestion) %>% bind_row %>% group_by(Winner) %>% summarise(Count=n())


#   Winner    Count
#   <chr>     <int>
# 1 Change    66906
# 2 FstChoose 33094


