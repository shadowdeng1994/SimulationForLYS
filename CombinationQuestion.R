#### Sampling <ddd> numbers from <fff> to <ttt>, with replacement or not, to compose a <ddd>-digit number randomly.
#### Calculating the frequency of the ten's place larger than the unit's place.


library(tidyverse)

fun_GetCombinationFromList <- function(ListIn){
	tmp.out <- ListIn[[1]] %>% tbl_df %>% mutate(AAA=1)
	for(i in 2:length(ListIn)){ tmp.out <- tmp.out %>% left_join(ListIn[[i]] %>% tbl_df %>% mutate(AAA=1),by="AAA")}
	tmp.out <- tmp.out %>% select(-AAA)
	if(length(names(ListIn))>0){
		tmp.names <- names(ListIn)
	}else{
		tmp.names <- paste0("V",1:length(ListIn))
	}
	colnames(tmp.out) <- tmp.names
	return(tmp.out)
}

0:5 %>% list(.,.,.,.,.) %>% fun_GetCombinationFromList %>% mutate(ID=1:n()) %>% gather(Dim,Value,-ID) %>% arrange(ID,Dim) %>% group_by %>% spread(Dim,Value) %>% filter(V5>0) %>% group_by((V1>V2)-(V1<V2)) %>% summarise(Count=n())


fff <- 0
ttt <- 5
ddd <- 5

lapply(1:ddd,function(x){ fff:ttt }) %>% fun_GetCombinationFromList %>% mutate(ID=1:n()) %>% gather(Dim,Value,-ID) %>% arrange(ID,Dim) %>% group_by(ID) %>% mutate(uuu=max(dense_rank(Value))==ddd) %>% group_by %>% spread(Dim,Value) %>% filter(V1>0) %>% filter(uuu) %>% group_by(Group=(V2>V3)-(V2<V3)) %>% summarise(Count=n()) %>% mutate(Faction=paste0(Count/sum(Count)*2*(ttt-fff+1),"/",2*(ttt-fff+1)))

lapply(1:ddd,function(x){ fff:ttt }) %>% fun_GetCombinationFromList %>% mutate(ID=1:n()) %>% gather(Dim,Value,-ID) %>% arrange(ID,Dim) %>% group_by(ID) %>% mutate(uuu=max(dense_rank(Value))==ddd) %>% group_by %>% spread(Dim,Value) %>% filter(V1>0) %>% group_by(Group=(V2>V3)-(V2<V3)) %>% summarise(Count=n()) %>% mutate(Faction=paste0(Count/sum(Count)*2*(ttt-fff+1),"/",2*(ttt-fff+1)))

