setwd('~/Desktop/Evolution/Tasks/Task_09')
library(diversitree)
transition_0to1<- 0.1
transition_1to0<- 0.1
speciation_0<- 0.2
extinction_0<- 0.1
speciation_1<- 0.2
extinction_1<- 0.1
maxN<- 1e3
maxT<- 50
Pars<- c(speciation_0, speciation_1, extinction_0, extinction_1,transition_0to1,transition_1to0)
simTree<- tree.bisse(Pars, max.taxa=maxN, max.t=maxT)
str(simTree)
stateTable<- table(simTree$tip.state)
stateTable/sum(stateTable)
library(ape)
pdf("plot1_09.pdf")
plot(simTree, show.tip.label=FALSE)
dev.off()
num_trees<- 10
tree_list<- list()
for (i in 1:num_trees){
simTree<-tree.bisse(Pars, max.taxa=maxN,max.t=maxT)  
tree_list[[i]]<-simTree
}
tree_list[[1]]
