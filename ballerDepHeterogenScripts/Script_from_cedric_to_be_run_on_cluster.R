source('~/from_cedric/prs_functions.R')
remote_wkdir <- '/data/joy/BBL/studies/pnc/'
load('/data/jux/BBL/projects/prsConnectivity/result/sample_qa_newkey.RData')
test_sample <- fc_sample_qa_newkey[1:10,]
test <- get_net_from_sample(sample = test_sample,parcellation = 'schaefer',resolution = 200,modality = 'fc')
test_matrix <- list_to_matrix(list = test)
test_average <- apply(test_matrix,c(2,3),function(adj) mean(adj,na.rm=T))
plot <- better_levelplot(adj = test_average,node_names = NULL,title = 'Erica\'s first matrix')
plot <- better_levelplot(adj = test_average,node_names = community_info$NodeName$V1,title = 'Erica\'s first matrix')

### 2.  calculate within/between community ###
community_info <-  get_schaefer_node_info(resolution = 200,num_com = 7)

com_net_names <- get_community_net_names(community_info$CommunityName)
com_net<-lapply(test,function(netmat) get_community_net(netmat,community_info$CommunityName,community_info$CommunityAffiliation))
plot_com <- better_levelplot(adj = com_net[[1]],node_names = community_info$CommunityName,title = 'Erica\'s 2nd matrix')
com_net_flat<-t(sapply(com_net,function(net) net[upper.tri(net,diag = TRUE)]))
colnames(com_net_flat) <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
com_net_names_flat <- com_net_names[upper.tri(com_net_names,diag = TRUE)]
com_net_flat <- cbind(com_net_flat,test_sample)
