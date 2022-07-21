#Normal WSCI
WS_CI = function(anova_results){
  k = anova_results$ANOVA$DFn[2]+1
  N = anova_results$ANOVA$DFd[1]+1
  SSe = anova_results$ANOVA$SSd[2]
  return(sqrt(SSe/(N*(N-1)*k))*(qt(0.95,(k*(N-1)))))}

WS_CI_Ind = function(norm_data){
  condition_deviations = matrix(NA,dim(norm_data)[1],dim(norm_data)[2])
  Condition_CI = matrix(NA,dim(norm_data)[2])
  condition_means = colMeans(norm_data)
  for (counter in 1:dim(norm_data)[2]){
    for (count in 1:dim(norm_data)[1]){
      condition_deviations[count,counter] = (norm_data[count,counter]-condition_means[counter])^2}
    Condition_CI[counter] = qt(0.95,dim(norm_data)[1]-1)*sqrt(sum(condition_deviations[,counter])/((dim(norm_data)[1])*(dim(norm_data)[1]-1)))}
  return(Condition_CI)}