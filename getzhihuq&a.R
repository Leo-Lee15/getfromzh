
library(XML)
getzhihuq<-function(){
  #id<-edit()
  q_sid<-edit()
  q_eid<-edit()
  allanswer<-data.frame()
  turl<-"http://www.zhihu.com/question/"
  for(i in q_sid:q_eid){
    url<-gsub(" ","",paste(turl,i))
    err<-try(htmlParse(url),silent = T)
    if ('try-error' %in% class(err)){
      print(2)
      next
    }
    doc<-xmlRoot(htmlParse(url))
    answer<-gsub("\n","",xpathSApply(doc,"//div[@class='fixed-summary zm-editable-content clearfix']",xmlValue))
    user<-gsub("\n","",xpathSApply(doc,"//h3[@class='zm-item-answer-author-wrap']/a[@data-tip]",xmlValue))
    b<-""
    user<-setdiff(user,b)
    id<-rep(i,length(answer))
    title<-gsub("\n","",xpathSApply(doc,"//div[@id='zh-question-title']",xmlValue))
    title<-rep(title,length(answer))
    if(length(user)!=length(answer)){
      print(3)
      next
    }
    data<-data.frame(user,answer,id,title)
    allanswer<-rbind(allanswer,data)
  }
  return(allanswer)
}
#R version 3.2.0