library(XML)
getzhihuq<-function(){
  #id<-edit()
  #q_sid<-edit()
  #q_eid<-edit()
  back<-keyword()
  n<-length(back)
  allanswer<-data.frame()
  turl<-"http://www.zhihu.com"
  for(i in 1:n){
    url<-gsub(" ","",paste(turl,back[i]))
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
    id<-rep(substr(back[i],11,18),length(answer))
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

keyword<-function(){
  key_word<-iconv(as.character(edit()),"utf-8","gbk")
  mainurl<-"http://www.zhihu.com/search?q="
  aimurl<-gsub(" ","",paste(mainurl,key_word,"&type=question"))
  content<-htmlParse(aimurl)
  link<-xpathSApply(content,"//div[@class='title']/a[@class='question-link']",xmlGetAttr,"href")
  return(link)
}
#R version 3.2.0
