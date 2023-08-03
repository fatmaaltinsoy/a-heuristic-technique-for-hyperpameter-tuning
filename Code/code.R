library(rlist)
library("base")
library("tm")
mydata <- read.csv("C:/Users/Desktop/stack/question1.csv")
tagData<- read.csv("C:/Users/Desktop/stack/Tags.csv") 

##The function calculating the number of words in a sentence
countWords <- function(x){
	issue <- sapply(strsplit(x, " "), length)
return (issue)
}
folderCount <- 150
k=1

##########################
##labeling.R run
###########################
############################################
while(k<folderCount){
###title
new<-unlist(mydata[8], use.names=FALSE)
##text
new2 <-unlist(mydata[7], use.names=FALSE)
##bu ayarlama alti adet dokuman cikarir tdm de
new <- as.character(new[k])
new2 <- as.character(new2[k])
###word and char count
wordCount <- countWords(new)
wordCount <- wordCount+countWords(new2)
vektor <-c(wordCount)
names(vektor) <- "wordCount"
charCount <- nchar(new)
charCount <- charCount+nchar(new2)
vektor2 <- c(charCount)
###############word and charcount end
corpus <- c(new,new2)
tm_corpus <- Corpus(VectorSource(corpus))
tm_corpus <- tm_map(tm_corpus, tolower)
#mystopwords <- c(stopwords('english'), "never", "<strong>")
#tm_corpus <- tm_map(tm_corpus, removeWords, mystopwords)
tm_corpus <- tm_map(tm_corpus, removePunctuation)
#tm_corpus <- tm_map(tm_corpus, toSpace, "<") 
tm_corpus <- tm_map(tm_corpus, removeNumbers)
tm_corpus <- tm_map(tm_corpus, stripWhitespace)
tdm <- TermDocumentMatrix(tm_corpus)
x<-as.matrix(tdm)

##kelime sikliklari
freq <- rowSums(x)
write.csv(freq,"C:/Users/Desktop/stack/freq.csv")

###satirlari sutun sutunlari satir yapiyoruz her kelimeyi ozellik
##olarak almak icin
 jk <- read.csv("C:/Users/Desktop/stack/freq.csv")
jk2 <- data.frame(t(jk[-1]),vektor,vektor2)
index <- length(jk2)
index2 <- index-1
colnames(jk2) <- jk[, 1]
#write.csv(jk2,"freq.csv")
names(jk2)[index] <- "charCount"
names(jk2)[index2] <- "wordCount"
 write.csv(jk2, file = paste0("C:/Users/Desktop/stack/freq",k, ".csv"))
k<- k+1
}

##########dosyalar birlesitiriliyor
library(plyr)
i=1
veriList <- list()
while(i<folderCount){
mydata2 <- read.csv(paste0("C:/Users/Desktop/stack/freq",i, ".csv"))
veriList=list.append(veriList,mydata2)
i
i<- i+1
}
result <- do.call('rbind.fill',veriList)
result[is.na(result)] = 0
###data frame e listeyi donusturuyoruz
result <- as.data.frame(result)
##birinci sutunda x ler var bunlari siliyoruz
result <- result[,-1]
write.csv(result,"C:/Users/Desktop/stack/result.csv")
result
########ozellik birlestirme sonu###
###########kural tabanli labeling bolumu#########
value <- colnames(result)
col <- length(result[1,])
row <- length(result[,1])

i=1
j=1
arrayCount=0
domCount=0
javascriptCount=0
jqueryCount=0
groupCount=0
orderCount=0
selectCount=0
sqlCount=0
javaCount=0
jarCount=0
netCount=0
aspCount=0
djangoCount=0
pyCount=0
pythonCount=0
problemCount=0
instanceCount=0
caseCount=0
compilerCount=0
cppCount=0
stdCount=0
addressCount=0
charCount=0
gccCount=0
mysqlCount=0
phpCount=0
activerecordCount=0
gemCount=0
hashCount=0
blockCount=0
betaCount=0
appleCount=0
iosCount=0
cocoaCount=0
datumCount=0
delegateCount=0
formCount=0
cgiCount=0
cpanCount=0
awkCount=0
bashCount=0
binCount=0
alignCount=0
backgroundCount=0
bodyCount=0
actorCount=0
applicationCount=0
cabalCount=0
dataframeCount=0
datasetCount=0
factorCount=0
attemptCount=0
buttonCount=0
classCount=0
codeCount=0
baslangicCount=0
ortaCount=0
yuksekCount=0
javascript <- c()
sql <- c()
java <- c()
csharp <- c()
python <- c()
cplus <- c()
c <- c()
php <- c()
ruby <- c()
swift <- c()
objectivec <- c()
visualbasic <- c()
perl <- c()
bash <- c()
css <- c()
scala<- c()
html <- c()
lua <- c()
haskell <- c()
markdown <- c()
r <- c()
experience <- c()
book <- c()
while(i<=row)
{

	while(j<=col)
	{
		if(value[j]=="array")
		arrayCount <- arrayCount+result[i,j]
		if(value[j]=="dom")
		domCount <- domCount+result[i,j]
		if(value[j]=="javascript")
		javascriptCount <- javascriptCount+result[i,j]
		if(value[j]=="jquery")
		jqueryCount <- jqueryCount+result[i,j]
		if(value[j]=="group")
		groupCount <- groupCount+result[i,j]
		if(value[j]=="order")
		orderCount <- orderCount+result[i,j]
		if(value[j]=="select")
		selectCount <- selectCount+result[i,j]
		if(value[j]=="sql")
		sqlCount <- sqlCount+result[i,j]
		if(value[j]=="java")
		javaCount <- javaCount+result[i,j]
		if(value[j]=="jar")
		jarCount <- jarCount+result[i,j]
		if(value[j]=="net")
		netCount <- netCount+result[i,j]
		if(value[j]=="asp")
		aspCount <- aspCount+result[i,j]
		if(value[j]=="instance")
		instanceCount <- instanceCount+result[i,j]
		if(value[j]=="django")
		djangoCount <- djangoCount+result[i,j]
		if(value[j]=="py")
		pyCount <- pyCount+result[i,j]
		if(value[j]=="python")
		pythonCount <- pythonCount+result[i,j]
		if(value[j]=="problem")
		problemCount <- problemCount+result[i,j]
		if(value[j]=="case")
		caseCount <- caseCount+result[i,j]
		if(value[j]=="cpp")
		cppCount <- cppCount+result[i,j]
		if(value[j]=="std")
		stdCount <- stdCount+result[i,j]
		if(value[j]=="compiler")
		compilerCount <- compilerCount+result[i,j]
			if(value[j]=="address")
		addressCount <- addressCount+result[i,j]
				if(value[j]=="char")
		charCount <- charCount+result[i,j]
		if(value[j]=="gcc")
		gccCount <- gccCount+result[i,j]
		if(value[j]=="mysql")
		mysqlCount <- mysqlCount+result[i,j]
		if(value[j]=="php")
		phpCount <- phpCount+result[i,j]
		if(value[j]=="activerecord")
		activerecordCount <- activerecordCount+result[i,j]
		if(value[j]=="gem")
		gemCount <- gemCount+result[i,j]
		if(value[j]=="hash")
		hashCount <- hashCount+result[i,j]
			if(value[j]=="block")
		blockCount <- blockCount+result[i,j]
	if(value[j]=="beta")
		betaCount <- betaCount+result[i,j]
	if(value[j]=="ios")
		iosCount <- iosCount+result[i,j]
	if(value[j]=="apple")
		appleCount <- appleCount+result[i,j]	
	if(value[j]=="cocoa")
		cocoaCount <- cocoaCount+result[i,j]
	if(value[j]=="datum")
		datumCount <- datumCount+result[i,j]
	if(value[j]=="delegate")
		delegateCount <- delegateCount+result[i,j]
	if(value[j]=="form")
		formCount <- formCount+result[i,j]
	if(value[j]=="cgi")
		cgiCount <- cgiCount+result[i,j]
	if(value[j]=="cpan")
		cpanCount <- cpanCount+result[i,j]
	if(value[j]=="awk")
		awkCount <- awkCount+result[i,j]
	if(value[j]=="bash")
		bashCount <- bashCount+result[i,j]
	if(value[j]=="align")
		alignCount <- alignCount+result[i,j]
	if(value[j]=="background")
		backgroundCount <- backgroundCount+result[i,j]
	if(value[j]=="body")
		bodyCount <- bodyCount+result[i,j]	
	if(value[j]=="actor")
		actorCount <- actorCount+result[i,j]	
	if(value[j]=="cabal")
		cabalCount <- cabalCount+result[i,j]
		if(value[j]=="dataframe")
		dataframeCount <- dataframeCount+result[i,j]
	if(value[j]=="dataset")
		datasetCount <- datasetCount+result[i,j]
	if(value[j]=="factor")
		factorCount <- factorCount+result[i,j]
	if(value[j]=="application")
		applicationCount <- applicationCount+result[i,j]
	if(value[j]=="attempt")
		attemptCount <- attemptCount+result[i,j]
	if(value[j]=="button")
		buttonCount <- buttonCount+result[i,j]
	if(value[j]=="class")
	  classCount <- classCount+result[i,j]
	if(value[j]=="tag"|| value[j]=="columns" || value[j]=="datastructures" || value[j]=="string" || value[j]=="textbox" || value[j]=="hashcode" || value[j]=="diagrams"  || value[j]=="setup" || value[j]=="table" || value[j]=="foreign" || value[j]=="array" || value[j]=="function" || value[j]=="variable"|| value[j]=="debug" || value[j]=="DataGrid"|| value[j]=="GridView" || value[j]=="class"  || value[j]=="select" || value[j]=="scripts" )
	  baslangicCount <- baslangicCount+result [i,j]
	if(value[j]=="LINQ"|| value[j]=="threads" || value[j]=="package" || value[j]=="method" || value[j]=="enum" || value[j]=="domain" || value[j]=="firewall" || value[j]=="plugin" || value[j]=="reports" || value[j]=="permissions" || value[j]=="tools" || value[j]=="cookie"|| value[j]=="paging" || value[j]=="loop"  || value[j]=="pack" || value[j]=="json" || value[j]=="encryption")
	  ortaCount <- ortaCount+result [i,j]
	if(value[j]=="stored"|| value[j]=="trigger" || value[j]=="library" || value[j]=="WCF" || value[j]=="REST" || value[j]=="SOAP" || value[j]=="services"|| value[j]=="buffers" || value[j]=="protocols" || value[j]=="blazor")
	  yuksekCount <- yuksekCount+result [i,j]
		j=j+1	
	}
	result1 <- arrayCount+domCount+javascriptCount+jqueryCount
	result2 <- groupCount+orderCount+selectCount+sqlCount
	result3 <- javaCount+jarCount
	result4 <- netCount+aspCount+instanceCount
	result5 <- djangoCount+pyCount+pythonCount+problemCount+caseCount
	result6 <- cppCount+stdCount+compilerCount
	result7 <- addressCount+charCount+gccCount
	result8 <- mysqlCount+phpCount
	result9 <- activerecordCount+gemCount+hashCount+blockCount
	result10 <- betaCount + appleCount + iosCount
	result11 <- cocoaCount + datumCount + delegateCount
	result12 <- formCount+aspCount
	result13 <- cpanCount+cgiCount+caseCount
	result14 <- awkCount+bashCount+binCount
	result15 <- alignCount+backgroundCount+bodyCount
	result16 <- actorCount+applicationCount+arrayCount
	result18 <- attemptCount+buttonCount+arrayCount
	result19 <- cabalCount+caseCount+classCount
	result20 <- codeCount+classCount+bodyCount
	result21 <- dataframeCount+factorCount+datasetCount
	result22<- baslangicCount
	result23 <- ortaCount
	result24 <-yuksekCount
	result1 <- if (result1>=1 || tagHesaplaJs(mydata[i,2])==1) 1 else 0
	result2 <- if (result2>=2 || tagHesaplaSql(mydata[i,2])==1) 1 else 0
	result3 <- if (result3>=1 || tagHesaplaJava(mydata[i,2])==1) 1 else 0
	result4 <- if (result4>=2 || tagHesaplaSharp(mydata[i,2])==1) 1 else 0
	result5 <- if (result5>=1 || tagHesaplaPy(mydata[i,2])==1) 1 else 0
	result6 <- if (result6>=1 || tagHesaplaCpp(mydata[i,2])==1) 1 else 0
	result7 <- if (result7>=1 || tagHesaplaC(mydata[i,2])==1) 1 else 0
	result8 <- if (result8>=1 || tagHesaplaphp(mydata[i,2])==1) 1 else 0
	result9 <- if (result9>=1 || tagHesaplaRuby(mydata[i,2])==1) 1 else 0
	result10 <- if(result10>=1 || tagHesaplaSwift(mydata[i,2])==1) 1 else 0
	result11 <- if(result11>=1 || tagHesaplaObjective(mydata[i,2])==1) 1 else 0
	result12 <- if(result12>=2 || tagHesaplaVb(mydata[i,2])==1) 1 else 0
	result13 <- if(result13>=2 || tagHesaplaPerl(mydata[i,2])==1) 1 else 0
	result14 <- if(result14>=1 || tagHesaplaBash(mydata[i,2])==1) 1 else 0
	result15 <- if(result15>=1 || tagHesaplaCss(mydata[i,2])==1) 1 else 0
	result16 <- if(result16>=1 || tagHesaplaScala(mydata[i,2])==1) 1 else 0
	result17 <- if(tagHesaplaHtml(mydata[i,2])==1) 1 else 0
	result18 <- if(result18>=1 || tagHesaplaLua(mydata[i,2])==1) 1 else 0
	result19 <- if(result19>=1 || tagHesaplaHaskell(mydata[i,2])==1) 1 else 0
	result20 <- if(result20>=1 || tagHesaplaMarkdown(mydata[i,2])==1) 1 else 0
	result21 <- if(result21>=1 || tagHesaplaR(mydata[i,2])==1) 1 else 0
	result222 <- if(result22>=1) 1  else if (result23>=1) 2 else if (result24>=1) 3 else 0
	result25<- if (result1>=1 && result22>=1) 11 else if (result1>=1 && result23>=1) 12 else if (result1>=1 && result24>=1) 13 else if (result2>=1 && result22>=1) 21 else if (result2>=1 && result23>=1) 22 else if  (result2>=1 && result24>=1) 23 else if (result3>=1 && result22>=1) 31 else if (result3>=1 && result23>=1) 32 else if  (result3>=1 && result24>=1) 33 else if (result4>=1 && result22>=1) 41 else if (result4>=1 && result23>=1) 42 else if  (result4>=1 && result24>=1) 43 else if (result5>=1 && result22>=1) 51 else if (result5>=1 && result23>=1) 52 else if  (result5>=1 && result24>=1) 53 else if  (result6>=1 && result22>=1) 61 else if  (result6>=1 && result23>=1) 62 else if  (result6>=1 && result24>=1) 63 else if  (result7>=1 && result22>=1) 71 else if  (result7>=1 && result23>=1) 72 else if  (result7>=1 && result24>=1) 73  else if (result8>=1 && result22>=1) 81 else if (result8>=1 && result23>=1) 82 else if (result9>=1 && result24>=1) 83 else if (result9>=1 && result22>=1) 91 else if (result9>=1 && result23>=1) 92 else if (result9>=1 && result24>=1) 93 else if (result10>=1 && result22>=1) 101 else if (result10>=1 && result23>=1) 102 else if (result10>=1 && result24>=1) 103 else if (result11>=1 && result22>=1) 111 else if (result11>=1 && result23>=1) 112 else if (result11>=1 && result24>=1) 113 else if (result12>=1 && result22>=1) 121 else if (result12>=1 && result23>=1) 122 else if (result12>=1 && result24>=1) 123 else if (result13>=1 && result22>=1) 131 else if (result13>=1 && result23>=1) 132 else if (result13>=1 && result24>=1) 133 	 else if (result8>=1 && result22>=1) 81 else if (result8>=1 && result23>=1) 82 else if (result9>=1 && result24>=1) 83 else if (result9>=1 && result22>=1) 91 else if (result9>=1 && result23>=1) 92 else if (result9>=1 && result24>=1) 93 else if (result10>=1 && result22>=1) 101 else if (result10>=1 && result23>=1) 102 else if (result10>=1 && result24>=1) 103 else if (result11>=1 && result22>=1) 111 else if (result11>=1 && result23>=1) 112 else if (result11>=1 && result24>=1) 113 else if (result12>=1 && result22>=1) 121 else if (result12>=1 && result23>=1) 122 else if (result12>=1 && result24>=1) 123 else if (result13>=1 && result22>=1) 131 else if (result13>=1 && result23>=1) 132 else if (result13>=1 && result24>=1) 133 else if (result14>=1 && result22>=1) 141 else if (result14>=1 && result23>=1) 142 else if (result14>=1 && result24>=1) 143 else if (result15>=1 && result22>=1) 151 else if (result15>=1 && result23>=1) 152 else if (result15>=1 && result24>=1) 153 else if (result16>=1 && result22>=1) 161 else if (result16>=1 && result23>=1) 162 else if (result16>=1 && result24>=1) 163 else if (result17>=1 && result22>=1) 171 else if (result17>=1 && result23>=1) 172 else if (result17>=1 && result24>=1) 173 else if (result18>=1 && result22>=1) 181 else if (result18>=1 && result23>=1) 182 else if (result18>=1 && result24>=1) 183 else if (result19>=1 && result22>=1) 191 else if (result19>=1 && result23>=1) 192 else if (result19>=1 && result24>=1) 193 else if (result20>=1 && result22>=1) 201 else if (result20>=1 && result23>=1) 202 else if (result20>=1 && result24>=1) 203 else if (result21>=1 && result22>=1) 211 else if (result21>=1 && result23>=1) 212 else if (result21>=1 && result24>=1) 213 else 0
	
		javascript <- append(javascript,result1)
		sql <- append(sql,result2)
		java <- append(java,result3)
		csharp <- append(csharp,result4)
		python <- append(python,result5)
		cplus <- append(cplus,result6)
		c <- append(c,result7)
		php <- append(php,result8)
		ruby <- append(ruby,result9)
		swift <- append(swift,result10)
		objectivec <- append(objectivec,result11)
		visualbasic <- append(visualbasic,result12)
		perl <- append(perl,result13)
		bash <- append(bash,result14)
		css <- append(css,result15)
		scala <- append(scala,result16)
		html <- append(html,result17)
		lua <- append(lua,result18)
		haskell <- append(haskell,result19)
		markdown <- append(markdown,result20)
		r <- append(r,result21)
		experience <- append(experience,result222)
		book <- append(book,result25)
	i=i+1
	j=1
	arrayCount=0
	domCount=0
	javascriptCount=0
	jqueryCount=0
	groupCount=0
	orderCount=0
	selectCount=0
	sqlCount=0
	javaCount=0
	jarCount=0
	netCount=0
	aspCount=0
	instanceCount=0
	djangoCount=0
	pyCount=0
	pythonCount=0
	problemCount=0
	caseCount=0
	compilerCount=0
	cppCount=0
	stdCount=0
	addressCount=0
	charCount=0
	gccCount=0
	mysqlCount=0
	phpCount=0
	activerecordCount=0
	gemCount=0
	hashCount=0
	blockCount=0
	betaCount=0
	appleCount=0
	iosCount=0
	cocoaCount=0
	datumCount=0
	delegateCount=0
	formCount=0
	cgiCount=0
	cpanCount=0
	awkCount=0
	bashCOunt=0
	binCount=0
	alignCount=0
	backgroundCount=0
	bodyCount=0
	actorCount=0
	applicationCount=0
	cabalCount=0
	dataframeCount=0
	datasetCount=0
	factorCount=0
	attemptCount=0
	buttonCount=0
	classCount=0
	codeCount=0
	baslangicCount=0
	ortaCount=0
	yuksekCount=0
}
###kural tabanli labeling sonu
###############feature selection
###yuksek korelasyona sahip ozellikler silinecek
library(caret)
correlationMatrix <- cor(result)
wC <- result$wordCount
cC <- result$charCount
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
result = subset(result, select = -highlyCorrelated )
result <- data.frame(wC,cC,result)
############################
#####Id eklemebolumu
##################################
###bu satiri biri 50 digeri 51 satir ciktigi icin yazdik
#############################
folderCount<- folderCount-1
result <- data.frame(mydata[1:folderCount,2],result)
names(result)[1]<-paste("Id")
####################################
#######yuksek korelasyonlu sutunlarin silinmesi son
#########################################


############1-50 arasi sutunlari aliyorum####################
result <- result[,1:50]
########multi-class ekliyorum############
#result <- data.frame(result,experience)
result <- data.frame(result,javascript,sql,java,csharp,python,cplus,c,php,ruby,swift,objectivec,visualbasic,perl,bash,css,scala,html,lua,haskell,markdown,r,experience,book)
write.csv(result,file="C:/Users/Desktop/stack/processedDataset.csv")


