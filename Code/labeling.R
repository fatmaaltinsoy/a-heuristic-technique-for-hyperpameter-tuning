########labeling functions#######################
tagHesaplaJs <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="javascript")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaSql <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="sql")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaJava <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="java")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaSharp <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="asp.net" || tagData[j,2]=="C#"  || tagData[j,2]==".net")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaPy <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="python")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaCpp <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="c++")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaC <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="c")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaphp <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="php")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaRuby <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="ruby")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaSwift <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="swift" || tagData[j,2]=="swiftmailer")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaObjective <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="objective-c")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaVb <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="vb.net")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaPerl <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="perl")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaBash <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="bash")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaCss <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="css")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaScala <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="scala")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaHtml <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="html")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaLua <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="lua")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaHaskell <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="haskell")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaMarkdown <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="markdown")
		return(1)
	}
j <- j+1
}
return(0)
}
tagHesaplaR <- function(x){
j=1
uzunluk <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="r")
		return(1)
	}
j <- j+1
}
return(0)
}
########################################
