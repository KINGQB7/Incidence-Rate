#############################################################################
#File layout
#First column is sex (1 male/2 female)  
#Second column is the cancer site number (1 to 244, see "cancer.txt") 
#Third column is age-group, 0-4,5-9,10-14,...,80-84,85+,age unknown (1 to 19)  
#Fourth column is number of cases  
#Fifth column is person-years at risk
#Each file has 2x244x19=9272 records
library(reshape2)
setwd("C:/Users/KINGQB7/Desktop/KHNP/Cancer Incidence/CI5data9")
registry <- readLines("registry.txt")
registry <- do.call(rbind, strsplit(gsub(" ", "", readLines("registry.txt")), "\t"))
registry[,2] <- gsub("\\*", "", registry[,2])
row.names(registry) <- 1:nrow(registry)
registry[which(substring(registry[,2], first = 1, last = 5) == "Japan"),]
registry[which(substring(registry[,2], first = 1, last = 5) == "Korea"),]
# 155 Hiroshima 157 Nagasaki
registry.Hiroshima <- registry[155,1] # Hiroshima
registry.Nagasaki <- registry[157,1] # Nagasaki
registry.Korea <- registry[160,1] # Korea

h.dat.09 <- read.csv(paste0(registry.Hiroshima, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
n.dat.09 <- read.csv(paste0(registry.Nagasaki, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
k.dat.09 <- read.csv(paste0(registry.Korea, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
#j.dat.09 <- rbind(data.frame(h.dat.09, City = "Hiroshima"), data.frame(n.dat.09, City = "Nagasaki"))
j.dat.09 <- data.frame(h.dat.09[,1:3], Case = h.dat.09$Case + n.dat.09$Case, PY=h.dat.09$PY + n.dat.09$PY)
cancer <- readLines("cancer.txt", n=244)

ICD <- matrix(NA,length(cancer), 2)
for(l in 1:length(cancer)){
  ICD[l,1] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][1]
  ICD[l,2] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][2]
}
cancer.dat <- data.frame(Code = substring(cancer, first = 1, last = 3),
                         Name = ICD[,1],
                         ICD= ICD[,2])
write.csv(cancer.dat, "../cancer09.csv", row.names = F)

setwd("C:/Users/KINGQB7/Desktop/KHNP/Cancer Incidence/CI5data10")
registry <- readLines("registry.txt")
registry <- do.call(rbind, strsplit(gsub(" ", "", readLines("registry.txt")), "\t"))
registry[,2] <- gsub("\\*", "", registry[,2])
row.names(registry) <- 1:nrow(registry)
registry[which(substring(registry[,2], first = 1, last = 5) == "Japan"),]
registry[which(substring(registry[,2], first = 1, last = 5) == "Repub"),]
#245 Hiroshima 247 Nagasaki
registry.Hiroshima <- registry[245,1] # Hiroshima
registry.Nagasaki <- registry[247,1] # Nagasaki
registry.Korea <- registry[253,1] # Korea

h.dat.10 <- read.csv(paste0(registry.Hiroshima, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
n.dat.10 <- read.csv(paste0(registry.Nagasaki, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
k.dat.10 <- read.csv(paste0(registry.Korea, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
#j.dat.10 <- rbind(data.frame(h.dat.10, City = "Hiroshima"), data.frame(n.dat.10, City = "Nagasaki"))
j.dat.10 <- data.frame(h.dat.10[,1:3], Case = h.dat.10$Case + n.dat.10$Case, PY=h.dat.10$PY + n.dat.10$PY)
cancer <- readLines("cancer.txt", n=244)
ICD <- matrix(NA,length(cancer), 2)
for(l in 1:length(cancer)){
  ICD[l,1] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][1]
  ICD[l,2] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][2]
}
cancer.dat <- data.frame(Code = substring(cancer, first = 1, last = 3),
                         Name = ICD[,1],
                         ICD= ICD[,2])
write.csv(cancer.dat, "../cancer10.csv", row.names = F)

setwd("C:/Users/KINGQB7/Desktop/KHNP/Cancer Incidence/CI5data11")
registry <- readLines("registry.txt")
registry <- do.call(rbind, strsplit(gsub(" ", "", readLines("registry.txt")), "\t"))
registry[,2] <- gsub("\\*", "", registry[,2])
row.names(registry) <- 1:nrow(registry)
registry[which(substring(registry[,2], first = 1, last = 5) == "Japan"),]
registry[which(substring(registry[,2], first = 1, last = 5) == "Repub"),]
# 271 Hiroshima 273 Nagasaki 
registry.Hiroshima <- registry[271,1] # Hiroshima
registry.Nagasaki <- registry[273,1] # Nagasaki
registry.Korea <- registry[279,1] # Korea

h.dat.11 <- read.csv(paste0(registry.Hiroshima, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
n.dat.11 <- read.csv(paste0(registry.Nagasaki, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
k.dat.11 <- read.csv(paste0(registry.Korea, ".csv"), header = F, col.names = c("Sex", "Cancer", "Age", "Case", "PY"))
#j.dat.11 <- rbind(data.frame(h.dat.11, City = "Hiroshima"), data.frame(n.dat.11, City = "Nagasaki"))
j.dat.11 <- data.frame(h.dat.11[,1:3], Case = h.dat.11$Case + n.dat.11$Case, PY=h.dat.11$PY + n.dat.11$PY)
cancer <- readLines("cancer.txt", n=241)
ICD <- matrix(NA,length(cancer), 2)
for(l in 1:length(cancer)){
  ICD[l,1] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][1]
  ICD[l,2] <- strsplit(substring(cancer, first = 4), "\\(")[[l]][2]
}
cancer.dat <- data.frame(Code = substring(cancer, first = 1, last = 3),
                         Name = ICD[,1],
                         ICD= ICD[,2])
write.csv(cancer.dat, "../cancer11.csv", row.names = F)


ICD.09 <- read.csv("../ICD09.csv", stringsAsFactors = F)
ICD.10 <- read.csv("../ICD10.csv", stringsAsFactors = F)
ICD.11 <- read.csv("../ICD11.csv", stringsAsFactors = F)

c.name <- c('??????, ?εξ?', '?ĵ???', '��??', '??????', '??????', 
            '????', '?㳶??', '??????', '????????, ????', '?񰭾?, ?ĵξ?, ????????', 
            '?��?', '????��????', '?Ǽ?????��', '??????��?Ǻξ?(??????????)', '??????��?Ǻξ?(??????????)', 
            '��????(????)', '��????(????)', '???Ҿ?', '???Ҿ? ?? ???????ı???', '???????ı???', 
            '?汤??', '??????, ?????? ?񴢱?????', '?Ⱦ?', '????, ?Ű?????', '???󼱾?', 
            '?????? ???к񼱾?', '????????, ?Һи??? ??', '????��, ?ٹ߼?????��', '??????(CLL ��??)', '?޼????��?????????',
            '?޼?????????????', '????????????????')


setwd("C:/Users/KINGQB7/Desktop/KHNP/Cancer Incidence")
#h.n.comp <- 100000*data.frame(Hiro_ratio = h.dat.11$Case / h.dat.11$PY, Naga_ratio = n.dat.11$Case/n.dat.11$PY)
#plot(log(h.n.comp[,1]+1), type="l")
#points(log(h.n.comp[,2]+1), type="l", col=2)

i.cc <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,24,25,26,28)
for(vvv in c("09", "10", "11")){
  ICD.dat <- get(paste0("ICD.", vvv))
  icd.dat <- subset(ICD.dat, nchar(Korea)!=0)
  
  j.dat <- get(paste0("j.dat.", vvv))
  k.dat <- get(paste0("k.dat.", vvv))

  j.dat <- subset(j.dat, Age!=19)
  k.dat <- subset(k.dat, Age!=19)

  for( sss in 1:2){
    jp.dat <- kr.dat <- data.frame(Code=1:32, Cancer = c.name, 
                                   Age_0 = NA, Age_5 = NA,
                                   Age_10 = NA, Age_15 = NA,
                                   Age_20 = NA, Age_25 = NA,
                                   Age_30 = NA, Age_35 = NA,
                                   Age_40 = NA, Age_45 = NA,
                                   Age_50 = NA, Age_55 = NA,
                                   Age_60 = NA, Age_65 = NA,
                                   Age_70 = NA, Age_75 = NA,
                                   Age_80 = NA, Age_85 = NA)
    
    j.temp <- subset(j.dat, Sex == sss)
    k.temp <- subset(k.dat, Sex == sss)
    py.jp <- subset(j.dat, Sex==sss)[1:18, c("Age", "PY")]
    py.kr <- subset(k.dat, Sex==sss)[1:18, c("Age", "PY")]
    sex <- ifelse(sss==1, "male", "female")
    for( i in i.cc){
      for(aaa in 1:18){
        iarc.code <- icd.dat$Code[which(icd.dat$Korea==i)]
        jp.dat[i, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
        kr.dat[i, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
      }
    }
    # i = 14, 15
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="14,15")]
      jp.dat[14, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
      jp.dat[15, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
      kr.dat[14, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
      kr.dat[15, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
    }
    # i = 16, 17
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="16,17")]
      if(sss == 1){
        jp.dat[16, aaa+2] <- 0
        jp.dat[17, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
        kr.dat[16, aaa+2] <- 0
        kr.dat[17, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
      }else{
        jp.dat[16, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
        jp.dat[17, aaa+2] <- 0
        kr.dat[16, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
        kr.dat[17, aaa+2] <- 0
      }
    }
    
    # i = 29
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="29")]
      iarc.minus <- icd.dat$Code[which(icd.dat$Korea=="-29")]
      jp.dat[29, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) - 
        sum(subset(j.temp, (Cancer %in% iarc.minus)&(Age==aaa))$Case)
      kr.dat[29, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) - 
        sum(subset(k.temp, (Cancer %in% iarc.minus)&(Age==aaa))$Case)
    }
    
    # i = 27
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="0")]
      jp.dat[27, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) - 
        sum(jp.dat[c(1:14, 16:26, 28:29),aaa+2])
      kr.dat[27, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) - 
        sum(kr.dat[c(1:14, 16:26, 28:29),aaa+2])
    }
   
    # i = 30
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="30")]
      iarc.plus <- icd.dat$Code[which(icd.dat$Korea=="30,31")]
      jp.dat[30, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) + 
        sum(subset(j.temp, (Cancer %in% iarc.plus)&(Age==aaa))$Case)
      kr.dat[30, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) + 
        sum(subset(k.temp, (Cancer %in% iarc.plus)&(Age==aaa))$Case)
    }
    
    # i = 31
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="31")]
      iarc.plus <- icd.dat$Code[which(icd.dat$Korea=="30,31")]
      jp.dat[31, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) + 
        sum(subset(j.temp, (Cancer %in% iarc.plus)&(Age==aaa))$Case)
      kr.dat[31, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) + 
        sum(subset(k.temp, (Cancer %in% iarc.plus)&(Age==aaa))$Case)
    }
    
    # i = 32
    for(aaa in 1:18){
      iarc.code <- icd.dat$Code[which(icd.dat$Korea=="32")]
      jp.dat[32, aaa+2] <- sum(subset(j.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case) 
      kr.dat[32, aaa+2] <- sum(subset(k.temp, (Cancer %in% iarc.code)&(Age==aaa))$Case)
    }
    
    n.cancer <- 32
    melt.jp.dat <- melt(jp.dat, id = c("Code", "Cancer"), variable.name = "Age", value.name = "Case")
    melt.jp.dat$Age <- as.numeric(substring(melt.jp.dat$Age, first = 5))
    melt.kr.dat <- melt(kr.dat, id = c("Code", "Cancer"), variable.name = "Age", value.name = "Case")
    melt.kr.dat$Age <- as.numeric(substring(melt.kr.dat$Age, first = 5))
    
    py.jp$Age <- (py.jp$Age-1)*5
    py.kr$Age <- (py.kr$Age-1)*5
    melt.jp.dat <- data.frame(melt.jp.dat, PY = rep(py.jp$PY, each = n.cancer))
    melt.kr.dat <- data.frame(melt.kr.dat, PY = rep(py.kr$PY, each = n.cancer))
    
    write.csv(melt.jp.dat, paste0("JP_Melt_Case_", vvv, sex,".csv"), row.names = F)
    write.csv(melt.kr.dat, paste0("KR_Melt_Case_", vvv, sex,".csv"), row.names = F)
    
    r.jp <- data.frame(jp.dat[,1:2], t(100000*t(jp.dat[,-(1:2)])/py.jp[,2]))
    r.kr <- data.frame(kr.dat[,1:2], t(100000*t(kr.dat[,-(1:2)])/py.kr[,2]))
    write.csv(jp.dat, paste0("JP_Count_", vvv, sex,".csv"), row.names = F)
    write.csv(kr.dat, paste0("KR_Count_", vvv, sex,".csv"), row.names = F)
    write.csv(r.jp, paste0("JP_Rate_", vvv, sex,".csv"), row.names = F)
    write.csv(r.kr, paste0("KR_Rate_", vvv, sex,".csv"), row.names = F)
    
  }
  
}









setwd("C:/Users/KINGQB7/Desktop/????/?????Ϲ߻???/CI5data_PC")

K.list <- list()
for( s in 1:2){
  for( v in 1:3){
    ss <- c("male", "female")[s]
    vv <- c("09", "10", "11")[v]
    csv.name <- paste0("./data/KR_Melt_Case_", vv, ss, ".csv")
    K.list[[(s-1)*3 + v]] <- data.frame(read.csv(csv.name), Version = vv, Sex = ss)
  }
}
K.dat <- do.call(rbind, K.list)
write.csv(K.dat, "CI5_KR.csv", row.names = F)



J.list <- list()
for( s in 1:2){
  for( v in 1:3){
    ss <- c("male", "female")[s]
    vv <- c("09", "10", "11")[v]
    csv.name <- paste0("./data/JP_Melt_Case_", vv, ss, ".csv")
    J.list[[(s-1)*3 + v]] <- data.frame(read.csv(csv.name), Version = vv, Sex = ss)
  }
}
J.dat <- do.call(rbind, J.list)
write.csv(J.dat, "CI5_JP.csv", row.names = F)










setwd("C:/Users/KINGQB7/Desktop/????/?????Ϲ߻???")


list.kr.male <- list.files(path="./BCI/male", pattern="csv")
list.kr.female <- list.files(path="./BCI/female", pattern="csv")

ss <- c("male", "female")
yyyy <- 1999:2014
aaa <- 0:17*5
ccc <- 1:33

c.model.M <- c(3,3,3,3,3,2,2,3,3,4,3,3,3,10,10,10,NA,2,NA,NA,3,3,3,3,3,9,3,3,3,7,NA,NA,NA)
c.model.F <- c(3,3,2,3,3,2,2,3,3,4,3,3,3,10,10,10,2,NA,3,4,NA,3,3,3,3,9,3,3,3,7,NA,NA,NA)
c.ind <- paste0("cancer", 1:33)
c.name <- c('??????, ?εξ?', '?ĵ???', '��??', '??????', '??????', 
            '??????ȭ??????', '????', '?㳶??', '??????', '????????, ????',
            '?񰭾?, ?ĵξ?, ????????', '?��?', '????��????', '?Ǽ?????��', '??????��?Ǻξ?(??????????)',
            '??????��?Ǻξ?(??????????)', '��????(????)', '��????(????)', '???Ҿ?', '???Ҿ? ?? ???????ı???', 
            '???????ı???', '?汤??', '??????, ?????? ?񴢱?????', '?Ⱦ?', '????, ?Ű?????',
            '???󼱾?', '?????? ???к񼱾?', '????????, ?Һи??? ??', '????��, ?ٹ߼?????��', '??????(CLL ��??)',
            '?޼? ???��??? ??????', '?޼? ?????? ??????', '???? ?????? ??????')


for(y in 1:length(yyyy)){
  assign(paste("BCI.kr.male.", y + 1998, sep=""), read.csv(paste("./BCI/male/", list.kr.male[y], sep="")))
  assign(paste("BCI.kr.female.", y + 1998, sep=""), read.csv(paste("./BCI/female/", list.kr.female[y], sep="")))
}


for(s in ss){
  list.dat <- list()
  for(cc in ccc){
    list.temp <- matrix(NA, length(yyyy), length(aaa))
    for(y in yyyy){
      file.name <- paste(s, y, sep=".")
      list.temp[y-1998,] <- t(get(paste("BCI.kr.", file.name, sep=""))[cc, -1])
    }
    row.names(list.temp) <- yyyy
    list.dat[[cc]] <- list.temp
  }
  eval(parse(text = paste("list.dat.", s, " <- list.dat", sep="")))
}


# dim: {18(age)*16(year)} * 33(cancer)
incidence.dat.male <- sapply(list.dat.male, as.vector)
incidence.dat.female <- sapply(list.dat.female, as.vector)

# eliminate cancers that have only zeros
table.list.male <- apply(incidence.dat.male, 2, table)
table.list.female <- apply(incidence.dat.female, 2, table)
TF.sd0.male <- c()
TF.sd0.female <- c()
for(cc in ccc){
  TF.sd0.male[cc] <- length(table.list.male[[cc]])==1
  TF.sd0.female[cc] <- length(table.list.female[[cc]])==1
}
ind.sd0.male <- which(TF.sd0.male)
ind.sd0.female <- which(TF.sd0.female)


#### for male
incidence.dat <- incidence.dat.male[, -ind.sd0.male]


cancer.ind <- c.ind[-ind.sd0.male]
cancer.name <- c.name[-ind.sd0.male]
cancer.name <- factor(cancer.name, levels = cancer.name)
colnames(incidence.dat) <- cancer.ind


########################################################################################
# Age <- Age + 2.5
col.dat <- data.frame(Year = rep(yyyy, times = length(aaa)), Age = rep(aaa, each = length(yyyy)) + 2.5)

all.dat <- data.frame(col.dat, incidence.dat)

melt.all.dat <- melt(all.dat, id=c("Year", "Age"))


melt.all.dat$variable <- cancer.name[match(melt.all.dat$variable, cancer.ind)]

jp.male.9 <- read.csv("Melt_Case_09male.csv")
jp.male.10 <- read.csv("Melt_Case_10male.csv")
jp.male.11 <- read.csv("Melt_Case_11male.csv")


WSP <- c(12000, 10000, 9000, 9000, 8000, 8000, 6000, 6000, 6000, 6000, 5000, 4000, 4000, 3000, 2000, 1000, 500, 500)
length(table(melt.all.dat$variable))
length(table(jp.male.10$Cancer))

ccc.male <- setdiff(ccc, ind.sd0.male)
ccc.female <- setdiff(ccc, ind.sd0.female)
jp.male.9
cc=1
c.name[cc]
jp.cc.male.09 <- subset(jp.male.9, Cancer == c.name[cc])
jp.cc.male.09 <- within(jp.cc.male.09, Rate <- 100000*Case/PY)
jp.cc.male.10 <- subset(jp.male.10, Cancer == c.name[cc])
jp.cc.male.10 <- within(jp.cc.male.10, Rate <- 100000*Case/PY)
jp.cc.male.11 <- subset(jp.male.11, Cancer == c.name[cc])
jp.cc.male.11 <- within(jp.cc.male.11, Rate <- 100000*Case/PY)

head(list.dat.male[[cc]])
head(jp.male.9)
for(cc in ccc.male){
  cc.name <- c.name[cc]
  png(paste0("Transfer_male", cc, "_", cc.name, ".png"), width = 1500, height = 1000)
  par(mfrow = c(4,4), mai = c(3,3,2,1)/5, cex.main = 3, cex.lab = 2, cex.axis = 2, oma = c(1,1,5,1))
  for( yy in 1999:2002){
    kr.rate <- list.dat.male[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.male.09$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  for(yy in 2003:2007){
    kr.rate <- list.dat.male[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.male.10$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  for(yy in 2008:2014){
    kr.rate <- list.dat.male[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.male.11$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  mtext(paste0("[????] ", cc.name), 3, outer = T, cex = 5)
  dev.off()
}


jp.female.9 <- read.csv("Melt_Case_09female.csv")
jp.female.10 <- read.csv("Melt_Case_10female.csv")
jp.female.11 <- read.csv("Melt_Case_11female.csv")


jp.cc.female.09 <- subset(jp.female.9, Cancer == c.name[cc])
jp.cc.female.09 <- within(jp.cc.female.09, Rate <- 100000*Case/PY)
jp.cc.female.10 <- subset(jp.female.10, Cancer == c.name[cc])
jp.cc.female.10 <- within(jp.cc.female.10, Rate <- 100000*Case/PY)
jp.cc.female.11 <- subset(jp.female.11, Cancer == c.name[cc])
jp.cc.female.11 <- within(jp.cc.female.11, Rate <- 100000*Case/PY)

head(list.dat.female[[cc]])
head(jp.female.9)
for(cc in ccc.female){
  cc.name <- c.name[cc]
  png(paste0("Transfer_female", cc, "_", cc.name, ".png"), width = 1500, height = 1000)
  par(mfrow = c(4,4), mai = c(3,3,2,1)/5, cex.main = 3, cex.lab = 2, cex.axis = 2, oma = c(1,1,5,1))
  for( yy in 1999:2002){
    kr.rate <- list.dat.female[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.female.09$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  for(yy in 2003:2007){
    kr.rate <- list.dat.female[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.female.10$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  for(yy in 2008:2014){
    kr.rate <- list.dat.female[[cc]][yy-1998,]
    ASR.kr <- sum(WSP*kr.rate)/100000
    jp.rate <- jp.cc.female.11$Rate
    ASR.jp <- sum(WSP*jp.rate)/100000
    ASR.ratio <- ASR.jp/ASR.kr
    CR.ratio <- jp.rate/kr.rate
    x.na <- which((is.na(CR.ratio))|(CR.ratio==Inf))
    plot((0:17)*5, CR.ratio, xlab = "Age", ylab = "Correction factor", main = yy, type = "p", lwd = 2)
    lines((0:17)*5, CR.ratio, type="l", lwd = 2)
    abline(h = ASR.ratio, col="red", lwd = 2)
    abline(v = ((0:17)*5)[x.na], col = "blue", lwd = 2, lty = 2)
  }
  mtext(paste0("[????] ", cc.name), 3, outer = T, cex = 5)
  dev.off()
}


dir.create("./CI5data_PC")
setwd("../CI5data_PC")
write.csv(h.dat.09, "Hiroshima_09.csv", row.names = F)
write.csv(h.dat.10, "Hiroshima_10.csv", row.names = F)
write.csv(h.dat.11, "Hiroshima_11.csv", row.names = F)
write.csv(n.dat.09, "Nagasaki_09.csv", row.names = F)
write.csv(n.dat.10, "Nagasaki_10.csv", row.names = F)
write.csv(n.dat.11, "Nagasaki_11.csv", row.names = F)
write.csv(k.dat.09, "Korea_09.csv", row.names = F)
write.csv(k.dat.10, "Korea_10.csv", row.names = F)
write.csv(k.dat.11, "Korea_11.csv", row.names = F)

h.dat.09