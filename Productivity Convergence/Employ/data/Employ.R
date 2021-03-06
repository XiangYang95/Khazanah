setwd("/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/data")
rm(list = ls(all = T))

library(xlsx)

#Johor
J1 = read.xlsx("Johor.xlsx", 1, endRow= 6) 
J2 = read.xlsx("Johor.xlsx", 2, endRow = 8) 

#Kedah
Ked1 = read.xlsx("Kedah.xlsx", 1, endRow= 6) 
Ked2 = read.xlsx("Kedah.xlsx", 2, endRow = 8) 

#Kelantan
Kel1 = read.xlsx("Kelantan.xlsx", 1, endRow= 6) 
Kel2 = read.xlsx("Kelantan.xlsx", 2, endRow = 8) 

#KL
KL1 = read.xlsx("KL.xlsx", 1, endRow= 6) 
KL2 = read.xlsx("KL.xlsx", 2, endRow = 8) 

#Labuan
Lab1 = read.xlsx("Labuan.xlsx", 1, endRow= 6) 
Lab2 = read.xlsx("Labuan.xlsx", 2, endRow = 8) 

#Melaka
Mel1 = read.xlsx("Melaka.xlsx", 1, endRow= 6) 
Mel2 = read.xlsx("Melaka.xlsx",  2, endRow = 8) 

#Negeri Sembilan
N91 = read.xlsx("N9.xlsx", 1, endRow= 6) 
N92 = read.xlsx("N9.xlsx",  2, endRow = 8) 

#Pulau Pinang
PP1 = read.xlsx("P.Pinang.xlsx", 1, endRow= 6) 
PP2 = read.xlsx("P.Pinang.xlsx",  2, endRow = 8) 

#Pahang
Pah1 = read.xlsx("Pahang.xlsx", 1, endRow= 6) 
Pah2 = read.xlsx("Pahang.xlsx",  2, endRow = 8) 

#Perak
Per1 = read.xlsx("Perak.xlsx", 1, endRow= 6) 
Per2 = read.xlsx("Perak.xlsx",  2, endRow = 8) 

#Perlis
Perl1 = read.xlsx("Perlis.xlsx", 1, endRow = 6)
Perl2 = read.xlsx("Perlis.xlsx",  2, endRow = 8)

#Sabah
Sab1 = read.xlsx("Sabah.xlsx", 1, endRow = 6)
Sab2 = read.xlsx("Sabah.xlsx",  2, endRow = 8) 

#Sarawak
Sar1 = read.xlsx("Sarawak.xlsx", 1, endRow = 6)
Sar2 = read.xlsx("Sarawak.xlsx",  2, endRow = 8) 

#Selangor
Sel1 = read.xlsx("Selangor.xlsx", 1, endRow = 6)
Sel2 = read.xlsx("Selangor.xlsx",  2, endRow = 8) 

#Terengganu
Ter1 = read.xlsx("Terengganu.xlsx", 1, endRow = 6)
Ter2 = read.xlsx("Terengganu.xlsx",  2, endRow = 8) 

##
#Agriculture is done

#Mining
johor <- c(J1$C, J2$B)
kedah <- c(Ked1$C, Ked2$B)
kelantan <- c(Kel1$C, Kel2$B)
melaka <- c(Mel1$C, Mel2$B)
N9 <- c(N91$C,N92$B)
pahang <- c(Pah1$C, Pah2$B)
PP <- c(PP1$C, PP2$B)
perak <- c(Per1$C, Per2$B)
perlis <- c(Perl1$C, Perl2$B)
selangor <- c(Sel1$C,Sel2$B)
terengganu <- c(Ter1$C, Ter2$B)
sabah <- c(Sab1$C,Sab2$B)
sarawak <- c(Sar1$C, Sar2$B)
KL <- c(KL1$B, KL2$B)
labuan <- c(Lab1$C, Lab2$B)
mining <- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                   terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(mining) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(mining) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                      "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(mining, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Mining.xlsx")

#Manufacturing
johor <- c(J1$D, J2$C)
kedah <- c(Ked1$D, Ked2$C)
kelantan <- c(Kel1$D, Kel2$C)
melaka <- c(Mel1$D, Mel2$C)
N9 <- c(N91$D,N92$C)
pahang <- c(Pah1$D, Pah2$C)
PP <- c(PP1$D, PP2$C)
perak <- c(Per1$D, Per2$C)
perlis <- c(Perl1$D, Perl2$C)
selangor <- c(Sel1$D,Sel2$C)
terengganu <- c(Ter1$D, Ter2$C)
sabah <- c(Sab1$D,Sab2$C)
sarawak <- c(Sar1$D, Sar2$C)
KL <- c(KL1$C, KL2$C)
labuan <- c(Lab1$D, Lab2$C)
Manufacturing <- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                              terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Manufacturing) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Manufacturing) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                      "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Manufacturing, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Manufacturing.xlsx")

#Utilities, Transportation, Storage, Communication
johor <- c(J1$E+J1$I, J2$D+J2$E+J2$H+J2$J)
kedah <- c(Ked1$E+Ked1$I, Ked2$D+Ked2$E+Ked2$H+Ked2$J)
kelantan <- c(Kel1$E+Kel1$I, Kel2$D+Kel2$E+Kel2$H+Kel2$J)
melaka <- c(Mel1$E+Mel1$I, Mel2$D+Mel2$E+Mel2$H+Mel2$J)
N9 <- c(N91$E+N91$I,N92$D+N92$E+N92$H+N92$J)
pahang <- c(Pah1$E+Pah1$I, Pah2$D+Pah2$E+Pah2$H+Pah2$J)
PP <- c(PP1$E+PP1$I, PP2$D+PP2$E+PP2$H+PP2$J)
perak <- c(Per1$E+Per1$I, Per2$D+Per2$E+Per2$H+Per2$J)
perlis <- c(Perl1$E+Perl1$I, Perl2$D+Perl2$E+Perl2$H+Perl2$J)
selangor <- c(Sel1$E+Sel1$I,Sel2$D+Sel2$E+Sel2$H+Sel2$J)
terengganu <- c(Ter1$E+Ter1$I, Ter2$D+Ter2$E+Ter2$H+Ter2$J)
sabah <- c(Sab1$E+Sab1$I,Sab2$D+Sab2$E+Sab2$H+Sab2$J)
sarawak <- c(Sar1$E+Sar1$I, Sar2$D+Sar2$E+Sar2$H+Sar2$J)
KL <- c(KL1$D+KL1$H, KL2$D+KL2$E+KL2$H+KL2$J)
labuan <- c(Lab1$E+Lab1$I, Lab2$D+Lab2$E+Lab2$H+Lab2$J)
UTSC<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                                     terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(UTSC) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(UTSC) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                             "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(UTSC, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/UTSC.xlsx")

#Construction
johor <- c(J1$F, J2$F)
kedah <- c(Ked1$F, Ked2$F)
kelantan <- c(Kel1$F, Kel2$F)
melaka <- c(Mel1$F, Mel2$F)
N9 <- c(N91$F,N92$F)
pahang <- c(Pah1$F, Pah2$F)
PP <- c(PP1$F, PP2$F)
perak <- c(Per1$F, Per2$F)
perlis <- c(Perl1$F, Perl2$F)
selangor <- c(Sel1$F,Sel2$F)
terengganu <- c(Ter1$F, Ter2$F)
sabah <- c(Sab1$F,Sab2$F)
sarawak <- c(Sar1$F, Sar2$F)
KL <- c(KL1$E, KL2$F)
labuan <- c(Lab1$F, Lab2$F)
Construction <- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                                     terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Construction) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Construction) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                             "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Construction, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Construction.xlsx")


#Wholesale
johor <- c(J1$G+J1$H, J2$G+J2$I)
kedah <- c(Ked1$G+Ked1$H, Ked2$G+Ked2$I)
kelantan <- c(Kel1$G+Kel1$H, Kel2$G+Kel2$I)
melaka <- c(Mel1$G+Mel1$H, Mel2$G+Mel2$I)
N9 <- c(N91$G+N91$H,N92$G+N92$I)
pahang <- c(Pah1$G+Pah1$H, Pah2$G+Pah2$I)
PP <- c(PP1$G+PP1$H, PP2$G+PP2$I)
perak <- c(Per1$G+Per1$H, Per2$G+Per2$I)
perlis <- c(Perl1$G+Perl1$H, Perl2$G+Perl2$I)
selangor <- c(Sel1$G+Sel1$H,Sel2$G+Sel2$I)
terengganu <- c(Ter1$G+Ter1$H, Ter2$G+Ter2$I)
sabah <- c(Sab1$G+Sab1$H,Sab2$G+Sab2$I)
sarawak <- c(Sar1$G+Sar1$H, Sar2$G+Sar2$I)
KL <- c(KL1$F+KL1$G, KL2$G+KL2$I)
labuan <- c(Lab1$G+Lab1$H, Lab2$G+Lab2$I)
Wholesale<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                           terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Wholesale) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Wholesale) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                    "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Wholesale, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Wholesale.xlsx")


#Utilities, Transportation, Storage, Communication
johor <- c(J1$E+J1$I, J2$D+J2$E+J2$H+J2$J)
kedah <- c(Ked1$E+Ked1$I, Ked2$D+Ked2$E+Ked2$H+Ked2$J)
kelantan <- c(Kel1$E+Kel1$I, Kel2$D+Kel2$E+Kel2$H+Kel2$J)
melaka <- c(Mel1$E+Mel1$I, Mel2$D+Mel2$E+Mel2$H+Mel2$J)
N9 <- c(N91$E+N91$I,N92$D+N92$E+N92$H+N92$J)
pahang <- c(Pah1$E+Pah1$I, Pah2$D+Pah2$E+Pah2$H+Pah2$J)
PP <- c(PP1$E+PP1$I, PP2$D+PP2$E+PP2$H+PP2$J)
perak <- c(Per1$E+Per1$I, Per2$D+Per2$E+Per2$H+Per2$J)
perlis <- c(Perl1$E+Perl1$I, Perl2$D+Perl2$E+Perl2$H+Perl2$J)
selangor <- c(Sel1$E+Sel1$I,Sel2$D+Sel2$E+Sel2$H+Sel2$J)
terengganu <- c(Ter1$E+Ter1$I, Ter2$D+Ter2$E+Ter2$H+Ter2$J)
sabah <- c(Sab1$E+Sab1$I,Sab2$D+Sab2$E+Sab2$H+Sab2$J)
sarawak <- c(Sar1$E+Sar1$I, Sar2$D+Sar2$E+Sar2$H+Sar2$J)
KL <- c(KL1$D+KL1$H, KL2$D+KL2$E+KL2$H+KL2$J)
labuan <- c(Lab1$E+Lab1$I, Lab2$D+Lab2$E+Lab2$H+Lab2$J)
UTSC<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                           terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(UTSC) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(UTSC) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                    "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(UTSC, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/UTSC.xlsx")

#Finance
johor <- c(J1$J+J1$K, J2$K+J2$L+J2$M+J2$N)
kedah <- c(Ked1$J+Ked1$K, Ked2$K+Ked2$L+Ked2$M+Ked2$N)
kelantan <- c(Kel1$J+Kel1$K, Kel2$K+Kel2$L+Kel2$M+Kel2$N)
melaka <- c(Mel1$J+Mel1$K, Mel2$K+Mel2$L+Mel2$M+Mel2$N)
N9 <- c(N91$J+N91$K,N92$K+N92$L+N92$M+N92$N)
pahang <- c(Pah1$J+Pah1$K, Pah2$K+Pah2$L+Pah2$M+Pah2$N)
PP <- c(PP1$J+PP1$K, PP2$K+PP2$L+PP2$M+PP2$N)
perak <- c(Per1$J+Per1$K, Per2$K+Per2$L+Per2$M+Per2$N)
perlis <- c(Perl1$J+Perl1$K, Perl2$K+Perl2$L+Perl2$M+Perl2$N)
selangor <- c(Sel1$J+Sel1$K,Sel2$K+Sel2$L+Sel2$M+Sel2$N)
terengganu <- c(Ter1$J+Ter1$K, Ter2$K+Ter2$L+Ter2$M+Ter2$N)
sabah <- c(Sab1$J+Sab1$K,Sab2$K+Sab2$L+Sab2$M+Sab2$N)
sarawak <- c(Sar1$J+Sar1$K, Sar2$K+Sar2$L+Sar2$M+Sar2$N)
KL <- c(KL1$I+KL1$J, KL2$K+KL2$L+KL2$M+KL2$N)
labuan <- c(Lab1$J+Lab1$K, Lab2$K+Lab2$L+Lab2$M+Lab2$N)
Finance<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                           terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Finance) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Finance) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                    "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Finance, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Finance.xlsx")

#Government
johor <- c(J1$L+J1$M+J1$N, J2$O+J2$P+J2$Q+J2$R)
kedah <- c(Ked1$L+Ked1$M+Ked1$N, Ked2$O+Ked2$P+Ked2$Q+Ked2$R)
kelantan <- c(Kel1$L+Kel1$M+Kel1$N, Kel2$O+Kel2$P+Kel2$Q+Kel2$R)
melaka <- c(Mel1$L+Mel1$M+Mel1$N, Mel2$O+Mel2$P+Mel2$Q+Mel2$R)
N9 <- c(N91$L+N91$M+N91$N,N92$O+N92$P+N92$Q+N92$R)
pahang <- c(Pah1$L+Pah1$M+Pah1$N, Pah2$O+Pah2$P+Pah2$Q+Pah2$R)
PP <- c(PP1$L+PP1$M+PP1$N, PP2$O+PP2$P+PP2$Q+PP2$R)
perak <- c(Per1$L+Per1$M+Per1$N, Per2$O+Per2$P+Per2$Q+Per2$R)
perlis <- c(Perl1$L+Perl1$M+Perl1$N, Perl2$O+Perl2$P+Perl2$Q+Perl2$R)
selangor <- c(Sel1$L+Sel1$M+Sel1$N,Sel2$O+Sel2$P+Sel2$Q+Sel2$R)
terengganu <- c(Ter1$L+Ter1$M+Ter1$N, Ter2$O+Ter2$P+Ter2$Q+Ter2$R)
sabah <- c(Sab1$L+Sab1$M+Sab1$N,Sab2$O+Sab2$P+Sab2$Q+Sab2$R)
sarawak <- c(Sar1$L+Sar1$M+Sar1$N, Sar2$O+Sar2$P+Sar2$Q+Sar2$R)
KL <- c(KL1$K+KL1$L+KL1$M, KL2$O+KL2$P+KL2$Q+KL2$R)
labuan <- c(Lab1$L+Lab1$M+Lab1$N, Lab2$O+Lab2$P+Lab2$Q+Lab2$R)
Government<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                              terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Government) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Government) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                       "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Government, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Government.xlsx")

#Wholesale
johor <- c(J1$O+J1$P, J2$S+J2$T)
kedah <- c(Ked1$O+Ked1$P, Ked2$S+Ked2$T)
kelantan <- c(Kel1$O+Kel1$P, Kel2$S+Kel2$T)
melaka <- c(Mel1$O+Mel1$P, Mel2$S+Mel2$T)
N9 <- c(N91$O+N91$P,N92$S+N92$T)
pahang <- c(Pah1$O+Pah1$P, Pah2$S+Pah2$T)
PP <- c(PP1$O+PP1$P, PP2$S+PP2$T)
perak <- c(Per1$O+Per1$P, Per2$S+Per2$T)
perlis <- c(Perl1$O+Perl1$P, Perl2$S+Perl2$T)
selangor <- c(Sel1$O+Sel1$P,Sel2$S+Sel2$T)
terengganu <- c(Ter1$O+Ter1$P, Ter2$S+Ter2$T)
sabah <- c(Sab1$O+Sab1$P,Sab2$S+Sab2$T)
sarawak <- c(Sar1$O+Sar1$P, Sar2$S+Sar2$T)
KL <- c(KL1$N+KL1$O, KL2$S+KL2$T)
labuan <- c(Lab1$O+Lab1$P, Lab2$S+Lab2$T)
Other<- data.frame(matrix(c(johor,kedah,kelantan,melaka,N9,pahang,PP,perak,perlis,selangor,
                                terengganu,sabah,sarawak,KL,labuan), ncol = 15))
rownames(Other) <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
colnames(Other) <- c("Johor", "Kedah", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Pulau Pinang", "Perak",
                         "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "Kuala Lumpur", "Labuan")
write.xlsx(Other, "/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/Employ/Other.xlsx")

