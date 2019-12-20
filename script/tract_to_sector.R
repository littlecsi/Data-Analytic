library(stringr)

N <- c(5, 4, 14, 16, 3, 6, 13, 18)
L <- c(2, 7, 11, 12, 19, 20, 1, 8, 9, 10)
J <- c(15, 17, 29, 30, 31, 33, 27, 28, 36, 46)
U <- c(25, 26, 44, 43, 53, 21, 22, 24, 38, 39, 42, 41)
B <- c(32, 47, 34, 35, 48, 49, 45, 50, 51, 52, 54)
Q <- c(56, 57, 58, 59, 60, 67, 68, 69, 70, 71)
D <- c(80, 72, 61, 66, 73)
C <- c(65, 62, 63, 64, 76, 77, 78, 79)
E <- c(74, 75, 83, 84)
M <- c(81, 82)
K <- c(91, 92)
G <- c(85, 86, 87, 88, 89, 90)
O <- c(93, 109)
R <- c(94, 100, 104, 95, 101, 103, 102)
S <- c(110, 111, 117, 118, 119)
F <- c(107, 108, 114, 112, 113)
W <- c(96, 97, 98, 99, 105, 106, 116, 115, 120, 121)

# missing : 23, 37, 40, 55

tract <- c(1:22, 24:36, 38:39, 41:54, 56:121)

df <- read.csv(file="dataset/Income/Income by Location.csv", stringsAsFactors=F, header=T)
str(df)

geography <- df$Geography
geography

cond1 <- str_extract(string=geography, pattern="[:digit:]+") %in% tract
cond1

df01 <- df[cond1,]
str(df01)

sector <- c()
for(i in c(1:nrow(df01))) {
  num <- str_extract(string=df01[i,7], pattern="[:digit:]+")
  sec <- ifelse(num %in% as.character(N), 'N',
          ifelse(num %in% as.character(L), 'L',
            ifelse(num %in% as.character(J), 'J',
              ifelse(num %in% as.character(U), 'U',
                ifelse(num %in% as.character(B), 'B',
                  ifelse(num %in% as.character(Q), 'Q',
                    ifelse(num %in% as.character(D), 'D',
                      ifelse(num %in% as.character(C), 'C',
                        ifelse(num %in% as.character(E), 'E',
                          ifelse(num %in% as.character(M), 'M',
                            ifelse(num %in% as.character(K), 'K',
                              ifelse(num %in% as.character(G), 'G',
                                ifelse(num %in% as.character(O), 'O',
                                  ifelse(num %in% as.character(R), 'R',
                                    ifelse(num %in% as.character(S), 'S',
                                      ifelse(num %in% as.character(F), 'F', 'W'))))))))))))))))
  sector <- c(sector, sec)
}

df01$Sector <- sector

write.csv(df01, file="dataset/Income/Seattle Income")