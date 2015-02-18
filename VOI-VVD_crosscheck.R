# checking to see if .rtc, .vvd files, etc are giving me same output

#dat2.b1 = read.delim("./temp/WIT001_b1_Ventricle.rtc", skip=9, header=F)
dat1.b1 = read.delim("./temp/WIT001_b1_ventricleVOI.rtc", skip=9, header=F)
dat1.b2 = read.delim("./temp/WIT001_b2_ventricleVOI.rtc", skip=9, header=F)
dat1.b3 = read.delim("./temp/WIT001_b3_ventricleVOI.rtc", skip=9, header=F)
dat1.b4 = read.delim("./temp/WIT001_b4_ventricleVOI.rtc", skip=9, header=F)
dat1.b5 = read.delim("./temp/WIT001_b5_ventricleVOI.rtc", skip=9, header=F)
dat1.b6 = read.delim("./temp/WIT001_b6_ventricleVOI.rtc", skip=9, header=F)

dat = read.delim("./temp/WIT001_confoundVOI.vvd", sep=" ")
dat.b1 = dat[1:158,1]
dat.b2 = dat[1:158+158*1,1]
dat.b3 = dat[1:158+158*2,1]
dat.b4 = dat[1:158+158*3,1]
dat.b5 = dat[1:158+158*4,1]
dat.b6 = dat[1:158+158*5,1]

summary(dat.b1 - dat1.b1); hist(dat.b1 - dat1.b1[,1])
summary(dat.b2 - dat1.b2)
summary(dat.b3 - dat1.b3)
summary(dat.b4 - dat1.b4)
summary(dat.b5 - dat1.b5)
summary(dat.b6 - dat1.b6)

# dat.b2 = dat[159:(159+157),1]
# dat1.b2 = read.delim("./temp/WIT123_b2_Ventricle.rtc", skip=9, header=F)
# 
# summary(dat.b2 - dat1.b2)
# 
# for (bold in 1:6) {
#   startRow = 1+158*(bold-1); endRow = startRow+157
#   print(c(bold, startRow, endRow))
# }

dat = read.delim("./temp/WIT123_confoundVOI.vvd", sep=" ")
dat.b1 = dat[1:158,1]
dat.b2 = dat[1:158+158*1,1]
dat.b3 = dat[1:158+158*2,1]
dat.b4 = dat[1:158+158*3,1]
dat.b5 = dat[1:158+158*4,1]
dat.b6 = dat[1:158+158*5,1]

dat1.b1 = read.delim("./temp/WIT123_b1_ventricleVOI.rtc", skip=9, header=F)
dat1.b2 = read.delim("./temp/WIT123_b2_ventricleVOI.rtc", skip=9, header=F)
dat1.b3 = read.delim("./temp/WIT123_b3_ventricleVOI.rtc", skip=9, header=F)
dat1.b4 = read.delim("./temp/WIT123_b4_ventricleVOI.rtc", skip=9, header=F)
dat1.b5 = read.delim("./temp/WIT123_b5_ventricleVOI.rtc", skip=9, header=F)
dat1.b6 = read.delim("./temp/WIT123_b6_ventricleVOI.rtc", skip=9, header=F)

summary(dat.b1 - dat1.b1); hist(dat.b1 - dat1.b1[,1])
summary(dat.b2 - dat1.b2)
summary(dat.b3 - dat1.b3)
summary(dat.b4 - dat1.b4)
summary(dat.b5 - dat1.b5)
summary(dat.b6 - dat1.b6)
