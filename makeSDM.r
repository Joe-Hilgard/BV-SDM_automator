# It seems like this will be easier to make from the ePrime file than from a PRT.
# Check lines 35+ and 50+ for things that change according to length of p!
# Also header info at line 90+

removeDeadCols = T # set to T if you think BV can handle different numbers of columns across SDM files.
TooSlowConfound = T # set to T to treat TooSlow condition as confound. May allow diff #col across files.
# Setting both these to TRUE does keep BV from barking at me, this is promising. 

# setwd("C:/data_2014/Thesis/prt_sdm_automation/BV-SDM_automator")
# load predictor conditions:
conditions = read.delim("conditions.txt", stringsAsFactors=F)
# First, name predictors & specify timepoints/predictor:
p = c(conditions$predictor); 
k = 9;
TRlength = 2000
# Generate names vector based on that:
predNames = paste(rep(p, each=k), "_D", rep(0:(k-1), length(p)), sep="")
# Specify number of volumes:
t = 158
# badbolds vector for IDing SDMs featuring NAs
badbolds = c()
# badMotion data frame for IDing SDMs featuring excess raw motion
badMotion = data.frame(NULL)
# colinearity data frame for IDing colinear predictors
colin = data.frame(NULL)
# Fourier confounds
fourier = read.table("./movement-files/Modified_Fourier.sdm", skip=8, header=T)
fourier = fourier[,1:4] # Removing the "Constant" column b/c I think it results in singular matrix
# read in the data
megadata = read.delim("eprime_thesis_1fix.txt") # read in all the data at once
dir.create("sdms")
# then restrict it to just one subject's one bold

## BEGIN LOOP
for (sub in unique(megadata$Subject)) {
  for (bold in 1:6) {
    print(paste("Retrieving data for subject", sub, "bold", bold))
dat = megadata[megadata$Subject == sub & megadata$Session == bold,] 

# Create spreadsheet
matrixNames = list(1:t, predNames)
sdm = matrix(nrow=t, ncol=length(predNames), dimnames=list(1:t, predNames))
# Get timepoints at which the events happened:
# if the codes are mutually exclusive & exhaustive they will sum to 64
  # but can we count on that? no, i'll make a list
dat$TR = (dat$Mask.OnsetTime - dat$ready.RTTime) / TRlength - 1 # Mask.OnsetTime, or ITI.OnsetTime??
dat$TR = round(dat$TR)

codes = vector("list", length(p))
# Fetch TRs that match each condition
for (i in 1:length(conditions$condition)) {
  command = paste("which(", conditions$condition[i], ")")
  logicalTest = eval(parse(text=command))
  codes[[i]] = dat$TR[logicalTest]
}
names(codes) = conditions[,1]

# Populate values of SDM
#print(paste("Populating values of SDM file!"))
for (j in 1:length(p)) {
  for (i in 1:k) {
    peak = codes[[j]] + (i - 1)  # nudge it up for lagged predictors
    peak = peak[peak < t] # no peak should come after the last TR.
    currCol = (j-1)*k + i
    sdm[peak,currCol] = 1 # fill in the peaks
    sdm[-peak, currCol] = 0 # fill in the zeroes
  }
}

# NAs in sdm therefore represent a real failure
# Delete columns with NAs (e.g. no too-slow trials) if removeDeadCols option is T
if (removeDeadCols == T) {
if (sum(complete.cases(t(sdm))) < k*length(p)) print(paste("Deleting columns from subject", sub, "bold", bold))
sdm = sdm[, complete.cases(t(sdm))] # returns only complete columns
}

# DEBUG COMMAND
print (sum(complete.cases(sdm)))
if (sum(complete.cases(sdm)) < 158) badbolds = c(badbolds, paste("Subject", sub, "Bold", bold))
#if (sum(complete.cases(sdm)) < 158) break

# Add motion confounds and fourier confounds. 
if (TooSlowConfound == T) {
  firstConfoundPredictor = min(grep("Slow", colnames(sdm))[1], dim(sdm)[2] + 1, na.rm=T)
} else firstConfoundPredictor = dim(sdm)[2] + 1

zeroes = paste(rep(0, 3-nchar(sub)), sep="", collapse="")
subSuffix = paste(zeroes, sub, sep="")
motionFileDir = "./movement-files/"
motionFileRTC = paste("WIT", subSuffix, "_b", bold, "_3DMC.rtc", sep="")
motionFileSDM = paste("WIT", subSuffix, "_b", bold, "_3DMC.sdm", sep="")
if (length(list.files(motionFileDir, pattern=motionFileRTC)) > 0) {
  #motion = read.table(file=paste(motionFileDir, motionFileRTC, sep=""), skip=5, header=T)
  motion = read.delim(file=paste(motionFileDir, motionFileRTC, sep=""), #widths=rep(9, 6),
                      skip=5, sep=" ")
  } else {
  #motion = read.table(file=paste(motionFileDir, motionFileSDM, sep=""), skip=8, header=T)
  motion = read.fwf(file=paste(motionFileDir, motionFileSDM, sep=""), widths=rep(12, 6),
                    skip=9, header=F,
                    col.names=c("Translation_BV-X_mm", "Translation_BV-Y_mm", "Translation_BV-Z_mm",
                                "Rotation_BV-X_deg", "Rotation_BV-Y_deg", "Rotation_BV-Z_deg"))
  }

# Check range of motion
check = apply(motion, 2, FUN=max) - apply(motion, 2, FUN=min)
if (sum(check>3)>0) {
  print(paste("Excess motion in subject ", sub, ", bold ", bold, 
              ", column ", names(check)[check>3], sep=""))
  temp = data.frame("subject" = sub, "bold"=bold, "column"=names(check)[check>3])
  badMotion = rbind(badMotion, temp)
}

# Standardize the motion
motion = data.frame(scale(motion))

# generate first derivatives
# Could add a check here for spikes (e.g. throw an alarm if dx/dt exceeds 1 at any point)
motion.deriv = apply(motion, 2, FUN=diff)
motion.deriv = rbind(motion.deriv, 0) # append a zero at the end to coerce fit
motion.deriv = scale(motion.deriv) # convert to z-scores
# plot(1:dim(motion)[1], motion.deriv[,1], typ='l')
colnames(motion.deriv) = paste(names(motion), "_dt", sep="")

# Fetch VOI motion confound, make 1st derivatives of VOI, and append
vvdFileName = paste("./vvd-files/WIT", subSuffix, "_confoundVOI.vvd", sep="")
startRow = 1+158*(bold-1); endRow = startRow+157
vvd = read.delim(vvdFileName, sep="", stringsAsFactors=F)[startRow:endRow,]
vvd.deriv = apply(vvd, 2, FUN=diff)
vvd.deriv = rbind(vvd.deriv, 0)
vvd.deriv = scale(vvd.deriv)
colnames(vvd.deriv) = paste(names(vvd), "_dt", sep="")
vvd = scale(vvd) 

# Append the confounds
sdm = data.frame(sdm, motion, motion.deriv, vvd, vvd.deriv, fourier, "constant"=1)

# Check matrix rank
#stopifnot(qr(sdm)$rank == 40)
tooBig = function(x) ifelse(.9 < max(abs(x[x<1]), na.rm=T), T, F)
rowIndex = as.vector(apply(cor(sdm), 1, tooBig))
colIndex = as.vector(apply(cor(sdm), 2, tooBig))
trouble = cor(sdm)[rowIndex , colIndex]
preds = colnames(trouble)
if (!is.null(preds)) {
  temp = data.frame("sub" = sub, "bold" = bold, "colinearPreds" = preds)
  colin = rbind(colin, temp)
}

# Create PRT for debug purposes
dir.create("prts")
PRTFileName = paste("./prts/WIT", subSuffix, "_b", bold, ".prt", sep="")
cat("FileVersion:  2  	

ResolutionofTime:	Volumes

Experiment:	WIT

BackgroundColor:	0 0 0		
TextColor:	255 255 255
TimeCourseColor:	255 255 255
TimeCourseThick:	3
ReferenceFuncColor:	0 0 80		
ReferenceFuncThick: 	3	

NrOfConditions: ",
    length(codes),
    file = PRTFileName)
for (z in 1:length(codes)) {
  m = matrix(rep(codes[[z]],2), ncol=2)
  cat("\n\n",
      names(codes)[z], "\n",
      length(codes[[z]]), "\n",
      file = PRTFileName, append=T)
  write(t(m), file=PRTFileName, ncolumns=2, append=T)
  cat("Color:", z, z, z, file=PRTFileName, append=T)
}

# Okay! I think we're there. Just need to export it to a file and add the header.
exportName = paste("./sdms/","WIT", subSuffix, "_b", bold, ".sdm", sep="")
#print(paste("Exporting to file", exportName)) # Check NrOfPredictors and FirstConfoundPredictor!!
cat("FileVersion:             1
    
    NrOfPredictors:          ", dim(sdm)[2],"
    NrOfDataPoints:          158
    IncludesConstant:        1
    FirstConfoundPredictor:   ",  firstConfoundPredictor, "    
    255 50 50   50 255 50   50 50 255   255 255 0   255 0 255   0 255 255
    ", 
    file=exportName
)
write.table(sdm, file=exportName, row.names=F, append=T)
  }
}
# Write debug logs
write(badbolds, file="badbolds.txt", ncolumns=1)
write.table(badMotion, file="badMotion.txt", sep="\t", row.names=F)
write.table(colin, file="colinear_predictors.txt", sep="\t", row.names=F)
table(colin$sub, colin$bold)
