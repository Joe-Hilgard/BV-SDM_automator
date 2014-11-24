# It seems like this will be easier to make from the ePrime file than from a PRT.
# Check lines 35+ and 50+ for things that change according to length of p!
# Also header info at line 90+
setwd("C:/data_2014/Thesis/prt_sdm_automation/BV-SDM_automator")
# load predictor conditions:
conditions = read.delim("conditions.txt", stringsAsFactors=F)
protocol = "CurrentTrial"
# First, name predictors & specify timepoints/predictor:
p = c(conditions$predictor); 
k = 10;
TRlength = 2000
# Generate names vector based on that:
predNames = paste(rep(p, each=k), "_D", rep(0:(k-1), length(p)), sep="")
# Specify number of volumes:
t = 158
# badbolds vector for IDing SDMs featuring NAs
badbolds = c()
# Fourier confounds
fourier = read.table("./movement-files/Modified_Fourier.sdm", skip=8, header=T)
fourier = fourier[,1:4] # Removing the "Constant" column b/c I think it results in singular matrix
# read in the data
megadata = read.delim("fMRI_grand-dataset.txt", skip=1) # read in all the data at once

# then restrict it to just one subject's one bold
  # A loop would start about here, 
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
dat$TR = floor(dat$TR)

codes = vector("list", length(p))
# Fetch TRs that match each condition
for (i in 1:length(conditions$condition)) {
  command = paste("which(", conditions$condition[i], ")")
  logicalTest = eval(parse(text=command))
  codes[[i]] = dat$TR[logicalTest]
}

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
# DEBUG COMMAND
print (sum(complete.cases(sdm)))
if (sum(complete.cases(sdm)) < 158) badbolds = c(badbolds, paste("Subject", sub, "Bold", bold))
#if (sum(complete.cases(sdm)) < 158) break

# Add motion confounds and fourier confounds. (WIP)
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

sdm = data.frame(sdm, motion, fourier)
#sdm = format(sdm, nsmall=6) # I think BV is barfing because columns are 0 1 instead of 0.000000 1.000000

# Okay! I think we're there. Just need to export it to a file and add the header.
exportName = paste("./sdms/","WIT", subSuffix, "_b", bold, "_", protocol, ".sdm", sep="")
#print(paste("Exporting to file", exportName)) # Check NrOfPredictors and FirstConfoundPredictor!!
cat("FileVersion:             1
    
    NrOfPredictors:          50
    NrOfDataPoints:          158
    IncludesConstant:        0
    FirstConfoundPredictor:  41
    
    255 50 50   50 255 50   50 50 255   255 255 0   255 0 255   0 255 255
    ", 
    file=exportName
)
write.table(sdm, file=exportName, row.names=F, append=T)
  }
}
