Fed up with having to perform thousands of mouse clicks per analysis I wanted to attempt, I created BV-SDM_automator.R.

#################### makeSDM.R ####################
makeSDM.R creates a function called makeSDM(). 
It takes a single argument, a conditions file.
It outputs an entire folder of SDM files for deconvolution design, as well as a folder of PRT files if you want to double-check.

To make a conditions file, make a tab-delimited spreadsheet.
Name it "conditions-<CONTRAST>.txt", where <CONTRAST> is the name you want to know this coding scheme by.
Make two columns, "predictor" and "condition". Like everything else in R, these are CASE-SENSITIVE, so do not capitalize them.
In the "predictor" column, give each a recognizable name.
In the "condition" column, write, in double quotes, a logical statement that R can parse to return TRUE for those cases you're looking for.
If you need to refer to a string, enclose it in single quotes so that R does not get confused and think the logical statement has ended.

e.g. "dat$Prev.ACC %in% 1 & dat$Prev.feedbackmask %in% 'fast' & dat$Probe.ACC %in% 1 & dat$feedbackmask %in% 'fast'"
this will return TRUE only when both the current trial is fast and correct and the previous trial was fast and correct.

Finally, you can list confound conditions at the bottom of your conditions file.
Be sure to give them a name beginning with "Confound" (again, case-sensitive). BV-SDM automator will automatically label them as confounds.

Things you could modify manually in the makeSDM code:
Number of lags per condition
Criteria for exclusion based on insufficient trials or excessive motion
Whether or not to use the expanded set of confounds suggested by Shawn (1st derivatives of motion confounds, average intensity confounds, 1st derivatives of avg intensity confounds)

#################### makeMDM.R ####################
makeMDM.R creates a function called makeMDM().
It takes a single argument, again the conditions file used by makeSDM().
It outputs a single .MDM file which can be used in BrainVoyager to conduct the multi-study, multi-subject RFX GLM.

So long as makeSDM.R ran correctly, there's nothing to it.
makeMDM() will automatically read in the output from makeSDM(), including the SDM files and the output about bad BOLDS or subjects.

Note that makeMDM.R assumes a certain file path for the SDM files (e.g. "/data/BartholowLab/JH_racebias/analysis/SDMs_prevAcc/")
If you change the path, you will have to change these paths in the makeMDM.R code. I don't recommend it! 

#################### master_script.R ####################
It is most convenient just to call makeSDM() and makeMDM() from a single script.
master_script.R first uses source() to run each of makeSDM.R and makeMDM.R, thereby loading up the makeSDM() and makeMDM() functions.
From there, it's easy to apply both makeSDM() and makeMDM() to the same conditions file.

#################### Conducting analysis ####################
Use WinSCP to drag the SDM folder and MDM file to the BrainVoyager server.
Open the MDM file. So long as you didn't muck up the paths, the MDM will open fine and you will be ready to run your MDM, then your ANOVA, as per usual.