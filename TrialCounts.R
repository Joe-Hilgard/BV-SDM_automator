require(dplyr)

megadata = read.delim("eprime_thesis_1fix.txt", stringsAsFactors=F) # read in all the data at once
# Make previous-trial columns
megadata$Prev.ACC = head(c(NA, megadata$Probe.ACC), -1)
megadata$Prev.ACC[megadata$SubTrial %in% c(NA, 1, 33+1)] = NA
megadata$Prev.TrialType = head(c(NA, megadata$TrialType), -1)
megadata$Prev.TrialType[megadata$SubTrial %in% c(NA, 1, 33+1)] = NA
megadata$Prev.feedbackmask = head(c(NA, megadata$feedbackmask), -1)
megadata$Prev.feedbackmask[megadata$SubTrial %in% c(NA, 1, 33+1)] = NA
# Check accuracy of these columns
megadata %>%
  select(Subject, SubTrial, Probe.ACC, Prev.ACC, TrialType, Prev.TrialType, 
         feedbackmask, Prev.feedbackmask) %>%
  View
# Looks good.

# flat-field data
megadata  %>% 
  filter(TrialType != "") %>%
  select(Subject, Probe.ACC, TrialType, feedbackmask, SubTrial) %>%
  group_by(Subject, TrialType, Probe.ACC, feedbackmask) %>%
  summarise("count" = length(SubTrial)) %>%
  arrange(Subject, TrialType, Probe.ACC, feedbackmask) %>%
  View

# TrialType, too-slows discarded
megadata  %>% 
  filter(TrialType != "") %>%
  select(Subject, Probe.ACC, TrialType, feedbackmask, SubTrial) %>%
  group_by(Subject, TrialType, Probe.ACC, feedbackmask) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  filter(feedbackmask == "fast") %>%
  arrange(count) %>%
  View

# Current Acc analysis, collapsing across type, too-slows discarded
megadata  %>% 
  filter(TrialType != "", feedbackmask == "fast") %>%
  select(Subject, Probe.ACC, SubTrial) %>%
  group_by(Subject, Probe.ACC) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  arrange(count) %>%
  View

# TrialType analysis, correct trials only
megadata  %>% 
  filter(TrialType != "", feedbackmask == "fast", Probe.ACC == 1) %>%
  select(Subject, TrialType, SubTrial) %>%
  group_by(Subject, TrialType) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  arrange(count) %>%
  View

# prevACC analysis, current-correct trials only, no prev-slow or current-slow
megadata  %>% 
  filter(TrialType != "", Probe.ACC == 1,
         feedbackmask == "fast", Prev.feedbackmask == "fast") %>%
  select(Subject, Prev.ACC, SubTrial) %>%
  group_by(Subject, Prev.ACC) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  arrange(count) %>%
  View

# ErrorType analysis 1
megadata  %>% 
  filter(TrialType != "", Probe.ACC == 0, feedbackmask == "fast") %>%
  #filter(TrialType %in% c("BlackTool", "WhiteTool")) %>%
  select(Subject, TrialType, SubTrial) %>%
  group_by(Subject, TrialType) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  arrange(count) %>%
  View

# ErrorType analysis 2
megadata  %>% 
  filter(TrialType != "", Probe.ACC == 0, feedbackmask == "fast") %>%
  mutate(TrialBin = ifelse(TrialType == "BlackTool", "BlackTool", "Other")) %>%
  select(Subject, TrialBin, SubTrial) %>%
  group_by(Subject, TrialBin) %>%
  summarise("count" = length(SubTrial))  %>%
  ungroup %>%
  arrange(count) %>%
  View



# Error rates?
megadata  %>% 
  filter(TrialType != "", feedbackmask == "fast") %>%
  select(Subject, Session, Probe.ACC, TrialType) %>%
  group_by(Subject, Session, TrialType) %>%
  summarise("meanAcc" = mean(Probe.ACC, na.rm=T)) %>%
  View