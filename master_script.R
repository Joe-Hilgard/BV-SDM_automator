source("makeSDM.r")
source("../BV-MDM_automator/makeMDM.r")
# would be nice to put them both in same working directory. Really they're two sides of same project.

# Bruce request 1: BlackTool errors vs. other errors.
makeSDM("conditions-ErrorType2.txt")
makeMDM("conditions-ErrorType2.txt")

# Bruce request 2: BlackTool errors vs. WhiteTool errors.
makeSDM("conditions-ErrorType1.txt")
makeMDM("conditions-ErrorType1.txt")

# Bruce request 3: Hits vs. Misses.
makeSDM("conditions-currentAcc.txt")
makeMDM("conditions-currentAcc.txt")

# Bruce request 4: PrevMiss vs. PrevHit trials.
makeSDM("conditions-prevAcc.txt")
makeMDM("conditions-prevAcc.txt")

# For shits and giggles: Congruent and Incongruent hits.
makeSDM("conditions-CurrentTrial.txt")
makeMDM("conditions-CurrentTrial.txt")


# Bruce request 5: "Interactions of the above w/ beverage"
  # Does not require novel sdm/mdm, can be run w/in ANOVA tools.

# Bruce request 6: "Beta-map connectivity"
  # Barf, good luck.

