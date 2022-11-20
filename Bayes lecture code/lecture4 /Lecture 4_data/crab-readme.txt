# to read crab.txt in R, place the file into your R working directory and
# run the following
#
crabs <- read.table("crab.txt")
names(crabs) <- c("Obs","Colour","Spine","Weight","Width","Satellites")
colour.map <- c("lt.med","med","dk.med","dk")
crabs$Colour <- as.factor(colour.map[crabs$Colour])
spine.map  <- c("both.gd","one.bad","both.bad")
crabs$Spine <- as.factor(spine.map[crabs$Spine])
rm(colour.map,spine.map)