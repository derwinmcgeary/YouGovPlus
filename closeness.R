loadfile <- function(myurl) {
  library(downloader)
  if(!file.exists("Report.pdf")) {
    download(myurl, "Report.pdf") }
  if(!file.exists("Report.txt")) {
    system("pdftotext -layout Report.pdf") }
  readLines("Report.txt")
}

raw2stats <- function(rawdata, party, start) {
# conservative is 2, labour is 1... let's not get fancy
  thisparty <- ifelse(party==2,"Conservative","Labour")
  # Category name, CLOSE, NOT CLOSE
  coords <- as.data.frame(rbind(c(start,1,24), c(start+3,81,83), c(start+6,81,83)))
  colnames(coords) <- c("line","start","end")
  group <- substr(rawdata[start],1,24)
  liq <- as.numeric(unlist(strsplit(rawdata[sum(start,3)]," ")))
  lir <- as.numeric(unlist(strsplit(rawdata[sum(start,6)]," ")))
  liq <- liq[!is.na(liq)]
  lir <- lir[!is.na(lir)]
  close <- liq[2]
  notclose <- lir[2]
  #framedata <- as.data.frame(rbind(with(coords, substr(rawdata[line],start,end))))
  framedata <- cbind(thisparty, group, close, notclose)
  colnames(framedata) <- c("Party", "Group", "Close", "NotClose")
  framedata
}

plotme <- function(dataset) {
  library(ggplot2)
  library(xkcd)
  p <- ggplot(data=dataset, aes(x=Group, y=Number, fill=Party)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_y_continuous(breaks=seq(-90,90,10),label=abs(seq(-90,90,10))) +
    scale_fill_manual(values=c("#9999FF","#FF5555")) +
    scale_colour_manual(values=c("#555555","#FF5555")) +
    ggtitle(expression(atop("How close is each party to the following groups?", atop("      % Consider Close <--------------------       -------------------> % Consider Not Close","")))) +
    ylab("") +
    xlab("") +
    theme_bw() +
    geom_hline(aes(yintercept=0),colour="#FFFFFF") +
    coord_flip()
  print(p)
}

# Read the data in as a character vector of lines
reporturl <- "https://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/dmi10ux8ie/InternalResults_151002_PartyGroups_Website.pdf"
raw <- loadfile(reporturl) 

# Convert to a Dataframe
conservative <- c(21,29,37,45,72,80,88,96,104,131,139,147) # Line numbers for 
labour <- c(179,187,195,203,230,238,247,255,263,289,297,305) # each sub table in the pdf
starts <- as.data.frame(cbind(conservative,labour))
output <- data.frame()
for(i in starts[,1]) output <- rbind(output,raw2stats(raw,2,i))
for(i in starts[,2]) output <- rbind(output,raw2stats(raw,1,i))

# Tidy it up a bit
library(tidyr)
output$Close <- (-1)*as.numeric(as.character(output$Close)) # We make the "Close" value negative...
output$NotClose <- as.numeric(as.character(output$NotClose)) # ...  so that it's physically closer to the label
output <- gather(output, Measure, Number, Close:NotClose)
output$Group <- factor(output$Group, levels=unique(output$Group[order(output$Party,output$Measure, (-1)*output$Number)]))

# ... and plot!
plotme(output)