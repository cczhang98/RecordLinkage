
## libraries
library(dplyr)
library(stringdist)
library(stringr)

### SET PARAMETERS ###
# input files directory
left_file <- read.csv("~/Desktop/master1930.csv", fileEncoding="latin1")
right_file <- read.csv("~/Desktop/using1930.csv", fileEncoding="latin1")
individual_links <- read.csv("~/Desktop/individual_links.csv")

# output directory
output_directory = "~/Desktop/output.csv"

# specify the census data 
# the left should be the left column in individual links file and right should be the right column
leftname = "master1930"
rightname = "ICR"

blocking = c("sex","metastate")

firstname_cutoff = 0.5
lastname_cutoff = 0.5
age_cutoff = 2
requirements = 2

percent_match = 0.25

lowest_similarity = 0.75

### PARAMETERS END ###


#select only relevant variables
left = left_file[,c("personID","householdID","namelast","namefrst","birthyr",blocking)]
right = right_file[,c("personID","householdID","namelast","namefrst","birthyr",blocking)]
#remove NAs
left = left[complete.cases(left), ]
right = right[complete.cases(right), ]

# rename variables by "left" and "right"
colnames(left) = paste("left",colnames(left),sep="_")
colnames(right) = paste("right",colnames(right),sep="_")
colnames(individual_links) = c("similscore","left_personID","right_personID")

# convert to correct data type
individual_links [,1:3] <- sapply(individual_links [,1:3] ,as.character)
individual_links [,1] <- sapply(individual_links [,1] ,as.numeric)

left[,1:ncol(left)] <- sapply(left[,1:ncol(left)],as.character)
right[,1:ncol(right)] <- sapply(right[,1:ncol(right)],as.character)
left[,"left_birthyr"]<- sapply(left[,"left_birthyr"],as.numeric)
right[,"right_birthyr"]<-sapply(right[,"right_birthyr"],as.numeric)

# create the reference class for linking
person <- setRefClass("person", 
                      fields=list(lastname="character",frstname="character",birthyr="numeric"),
                      methods = list(
                        initialize=function(...,lastname="lastname",frstname="frstname",birthyr=1800)
                        {
                          callSuper(..., lastname = lastname,frstname = frstname, birthyr = birthyr)
                        },
                        
                        compare = function(b){
                          score = 0
                          if (abs(birthyr-b$birthyr) <= age_cutoff) score = score+1
                          if (stringsim(frstname,b$frstname) >= firstname_cutoff) score = score+1
                          if (stringsim(lastname,b$lastname)>=extra_lastname_cutoff) score = score+1
                          return (score)
                        }
                      )
)


# calculate hscore
individual_links$hscore = rep(0, nrow(individual_links))
individual_links$lefthsize = rep(0, nrow(individual_links))
individual_links$righthsize = rep(0, nrow(individual_links ))

for (i in 1:nrow(individual_links)){
  leftid <- individual_links$left_personID[i]
  rightid <- individual_links$right_personID[i]
  
  lefthousehold <- left$left_householdID[left$left_personID==leftid]
  righthousehold <- right$right_householdID[right$right_personID==rightid]
  
  lefth <- left[left$left_householdID==lefthousehold,]
  righth <- right[right$right_householdID==righthousehold,]
  
  individual_links$lefthsize[i] = nrow(lefth)
  individual_links$righthsize[i] = nrow(righth)
  
  if(nrow(lefth)==0 | nrow(righth)==0){
    individual_links$hscore[i] <- 0
    
    next
  }
  
  for(j in 1:nrow(lefth)){
    leftp <- person$new(lastname = lefth$left_namelast[j], frstname = lefth$left_namefrst[j], birthyr = lefth$left_birthyr[j])
    
    for(k in 1:nrow(righth)){
      rightp <- person$new(lastname = righth$right_namelast[k], frstname = righth$right_namefrst[k], birthyr = righth$right_birthyr[k])
      
      if(leftp$compare(rightp)>=requirements) {
        individual_links$hscore[i] <- individual_links$hscore[i] + 1
        break
      } 
      
    }
  }
}

# merge with other variables
full <- merge(individual_links, left, by="left_personID")
full <- merge(full, right, by="right_personID")


# select final match if there are multiple Stage1 matches
final <- full %>% group_by(right_personID) %>% slice(which.max(similscore+hscore))
final <- final %>% group_by(left_personID) %>% slice(which.max(similscore+hscore))


## Find extra matches based on hscore 
# choose households based on hscore and household sizes
final = mutate(final, hchoose = ifelse(hscore/mean(lefthsize,righthsize) >= percent_match,TRUE,FALSE))

# remove "false positives" based on similscore and household information
final <- final  %>% filter(!(similscore<lowest_similarity & hchoose==FALSE))

# create data frame to store the extra matches 
extra <- data.frame(similscore = logical(), left_personID=logical(), right_personID=logical(),hscore=numeric())

# households to look for matches
finalh <- unique(final[final$hchoose==TRUE,c("left_householdID","right_householdID","hscore")])
finalh <- finalh %>% group_by(left_householdID) %>% slice(which.max(hscore))
finalh <- finalh %>% group_by(right_householdID) %>% slice(which.max(hscore))

# find extra matches in these households
for (i in 1:nrow(finalh)){
  lefthid <- finalh$left_householdID[i]
  righthid <- finalh$right_householdID[i]
  
  left2 <- left[left$left_householdID==lefthid,] %>% filter(!(left_personID %in% final$left_personID))
  right2 <- right[right$right_householdID==righthid,] %>% filter(!(right_personID %in% final$right_personID))
  
  if(nrow(left2)==0 | nrow(right2)==0){
    next
  }
  
  
  for(j in 1:nrow(left2)){
    leftp <- person$new(lastname = left2$left_namelast[j], frstname = left2$left_namefrst[j], birthyr = left2$left_birthyr[j])
    leftp2 <- left2[j,paste("left",blocking,sep="_")]
    
    max_score = 0
    max_index = 0
    
    for(k in 1:nrow(right2)){
      rightp <- person$new(lastname = right2$right_namelast[k], frstname = right2$right_namefrst[k], birthyr = right2$right_birthyr[k])
      rightp2 <- right2[k,paste("right",blocking,sep="_")]
      
      if(sum(leftp2 == rightp2) == length(blocking)){
        
        if(leftp$compare(rightp)>max_score){
          max_index = k
          max_score = leftp$compare(rightp)
        } 
      }
    }
    
    if(max_score>=extra_requirements){
      extra[nrow(extra)+1,] <- c(NA, left2$left_personID[j], right2$right_personID[max_index],finalh$hscore[i])
    }
    
  }
  
}

# merge with other variables
extra <- merge(extra, left, by="left_personID")
extra <- merge(extra, right, by="right_personID")

# remove unecessary columns
final = subset(final, select = -c(lefthsize,righthsize,hchoose))
extra$hscore <- as.numeric(extra$hscore)
final <- dplyr::bind_rows(final,extra)

# convert variable names back to the ones in original file
colnames(final) = sub("left",leftname,colnames(final))
colnames(final) = sub("right",rightname,colnames(final))

write.csv(final, output_directory)


