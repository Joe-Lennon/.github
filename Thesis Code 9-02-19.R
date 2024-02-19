### Read in all the data and merge it
#C:\Users\Joe\Documents\CCSU\Thesis\Lahman db\baseballdatabank-2019.2\core

appearances<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/appearances.csv",header=T)
#appearances<-read.csv("E:/SanDiskSecureAccess/MLBproj/baseballdatabank-2019.2/core/appearances.csv",header=T)
appearances
all_star<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/AllstarFull.csv",header=T)
all_star
batting<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/batting.csv",header=T)
batting
batting_postseason<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/battingPost.csv",header=T)
batting_postseason
college<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/collegePlaying.csv",header=T)
college
fielding<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/fielding.csv",header=T)
fielding
fielding_outfield<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/fieldingOF.csv",header=T)
fielding_outfield
player_award_vote<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/AwardsSharePlayers.csv",header=T)
player_award_vote
awards_players<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/AwardsPlayers.csv",header=T)
awards_players

player<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/People.csv",header=T)
player

pitching<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/Lahman db/baseballdatabank-2019.2/core/pitching.csv",header=T)
pitching

warhit<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/war_daily_bat.csv",header=T,                 )
warhit
warpitch<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis//war_daily_pitch.txt",header=T)
warpitch

str(warhit)
#### Teams
teams<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis//Lahman db/baseballdatabank-2019.2/core/teams.csv",header=T,                 )
teams

#### Clean it up and merge stuff

mvp <- subset(player_award_vote, awardID=="MVP")


## SLUG = total bases/AB
## OBP = H+BB+HBP/(AB+BB+HBP+SF)

batting[is.na(batting)] <- 0

batting$batavg  <-batting$H/batting$AB 
batting$obp  <-(batting$H+batting$BB+batting$HBP)/(batting$AB +batting$BB+batting$HBP+batting$SF) 
batting$TB <- (batting$H-(batting$X2B +batting$X3B+batting$HR) +batting$X2B*2 + batting$X3B*3 + batting$HR*4)
batting$slg <- (batting$H-(batting$X2B +batting$X3B+batting$HR) +batting$X2B*2 + batting$X3B*3 + batting$HR*4)/batting$AB
batting$OPS_1 <- batting$obp+batting$slg

bonds <- subset(batting,(playerID == 'bondsba01' & yearID == 2001))



hitmvp <-  merge(batting, mvp, all.x = TRUE, c("yearID","playerID"))
hitmvp <- subset(hitmvp, yearID > 1930)

## check so tough ones, to make sure they come through
chkx <- subset(hitmvp,playerID == 'ueckebo01')
chky <- subset(warhit,player_ID == 'ortizda01')

##Merge the War data
## http://www.programmingr.com/examples/r-dataframe/merge-data-frames/

allwar <-  merge(warhit, warpitch, all = TRUE, by = c("year_ID","player_ID","team_ID"))
chk3 <- subset(allwar,player_ID == 'gagneer01')

allwar$yearID <- allwar$year_ID
allwar$playerID <- allwar$player_ID
allwar$team_id <- allwar$team_ID
allwar <-subset(allwar, yearID >1930)

## Add in All WAR data, clean up the teams so the merge works
library(plyr)
count(hitmvp, 'teamID')
count(allwar, 'teamID')
#### Recode Team so the merge works;
# Replace the data in a field based on equal to some value
#CHW	CHA #FLA	FLO #KCA	KC1 #KCR	KCA #LAD	LAN
#MLN	ML1 #NYG	NY1 #NYM	NYN #NYY	NYA #SDP	SDN
#SEP	SE1 #SFG	SFN #SLB	SLA #STL	SLN #WSA	WAS
### whittle down the dupes, the league team names are different on the different sources
###use lestejo01
allwar$team_id <-ifelse(allwar$team_id =="CHC","CHN",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="CHW","CHA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="FLA","FLO",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="KCR","KCA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="KC2","KCA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="LAD","LAN",as.character(allwar$team_id))

allwar$team_id <-ifelse(allwar$team_id =="MLN","ML1",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="NYG","NY1",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="NYM","NYN",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="NYY","NYA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="SDP","SDN",as.character(allwar$team_id))

allwar$team_id <-ifelse(allwar$team_id =="SEP","SE1",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="SFG","SFN",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="SLB","SLA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="STL","SLN",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="WSA","WAS",as.character(allwar$team_id))
#WSH	WAS #WSN	WAS #WS1	WAS #WS2	WAS
allwar$team_id <-ifelse(allwar$team_id =="WSH","WAS",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="WSN","WAS",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="WS1","WAS",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="WS2","WAS",as.character(allwar$team_id))

allwar$team_id <-ifelse(allwar$team_id =="TBD","TBA",as.character(allwar$team_id))
allwar$team_id <-ifelse(allwar$team_id =="TBR","TBA",as.character(allwar$team_id))


### Rename so can merge with WAR
colnames(hitmvp)[colnames(hitmvp)=="teamID"] <- "team_id"
## Fix Mil Braves to match
hitmvp$team_id <-ifelse(hitmvp$team_id =="ML4","MIL", as.character(hitmvp$team_id))


hitmvpWAR <-  merge(hitmvp, allwar, all.x = TRUE, by = c("yearID","playerID","team_id"))

chk4 <- subset(hitmvpWAR,player_ID == 'gagneer01')

chk5 <- subset(allwar,player_ID == 'aaronha01')
chk6 <- subset(hitmvpWAR,playerID == 'aaronha01')
chk7 <- subset(all_star,playerID == 'aaronha01')

library(dplyr)


## go back and look for the WAR that drops out 5k

## Add in All star appearences
## a couple years had two ASG games keep one per year
all_star2<-all_star %>% 
    group_by(yearID, playerID,teamID) %>%                            # multiple group columns
    summarise(sum_ASG = sum(GP))  # multiple summary columns


hitmvpWARASG <-  merge(hitmvpWAR, all_star2, all.x = TRUE, c("yearID","playerID"))
chk8 <- subset(hitmvpWARASG,playerID == 'aaronha01')

## Add pitching
#pitching$team_id.x <- as.character(pitching$teamID)
colnames(pitching)[colnames(pitching)=="teamID"] <- "team_id"
wpitch <- merge(hitmvpWARASG, pitching, all.x = TRUE, by = c("yearID","playerID","team_id"))
### Figure out position for each player for each year and merge
## need player, year, sum(games), position
## keep highest position
#install.packages("dplyr")
gamesby<-fielding %>% 
  group_by(yearID, playerID,POS) %>%                            # multiple group columns
  summarise(sum_g = sum(G))  # multiple summary columns

### Find highest # of games for each player each year
gamesby <- dplyr::arrange(gamesby,yearID,playerID,desc(sum_g))
### keep the highest
gamesby2 <- gamesby[!duplicated(gamesby$playerID, gamesby$yearID),]
## Add postition
wpos <- merge(wpitch, gamesby2, all.x = TRUE, c("yearID","playerID"))

wpos$playername <-ifelse(is.na(wpos$name_common.y),as.character(wpos$name_common.x),as.character(wpos$name_common.y))

colnames(appearances)[colnames(appearances)=="teamID"] <- "team_id"

chk5 <- subset(gamesby,playerID == 'aaronha01')

## play with iding pos
library(data.table)
DT <- data.table(appearances)
appear<- DT[, MAXPOS := colnames(.SD)[max.col(.SD, ties.method="last")],
            .SDcols = c( "G_c", "G_1b","G_2b","G_3b","G_ss","G_dh","G_lf",
                        "G_rf","G_cf", "G_ph","G_pr")]

chk5 <- subset(appear,playerID == 'overbly01') 

keeper<- c('yearID', 'playerID', 'team_id','G_all', 'MAXPOS')
### drop vars I don't need
playpos = subset(appear, select = c('yearID', 'playerID', 'team_id','G_all', 'MAXPOS'))
playpos = subset(appear, select = keeper)
### Fix Milwaukee
## Fix Mil Braves to match
playpos$team_id <-ifelse(playpos$team_id =="ML4","MIL", as.character(playpos$team_id))

playpos$team_id.x <-playpos$team_id

wpos2 <- merge(wpitch, playpos, all.x = TRUE,by= c("yearID","playerID","team_id"))

chk5 <- subset(wpos2,playerID == 'aaronha01') 
chk6 <- subset(playpos,playerID == 'aaronha01') 
chk7 <- subset(wpos2,playerID == 'ortizda01') 

### Select a Random thousand to see how it looks so far
### limit the analytic dataset
#check<- subset(wpos2, year == '2014' & (g.x >30 | g.y > 15))
#write.csv(check, file = "C:/Users/Joe/Documents/CCSU/Thesis/TestData.csv")

### Drop some variables as I have plenty of other vars to look at;
#https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
#drop <- c("x","z")
#df = mydata[,!(names(mydata) %in% drop)]

dropvars<- c('award_id','league_id.y','points_max','year_ID','player_ID','team_ID',	
            'mlb_ID.x','stint_ID.x','lg_ID.x','G.x',	'WAR_rep.y','ERA_plus',
            'TOB_lg','TB_lg','name_common.y','salary.y','H.y','ER','HR.y','BB.y','SO.y',
            'mlb_ID.y','stint_ID.y','lg_ID.y','G.y','xRA','xRA_sprp_adj','xRA_def_pitcher','PPF',	
            'PPF_custom','xRA_final','BIP','BIP_perc','RS_def_total','runs_above_avg.y',	
            'runs_above_avg_adj','runs_above_rep.y','RpO_replacement','GR_leverage_index_avg',	
            'teamRpG.y','oppRpG.y','pyth_exponent.y','waa_win_perc.y','WAA.y','WAA_adj','oppRpG_rep.y',	
            'pyth_exponent_rep.y','waa_win_perc_rep.y','ER_lg','game_id','team_id.y','league_id.x.1',	
            'gp','starting_pos','age.y', 'GS.x', 'IPouts.x','IPouts_start','IPouts_relief','RA','WAR.y',
            'teamID','stint.y','lgID','W.x','L.x','GS.y','CG','SHO','SV','IPouts.y',
            'BAOpp','ERA','IBB.y','WP','HBP.y','BK','BFP','GF','R.y','SH.y','SF.y','GIDP.y')	


#### Limit the Analyticsal Dataset by Games Played in a Season
#### Get rid of Pitchers cause it now an Estimation Model #####

fewer <- wpos2[,!(names(wpos2) %in% dropvars)]
#| g.y > 15
fewer$PA <- as.numeric(as.character(fewer$PA))  # Convert one variable to numeric
fewer <- subset(fewer,  (PA > 0 & pitcher =="N" ))
str(fewer)

chk3 <- subset(fewer,playerID == 'aaronha01')

### handle nulls and clean it up for model
library(plyr)
### Make a dummy for votes
fewer$hasvote <- ifelse(is.na(fewer$pointsWon), 0, 1) # where variable type is number of points
fewer$frstplvte <- ifelse((fewer$votesFirst) == 0 |is.na(fewer$votesFirst), 0, 1) # where variable type is number of points

str(fewer)
## check Yaz
yaz <- subset(fewer,playerID == 'yastrca01')

x<-(table(fewer$hasvote, fewer$year))
x

### Convert all Factors to Numeric
vec_5 <- subset(fewer,playerID != 'lennon') 
str(vec_5)

vec_5$age.x <- as.numeric(as.character(vec_5$age.x))  # Convert one variable to numeric
vec_5$Inn <- as.numeric(as.character(vec_5$Inn))  # Convert one variable to numeric
vec_5$runs_good_plays <- as.numeric(as.character(vec_5$runs_good_plays))  # Convert one variable to numeric
vec_5$runs_position  <- as.numeric(as.character(vec_5$runs_position))  # Convert one variable to numeric
vec_5$runs_replacement  <- as.numeric(as.character(vec_5$runs_replacement))  # Convert one variable to numeric
vec_5$runs_above_rep.x  <- as.numeric(as.character(vec_5$runs_above_rep.x))  # Convert one variable to numeric
vec_5$runs_above_avg.x  <- as.numeric(as.character(vec_5$runs_above_avg.x))  # Convert one variable to numeric
vec_5$runs_above_avg_off  <- as.numeric(as.character(vec_5$runs_above_avg_off))  # Convert one variable to numeric
vec_5$runs_above_avg_def  <- as.numeric(as.character(vec_5$runs_above_avg_def))  # Convert one variable to numeric
vec_5$WAA_def  <- as.numeric(as.character(vec_5$WAA_def))  # Convert one variable to numeric
vec_5$WAR_def  <- as.numeric(as.character(vec_5$WAR_def))  # Convert one variable to numeric
vec_5$WAR.x  <- as.numeric(as.character(vec_5$WAR.x))  # Convert one variable to numeric
vec_5$WAR.y  <- as.numeric(as.character(vec_5$WAR.y))  # Convert one variable to numeric
vec_5$WAR_off  <- as.numeric(as.character(vec_5$WAR_off))  # Convert one variable to numeric
vec_5$WAR_def  <- as.numeric(as.character(vec_5$WAR_def))  # Convert one variable to numeric
vec_5$WAR_rep.x  <- as.numeric(as.character(vec_5$WAR_rep.x))  # Convert one variable to numeric
vec_5$WAR_rep.y  <- as.numeric(as.character(vec_5$WAR_rep.y))  # Convert one variable to numeric
vec_5$salary.x  <- as.numeric(as.character(vec_5$salary.x))  # Convert one variable to numeric
vec_5$salary.y  <- as.numeric(as.character(vec_5$salary.y))  # Convert one variable to numeric
vec_5$OPS_plus  <- as.numeric(as.character(vec_5$OPS_plus))  # Convert one variable to numeric
vec_5$oppRpG.x  <- as.numeric(as.character(vec_5$oppRpG.x))  # Convert one variable to numeric
vec_5$teamRpG.x  <- as.numeric(as.character(vec_5$teamRpG.x))  # Convert one variable to numeric
vec_5$age.y  <- as.numeric(as.character(vec_5$age.y))  # Convert one variable to numeric
vec_5$IPouts_start  <- as.numeric(as.character(vec_5$IPouts_start))  # Convert one variable to numeric
vec_5$IPouts_relief  <- as.numeric(as.character(vec_5$IPouts_relief))  # Convert one variable to numeric
vec_5$ERA_plus  <- as.numeric(as.character(vec_5$ERA_plus))  # Convert one variable to numeric

str(vec_5)

chk5 <- subset(vec_5,playerID == 'ortizda01')

## fix nulls and recode Pitcher
vec_5[is.na(vec_5)] <- 0
vec_5[is.null(vec_5)] <- 0

library(forcats)
#https://stackoverflow.com/questions/39126537/replace-na-in-a-factor-column
## Plug the null factors so I don't lose the obs.
vec_5$pitcher<- fct_explicit_na(vec_5$pitcher, "N")

### Clean up the Teams
### https://www.statmethods.net/management/subset.html
# select variables v1, v2, v3
### Drop everything except what I need
myvars <- c("yearID", "teamID", "Rank","W","L", "DivWin","WCWin", "LgWin")
Wins <- teams[myvars]
Wins
### Rename Vars so they match
# Rename a column in R http://rprogramming.net/rename-columns-in-r/
#colnames(Wins)[colnames(Wins)=="yearID"] <- "year"
colnames(Wins)[colnames(Wins)=="teamID"] <- "team_id"
#Wins

### Merge it onto the data set
GotWins <-  merge(vec_5, Wins, all.x = TRUE, by = c("yearID","team_id"))
chk3 <- subset(GotWins,playerID == 'ortizda01')

### Make a dummy for WINS
GotWins$Winner <- ifelse((GotWins$DivWin =='Y'), 1, 0) 

x<-(table(GotWins$Winner, GotWins$year))
x

countTeambat<-(table(batting$teamID))
countTeambat

countTeamWAR<-(table(warhit$team_ID))
countTeamWAR

### Add in fielding stats
fielding$team_id <-fielding$teamID

library(dplyr)    ## load the package
fieldby <-fielding %>% 
    group_by(yearID, playerID,team_id) %>%                            # multiple group columns
    summarise(sum_g = sum(G), sum_PO = sum(PO), sum_A = sum(A), sum_E = sum(E), sum_DP=sum(DP),
              sum_ZR = sum(ZR)) 
# multiple summary columns
## Fix Mil Braves to match
fieldby$team_id <-ifelse(fieldby$team_id =="ML4","MIL", as.character(fieldby$team_id))

GotFielding <-  merge(GotWins, fieldby, all.x = TRUE, by = c("yearID","playerID","team_id"))

### Add ASG Flag
GotFielding$ASGAppear <- ifelse((GotFielding$sum_ASG != 0), 1, 0) 
chk3 <- subset(GotFielding,playerID == 'aaronha01')

uecker <- subset(warhit,player_ID == 'ueckebo01')
papi <- subset(GotFielding,playerID == 'ortizda01')
hank <- subset(GotFielding,playerID == 'aaronha01')

hankW <- subset(warhit,player_ID == 'aaronha01')

bonds <- subset(GotFielding,(playerID == 'bondsba01' & yearID == 2001))

### Do Some Correlation ###
library ('ggcorrplot')
dataforspear <-subset(GotFielding,pitcher =="N" & yearID >1959)

keepforcorr <- c('R.x','H.x','X2B','X3B','HR.x','RBI','SB','BB.x','SO.x',
                  'age.x','batavg','obp','PA', 'GIDP.x','WAR.x','OPS_plus',
                 'sum_PO','sum_A','sum_E', 'sum_DP', 'sum_ZR', 'slg')
dataforspear <- GotFielding[,(names(GotFielding) %in% keepforcorr)]

## fix nulls and recode Pitcher
dataforspear[is.na(dataforspear)] <- 0

corrdata <- cor(dataforspear, method = "pearson")

library(corrplot)
corrplot(corrdata, method="number")

#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrdata, method = "color", col = col(200),
         type = "full", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)





str(GotFielding$MAXPOS)

countPos<-(table(GotFielding$MAXPOS))
countPos

## clean up UZR
GotFielding[is.na(GotFielding)] <- 0
## add fielding pct
## (putouts + assists + errors) is the base
GotFielding$fldpct <- (GotFielding$sum_A + GotFielding$sum_PO)/(GotFielding$sum_A + GotFielding$sum_PO+GotFielding$sum_E)

## add OF pos

## add position flags
GotFielding$is.DH <- ifelse(GotFielding$MAXPOS %in% c('G_dh'),1,0)
GotFielding$is.1B <- ifelse(GotFielding$MAXPOS %in% c('G_1b'),1,0)
GotFielding$is.2B <- ifelse(GotFielding$MAXPOS %in% c('G_2b'),1,0)
GotFielding$is.3B <- ifelse(GotFielding$MAXPOS %in% c('G_3b'),1,0)
GotFielding$is.SS <- ifelse(GotFielding$MAXPOS %in% c('G_ss'),1,0)
GotFielding$is.C  <- ifelse(GotFielding$MAXPOS %in% c('G_c') ,1,0)
GotFielding$is.PH <- ifelse(GotFielding$MAXPOS %in% c('G_ph') ,1,0)
GotFielding$is.LF <- ifelse(GotFielding$MAXPOS %in% c('G_lf'),1,0)
GotFielding$is.RF <- ifelse(GotFielding$MAXPOS %in% c('G_rf'),1,0)
GotFielding$is.CF <- ifelse(GotFielding$MAXPOS %in% c('G_cf'),1,0)

## drop a few things, like ZR only have it for 10 years in 1960s?
moredrops <- c('W.y','L.y','DivWin','WCWin','LgWin','Winner','Rank','sum_ZR',
               'pyth_exponent_x','pyth_exponent_rep.x','pointsMax','lgID.y','pyth_exponent.x','W.x',
               'L.x')
GotFielding <- GotFielding[,!(names(GotFielding) %in% moredrops)]

GotFielding[is.na(GotFielding)] <- 0

hank <- subset(GotFielding,playerID == 'aaronha01')

### Clean up
ls()
# Or remove objects listed in a vector
rm(list = c("wpos", "college", "DT","field_x","wpitch","warpitch","appearances","yaz","pitching"
            ,"batting_postseason","player","chk4","chk5","chkx","chky","eepforcorr"))
ls()


### Set up plots
bigdf <- subset(GotFielding,(yearID > 1959 & PA > 0))
x1    <-bigdf$R.x
x2    <-bigdf$WAR.x

########################################################################################################
#####################################END END END END####################################################
########################################################################################################
#### Make a dataset to experiment with models in SPSS modeler
#write.csv(GotFielding, file = "C:/Users/Joe/Documents/CCSU/Thesis/FullData062419.csv")
#write.csv(GotFielding, file = "E:/SanDiskSecureAccess/MLBproj/FullData062319.csv")


####### DO some EDA #######
install.packages("hexbin")


smoothScatter(GotFielding$R.x,GotFielding$WAR.x, 
xlab ="Runs", ylab = "WAR", main = "Runs(x) vs WAR(y)")
abline(lm( GotFielding$WAR.x ~ GotFielding$R.x), col = "red") 

library(knitr)
knitr::opts_chunk$set(tidy=FALSE, 
                      fig.width=10,
                      fig.height=5,
                      fig.align='left',
                      warning=FALSE,
                      message=FALSE,
                      echo=TRUE)
options(width = 120)
library(ggplot2)
library(colorspace)
library(gridExtra)

###1 RUNS ####
p  = ggplot(bigdf,aes(x=x1,y=x2))  + ggtitle("Runs vs WAR") + xlab("Runs") + ylab("WAR")
p2 = p + geom_point(alpha = .50, colour="black",shape = 21, fill="blue", size = 4) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)   
grid.arrange(p2,ncol=1)

###2 HRs ####
p  = ggplot(bigdf,aes(x=HR.x,y=WAR.x))  + ggtitle("HRs vs WAR") + xlab("HRs") + ylab("WAR")
p2 = p + geom_point(alpha = .70, colour="black",shape = 21, fill="blue", size = 3.5) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

##3 PAs ####
p  = ggplot(bigdf,aes(x=PA,y=WAR.x))  + ggtitle("Plate Appearances vs WAR") + xlab("PAs") + ylab("WAR")
p2 = p + geom_point(alpha = .50, colour="black",shape = 21, fill="blue",size = 3.0) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)


###4 BA ####
p  = ggplot(bigdf,aes(x=batavg,y=WAR.x))  + ggtitle("Batting Average vs WAR") + xlab("Batting Avg") + ylab("WAR")
p2 = p + geom_point(alpha = .80, colour="black",shape = 21, fill="blue",size=3) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

###41 BA.2 ####
minPA <- subset(GotFielding,(yearID > 1959 & PA > 150))
p  = ggplot(minPA,aes(x=batavg,y=WAR.x))  + ggtitle("Batting Average vs WAR") + xlab("Batting Avg") + ylab("WAR")
p2 = p + geom_point(alpha = .80, colour="black",shape = 21, fill="blue",size=3) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)


###5 RBI ####
p  = ggplot(bigdf,aes(x=RBI,y=WAR.x))  + ggtitle("RBIs vs WAR") + xlab("RBIs") + ylab("WAR")
p2 = p + geom_point(alpha = .60, colour="black",shape = 21, fill="blue", size = 3) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)


###6 SLG ####
p  = ggplot(bigdf,aes(x=slg,y=WAR.x))  + ggtitle("Slugging Pct. vs WAR") + xlab("slg pct") + ylab("WAR")
p2 = p + geom_point(alpha = .80, colour="black",shape = 21, fill="blue", size = 3) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

###61 SLG ####
p  = ggplot(minPA,aes(x=slg,y=WAR.x))  + ggtitle("Slugging Pct. vs WAR") + xlab("slg pct") + ylab("WAR")
p2 = p + geom_point(alpha = .70, colour="black",shape = 21, fill="blue", size = 3) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)


###7 OBP ####
p  = ggplot(minPA,aes(x=obp,y=WAR.x))  + ggtitle("obp vs WAR") + xlab("obp") + ylab("WAR")
p2 = p + geom_point(alpha = .70, colour="black",shape = 21, fill="blue", size = 3.5) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

###8 OPS_Plus ####
p  = ggplot(minPA,aes(x=OPS_plus,y=WAR.x))  + ggtitle("OPS Plus vs WAR") + xlab("OPS") + ylab("WAR")
p2 = p + geom_point(alpha = .70, colour="black",shape = 21, fill="blue", size = 3.5) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

### 9 Do Box Plot by Pos
#GotFielding <-read.csv("E:/SanDiskSecureAccess/MLBproj/FullData062319.csv",header=T)
#GotFielding

# Change box plot line colors by groups
p<-ggplot(bigdf, aes(x=bigdf$MAXPOS, y=bigdf$WAR.x, color=MAXPOS)) +
   ggtitle("Boxplots - WAR by Primary Fielding Position") + xlab("Position") + ylab("WAR") +
    geom_boxplot()+theme_gray() +
  theme(axis.text.x=element_text(size=rel(1.40),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.40),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.40),face="bold"))+ 
  theme(axis.title.y=element_text(size=rel(1.40),face="bold")) +
  theme(legend.text = element_text(size=rel(1.40)))
p

###10 Fielding ####
justPos <- subset(GotFielding,(yearID > 1959 & 
                              MAXPOS %in% c('G_1b','G_2b','G_3b','G_ss','G_cf','G_lf','G_rf','G_c') &
                              sum_PO >100 ))

p  = ggplot(justPos,aes(x=fldpct,y=WAR.x))  + ggtitle("Fielding Pct. vs WAR") + xlab("fldpct") + ylab("WAR")
p2 = p + geom_point(alpha = .65, colour="black",shape = 21, fill="blue", size = 4) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

###11 Errors ####
justPos <- subset(GotFielding,(yearID > 1960 & 
                                 MAXPOS %in% c('G_1b','G_2b','G_3b','G_ss','G_cf','G_lf','G_rf','G_c') ))

p  = ggplot(bigdf,aes(x=sum_E,y=WAR.x))  + ggtitle("Errors vs WAR") + xlab("Errors") + ylab("WAR")
p2 = p + geom_point(alpha = .75, colour="black",shape = 21, fill="blue", size = 4) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)

### Do Box Plot by ASG
# Change box plot line colors by groups
bigdf$ASG <- ifelse(bigdf$ASGAppear == 1,"ASG","No_ASG")
bigdf$MVPvtes <- ifelse(bigdf$hasvote == 1,"MVPvotes","No_MVPvotes")
## 12 MVP
p<-ggplot(bigdf, aes(x=bigdf$MVPvtes, y=bigdf$WAR.x, color=MVPvtes)) +
  ggtitle("Boxplots - WAR by MVP Votes or Not") + xlab("MVP Votes or Not") + ylab("WAR") +
  geom_boxplot()+theme_gray() +
  theme(axis.text.x=element_text(size=rel(1.40),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.40),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.40),face="bold"))+ 
  theme(axis.title.y=element_text(size=rel(1.40),face="bold")) +
  theme(legend.text = element_text(size=rel(1.40)))
p
## 13 ASG
p<-ggplot(bigdf, aes(x=bigdf$ASG, y=bigdf$WAR.x, color=ASG)) +
  ggtitle("Boxplots - WAR by ASG or Not") + xlab("ASG or Not") + ylab("WAR") +
  geom_boxplot()+theme_gray() +
  theme(axis.text.x=element_text(size=rel(1.40),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.40),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.40),face="bold"))+ 
  theme(axis.title.y=element_text(size=rel(1.40),face="bold")) +
  theme(legend.text = element_text(size=rel(1.40)))
p
### Fix ASG - done

## Rank Salary by Year
### https://stackoverflow.com/questions/34967837/rank-variable-by-group-dplyr/34968528
library(dplyr)
bigdf <- bigdf %>% arrange(yearID, salary.x) %>%
  group_by(yearID) %>% 
  mutate(rank = rank(desc(salary.x), ties.method = "first"))
#Arod
chk7 <- subset(by_pay,playerID == 'rodrial01')

###11 Salary Rank ####
p  = ggplot(by_pay,aes(x=rank,y=WAR.x))  + ggtitle("Salary Highest Rank to Lowest Rank vs WAR") + xlab("Salary Rank") + ylab("WAR")
p2 = p + geom_point(alpha = .75, colour="black",shape = 21, fill="blue", size = 4) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30),face="bold"))+
  theme(axis.text.y=element_text(size=rel(1.30),face="bold"))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ 
  theme(axis.title.y=element_text(size=rel(1.30)))
#+
#  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)


### Add some transformations
bigdf$R.x.2 <- bigdf$R.x^2
bigdf$PA.2 <- bigdf$PA^2
bigdf$BA.2 <- bigdf$batavg^2
bigdf$BA.3 <- bigdf$batavg^3
bigdf$slg.2 <- bigdf$slg^2
bigdf$obp.2 <- bigdf$obp^2
bigdf$ops.2 <- bigdf$OPS_1^2
bigdf$topsal <- ifelse(bigdf$rank < 50 ,1,0)
bigdf$young <- ifelse(bigdf$age.x < 21 ,1,0)
bigdf$old <- ifelse(bigdf$age.x > 35 ,1,0)
bigdf$pa_r <- bigdf$R.x*bigdf$PA

library(fBasics)
cols <- (c(6:26,28,29,31,33:34,72:90))
x <-bigdf[,cols]

MLBStats <- as.data.frame(basicStats(x))

## GO for Gold Glove;
countPos<-(table(awards_players$awardID))
countPos
goldglove <- subset(awards_players[1:3], awardID =='Gold Glove')

forout <-  merge(bigdf, goldglove, all.x = TRUE, by = c("yearID","playerID"))

colnames(forout)[colnames(forout)=="awardID.y"] <- "goldglove"
forout$gldglve <- ifelse(forout$goldglove == "Gold Glove",1,0)

forout[is.na(forout)] <- 0
forout <- forout[ -c(4, 105) ]


#write.csv(forout, file = "C:/Users/Joe/Documents/CCSU/Thesis/FullData062719.csv")
#write.csv(bigdf, file = "E:/SanDiskSecureAccess/MLBproj/FullData062719.csv")
#write.csv(MLBStats, file = "E:/SanDiskSecureAccess/MLBproj/SummaryStats.csv")




### Try to Box Cox transform Age
library(e1071)
library(caret) # for box-cox transformation
transCap <- BoxCoxTrans(GotFielding$age.x)
print(transCap)
cap.income <-GotFielding$age.x
trans <- (cap.income^(transCap$lambda) - 1)/(transCap$lambda)
trans
agemod <- lm(WAR.x ~ trans  ,data = GotFielding)
summary(agemod)
### doesn't really work

### Fix P pos - done
### transform variables for LM
ba <- GotFielding$obp
ba.2 <- GotFielding$obp^2
ba.3 <- GotFielding$obp^3

mod_1 <- lm(WAR.x ~ ba + ba.2 + ba.3  ,data = GotFielding)
summary(mod_1)

age.e <- log(GotFielding$age.x)
age.lg <- 1/(exp^(GotFielding$age.x))

agemod <- lm(WAR.x ~ age.lg  ,data = GotFielding)
summary(agemod)



###14 Age ####
justPos <- subset(GotFielding,(yearID > 1959 & 
                                 MAXPOS %in% c('G_1b','G_2b','G_3b','G_ss','G_cf','G_lf','G_rf','G_c') &
                                 sum_A >50 ))

p  = ggplot(justPos,aes(x=age.x,y=WAR.x))  + ggtitle("Player Age vs WAR") + xlab("age") + ylab("WAR")
p2 = p + geom_point(alpha = .25, colour="dodgerblue4",shape = 21, fill="blue", size = 4) + theme_classic()+ 
  theme(axis.text.x=element_text(size=rel(1.30)))+
  theme(axis.text.y=element_text(size=rel(1.30)))+
  theme(axis.title.x=element_text(size=rel(1.30)))+ theme(axis.title.y=element_text(size=rel(1.30)))+
  geom_smooth(color ="red", method=loess, se=FALSE)
grid.arrange(p2,ncol=1)




### finish descriptives



####
p = ggplot(bigdf,aes(x=x1,y=x2)) +
  ggtitle("Plot of 100K Point Dataset") +
  xlab("x1") +
  ylab("x2")
p1 = p + 
  geom_point(alpha = 0.01, colour="grey") + 
  geom_density2d() + 
  theme_bw()
p2 = p +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("blue","red"), 
                       name = "Frequency", 
                       na.value=NA)
grid.arrange(p1,p2,ncol=2)


#### Random Forest Model

#install.packages("rsample")
#install.packages("randomForest")
#install.packages("ranger")
#install.packages("caret")
#install.packages("h2o")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
#install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library("MASS")

#### Bring in Training Dataset
trainingD<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/TrainOutdata0629.csv",header=T)
trainingD
testD<-read.csv("C:/Users/Joe/Documents/CCSU/Thesis/TestOutdata0629",header=T)
testD

library("caret")
library("lattice")
library("randomForest")

#1 Build Random Forest Model with 1000 Trees
keepforRF <- c('R.x','H.x','X2B','X3B','HR.x','RBI','SB','BB.x','SO.x',
                 'age.x','batavg','obp','PA', 'GIDP.x','OPS_plus',
                 'sum_PO','sum_A','sum_E', 'sum_DP', 'sum_ZR', 'slg')
dataforRF <- trainingD[,(names(trainingD) %in% keepforRF)]
dataforRF_test <- testD[,(names(testD) %in% keepforRF)]


set.seed(100)

rfModel1 <- randomForest(dataforRF,trainingD$WAR.x, ntree=200, importance = TRUE,
                         do.trace=50, keep.forest = TRUE)

# Make a dataframe summarize and plot the summary
MSEvalues <-data.frame(MSE = rfModel1$mse, RSQ = rfModel1$rsq, Ntrees = seq(1,200))
MSEvalues$roundit <- (MSEvalues$Ntrees/200)
MSEvalues$group <- ceiling((MSEvalues$roundit))
# Plot the  OOB Error Estimate vs N of Trees #
plot(MSEvalues$Ntrees,MSEvalues$MSE,
     xlab = 'Number of Trees', ylab = 'MSE',
     main = "OOB Error Estimate (y) vs N of Trees (x)", type = "p")

# 2 - look at importance #
importance(rfModel1)
varImpPlot(rfModel1, n.var=20)
#3
#http://ugrad.stat.ubc.ca/R/library/randomForest/html/predict.randomForest.html
# - Use predict() function to find OOB predictions for the training data 
rfpredTrainOOB <- predict(rfModel1)
mean((rfpredTrainOOB-trainingD$WAR.x)^2) 


#4 - Use the model to predict Solubility of the Training Points
rfpredTraining <- predict(rfModel1,newdata = dataforRF)
MSEtrainErr <- mean((rfpredTraining-trainingD$WAR.x)^2) 
MSEtrainErr
MAEtrainErr <- mean(abs(rfpredTraining-trainingD$WAR.x)) 
MAEtrainErr


#5 - Use the model to predict Solubility of the Test Points
rfpredTest <- predict(rfModel1,newdata = dataforRF_test)
MSEtestErr <- mean((rfpredTest-testD$WAR.x)^2) 
MSEtestErr
MAEtestErr <- mean(abs(rfpredTest-testD$WAR.x)) 
MAEtestErr

## Bigger Model
keepforRF2 <- c('R','H.x', 'OPS_plus','PA.2','BB.x','R.x','HR.x','R.x.2','X2B','SB','SH.x',
'X3B','HBP.x','is.C', 'BA.2', 'is.SS','is.3B','ASGAppear',
 'pointsWon', 'is.2B','is.CF', 'is.RF', 'topsal', 'old', 'is.LF',
 'is.1B', 'IBB.x', 'BA.3', 'batavg', 'RBI', 'CS', 'SO.x', 'GIDP.x',
 'slg', 'obp', 'OPS_1', 'pa_r', 'PA', 'sum_PO','sum_A','sum_E', 'sum_DP', 'sum_ZR')
dataforRF2 <- trainingD[,(names(trainingD) %in% keepforRF2)]
dataforRF_test2 <- testD[,(names(testD) %in% keepforRF2)]

rfModel2 <- randomForest(dataforRF2,trainingD$WAR.x, ntree=200, importance = TRUE,
                         do.trace=50, keep.forest = TRUE)


#4 - Bigger Model Train
rfpredTraining2 <- predict(rfModel2,newdata = dataforRF2)
MSEtrainErr2 <- mean((rfpredTraining2-trainingD$WAR.x)^2) 
MSEtrainErr2
MAEtrainErr2 <- mean(abs(rfpredTraining2-trainingD$WAR.x)) 
MAEtrainErr2

rfpredTest2 <- predict(rfModel2,newdata = dataforRF_test2)
MSEtestErr2 <- mean((rfpredTest2-testD$WAR.x)^2) 
MSEtestErr2
MAEtestErr2 <- mean(abs(rfpredTest2-testD$WAR.x)) 
MAEtestErr2



xyplot(testD$WAR.x-rfpredTest~ rfpredTest,type =c("p","g"), xlab = "Predicted", ylab = "Residuals")
xyplot(testD$WAR.x ~ rfpredTest,type =c("p","g"), xlab = "Predicted", ylab = "Observed")

max (testD$WAR.x-rfpredTest)
min (testD$WAR.x-rfpredTest)

WAR <- trainingD$WAR.x
WAR

# EXTRA CREDIT
#v https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
set.seed(100)
result <- rfcv(dataforRF, trainingD$WAR.x, cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

RFfeat <- rfcv(trainx = dataforRF, trainy = WAR, scale ="log")
RFfeat$error.cv

with(RFfeat, plot(n.var, error.cv, type="b", col="red"))

(which.min(RFfeat$error.cv))

plot(RFfeat$n.var, RFfeat$error.cv)
## Took 30 minutes to run
# ensure the results are repeatable
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
set.seed(7)
# load the library
#install.packages("mlbench")
library(mlbench)
library(caret)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=3)
# run the RFE algorithm
results <- rfe( dataforRF, WAR, sizes=c(5,8,10),
               rfeControl=control, metric = "MAE", maximize = FALSE)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o")) 
#use the 100 variables in RF model#
results$optVariables
results$bestSubset
VarforReduced <- results$optVariables
ReducedTrainingData <- solTrainXtrans[VarforReduced]


set.seed(100)
RedRFModl<- tuneRF(ReducedTrainingData, solTrainY,  ntreeTry=1000,
                   stepFactor=1.5, improve=0.01,trace=TRUE, plot=TRUE, doBest=FALSE)
print(RedRFModl)


#http://ugrad.stat.ubc.ca/R/library/randomForest/html/predict.randomForest.html
# - Use predict() function to find OOB predictions for the training data 
rfpredTrainOOB_Red <- predict(RedRFModl)
mean((rfpredTrainOOB_Red-solTrainY)^2) 


#4 - Use the model to predict Solubility of the Training Points
rfpredTraining <- predict(rfModel1,newdata = solTrainXtrans)
mean((rfpredTraining-solTrainY)^2) 

#5 - Use the model to predict Solubility of the Test Points
rfpredTest_Red <- predict(RedRFModl,newdata = solTestXtrans)
mean((rfpredTest_Red-solTestY)^2) 


xyplot(solTestY ~ rfpredTest_Red,type =c("p","g"), xlab = "Predicted", ylab = "Observed")
xyplot(solTestY-rfpredTest_Red~ rfpredTest_Red,type =c("p","g"), xlab = "Predicted", ylab = "Residuals")

max (solTestY-rfpredTest_Red)
min (solTestY-rfpredTest_Red)


# OOB Error Estimate #
# Try ike the book #
set.seed(1)
rf.model2=randomForest(x=as.matrix(solTrainXtrans), y=solTrainY, importance= TRUE,
                       ntree=1000, type = FIT)
yhat.rf2 = predict(rf.model2 ,newdata=solTestX)
mean((yhat.rf2-solTestY)^2) 
plot(yhat.rf2 ,solTestY)
aggregate(x=MSEvalues$MSE, by = list(grp = MSEvalues$group),FUN = max )


### Do some feature selection the Lasso
# Loaging the library
# https://www.rstatisticsblog.com/data-science-in-action/lasso-regression/
# Loaging the library
library(glmnet)

varsformod <- c('R.x','H.x','X2B','X3B','HR.x','RBI','SB','BB.x','SO.x',
                   'age.x','batavg','obp','PA', 'GIDP.x','OPS_plus',
                   'sum_PO','sum_A','sum_E', 'sum_DP', 'sum_ZR', 'slg')  

lambda_seq <- 10^seq(2, -2, by = -.1)

lambda_seq

Lasso_x_vars <- as.matrix(trainingD[,(names(trainingD) %in% varsformod)])

y.train <-trainingD$WAR.x

str(Lasso_x_vars)

fit.lasso <- glmnet(Lasso_x_vars, y.train, family="gaussian", alpha=1)


cv_output <- cv.glmnet(x = Lasso_x_vars, trainingD$WAR.x, 
                       alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(Lasso_x_vars, trainingD$WAR.x, alpha = 1, lambda = .02)
# Inspecting beta coefficients
coef(lasso_best)

fit.lasso






