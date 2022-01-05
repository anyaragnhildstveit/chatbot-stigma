## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | CHATBOT STIGMA STUDY | EXPERIMENT 1               
## ================================================================================================================

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(rstatix)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools'        # get Cramer's V
)

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('chatbot_raw.csv') 

## explore dataframe: 
dim(d) # will provide dimensions of the dataframe by row [1] and column [2]
colnames(d) # will provide all column names
summary(d)
dim(d)[1]
table(d$FL_4_DO)

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att1 == 2 & d$att2 == 2))
dim(d) # number of participants should decrease after attention exclusions

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
n_original 

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 2 | d$comp_1.1 == 1 & d$comp_2.1 == 2))
dim(d) # number of participants should decrease after comprehension exclusions

## get number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$FL_4_DO)

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 

## gender
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females

## ethnicity
table(d$ethnicity)[1]/sum(table(d$ethnicity)) ## percentage of blacks
table(d$ethnicity)[2]/sum(table(d$ethnicity)) ## percentage of asians
table(d$ethnicity)[3]/sum(table(d$ethnicity)) ## percentage of whites
table(d$ethnicity)[4]/sum(table(d$ethnicity)) ## percentage of hispanics
table(d$ethnicity)[5]/sum(table(d$ethnicity)) ## percentage of mixed
table(d$ethnicity)[6]/sum(table(d$ethnicity)) ## percentage of other

## education
table(d$edu)[1]/sum(table(d$edu)) ## percentage of high school
table(d$edu)[2]/sum(table(d$edu)) ## percentage of vocational school
table(d$edu)[3]/sum(table(d$edu)) ## percentage of some college
table(d$edu)[4]/sum(table(d$edu)) ## percentage of college graduate
table(d$edu)[5]/sum(table(d$edu)) ## percentage of masters degree
table(d$edu)[6]/sum(table(d$edu)) ## percentage of doctoral degree
table(d$edu)[7]/sum(table(d$edu)) ## percentage of professional degree
table(d$edu)[8]/sum(table(d$edu)) ## percentage of other degree

## ai experience 
table(d$ai_companion_exp)[1]/sum(table(d$ai_companion_exp)) ## percentage of yes
table(d$ai_companion_exp)[2]/sum(table(d$ai_companion_exp)) ## percentage of no

## ai capability 
mean(d$ai_capability_1, trim = 0, na.rm = TRUE) ## mean age 

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

## define new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 9))
colnames(d_subset) <- c('cond', 'willing_friend', 'willing_romantic', 'wtp', 'true', 'talk_listen', 
                        'not_judge', 'comp1', 'comp2')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract good data from the middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,21:36][!is.na(d[i,21:36])] # for a given row, get only the non-NA values
  d_subset[i,2:9] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  d_subset[i,1] <- d[i,58][!is.na(d[i,58])]
}

## merge good data with first and last halves of raw data:
# this is the new dataframe to work with.
d_merged <- cbind(d_subset, d[,37:57])
d_merged$ss <- 1:dim(d_merged)[1]     

## ================================================================================================================
##                                              DATA ANALYSIS - MEASURES                
## ================================================================================================================

## (1) LONELINESS
## calculate scale loading, frequency counts, and reliability 
keys.list <- list(lonely= c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1"))
scores <- psych::scoreItems(keys.list, d_merged)
print(scores,short = FALSE)

## calculate average items if cronbach's alpha > 0.80
d_merged$lonely <- rowMeans(d_merged[,c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1")])
d_merged$lonely

## (2) SENSATION 
## calculate scale loading, frequency counts, and reliability 
keys.list <- list(sensation= c("sensation_1_1", "sensation_2_1", "sensation_3_1", 
                "sensation_4_1", "sensation_5_1", "sensation_6_1", 
                "sensation_7_1", "sensation_8_1"))
scores <- psych::scoreItems(keys.list, d_merged)
print(scores, short = FALSE)

## calculate average items if cronbach's alpha > 0.80
d_merged$sensation <- rowMeans(d_merged[,c("sensation_1_1", "sensation_2_1", "sensation_3_1", 
                                           "sensation_4_1", "sensation_5_1", "sensation_6_1", 
                                           "sensation_7_1", "sensation_8_1")])
d_merged$sensation

## ================================================================================================================
##                                              DATA ANALYSIS - DISTRIBUTIONS                
## ================================================================================================================

## (1) FRIEND
## check distribution
hist(d_merged$willing_friend, 
     main="Willingness to Find a Friend", 
     xlab="Willingness", 
     border="black", 
     col="light blue")

## check for normal distribution (shapiro-wilk normality test)
shapiro.test(d_merged$willing_friend)

## (2) PARTNER
## check distribution
hist(d_merged$willing_romantic, 
     main="Willing to Find a Partner", 
     xlab="Willingness", 
     border="black", 
     col="light blue")

## check for normal distribution (shapiro-wilk normality test)
shapiro.test(d_merged$willing_romantic)

## (3) PAYMENT
## check distribution
hist(d_merged$wtp, 
     main="Willingness to Pay", 
     xlab="Money (USD)", 
     border="black", 
     col="light blue")

## check for normal distribution (shapiro-wilk normality test)
shapiro.test(d_merged$wtp)

## transform values to reduce skew
d_merged$wtp_logged <- log(d_merged$wtp+1) + 1

hist(d_merged$wtp_logged, 
     main = "Willingness to Pay", 
     xlab = "Money (USD)", 
     border = "black", 
     col = "light blue")

shapiro.test(d_merged$wtp_logged)

## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS               
## ================================================================================================================

## (1) FRIEND
## get summary statistics
d_merged %>%
  group_by(cond) %>%
  get_summary_stats(willing_friend, type = "mean_sd")

## run two sample t-test
t.test(willing_friend ~ cond, data = d_merged, paired = FALSE, var.equal = FALSE)

## calculate effect size
d_merged %>% cohens_d(willing_friend ~ cond, paired = FALSE, var.equal = FALSE)

## create box-plot visual
bxp_1 <- ggboxplot(
  d_merged, x = "cond", y = "willing_friend", 
  ylab = "Willingness to Find a Friend", xlab = "Conditions", 
  add = "jitter", color ="black", fill = "light blue")

bxp_1

## (2) PARTNER
## get summary statistics
d_merged %>%
  group_by(cond) %>%
  get_summary_stats(willing_romantic, type = "mean_sd")

## run two sample t-test
t.test(willing_romantic ~ cond, data = d_merged, paired = FALSE, var.equal = FALSE)

## calculate effect size
d_merged %>% cohens_d(willing_romantic ~ cond, paired = FALSE, var.equal = FALSE)

## create box-plot visual
bxp_2 <- ggboxplot(
  d_merged, x = "cond", y = "willing_romantic", 
  ylab = "Willingness to Find a Partner", xlab = "Conditions", 
  add = "jitter", color ="black", fill = "light blue")

bxp_2

## (3) PAYMENT
## get summary statistics
d_merged %>%
  group_by(cond) %>%
  get_summary_stats(wtp_logged, type = "mean_sd")

## run two sample t-test
t.test(wtp_logged ~ cond, data = d_merged, paired = FALSE, var.equal = FALSE)

## calculate effect size
d_merged %>% cohens_d(wtp_logged ~ cond, paired = FALSE, var.equal = FALSE)

## create box-plot visual
bxp_3 <- ggboxplot(
  d_merged, x = "cond", y = "wtp_logged", 
  ylab = "Willingness to Pay", xlab = "Conditions", 
  add = "jitter", color ="black", fill = "light blue")

bxp_3

## ================================================================================================================
##                                      DATA ANALYSIS - MEDIATION USING 'PROCESS'                 
## ================================================================================================================

t = as.factor(d_merged$cond) ## turn into factor
d_merged$cond = as.numeric(t) ## set numeric of factor to variable

## PARALLEL MEDIATION:
## bind mediators together with the c function 
## compare strengths of mediators by using contrast function: 
## (1) = compare indirect effects, (2) = compares absolute values of indirect effect

## (1) FRIEND
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_friend", x = "cond", 
        m =c("true", "talk_listen", "not_judge"), model = 4, effsize =1, total =1, stand =1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## (2) PARTNER
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_romantic", x = "cond", 
        m =c("true", "talk_listen", "not_judge"), model = 4, effsize =1, total =1, stand =1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## (3) PAYMENT
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "wtp_logged", x = "cond", 
        m =c("true", "talk_listen", "not_judge"), model = 4, effsize =1, total =1, stand =1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Interpretation Guide
## (Q): a-path to M1
## (R): a-path to the M2
## (S): b-path from M1
## (T): b-path from M2
## (U): total indirect effect. Indirect effects mediated by M1 or M2 taken together
## (V): indirect effect mediated by M1.
## (W): indirect effect mediated by M2
## (X): contrast between indirect effect mediated by M1 and indirect effect mediated by M2
## (Y): contrast definition (important if there are more than two mediators and therefore more than one contrast).

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

## plotting all measures
t_names <- c("AI", "Human")

## (1) FRIEND
p1 <- ggplot(d_merged,aes(x=factor(cond),y=willing_friend)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))

p1 <- p1 + theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Willingness to Find a Friend") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p1

## (2) PARTNER
p2 <- ggplot(d_merged,aes(x=factor(cond),y=willing_romantic)) +  
  theme_bw()+coord_cartesian(ylim=c(1,100))

p2 <- p2 + theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Willingness to Find a Partner") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p2

## (3) PAYMENT
p3 <- ggplot(d_merged,aes(x=factor(cond),y=wtp_logged)) +  
  theme_bw()+coord_cartesian(ylim=c(1,5))

p3 <- p3 + theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Willingness to Pay") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  geom_violin(width=0.9, alpha=0.38, size=0.75)+  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p3

## (4) ALL FIGURES
dev.new(width=13,height=6,noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, p3, nrow=1,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean Rating", color="black", face ="plain",size=16, rot=90),
                bottom = text_grob("Tetheredness", color="black", face ="plain",size=16)) 
figure

write.csv(d_merged,"chatbot_merged.csv")

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================