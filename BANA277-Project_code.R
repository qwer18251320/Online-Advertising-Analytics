# chi-sq test

all_lead <- read_excel("2019 Applicants - All Lead Sources.xls.xlsx")
# correlation residency status and program (2 binary var)
all_lead$Programs [all_lead$Programs == 'Full-Time MBA'] <-1
all_lead$Programs [all_lead$Programs == 'Fully Employed MBA'] <-1
all_lead$Programs [all_lead$Programs == 'Executive MBA']<-1
all_lead$Programs [all_lead$Programs == 'MD/MBA']<-1
all_lead$Programs [all_lead$Programs == 'JD/MBA']<-1
all_lead$Programs [all_lead$Programs == 'Master of Science in Business Analytics']<-0
all_lead$Programs [all_lead$Programs == 'Master of Finance']<-0
all_lead$Programs [all_lead$Programs == 'Master of Professional Accountancy']<-0
all_lead$Programs [all_lead$Programs == 'Master of Innovation and Entrepreneurship']<-0

#all_lead$Programs [all_lead$Programs == 'Ph.D. in Management']<-0

tb1=table(all_lead$Resident, all_lead$Programs)
prop.table(tb1)
all_lead$Resident <- as.numeric(all_lead$Resident)
all_lead$Programs <- as.numeric(all_lead$Programs)

#chi sq test
chi2 = chisq.test(tb1, correct=F) #significant

# application submit time vs residency
names(all_lead)[3]<- "submit"
all_lead$sumbit <- format(as.Date(all_lead$submit), "%Y-%m")


subset(all_lead$submit, format.Date(date, "%m")<="12" & format.Date(date, "%Y")<="2018")

sub2019 <- subset(all_lead, format(submit, "%Y") %in% c("2019")  ) 
sub2019 = cbind(binary = 0, sub2019)
View(sub2019)
sub2019 <- as.data.frame(sub2019)

sub2018 <- subset(all_lead, format(submit, "%Y") %in% c("2018")  )
sub2018 <- as.data.frame(sub2018)
sub2017 <- subset(all_lead, format(submit, "%Y") %in% c("2017")  )
sub2017 <- as.data.frame(sub2017)
sub1718 <- rbind(sub2018, sub2017)
sub1718 = cbind(binary = 1, sub1718)
all <- rbind(sub2019, sub1718)

names(all)[5]<- 'Resident'
all$Resident [all$Resident == "I am a U.S. Citizen or U.S. Permanent Resident"] <- 1
all$Resident [all$Resident == "I am an International applicant"] <- 0

tb2=table(all$Resident, all$binary)
prop.table(tb2)
chi21 = chisq.test(tb2, correct=F) #significant


library(dplyr)

appli_clean <- read.csv("2019 Applicants - All Lead Sources.csv", header = TRUE)

# data cleaning
appli_clean  <- appli_clean [-c(6,7)]

# convert to dummy/binary
appli_clean  <- mutate(appli_clean, Resident=ifelse(Residency.Status=="I am a U.S. Citizen or U.S. Permanent Resident", 1, 0))
appli_clean <- mutate(appli_clean, International=ifelse(Residency.Status=="I am an International applicant", 1, 0))

appli_clean <- mutate(appli_clean, Program=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                    Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 1, 0))

appli_clean <- mutate(appli_clean, MBA=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 1, 0))

appli_clean <- mutate(appli_clean, special_1year=ifelse(Programs=="Full-Time MBA"|Programs=="Executive MBA"|
                                                          Programs=="Fully Employed MBA"|Programs=="JD/MBA"|Programs=="MD/MBA", 0, 1))




# logistic regression for program
glm_resident <- glm(Program~Resident,family = binomial(), data = appli_clean)
summary(glm_resident)
exp(coef(glm_resident))

glm_intl <- glm(Program~International,family = binomial(), data = appli_clean)
summary(glm_intl)
exp(coef(glm_intl))

######

all_inquiry <- read.csv("All Inquiry Form Conversions.csv", header = TRUE)

# data cleaning
all_inquiry  <- all_inquiry [-c(5,7,8)]

# filter
all_inquiry <- all_inquiry %>% filter(Student.Stage != "Archive")
all_inquiry <- all_inquiry %>% filter(Program != "Ph.D. in Management") %>% 
  filter(Program != "Master of Science in Engineering Management") %>% 
  filter(Program != "Master of Science in Biotechnology Management")

# convert
all_inquiry  <- mutate(all_inquiry, Program=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                                     Program=="Fully Employed MBA"|Program=="JD/MBA"|Program=="MD/MBA", 1, 0))
all_inquiry  <- mutate(all_inquiry, Applicant=ifelse(Student.Stage=="Applicant", 1, 0))
all_inquiry  <- mutate(all_inquiry, Inquiry=ifelse(Student.Stage=="Inquiry", 1, 0))

# logistic regression for inquiry
inquiry_applicant <- glm(Program~Applicant,family = binomial(), data = all_inquiry)
summary(inquiry_applicant)
exp(coef(inquiry_applicant))

inquiry_inquiry <- glm(Program~Inquiry,family = binomial(), data = all_inquiry)
summary(inquiry_inquiry)
exp(coef(inquiry_inquiry))

######

# data
fb <- read.csv("Media Type Conversion - Facebook.csv", header = TRUE)
google <- read.csv("Media Type COnversion - Google.csv", header = TRUE)
linkedin <- read.csv("Media Type COnversion - LinkedIn.csv", header = TRUE)

# clean
fb <- fb %>% filter(Student.Stage != "Archive")
fb <- fb %>% filter(Program != "Master of Science in Biotechnology Management") %>% 
  filter(Program != "Master of Science in Engineering Management")

google <- google %>% filter(Student.Stage != "Archive")
google <- google %>% filter(Program != "Master of Science in Engineering Management") %>%
  filter(Program != "PH.D. in Management") %>% 
  filter(Program != "Master of Science in Biotechnology Management") %>%
  filter(Program != "Ph.D. in Management")

linkedin <- linkedin %>% filter(Student.Stage != "Archive")
linkedin <- linkedin %>% filter(Program != "Master of Science in Biotechnology Management")

# convert
fb <- mutate(fb, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))
google <- mutate(google, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))
linkedin <- mutate(linkedin, Inquiry=ifelse(Student.Stage=="Inquiry", 0, 1))

fb <- mutate(fb, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                              Program=="Fully Employed MBA"|Program=="JD/MBA"|
                              Program=="MD/MBA", 1, 0))

fb <- mutate(fb, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                 Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                 Program=="MD/MBA", 0, 1))

google <- mutate(google, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                      Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                      Program=="MD/MBA", 1, 0))

google <- mutate(google, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                         Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                         Program=="MD/MBA", 0, 1))

linkedin <- mutate(linkedin, MBA=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                          Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                          Program=="MD/MBA", 1, 0))

linkedin <- mutate(linkedin, year_1=ifelse(Program=="Full-Time MBA"|Program=="Executive MBA"|
                                             Program=="Fully Employed MBA"|Program=="JD/MBA"|
                                             Program=="MD/MBA", 0, 1))

# logistic regression for facebook
fb_glm_mba<- glm(Inquiry~MBA,family = binomial(), data = fb)
summary(fb_glm_mba)
exp(coef(fb_glm_mba))

fb_glm_year_1 <- glm(Inquiry~year_1,family = binomial(), data = fb)
summary(fb_glm_year_1)
exp(coef(fb_glm_year_1))

# logistic regression -google
google_glm_mba <- glm(Inquiry~MBA,family = binomial(), data = google)
summary(google_glm_mba)
exp(coef(google_glm_mba))

google_glm_year_1 <- glm(Inquiry~year_1,family = binomial(), data = google)
summary(google_glm_year_1)
exp(coef(google_glm_year_1))

# logistic regression - linkedin
linkedin_glm_mba <- glm(Inquiry~MBA,family = binomial(), data = linkedin)
summary(linkedin_glm_mba )
exp(coef(linkedin_glm_mba ))

linkedin_glm_year_1<- glm(Inquiry~year_1,family = binomial(), data = linkedin)
summary(linkedin_glm_year_1)
exp(coef(linkedin_glm_year_1))

######### 

#wordcloud
# install
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")

# library
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(wordcloud)
library(tm)

# import data
search_google_ads <- read.csv("UCI Google Ads FTMBA and FEMBA Search terms Report Sept 2018 - Jan 2020.csv", header = TRUE)

# keyword
text <- search_google_ads$Keyword 
docs <- Corpus(VectorSource(text))

# clean
docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create a document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# wordcloud
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq =3,max.words=100, random.order=FALSE, 
          rot.per=0.35,colors=brewer.pal(9, "Dark2"),scale=c(4,1))

# Frequency chart
df %>% arrange(desc(freq)) %>% slice(1:8) %>% ggplot(., aes(x=word, y=freq))+
  geom_bar(stat='identity', fill="#E69F00", colour="black")+
  labs(title="Keyword Frequency",x=NULL,y="Frequency")

################

# keyword
text <- search_google_ads$Keyword 

# clean data
text <- gsub("https\\S*", "", text) 
text <- gsub("@\\S*", "", text) 
text <- gsub("amp", "", text) 
text <- gsub("[\r\n]", "", text)
text <- gsub("[[:punct:]]", "", text)
text

keyword <- data.frame(table(text))
sort(keyword$text)
keyword_order <- keyword %>% arrange(desc(Freq)) %>% slice(1:8) 

#vertical
ggplot(keyword_order, aes(x=text, y=Freq)) + 
  geom_bar(stat="identity", fill="#E69F00", colour="black")+
  theme(plot.title = element_text(hjust = 0, size=20), axis.text.x=element_text(angle=90, hjust=1, size=13), panel.spacing.x=unit(0.5, "lines")) +
  labs(title="Keyword Frequency",x=NULL,y="Frequency")


# horizontal
ggplot(keyword_order, aes(x=text, y=Freq)) + 
  geom_bar(stat="identity", fill="#E69F00", colour="black")+coord_flip()+
  theme(plot.title = element_text(hjust = 0, size=20), axis.text.x=element_text(angle=90, hjust=1, size=10), panel.spacing.x=unit(0.5, "lines"))+
  labs(title="Keyword Frequency",x=NULL,y="Frequency")+theme(axis.text = element_text(size=12))


# wordcloud
set.seed(1234)
wordcloud(words = keyword$text, freq = keyword$Freq, min.freq =2, max.words=50, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(9, "Dark2"))

################

# Search term
text <- search_google_ads$Search.term
docs <- Corpus(VectorSource(text))

# clean
docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create a document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# wordcloud
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=100, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(6, "Dark2"))

# Frequency chart
df %>% arrange(desc(freq)) %>% slice(1:8) %>% ggplot(., aes(x=word, y=freq))+
  geom_bar(stat='identity', fill="#E69F00", colour="black")+
  labs(title="Search Term Frequency",x=NULL,y="Frequency")+
  theme(axis.text = element_text(size = 8),axis.text.x=element_text(angle=90, hjust=1, size=6),plot.title = element_text(size=11))
# +coord_flip()

# searching term is suitable for word, and keyword is suitable for whole sentence

# MBA comparison
# readin the all lead sources data
als <- read_excel("2019 Applicants - All Lead Sources.xls.xlsx")
gle <- read_excel("Media Type COnversion - Google.xls.xlsx")
link <- read_excel("Media Type COnversion - LinkedIn.xls.xlsx")
attach(als)

# drop the last 6 rows as they contain non-relevant info
als <- als %>% filter(row_number() <= n()-6)


# Change the Residency Status to easy intrecept
als <- als %>% mutate(`Residency Status` = ifelse(`Residency Status`=='I am a U.S. Citizen or U.S. Permanent Resident',
                                                  'US', 'Intl'))

# create us and intl data frame based on residency status
us <- als %>% filter(`Residency Status`=='US')
intl <- als %>% filter(`Residency Status`=='Intl')

# How many applicants from the us and intl
nrow(us)  # 928
nrow(intl) # 3426

nrow(us)/nrow(alead)*100  # about 21.31% are us residency
nrow(intl)/nrow(alead)*100  # about 78.69% are intl residency

barplot(c(nrow(us)/nrow(als)*100,nrow(intl)/nrow(als)*100), ylab='Percentage', 
        names.arg=c('US', 'Intl'),
        ylim=c(0,100),
        main='Oppucation of Applicants')

# table of programs distribution for the us and intl
table(us$Programs)
table(intl$Programs)

# barplot of program distribution based on residency and all applicants
# all applicant
barplot(table(als$Programs), ylab='Frequency', main='Applicants Distribution with Programs',
        las=2, cex.names = 0.4)
# us
barplot(table(us$Programs), ylab='Frequency', main='US Residency Distribution with Programs',
        las=2, cex.names = 0.4)
# intl
barplot(table(intl$Programs), ylab='Frequency', main='Intl Residency Distribution with Programs',
        las=2, cex.names = 0.4)



# lead source 
source_us <- us %>% group_by(Programs, `Lead Source`) %>% summarise(COUNT = n())
source_intl <- intl %>% group_by(Programs, `Lead Source`) %>% summarise(COUNT = n())


### FTMBA FEMBA Correlation
library(dplyr)
library(tidyr)
library(ggplot2)
st <- read.csv('Search terms clean.csv', header = TRUE)
attach(st)
st <- mutate(st, program=ifelse(Campaign2=="FTMBA", 1, 0)) #FEMBA=0 FTMBA=1



library(Hmisc)
corr <- rcorr(as.matrix(st[,c(1:8,11)]))

library(corrplot)
# Insignificant correlations are leaved blank
corrplot(corr$r, type="upper", order="hclust", 
         p.mat = corr$P, sig.level = 0.05, insig = "blank")

library(PerformanceAnalytics)
chart.Correlation(st[,c(1:8,11)], histogram=TRUE, pch=19)

# mean and t-test for all variable based on program
data_cov <- c('Impr','Interactions','InteractionRate','Avgcost','Cost','Conversions','Cost.conv','ConvRate')

# mean for all variables divided on FE and FT
st %>%
  group_by(program) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# t-test
lapply(data_cov, function(v) {
  t.test(st[, v] ~ st$program)
})

# insignificant for all variables except cost and conversions


# logistic regression for all variables 
lg1 <- glm(program ~ log(Impr+1)+log(Interactions+1)+log(InteractionRate+1)+log(Avgcost+1)+
             log(Cost+1)+log(Conversions+1)+log(Cost.conv+1)+log(ConvRate+1),
           family = binomial(), data=st)
summary(lg1)

lg2 <- glm(program ~ log(Impr+1)+log(InteractionRate+1)+log(Avgcost+1)+log(Cost+1)+log(Conversions+1)
           +log(Cost.conv+1),
           family = binomial(), data=st)
summary(lg2)

exp(coef(lg2))


# conversion regression for both
con1 <- glm(Conversions ~ Impr+Interactions+InteractionRate+Avgcost+Cost+Cost.conv+ConvRate,
            family = poisson(), data=st)
summary(con1)

con2 <- glm(Conversions ~ Interactions+InteractionRate+Cost.conv+ConvRate,
            family = poisson(), data=st)
summary(con2)

# take log
con3 <- glm(log(Conversions+1) ~ log(Impr+1)+log(Interactions+1)+log(InteractionRate+1)+log(Avgcost+1)+
              log(Cost+1)+log(Cost.conv+1)+log(ConvRate+1),
            family = poisson(), data=st)
summary(con3)

con4 <- glm(log(Conversions+1) ~ log(Avgcost+1)+log(Cost.conv+1)+log(ConvRate+1),
            family = poisson(), data=st)
summary(con4)


ggplot(st, aes(x=log(Conversions+1), y=log(Avgcost+1)+log(Cost.conv+1)+log(ConvRate+1))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE)





# Interaction regression
inte1 <- glm(log(Interactions+1) ~ log(Impr+1)+log(InteractionRate+1)+log(Avgcost+1)+
               log(Cost+1)+log(Conversions+1)+log(Cost.conv+1)+log(ConvRate+1),
             family = poisson(), data=st)
summary(inte1)

## FT and FE

# FTMBA conversion regression
ft <- st[st$Campaign2=='FTMBA',]
ftcon1 <- glm(log(Conversions+1) ~ log(Impr+1)+log(Interactions+1)+log(InteractionRate+1)+log(Avgcost+1)+
                log(Cost+1)+log(Cost.conv+1)+log(ConvRate+1),
              family = poisson(), data=ft)
summary(ftcon1)

ftcon2 <- glm(Conversions ~ Cost.conv+ConvRate, family = poisson(), data=ft)
summary(ftcon2)

# FEMBA conversion regression
fe <- st[st$Campaign2=='FEMBA',]
fecon1 <- glm(Conversions ~ Impr+Interactions+InteractionRate+Avgcost+Cost+Cost.conv+ConvRate,
              family = poisson(), data=fe)
summary(fecon1)
fecon2 <- glm(Conversions ~ Impr+Interactions+InteractionRate+Cost+Cost.conv+ConvRate,
              family = poisson(), data=fe)
summary(fecon2)


fecon3 <- glm(log(Conversions+1) ~ log(Impr+1)+log(Interactions+1)+log(InteractionRate+1)+log(Avgcost+1)+
                log(Cost+1)+log(Cost.conv+1)+log(ConvRate+1), family = poisson(), data=fe)
summary(fecon3)




