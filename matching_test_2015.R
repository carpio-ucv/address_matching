
### Relevant libraries

# Relevant Libraries
library("data.table")
library("dplyr")
library("reshape2")
library("ggplot2")
library("stringdist")
library("stringr")
library("tm")
library("fuzzyjoin")


# Setting up data
setwd("//dscs/cscom/Policy/Corp Information/Olypmic Hosts NKM (secure)/Community Mapping 2017/Juans_tests/2015")
gp_raw<-fread("GP_Register_2015.csv",  na.strings=c(""))
gazet_raw<-fread("UNI2LIVEGAZVIEW.csv",  na.strings=c(""))
blpu_raw<-fread("BLPU.csv",  na.strings=c(""))
#elect_raw<-fread("ElectoralRegister2017.csv",  na.strings=c(""))


# Creating blpu clean data set. # 78,595
blpu_tidy<- blpu_raw %>% filter(LOGICAL_ST==1) %>%
  mutate(filt=grepl("Residential",BLPU_CLA_1)) %>% # Filter Residential UPRN
  filter(filt=="TRUE") %>%
  select(UPRN, BLPU_CLA_1, MAP_X, MAP_Y, WARD_CODE) 
nrow(blpu_tidy)


# Creating Gazeteer clean data set. # 78,841
gazet_tidy<- gazet_raw %>% filter(LOGICAL_ST==1) %>% 
  select(UPRN, ADDRESS, PAO_START_, PAO_START1,SAO_START1, POSTCODE, 
         MAP_EAST, MAP_NORTH) %>%
  filter(POSTCODE!="") %>% # Removing gazeteer data with no post codes 
  mutate(adr.NO=sub("\\Essex.*","",ADDRESS)) %>% #eliminate anything from "Essex"
  mutate(No=gsub("[^0-9]","",adr.NO)) %>% # Extract the house number (and premises)
  mutate(sfx1= ifelse(is.na(PAO_START1),"",PAO_START1)) %>% # Extract any suffix from PA
  mutate(sfx2= ifelse(is.na(SAO_START1),"",SAO_START1)) %>% # Extract any suffix from PA
  mutate(ID= paste0(No,sfx1,sfx2,POSTCODE)) %>% # create a unique identifier based on house number and postcode
  mutate(ID=gsub(" ","",ID)) # remove empty spaces
nrow(gazet_tidy)

# Creating Final Gazeeter dataset # 74,852 (uno menos) - 74,553 Unique
gazet_df <- semi_join(gazet_tidy,blpu_tidy, by="UPRN") #%>%
  #distinct(ID) 
nrow(gazet_df)

# Creating GPs clean data set. #207,755 (uniques ID=67056)
gp_unique<- gp_raw %>% select(UPRN_match,FORENAME, SURNAME, PREMISES, STREET, POSTCODE) %>%
  mutate(PREMISES=replace(PREMISES,which(is.na(PREMISES)),""))%>%
  mutate(premises.no=gsub("[^0-9]","",PREMISES)) %>%  
  mutate(str.no=gsub(" .*$","",STREET)) %>%
  mutate(str.no=ifelse(grepl("[0-99]",str.no)==TRUE,str.no,"")) %>%
  mutate(No= paste0(premises.no, str.no)) %>%
  mutate(ID= paste0(No, POSTCODE)) %>%
  mutate(ID=gsub(" ","",ID)) %>%
  distinct(ID) 
nrow(gp_unique)

gp_tidy<- gp_raw %>% select(UPRN_match,FORENAME, SURNAME, PREMISES, STREET, POSTCODE) %>%
  mutate(PREMISES=replace(PREMISES,which(is.na(PREMISES)),""))%>%
  mutate(premises.no=gsub("[^0-9]","",PREMISES)) %>%  
  mutate(str.no=gsub(" .*$","",STREET)) %>%
  mutate(str.no=ifelse(grepl("[0-99]",str.no)==TRUE,str.no,"")) %>%
  mutate(No= paste0(premises.no, str.no)) %>%
  mutate(ID= paste0(No, POSTCODE)) %>%
  mutate(ID=gsub(" ","",ID))
nrow(gp_tidy)

##############################
## MERGING GPs and GAZETEER ##
##############################

# Merging datasets of UNIQUE ID's
#################################
df<-semi_join(gp_unique,gazet_df, by="ID") 
nrow(df) # 63,644
no.match<-anti_join(gp_unique, gazet_df, by="ID")
nrow(no.match)# 3,412
# Accuracy
paste0(round(100-nrow(no.match)/nrow(df)*100,2),"%")# "94.64%"


# Merging back datasets
#################################
df2<-inner_join(df, gazet_df, by="ID") 
nrow(df2) # 63,784
df_final<-inner_join(df2,gp_tidy, by="ID") 
nrow(df_final) # 200,945

# Accuracy UPRN
#################################

audit<-df_final %>% mutate(check= ifelse(UPRN==UPRN_match,1,0))

perc<-audit %>% group_by(check) %>% count()  
paste0(round(perc[2,2]/(perc[2,2]+perc[1,2])*100,3),"%") #98.895% | 2210 / 197813 
                                             # previous: #98.799% | 2407 / 197934
# Visual inspection of no matches
exp<-audit %>% filter(check==0) %>% select(PREMISES, STREET, ADDRESS)
head(exp,200)
exp2<-audit %>% filter(check==0) %>% select(UPRN, UPRN_match)
head(exp2,200)

## CODES
## grepl("^[0-9].*$","hjg")
