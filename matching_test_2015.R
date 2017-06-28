
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
 # mutate(filt=grepl("PP",BLPU_CLASS)) %>% # Filter Residential UPRN
 # filter(filt=="FALSE") %>%
  select(UPRN, BLPU_CLASS,BLPU_CLA_1, MAP_X, MAP_Y, WARD_CODE) #%>%
  #filter(grepl("RH01|RH01|RH02|RH03|RI01|RI02|RI03",BLPU_CLASS))#NEW CODE
  #filter(grepl("PP",BLPU_CLASS))#NEW CODE
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
  mutate(ID=gsub(" ","",ID))%>% # remove empty spaces
  mutate(ID=gsub("-","",ID))# remove dashes
nrow(gazet_tidy)

# Creating Final Gazeeter dataset # 74,852 (uno menos) - 74,553 Unique
gazet_df <- semi_join(gazet_tidy,blpu_tidy, by="UPRN") #%>%
  #distinct(ID) 
nrow(gazet_df)

# Creating GPs clean data set. #207,755 (uniques ID=67056) # new unique
gp_unique<- gp_raw %>% select(UPRN_match,FORENAME, SURNAME, PREMISES, STREET, POSTCODE) %>%
  mutate(PREMISES=replace(PREMISES,which(is.na(PREMISES)),""))%>%
  mutate(premises.no=gsub("[^0-9]","",PREMISES)) %>%  
  mutate(str.no=gsub(" .*$","",STREET)) %>%
  mutate(str.no=ifelse(grepl("[0-99]",str.no)==TRUE,str.no,"")) %>%
  mutate(No= paste0(premises.no, str.no)) %>%
  mutate(ID= paste0(No, POSTCODE)) %>%
  mutate(ID=gsub(" ","",ID)) %>% # remove empty spaces
  mutate(ID=gsub("-","",ID)) %>% # remove dashes
  distinct(ID) 
nrow(gp_unique)

gp_tidy<- gp_raw %>% select(UPRN_match,FORENAME, SURNAME, PREMISES, STREET, POSTCODE) %>%
  mutate(PREMISES=replace(PREMISES,which(is.na(PREMISES)),""))%>%
  mutate(premises.no=gsub("[^0-9]","",PREMISES)) %>%  
  mutate(str.no=gsub(" .*$","",STREET)) %>%
  mutate(str.no=ifelse(grepl("[0-99]",str.no)==TRUE,str.no,"")) %>%
  mutate(No= paste0(premises.no, str.no)) %>%
  mutate(ID= paste0(No, POSTCODE)) %>%
  mutate(ID=gsub(" ","",ID)) %>%
mutate(ID=gsub("-","",ID)) # remove dashes
nrow(gp_tidy)

##############################
## MERGING GPs and GAZETEER ##
##############################

# Merging datasets of UNIQUE ID's
#################################
df<-semi_join(gp_unique,gazet_df, by="ID") 
nrow(df) # 64,195  new= 64,283
no.match<-anti_join(gp_unique, gazet_df, by="ID")
nrow(no.match)# 2,818 new=2,730 
# Accuracy
paste0(round(100-nrow(no.match)/nrow(df)*100,3),"%")# 96.61% (new=95.753%)


# Merging back datasets
#################################
df2<-inner_join(df, gazet_df, by="ID") 
nrow(df2) # 64,464 new=64,562

df_final<-inner_join(df2,gp_tidy, by="ID") 
nrow(df_final) # 203,710 new=204,145

# Accuracy UPRN
#################################

### Does not include Phil's non-matches
audit<-df_final %>% filter(!is.na(UPRN_match)) %>%
  filter(UPRN_match!="?") %>%
  mutate(check= ifelse(UPRN==UPRN_match,1,0))

perc<-audit %>% group_by(check) %>% count()  
paste0(round(perc[2,2]/(perc[2,2]+perc[1,2])*100,3),"%") #98.374%  new="98.324%"

wrongly.match<- audit %>% filter(check==0)

# Accuracy TOTAL RECORS FROM GP'S
#################################
paste0(round(nrow(df_final) / nrow(gp_tidy),5)*100,"%") #98.053% new= 98.262%%

# Visual inspection of no matches
exp<-audit %>% filter(check==0) %>% select(PREMISES, STREET, ADDRESS)
head(exp,200)
exp2<-audit %>% filter(check==0) %>% select(UPRN, UPRN_match)
head(exp2,200)


# NO MATCHES
names(gp_tidy)[1]<-"UPRN"
gp.fil <- gp_tidy %>% filter(!is.na(UPRN)) %>% # filtering the ones no UPRN matched by Phil
  filter(UPRN!="?")
no<-anti_join(gp.fil, df_final, by="UPRN")
nrow(no)# 4,224 new=3,942

write.csv(no,"no_match.csv")
write.csv(wrongly.match,"wrong_match.csv")

# E.G.  110 CHELMER CRESCENT matched with parent shel by Phil (tHERE IS 110A, 110B)... 
#       36 ESSEX ROAD       "" "" (IT HAS GROUND FLOOR AND FIRST FLOOR)
#       177 Howard Road
#       59 KING EDWARDS ROAD IG11 7TS (GAZEETER POST CODE FINISHES IN B).
#       16 CROSSNESS ROAD- PEOPLE USED PREMISE NUMBER(16) BUT NOT PROPERTY NUMBER (22)

## CODES
## grepl("^[0-9].*$","hjg")

gazet_df %>% filter(grepl("100057382",UPRN))#NEW CODE

names(df_final)[14]<-"UPRN"

filter(grepl("Faircross Care|Strathfield Gardens|Abbey Care|Westerley Lodge|
              Chestnut Court Care|Keith Lauder House|Lisnaveane Lodge|Woodlands Rainham| 
             Turning Point House| Parkview Nursing Home|Outlook Care|Chaseview Care Centre|
             Sahara Parkside Apartments|Mayesbrook Home For The Aged|Cloud House|Outlook 
             Care|Lisaveane House|Gascogine Road Care Home|Bennetts Castle Care Home|
             Kallar Lodge|Cherry Orchard|Richard Ryan Place|Hanbury Court",ADDRESS)) #NEW CODE