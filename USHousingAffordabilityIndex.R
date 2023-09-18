



library(tidyverse)
library(readr)
library(dbplyr)
library(DBI)
library(plotly)
library(lubridate)
library(sf)
library(stringi)




# Connection to Postgres


con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),    
                      host = "localhost",   
                      port = 5432,   
                      dbname = "Flower",   
                      user = "Flower",   
                      #password = "your_password" 
)
#####################################################


#READ TABLE FROM DATABASE TO DATAFRAME IN R


Raw_table<- tbl(con, "county_market_tracker", row.names=TRUE) %>% 
  select(date= period_end, region, state, property_type, median_sale_price) %>%
  collect()

##################### USING RBIND TO ADD MISSING STATE (WYOMING AND NORTH DAKOTA)
# DATA FROM https://smartasset.com/taxes/north-dakota-property-tax-calculator AND
#https://smartasset.com/taxes/wyoming-property-tax-calculator


# missing_state<-read_csv("data/missing_state.csv")%>%
#   mutate(median_sale_price=sub("\\$", "", median_sale_price),
#          median_sale_price=sub("\\,", "", median_sale_price),
#          date=as.character(date))
# 
# Raw_Table <- rbind(Raw_table, missing_state)%>%
#   mutate(region = gsub(",.*", "", region))

################## END OF RBING ######################

Raw_table1<- Raw_table%>%
  mutate(date = as.Date(date),
         median_sale_price=as.numeric(median_sale_price),
         month=month(date, label=TRUE, abbr=TRUE),
         year=year(date))


##############################################

################ ISOLATING THE HIGHER PRICE FROM EACH YEAR #############
higher_price<-Raw_table1%>%
  select(year, state, region, property_type, median_sale_price)%>%
  group_by(year, region, state, property_type)%>%
  summarise(max=max(median_sale_price))

################ ISOLATING THE MOST RECENT DATE #############
recent_date<-Raw_table1%>%
  filter(date==(max(date))) %>%
  select(state, region, property_type, median_sale_price)

################ most recent shift since higher price #############
recent_shift<-higher_price%>%
  inner_join(recent_date, by=c("region","state","property_type"))%>%
  mutate(shift=(median_sale_price-max)/max,
         region = gsub(",.*", "", region))
       


################ ADDING POPULATION DATA #############
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html

pop_county<-read_csv("data/pop_county.csv", skip=1) %>%
  mutate(state = gsub(".*, ", "", region),
         region = gsub(",.*", "", region),
         region=sub("\\.", "", region))   


recent_shiftPop<-recent_shift%>%
  inner_join(pop_county, by=c("region", "state"))

################ ADDING STATE TAX #############

#https://smartasset.com/taxes/alabama-property-tax-calculator


tax<-read_csv("data/taxes.csv") %>%
  select(region, tax, state)%>%
  mutate(tax = stri_replace_all_regex(tax, #remove % from values
                                      pattern='%',
                                      replacement="",
                                      vectorize=FALSE),
         tax=as.numeric(as.character(tax))/100)

recent_shiftPopTax<-recent_shiftPop%>%
  inner_join(.,tax, by=c("region", "state"))


################ ADDING INSURANCE BY STATE DATA #############

insurance<-read_csv("data/insurance.csv") %>%
  mutate(ins_rate =sub("\\%", "", ins_rate),
         ins_rate=as.numeric(as.character(ins_rate))/100)
           

recent_shiftPopTaxIns<-recent_shiftPopTax%>%
  inner_join(.,insurance, by="state")

################ ADDING INCOME BY STATE DATA #############
#https://www.justice.gov/ust/eo/bapcpa/20220401/bci_data/median_income_table.htm

income<-read_csv("data/inc_2022.csv")%>%
  mutate(state=str_to_lower(state),  #transform upper case to lower case
         state=str_to_title(state), #capitalize first letter
         INC1=sub("\\$", "", INC1),
         INC1=sub("\\,", "", INC1),
         INC1=as.numeric(as.character(INC1)),
         INC2=sub("\\$", "", INC2),
         INC2=sub("\\,", "", INC2),
         INC2=as.numeric(as.character(INC2)),
         INC3=sub("\\$", "", INC3),
         INC3=sub("\\,", "", INC3),
         INC3=as.numeric(as.character(INC3)),
         INC4=sub("\\$", "", INC4),
         INC4=sub("\\,", "", INC4),
         INC4=as.numeric(as.character(INC4)))  


recent_shiftPopTaxInsInc<-recent_shiftPopTaxIns%>%
  inner_join(.,income, by="state")

################ ADDING INEREST RATE #############
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),    
                      host = "localhost",   
                      port = 5432,   
                      dbname = "FED_StLouis",   
                      user = "postgres",   
                      #password = "your_password" 
)

mortgage<- tbl(con, "FredMortgage", row.names=TRUE) %>% 
  select(date, mortg_rate) %>%
  filter(date==max(date))%>%
  collect()


recent_shiftPopTaxInsIncInt<-recent_shiftPopTaxInsInc%>%
  mutate(interest=mortgage$mortg_rate/100)
 

################ HOME AFFORDABILITY CALCULATION #############

# https://www.atlantafed.org/center-for-housing-and-policy/data-and-tools/home-ownership-affordability-monitor

############# EXTRA

########### EXTRA 1 ###########
#Total annual cost = total monthly cost x 12

########### EXTRA 2 ###########
#Annual share of income = total annual cost/median household income



########### STEP 1 ###########

#Total monthly cost = 
#(P & I) + 
#PMI ((median home price x .9) x .00558) + 
#taxes ((county- or state-level rate x median home price)/12) + 
#insurance ((county- or metro-level rate x median home price)/12)

####Monthly principal and interest payment (P&I) = median home price x 0.9 x (interest rate/12)/ (1-(1/ (1+interest rate/12)^360))

########### STEP 2 ###########
#Affordability threshold income (ATI) = total monthly cost x 3.33 x 12

########### STEP 3 ###########

#Index value = (median income / ATI) x 100

recent_shiftPopTaxInsIncIntAford<-recent_shiftPopTaxInsIncInt%>%
  mutate(PI=median_sale_price * 0.9 * (interest/12)/ (1-(1/ (1+interest/12)^360)),
         tax_afford= (tax * median_sale_price)/12,
         ins_afford= (ins_rate * median_sale_price)/12,
         mthly_cost=PI+tax_afford+ins_afford,
         ATI=mthly_cost*3.33*12,
         afford_IndexValue=(INC4/ATI)*100)

################ WRITING DATA INTO PSQL DATABASE #############

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),    
                      host = "localhost",   
                      port = 5432,   
                      dbname = "Flower",   
                      user = "Flower",   
                      #password = "your_password" 
)

dbWriteTable(con, 
             name = "recent_shiftPopTaxInsIncIntAford", # TABLE NAME to be in postgres
             value = recent_shiftPopTaxInsIncIntAford,  # DataFrame from R
             row.names = TRUE,
             append = FALSE,
             overwrite = TRUE)


DBI::dbListTables(con)

dbDisconnect()

