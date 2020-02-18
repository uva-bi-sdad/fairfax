library(readr)
library(dplyr)

# Read in data (see Brandon's 01_ffiec-data-wrangling.Rmd for 2007-17 data join & filtering)
data <- read_csv("./rivanna_data/original/ffiec/cfbp-hmda-ffx-2007-17.csv", guess_max = 800000, progress = T)

# Years
table(data$as_of_year, useNA = "always") # No NAs
#  2007   2008   2009   2010   2011   2012   2013   2014   2015   2016   2017
# 93165  64013  98775  92577  87719 120748  92988  51080  22791  71761  51969

# Property types 
table(data$as_of_year, data$property_type_name, useNA = "always") # No NAs
#       Manufactured     Multifamily      One-to-four
#            housing        dwelling  family dwelling 
# 2007           155              29            92981 
# 2008           175              30            63808 
# 2009            71              27            98677
# 2010            65              30            92482
# 2011            41              22            87656
# 2012            74              28           120646
# 2013            82              32            92874 
# 2014            54              21            51005 
# 2015            22              13            22756 
# 2016            74              29            71658 
# 2017            73              28            51868 

# Loan purpose
table(data$as_of_year, data$loan_purpose_name, useNA = "always") # No NAs
#                  Home          Home
#           improvement      purchase   Refinancing  
# 2007             5672         38424       49069 
# 2008             3425         27909       32679 
# 2009             2326         29295       67154
# 2010             2159         24500       65918 
# 2011             2710         22630       62379 
# 2012             3693         24631       92424
# 2013             4103         26739       62146 
# 2014             3691         22729       24660 
# 2015             1310          8313       13168 
# 2016             3356         26495       41910  
# 2017             3179         26891       21899  

# Preapproval
table(data$as_of_year, data$preapproval_name, useNA = "always") # No NAs
#       Not applicable Preapproval was not requested Preapproval was requested 
# 2007          78473                         12729                      1963
# 2008          50183                         11871                      1959
# 2009          88966                          8549                      1260 
# 2010          85662                          6131                       784
# 2011          82084                          4860                       775 
# 2012         115119                          4680                       949
# 2013          86522                          5423                      1043
# 2014          46891                          3062                      1127
# 2015          19785                          1866                      1140
# 2016          66214                          4303                      1244
# 2017          46180                          4290                      1499

# Action taken
table(data$as_of_year, data$action_taken_name, useNA = "always") # No NAs
#           Loan purchased        Preapproval request        Preapproval request
#           by institution     approved not accepted  denied by fin. institution  
# 2007               20758                        0                          24 
# 2008               11323                        0                           4 
# 2009               23529                        0                          44   
# 2010               20154                        0                           0 
# 2011               19998                        1                           0   
# 2012               27354                        0                           1   
# 2013               19812                        0                           1  
# 2014                9174                        0                           0  
# 2015                2002                        0                           0  
# 2016               12253                        0                           2   
# 2017                9039                        0                           2   

# Applicant ethnicity
table(data$as_of_year, data$applicant_ethnicity_name, useNA = "always") # No NAs
#          Hispanic or        Information            Not   Not Hispanic
#               Latino       not provided     applicable      or Latino
# 2007           11283              15234           9455          57193
# 2008            5325              11372           4897          42419
# 2009            4400              16195          12460          65720 
# 2010            4071              15120           7979          65407 
# 2011            3891              13425           9862          60541 
# 2012            5449              19213          11756          84330 
# 2013            5251              14612          10157          62968 
# 2014            3358               7700           6806          33216 
# 2015            1701               3499           1633          15958 
# 2016            5109              10845           8711          47096  
# 2017            4074               8246           5338          34311 

# Applicant #1 race
table(data$as_of_year, data$applicant_race_name_1, useNA = "always") # No NAs
# American Indian or                  Information  Nat. Hawaiian            Not
#      Alaska Native   Asian  Black  not provided     or Pacific     applicable   White
# 2007          607    12005   6029         16083            489           9369   48583
# 2008          317     9030   3446         11660            303           4867   34390
# 2009          298    12375   3327         16612            306          12432   53425
# 2010          297    13608   3241         15483            314           7975   51659
# 2011          303    12668   3124         14021            330           9845   47428
# 2012          407    17964   4260         19696            366          11750   66305
# 2013          371    13845   4245         15346            319          10154   48708
# 2014          225     7750   2530          8079            193           6795   25508
# 2015          137     3542   1263          3698            103           1625   12423
# 2016          286    11599   3869         11663            265           8695   35384
# 2017          281     8630   3156          8913            220           5320   25449

# Applicant #1 gender
table(data$as_of_year, data$applicant_sex_name, useNA = "always") # No NAs
#      Female Not provided   Male   Not applicable
# 2007  26250         8307 49192           9416 
# 2008  17432         6527 35184           4870 
# 2009  21951         9283 55086          12455
# 2010  21092         8890 54618           7977 
# 2011  19320         8287 50260           9852 
# 2012  27010        11053 70936          11749 
# 2013  22028         8720 52086          10154 
# 2014  12138         4789 27349           6804  
# 2015   5892         2208 13067           1624 
# 2016  16620         7049 39392           8700  
# 2017  13436         5463 27748           5322  

# Owner occupancy
table(data$as_of_year, data$owner_occupancy_name, useNA = "always") # No NAs
#      Not applicable Not owner-occupied as a principal dwelling Owner-occupied as a principal dwelling 
# 2007             59                                       5268                                  87838
# 2008             99                                       3990                                  59924
# 2009           1490                                       4166                                  93119 
# 2010             73                                       4823                                  87681 
# 2011             57                                       5900                                  81762 
# 2012             68                                       9272                                 111408 
# 2013            148                                       8537                                  84303 
# 2014             25                                       4420                                  46635 
# 2015             12                                       1386                                  21393 
# 2016             94                                       4812                                  66855
# 2017             88                                       3823                                  48058 

# Denial status
table(data$as_of_year, data$denial_reason_name_1, useNA = "always") 
#                 Credit appl.    Credit   Debt-to-income  Employment  Insufficient  Mortgage insur.         Unverifiable
#      Collateral   incomplete   history            ratio     history          cash           denied  Other   information     NA
# 2007       2031         1576      1945             2192         124           209                9   1561           785  82733
# 2008       2246          879      1067             1674          79           190               21    848           378  56631
# 2009       2996          825       895             1994         113           203               20    855           470  90404
# 2010       1791         1440       890             1910          75           174               24   1352           409  84512
# 2011       1631         1478      1233             2043          98           196               11   1034           505  79490
# 2012       1807         2106      1661             2502         134           267               14   1185           628 110444
# 2013       1336         1294      1790             2200         114           229                3    805           485  84732
# 2014        731          738      1302             1530          76           143                1    424           271  45864
# 2015        265          284       392              582          27            48                1    198            94  20900
# 2016        752         1023       971             1540          98           142                3    516           280  66436
# 2017        557          769       763             1418          49           128                2    462           210  47611

# Lien status
table(data$as_of_year, data$lien_status_name, useNA = "always") 
#                Not            Not secured            Secured by a                  Secured by a 
#         applicable              by a lien              first lien              subordinate lien 
# 2007          20758                   554                   54383                         17470 
# 2008          11323                   865                   46221                          5604 
# 2009          23529                   400                   73134                          1712 
# 2010          20154                   356                   70306                          1761 
# 2011          19998                   671                   65152                          1898  
# 2012          27354                  1381                   90066                          1947  
# 2013          19812                  1585                   69397                          2194  
# 2014           9174                  1497                   38243                          2166  
# 2015           2002                   345                   19762                           682    
# 2016          12253                  1090                   56967                          1451  
# 2017           9039                   974                   40230                          1726    







# Purchaser type
table(data$as_of_year, data$purchaser_type_name, useNA = "always") # No NAs

