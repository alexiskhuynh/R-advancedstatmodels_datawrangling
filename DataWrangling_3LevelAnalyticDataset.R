#### DATA WRANGLING IN R
#### USE FACILITY-LEVEL PROPORTIONS TO CREATE 3-LEVEL DATASET WITH 131,388 RESPONDENTS AT 8 FACILITITIES OVER 28 QUARTERS
#### LEVEL 1 - RESPONDENTS (WITH Y=RECEIPT OF REFERRAL), LEVEL 2 - QUARTERS, LEVEL 3 - FACILITIES
#### 

# Working directory
setwd("C:/Users/Alexis K Huynh/OneDrive/Desktop/R")
setwd ("C:/Users/Alexis K Huynh/OneDrive/Desktop/R/Datasets")

list.files() 

# install.packages("labelled") # If you need to INSTALL the package.
require(labelled)

library(tidyverse)
library(dplyr)
library(readr)

SW <- read_csv("steppedwedge.csv")

View(SW)

# CREATE DATASET
#create variables & assign values
site_id  <- c(1:8)
quarter_id  <- c(1:28)
pat_id  <- c(1:131388)
referral01 <- c(0:1)
impmain01 <- (0:1)
duration <- (0:18)
quarter <- c(1:28)
site_num <- c(1:8)

# RECODE VARIABLES
referral01[(pat_id>=1 & pat_id<=665)|(pat_id>=5056 & pat_id<=5701)|(pat_id>= 10177& pat_id<=10803)|(pat_id>=15315 & pat_id<=15889)|(pat_id>=20278 & pat_id<=20840)|
       (pat_id>=25253 & pat_id<=25828)|(pat_id>= 30250& pat_id<=30810)|(pat_id>=35172 & pat_id<=35657)|(pat_id>=40026 & pat_id<=40514)|
       (pat_id>=44976 & pat_id<=45487) | (pat_id>=49732 & pat_id<=50202)|(pat_id>=54126 & pat_id<=54769)|(pat_id>=59518 & pat_id<=59685)|
       (pat_id>= 63610& pat_id<=64119)|(pat_id>=68064 & pat_id<=68579)|(pat_id>=72616 & pat_id<=73120)|(pat_id>= 77141& pat_id<=77649)|
       (pat_id>=81653 & pat_id<=82169 )|(pat_id>=86187 & pat_id<=86688)|(pat_id>=90655 & pat_id<=91132)|(pat_id>=95136 & pat_id<=95625)|
       (pat_id>=99674 & pat_id<=100153)|(pat_id>=104168 & pat_id<=104635)|(pat_id>=108695 & pat_id<=109150)|(pat_id>=113220 & pat_id<=113680)|
       (pat_id>=117783 & pat_id<=118222)|(pat_id>=122350 & pat_id<=122799)|(pat_id>=126891 & pat_id<=127357)]  <- 0

referral01[(pat_id>=1154 & pat_id<=1608)|(pat_id>=6204 & pat_id<=6700)|(pat_id>=11276& pat_id<=11834)|(pat_id>=16357& pat_id<=16903)|
       (pat_id>=21286 & pat_id<=21861)|(pat_id>=26272 & pat_id<=26858)|(pat_id>=31230 & pat_id<=31814)|(pat_id>=36029 & pat_id<=36589)|
       (pat_id>=40897 & pat_id<=41417)|(pat_id>=40897 & pat_id<=46142) |(pat_id>=50581 & pat_id<=50844)|(pat_id>=55150 & pat_id<=55150)|
       (pat_id>=60067 & pat_id<=60374)|(pat_id>=64492 & pat_id<=64804)|(pat_id>=68969 & pat_id<=69308)|(pat_id>=73520 & pat_id<=73860)|
       (pat_id>=78052 & pat_id<=78400)|(pat_id>=82564 & pat_id<=82906)|(pat_id>=87075 & pat_id<=87404)|(pat_id>=91502 & pat_id<=91834) |
       (pat_id>=96001 & pat_id<=96331)|(pat_id>=100513 & pat_id<=100850)|(pat_id>=104999 & pat_id<=105359)|
       (pat_id>=109514 & pat_id<=109871)|(pat_id>=114047 & pat_id<=114391)|(pat_id>=118591 & pat_id<=118891)|
       (pat_id>=123165 & pat_id<=123407)|(pat_id>=127731 & pat_id<=127962)]  <- 0

referral01[(pat_id>=1746 & pat_id<=2237)|(pat_id>=6847 & pat_id<=7328)|(pat_id>=11973 & pat_id<=12439)|
       (pat_id>=17050 & pat_id<=17424)|(pat_id>= 22016 & pat_id<=22382)|(pat_id>=27006 & pat_id<=27372)|
       (pat_id>= 31958 & pat_id<= 32317)|(pat_id>=36723 & pat_id<=37056)|(pat_id>= 41557& pat_id<=41885)|
       (pat_id>=46284 & pat_id<= 46611)|(pat_id>= 50987 & pat_id<= 51317)|(pat_id>= 55619& pat_id<=55962)|
       (pat_id>= 60525& pat_id<=60840)|(pat_id>= 64973 & pat_id<=65280)|(pat_id>= 69477& pat_id<=69794)|
       (pat_id>=74031 & pat_id<=74329)|(pat_id>=78560 & pat_id<=78855)|(pat_id>= 83066 & pat_id<=83369)|
       (pat_id>=87571 & pat_id<=87873) |(pat_id>= 92024 & pat_id<= 92327)|(pat_id>=96524 & pat_id<=96829)|
       (pat_id>=101050 & pat_id<=101338)|(pat_id>=105559 & pat_id<=105830)|(pat_id>=110064 & pat_id<=110330)|
       (pat_id>= 114586& pat_id<=114849)|(pat_id>=114849 & pat_id<=119336)|(pat_id>=123596& pat_id<=123852)|
       (pat_id>=128135 & pat_id<=128386)]  <- 0

referral01[(pat_id>=2343 & pat_id<=2782)|(pat_id>=7436& pat_id<=7885)|(pat_id>=12545 & pat_id<=13030)|
       (pat_id>=17528 & pat_id<=17956)|(pat_id>=22488 & pat_id<=22868)|(pat_id>=27475 & pat_id<=27839)|
       (pat_id>=32416 & pat_id<=32728)|(pat_id>=37180 & pat_id<=37552)|(pat_id>=42008 & pat_id<=42417)|
       (pat_id>=46738 & pat_id<=47262)|(pat_id>=51436 & pat_id<=51827)|(pat_id>=56086 & pat_id<=56703)|
       (pat_id>=60969 & pat_id<=61277)|(pat_id>=65413 & pat_id<=65699)|(pat_id>=69919 & pat_id<=70210)|
       (pat_id>=74453 & pat_id<=74733)|(pat_id>=78980 & pat_id<=79269)|(pat_id>=83499 & pat_id<=83781)|
       (pat_id>=88011 & pat_id<=88293)|(pat_id>=92458 & pat_id<=92749)|(pat_id>=96973 & pat_id<=97262)|
       (pat_id>=101480 & pat_id<=101763)|(pat_id>=105974 & pat_id<=106255)|(pat_id>=110468 & pat_id<=110745)|
       (pat_id>=114992 & pat_id<=115286)|(pat_id>=119485 & pat_id<=119791)|(pat_id>= 124007& pat_id<=124309)|
       (pat_id>=128541 & pat_id<=128832)]  <- 0

referral01[(pat_id>=3058 & pat_id<=3245)|(pat_id>=8149 & pat_id<=8342)|(pat_id>=13274 & pat_id<=13489)|(pat_id>=18197 & pat_id<=18420)|
       (pat_id>=231124 & pat_id<=23334)|(pat_id>=28102 & pat_id<=28312)|(pat_id>=32989 & pat_id<=33176)|
       (pat_id>=37807 & pat_id<=37967)|(pat_id>=42675 & pat_id<=42831)|(pat_id>=47508 & pat_id<=47666)|
       (pat_id>=52068 & pat_id<=52218)|(pat_id>=56954 & pat_id<=57107)|(pat_id>=61529 & pat_id<=61679)|
       (pat_id>=65956 & pat_id<=66118)|(pat_id>=70460 & pat_id<=70619)|(pat_id>=74982 & pat_id<=75139)|
       (pat_id>=79518 & pat_id<=79679)|(pat_id>=84035 & pat_id<=84195)|(pat_id>=88538 & pat_id<=88693)|
       (pat_id>=93000 & pat_id<=93154) | (pat_id>=97510 & pat_id<=97667)|(pat_id>=102008 & pat_id<=102146)|
       (pat_id>=106503 & pat_id<=106632)|(pat_id>=110995 & pat_id<=111101)|(pat_id>=115545 & pat_id<=115665)|
       (pat_id>=120059 & pat_id<=120186)|(pat_id>=124564 & pat_id<=124689)|(pat_id>=129081 & pat_id<=129209)]  <- 0

referral01[(pat_id>=3292 & pat_id<=3704)|(pat_id>=8404 & pat_id<=8819)|(pat_id>=13548 & pat_id<=13961)|(pat_id>=18480 & pat_id<=18915)|
       (pat_id>=23408 & pat_id<=23854)|(pat_id>=28387 & pat_id<=28849)|(pat_id>=33266 & pat_id<=33736)|
       (pat_id>=38031 & pat_id<=38496)|(pat_id>=42899 & pat_id<=43282)|(pat_id>=47740 & pat_id<=48226)|
       (pat_id>=52290 & pat_id<=52742)|(pat_id>=57190 & pat_id<=57651)|(pat_id>=61774 & pat_id<=62203)|
       (pat_id>=66218 & pat_id<=66652)|(pat_id>=70727 & pat_id<=71180)|(pat_id>=72251 & pat_id<=75692)|
       (pat_id>=79790 & pat_id<=80225)|(pat_id>=84294 & pat_id<=84739)|(pat_id>=88784 & pat_id<=89229)|
       (pat_id>=93240 & pat_id<=93688)|(pat_id>=97752 & pat_id<=98239)|(pat_id>=102260 & pat_id<=102763)|
       (pat_id>=106756 & pat_id<=107290)|(pat_id>=111251 & pat_id<=111819)|(pat_id>=115826 & pat_id<=116368)|
       (pat_id>=120361 & pat_id<=120893)|(pat_id>=124879 & pat_id<=125431)|(pat_id>=129410 & pat_id<=129960)]  <- 0


referral01[(pat_id>=3930 & pat_id<=4447)|(pat_id>=9059 & pat_id<=9566)|(pat_id>=14203 & pat_id<=14709)|(pat_id>=19181 & pat_id<=19645)|
       (pat_id>=24135 & pat_id<=24601)|(pat_id>=29138 & pat_id<=29569)|(pat_id>=34045 & pat_id<=34487)|
       (pat_id>=38850 & pat_id<=39338)|(pat_id>=43747 & pat_id<=44260)|(pat_id>=48580 & pat_id<=49115)|
       (pat_id>=53119 & pat_id<=53627)|(pat_id>=58049 & pat_id<=58549)|(pat_id>=62598 & pat_id<=63045)|
       (pat_id>=67065 & pat_id<=67517)|(pat_id>=71608 & pat_id<=72043)|(pat_id>=76139 & pat_id<=76549)|
       (pat_id>=80663 & pat_id<=81047)|(pat_id>=85202 & pat_id<=85571)|(pat_id>=89687 & pat_id<=90042)|
       (pat_id>=94168 & pat_id<=94480)|(pat_id>=98708 & pat_id<=99023)|(pat_id>=103226 & pat_id<=103533)|
       (pat_id>=107765 & pat_id<=108056)|(pat_id>=112292 & pat_id<=112574)|(pat_id>=116870 & pat_id<=117150)|
       (pat_id>=121387 & pat_id<=121675)|(pat_id>=125925 & pat_id<=126202)|(pat_id>=130461 & pat_id<=130736)]  <- 0

referral01[(pat_id>=4602 & pat_id<=4867)|(pat_id>=9722 & pat_id<=9998)|(pat_id>=14821 & pat_id<=15137)|(pat_id>=19770 & pat_id<=20089)|
       (pat_id>=24723 & pat_id<=25058)|(pat_id>=29697 & pat_id<=30060)|(pat_id>=34617 & pat_id<=34979)|
       (pat_id>=39481 & pat_id<=39841)|(pat_id>=44405 & pat_id<=44786)|(pat_id>=49238 & pat_id<=49538)|
       (pat_id>=53748 & pat_id<=53998)|(pat_id>=58693 & pat_id<=58935)|(pat_id>=63186 & pat_id<=63367)|
       (pat_id>=67661 & pat_id<=67803)|(pat_id>=72200 & pat_id<=72366)|(pat_id>=76716 & pat_id<=76862)|
       (pat_id>=81209 & pat_id<=81362)|(pat_id>=85736 & pat_id<=85892)|(pat_id>=90202 & pat_id<=90382)|
       (pat_id>=94668 & pat_id<=94823)|(pat_id>=99216 & pat_id<=99368)|(pat_id>=103718 & pat_id<=103861)|
       (pat_id>=108245 & pat_id<=108383)|(pat_id>=112765 & pat_id<=112907)|(pat_id>=117335 & pat_id<=117466)|
       (pat_id>=121878 & pat_id<=122027)|(pat_id>=126420 & pat_id<=126569)|(pat_id>=130935 & pat_id<=131077)]  <- 0

SW  <- data.frame(pat_id, referral01)

###### replace NA values with 1s 
missing_refs  <- !complete.cases(SW$referral01)

SW$referral01[missing_refs]  <- 1

# quarter_id
quarter_id[pat_id>=1 & pat_id<=5055]  <- 1
quarter_id[pat_id>=5056 & pat_id<=10176]  <- 2
quarter_id[pat_id>=10177 & pat_id<=15314]  <- 3
quarter_id[pat_id>=15315 & pat_id<=20277]  <- 4
quarter_id[pat_id>=20278 & pat_id<=25252]  <- 5
quarter_id[pat_id>=25253 & pat_id<=30249]  <- 6
quarter_id[pat_id>=30250 & pat_id<=35171]  <- 7
quarter_id[pat_id>=35172 & pat_id<=40025]  <- 8
quarter_id[pat_id>=40026 & pat_id<=44975]  <- 9
quarter_id[pat_id>=44976 & pat_id<=49731]  <- 10
quarter_id[pat_id>=49732 & pat_id<=54215]  <- 11
quarter_id[pat_id>=54216 & pat_id<=59157]  <- 12
quarter_id[pat_id>=59158 & pat_id<=63609]  <- 13 
quarter_id[pat_id>=63610 & pat_id<=68063]  <- 14
quarter_id[pat_id>=68064 & pat_id<=72615]  <- 15
quarter_id[pat_id>=72616 & pat_id<=77140]  <- 16 
quarter_id[pat_id>=77141 & pat_id<=81652]  <- 17 
quarter_id[pat_id>=81653 & pat_id<=86186]  <- 18
quarter_id[pat_id>=86187 & pat_id<=90654]  <- 19
quarter_id[pat_id>=90655 & pat_id<=95135]  <- 20 
quarter_id[pat_id>=95136 & pat_id<=99673]  <- 21
quarter_id[pat_id>=99674 & pat_id<=104167]  <- 22
quarter_id[pat_id>=104168 & pat_id<=108694]  <- 23
quarter_id[pat_id>=108695 & pat_id<=113219]  <- 24 
quarter_id[pat_id>=113220 & pat_id<=117782]  <- 25
quarter_id[pat_id>=117783 & pat_id<=122349]  <- 26
quarter_id[pat_id>=122350 & pat_id<=126890]  <- 27
quarter_id[pat_id>=126891 & pat_id<=131388]  <- 28

SW  <- data.frame(pat_id, quarter_id, referral01)

# site_id
site_id[pat_id>=1 & pat_id <=1153] <- 1
site_id[pat_id>=5056 & pat_id <=6203] <- 1
site_id[pat_id>=10177 & pat_id <=11275] <- 1
site_id[pat_id>=15315 & pat_id <=16356] <- 1
site_id[pat_id>=20278 & pat_id <=21285] <- 1
site_id[pat_id>=25253 & pat_id <=26271] <- 1
site_id[pat_id>=30250 & pat_id <=31229] <- 1
site_id[pat_id>=35172 & pat_id <=36028] <- 1
site_id[pat_id>=40026 & pat_id <=40896] <- 1
site_id[pat_id>=44976 & pat_id <=45862] <- 1
site_id[pat_id>=49732 & pat_id <=50580] <- 1
site_id[pat_id>=54216 & pat_id <=55149] <- 1
site_id[pat_id>=59158 & pat_id <=60066] <- 1
site_id[pat_id>=63610 & pat_id <=64491] <- 1
site_id[pat_id>=68064 & pat_id <=68968] <- 1
site_id[pat_id>=72616 & pat_id <=73519] <- 1
site_id[pat_id>=77141 & pat_id <=78051] <- 1
site_id[pat_id>=81653 & pat_id <=82563] <- 1
site_id[pat_id>=86187 & pat_id <=87074] <- 1
site_id[pat_id>=90655 & pat_id <=91501] <- 1
site_id[pat_id>=95136 & pat_id <=96000] <- 1
site_id[pat_id>=99674 & pat_id <=100512] <- 1
site_id[pat_id>=104168 & pat_id <=104998] <- 1
site_id[pat_id>=108695 & pat_id <=109513] <- 1
site_id[pat_id>=113220 & pat_id <=114046]  <- 1
site_id[pat_id>=117783 & pat_id <=118590] <- 1
site_id[pat_id>=122350 & pat_id <=123164]  <- 1
site_id[pat_id>=126891 & pat_id <=127730] <- 1

site_id[pat_id>=1154 & pat_id<=1745] <- 2
site_id[pat_id>=6204 & pat_id<=6846] <- 2
site_id[pat_id>=11276 & pat_id<=11972] <- 2
site_id[pat_id>=16357 & pat_id<=17049] <- 2
site_id[pat_id>=21286 & pat_id<=22015] <- 2
site_id[pat_id>=26272 & pat_id<=27005] <- 2
site_id[pat_id>=31230 & pat_id<=31957] <- 2
site_id[pat_id>=36029 & pat_id<=36722] <- 2
site_id[pat_id>=40897 & pat_id<=41556] <- 2
site_id[pat_id>=45863 & pat_id<=46283] <- 2
site_id[pat_id>=50581 & pat_id<=50986] <- 2
site_id[pat_id>=55150 & pat_id<=55618] <- 2
site_id[pat_id>=60067 & pat_id<=60524] <- 2
site_id[pat_id>=64492 & pat_id<=64972] <- 2
site_id[pat_id>=68969 & pat_id<=69476] <- 2
site_id[pat_id>=73520 & pat_id<=74030] <- 2
site_id[pat_id>=78052 & pat_id<=78559] <- 2
site_id[pat_id>=82564 & pat_id<=83065] <- 2
site_id[pat_id>=87075 & pat_id<=87570] <- 2
site_id[pat_id>=91502 & pat_id<=92023] <- 2
site_id[pat_id>=96001 & pat_id<=96523] <- 2
site_id[pat_id>=100513 & pat_id<=101049] <- 2
site_id[pat_id>=104999 & pat_id<=105558] <- 2
site_id[pat_id>=109514 & pat_id<=110063] <- 2
site_id[pat_id>=114047 & pat_id<=114585] <- 2
site_id[pat_id>=118591 & pat_id <=119087] <- 2
site_id[pat_id>=123165 & pat_id <=123595]  <- 2
site_id[pat_id>=127731 & pat_id <=128134] <- 2

site_id [pat_id>=1746&pat_id<=2342] <- 3
site_id[pat_id>=6847&pat_id<=7435] <- 3
site_id[pat_id>=11973&pat_id<=12544] <- 3
site_id[pat_id>=17050&pat_id<=17527] <- 3
site_id[pat_id>=22016&pat_id<=22487] <- 3
site_id[pat_id>=27006&pat_id<=27474] <- 3
site_id[pat_id>=31958&pat_id<=32415] <- 3
site_id[pat_id>=36723&pat_id<=37179] <- 3
site_id[pat_id>=41557&pat_id<=42007] <- 3
site_id[pat_id>=46284&pat_id<=46737] <- 3
site_id[pat_id>=50987&pat_id<=51435] <- 3
site_id[pat_id>=55619&pat_id<=56085] <- 3
site_id[pat_id>=60525&pat_id<=60968] <- 3
site_id[pat_id>=64973&pat_id<=65412] <- 3
site_id[pat_id>=69477&pat_id<=69918] <- 3
site_id[pat_id>=74031&pat_id<=74452] <- 3
site_id[pat_id>=78560&pat_id<=78979] <- 3
site_id[pat_id>=83066&pat_id<=83498] <- 3
site_id[pat_id>=87571&pat_id<=88010] <- 3
site_id[pat_id>=92024&pat_id<=92457] <- 3
site_id[pat_id>=96524&pat_id<=96972] <- 3
site_id[pat_id>=101050&pat_id<=101479] <- 3
site_id[pat_id>=105559&pat_id<=105973] <- 3
site_id[pat_id>=110064&pat_id<=110467] <- 3
site_id[pat_id>=114586&pat_id<=114991]  <- 3
site_id[pat_id>=119088&pat_id <=119484]  <- 3
site_id[pat_id>=123596&pat_id <=124006]  <- 3
site_id[pat_id>=128135&pat_id <=128540]  <- 3

site_id[pat_id>=2343&pat_id<=3057] <- 4
site_id[pat_id>=7436&pat_id<=8148] <- 4
site_id[pat_id>=12545&pat_id<=13273] <- 4
site_id[pat_id>=17528&pat_id<=18196] <- 4
site_id[pat_id>=22488&pat_id<=23123] <- 4
site_id[pat_id>=27475&pat_id<=28101] <- 4
site_id[pat_id>=32416&pat_id<=32988] <- 4
site_id[pat_id>=37180&pat_id<=37806] <- 4
site_id[pat_id>=42008&pat_id<=42674] <- 4
site_id[pat_id>=46738&pat_id<=47507] <- 4
site_id[pat_id>=51436&pat_id<=52067] <- 4
site_id[pat_id>=56086&pat_id<=56953] <- 4
site_id[pat_id>=60969&pat_id<=61528] <- 4
site_id[pat_id>=65413&pat_id<=65955] <- 4
site_id[pat_id>=69919&pat_id<=70459] <- 4
site_id[pat_id>=74453&pat_id<=74981] <- 4
site_id[pat_id>=78980&pat_id<=79517] <- 4
site_id[pat_id>=83499&pat_id<=84034] <- 4
site_id[pat_id>=88011&pat_id<=88537] <- 4
site_id[pat_id>=92458&pat_id<=92999] <- 4
site_id[pat_id>=96973&pat_id<=97509] <- 4
site_id[pat_id>=101480&pat_id<=102007] <- 4
site_id[pat_id>=105974&pat_id<=106502] <- 4
site_id[pat_id>=110468&pat_id<=110994] <- 4
site_id[pat_id>=114992&pat_id<=115544]  <- 4
site_id[pat_id>=119485&pat_id <=120058]  <- 4
site_id[pat_id>=124007&pat_id <=124563]  <- 4
site_id[pat_id>=128541&pat_id <=129080]  <- 4

site_id [pat_id>=3058&pat_id<=3291] <- 5
site_id[pat_id>=8149&pat_id<=8403] <- 5
site_id[pat_id>=13274&pat_id<=13547] <- 5
site_id[pat_id>=18197&pat_id<=18479] <- 5
site_id[pat_id>=23124&pat_id<=23407] <- 5
site_id[pat_id>=28102&pat_id<=28386] <- 5
site_id[pat_id>=32989&pat_id<=33265] <- 5
site_id[pat_id>=37807&pat_id<=38030] <- 5
site_id[pat_id>=42675&pat_id<=42898] <- 5
site_id[pat_id>=47508&pat_id<=47739] <- 5
site_id[pat_id>=52068&pat_id<=52289] <- 5
site_id[pat_id>=56954&pat_id<=57189] <- 5
site_id[pat_id>=61529&pat_id<=61773] <- 5
site_id[pat_id>=65956&pat_id<=66217] <- 5
site_id[pat_id>=70460&pat_id<=70726] <- 5
site_id[pat_id>=74982&pat_id<=75250] <- 5
site_id[pat_id>=79518&pat_id<=79789] <- 5
site_id[pat_id>=84035&pat_id<=84293] <- 5
site_id[pat_id>=88538&pat_id<=88783] <- 5
site_id[pat_id>=93000&pat_id<=93239] <- 5
site_id[pat_id>=97510&pat_id<=97751] <- 5
site_id[pat_id>=102008&pat_id<=102259] <- 5
site_id[pat_id>=106503&pat_id<=106755] <- 5
site_id[pat_id>=110995&pat_id<=111250] <- 5
site_id[pat_id>=115545&pat_id<=115825]  <- 5
site_id[pat_id>=120059&pat_id <=120360]  <- 5
site_id[pat_id>=124564&pat_id <=124878]  <- 5
site_id[pat_id>=129081&pat_id <=129409]  <- 5

site_id [pat_id>=3292&pat_id<=3929] <- 6
site_id[pat_id>=8404&pat_id<=9058] <- 6
site_id[pat_id>=13548&pat_id<=14202] <- 6
site_id[pat_id>=18480&pat_id<=19180] <- 6
site_id[pat_id>=23408&pat_id<=24134] <- 6
site_id[pat_id>=28387&pat_id<=29137] <- 6
site_id[pat_id>=33266&pat_id<=34044] <- 6
site_id[pat_id>=38031&pat_id<=38849] <- 6
site_id[pat_id>=42899&pat_id<=43746] <- 6
site_id[pat_id>=47740&pat_id<=48579] <- 6
site_id[pat_id>=52290&pat_id<=53118] <- 6
site_id[pat_id>=57190&pat_id<=58048] <- 6
site_id[pat_id>=61774&pat_id<=62597] <- 6
site_id[pat_id>=66218&pat_id<=67064] <- 6
site_id[pat_id>=70727&pat_id<=71607] <- 6
site_id[pat_id>=75251&pat_id<=76138] <- 6
site_id[pat_id>=79790&pat_id<=80662] <- 6
site_id[pat_id>=84294&pat_id<=85201] <- 6
site_id[pat_id>=88784&pat_id<=89686] <- 6
site_id[pat_id>=93240&pat_id<=94167] <- 6
site_id[pat_id>=97752&pat_id<=98707] <- 6
site_id[pat_id>=102260&pat_id<=103225] <- 6
site_id[pat_id>=106756&pat_id<=107764] <- 6
site_id[pat_id>=111251&pat_id<=112291] <- 6
site_id[pat_id>=115826&pat_id<=116869]  <- 6
site_id[pat_id>=120361&pat_id <=121386]  <- 6
site_id[pat_id>=124879&pat_id <=125924]  <- 6
site_id[pat_id>=129410&pat_id <=130460]  <- 6

site_id [pat_id>=3930&pat_id<=4601] <- 7
site_id[pat_id>=9059&pat_id<=9721] <- 7
site_id[pat_id>=14203&pat_id<=14820] <- 7
site_id[pat_id>=19181&pat_id<=19769] <- 7
site_id[pat_id>=24135&pat_id<=24722] <- 7
site_id[pat_id>=29138&pat_id<=29696] <- 7
site_id[pat_id>=34045&pat_id<=34616] <- 7
site_id[pat_id>=38850&pat_id<=39480] <- 7
site_id[pat_id>=43747&pat_id<=44404] <- 7
site_id[pat_id>=48580&pat_id<=49237] <- 7
site_id[pat_id>=53119&pat_id<=53747] <- 7
site_id[pat_id>=58049&pat_id<=58692] <- 7
site_id[pat_id>=62598&pat_id<=63185] <- 7
site_id[pat_id>=67065&pat_id<=67660] <- 7
site_id[pat_id>=71608&pat_id<=72199] <- 7
site_id[pat_id>=76139&pat_id<=76715] <- 7
site_id[pat_id>=80663&pat_id<=81208] <- 7
site_id[pat_id>=85202&pat_id<=85735] <- 7
site_id[pat_id>=89687&pat_id<=90201] <- 7
site_id[pat_id>=94168&pat_id<=94667] <- 7
site_id[pat_id>=98708&pat_id<=99215] <- 7
site_id[pat_id>=103226&pat_id<=103717] <- 7
site_id[pat_id>=107765&pat_id<=108244] <- 7
site_id[pat_id>=112292&pat_id<=112764] <- 7
site_id[pat_id>=116870&pat_id<=117334]  <- 7
site_id[pat_id>=121387&pat_id <=121877]  <- 7
site_id[pat_id>=125925&pat_id <=126419]  <- 7
site_id[pat_id>=130461&pat_id <=130934]  <- 7

site_id[pat_id>=4602&pat_id<=5055] <- 8
site_id[pat_id>=9722&pat_id<=10176] <- 8
site_id[pat_id>=14821&pat_id<=15314] <- 8
site_id[pat_id>=19770&pat_id<=20277] <- 8
site_id[pat_id>=24723&pat_id<=25252] <- 8
site_id[pat_id>=29697&pat_id<=30249] <- 8
site_id[pat_id>=34617&pat_id<=35171] <- 8
site_id[pat_id>=39481&pat_id<=40025] <- 8
site_id[pat_id>=44405&pat_id<=44975] <- 8
site_id[pat_id>=49238&pat_id<=49731] <- 8
site_id[pat_id>=53748&pat_id<=54215] <- 8
site_id[pat_id>=58693&pat_id<=59157] <- 8
site_id[pat_id>=63186&pat_id<=63609] <- 8
site_id[pat_id>=67661&pat_id<=68063] <- 8
site_id[pat_id>=72200&pat_id<=72615] <- 8
site_id[pat_id>=76716&pat_id<=77140] <- 8
site_id[pat_id>=81209&pat_id<=81652] <- 8
site_id[pat_id>=85736&pat_id<=86186] <- 8
site_id[pat_id>=90202&pat_id<=90654] <- 8
site_id[pat_id>=94668&pat_id<=95135] <- 8
site_id[pat_id>=99216&pat_id<=99673] <- 8
site_id[pat_id>=103718&pat_id<=104167] <- 8
site_id[pat_id>=108245&pat_id<=108694] <- 8
site_id[pat_id>=112765&pat_id<=113219] <- 8
site_id[pat_id>=117335&pat_id<=117782] <- 8
site_id[pat_id>=121878&pat_id <=122349] <- 8
site_id[pat_id>=126420&pat_id <=126890]  <- 8
site_id[pat_id>=130935&pat_id <=131388] <- 8

SW  <- data.frame( site_id, quarter_id, pat_id, referral01)

View(SW)
str(SW)

# data check 

SW %>% 
 group_by(site_id, quarter_id) %>% 
 summarize(Freq=n()) %>% 
 print(n=224)

# Indices
attach(SW)

# Index of total count - grouped by site_id & quarter_id
Ntotal_siteqtr <- SW %>% 
 group_by(site_id, quarter_id) %>% 
 mutate(Ntotal_siteqtr=n())         

SW  <- data.frame( site_id, quarter_id, pat_id, referral01, Ntotal_siteqtr)

View(SW)

# index of running count - grouped by site_id & quarter_id
n_siteqtr  <- SW %>% 
 group_by(site_id, quarter_id) %>% 
 mutate(n_siteqtr=seq(n()))

SW  <- data.frame( site_id, quarter_id, pat_id, referral01, Ntotal_siteqtr, n_siteqtr)

# keep only the relevant variables 
SW  <- SW[-c(1:8)]

# renaming variables for easier manipulation
str(SW)

SW <- SW %>%
 rename("pat_id"="pat_id.2",
     "quarter_id"="quarter_id.2",
     "site_id"="site_id.2",
     "referral01"="referral01.2")


# arrange SW in specific order
SW %>% 
 select(site_id, quarter_id, pat_id, Ntotal_siteqtr, n_siteqtr) %>% 
 view()

# implementation (0/1)
impmain01[(site_id==1) & (quarter_id>=1 & quarter_id<=19)] <- 0
impmain01[(site_id==1) & (quarter_id>=20 & quarter_id<=28)] <- 1

impmain01[(site_id==2) & (quarter_id>=1 & quarter_id<=18)] <- 0
impmain01[(site_id==2) & (quarter_id>=19 & quarter_id<=28)] <- 1

impmain01[(site_id==3) & (quarter_id>=1 & quarter_id<=23)] <- 0
impmain01[(site_id==3) & (quarter_id>=24 & quarter_id<=28)] <- 1

impmain01[(site_id==4) & (quarter_id>=1 & quarter_id<=28)] <- 0

impmain01[(site_id==5) & (quarter_id>=1 & quarter_id<=22)] <- 0
impmain01[(site_id==5) & (quarter_id>=23 & quarter_id<=28)] <- 1

impmain01[(site_id==6) & (quarter_id>=1 & quarter_id<=10)] <- 0
impmain01[(site_id==6) & (quarter_id>=11 & quarter_id<=28)] <- 1

impmain01[(site_id==7) & (quarter_id>=1 & quarter_id<=19)] <- 0
impmain01[(site_id==7) & (quarter_id>=20 & quarter_id<=28)] <- 1

impmain01[(site_id==8) & (quarter_id>=1 & quarter_id<=10)] <- 0
impmain01[(site_id==8) & (quarter_id>=11 & quarter_id<=28)] <- 1

SW  <- data.frame(site_id, quarter_id, pat_id, Ntotal_siteqtr, n_siteqtr, referral01, impmain01)


# data checks: frequencies by group
SW %>% 
 group_by(site_id, quarter_id, impmain01) %>% 
 summarize(frequency=n()) %>% 
 print(n=224)

# quarterly time trend
quarter <- quarter_id

SW  <- data.frame(site_id, quarter_id, pat_id, Ntotal_siteqtr, n_siteqtr, referral01, impmain01, quarter)

# data check 
SW %>%
 select(quarter_id, quarter) %>% 
 arrange (quarter_id) %>%  
 view()

#site_num as predictor
# label define sitenum 8 "Greater Los Angeles" 7 "Loma Linda" 6 "Long Beach" 5"New Mexico" 
# 4 "Northern Arizona" 3 "Phoenix" 2 "San Diego" 1 "Tuscon"

site_num[site_id==8] <- 1
site_num[site_id==7] <- 2
site_num[site_id==6] <- 3
site_num[site_id==5] <- 4
site_num[site_id==4] <- 5
site_num[site_id==3] <- 6
site_num[site_id==2] <- 7
site_num[site_id==1] <- 8

SW  <- data.frame(site_id, quarter_id, pat_id, Ntotal_siteqtr, n_siteqtr, 
         referral01, impmain01, quarter, site_num)

# duration
duration[(site_id==1) & (quarter_id>=1 & quarter_id<=19)]  <- 0
duration[(site_id==1) & (quarter_id==20)]  <- 1
duration[(site_id==1) & (quarter_id==21)]  <- 2
duration[(site_id==1) & (quarter_id==22)]  <- 3
duration[(site_id==1) & (quarter_id==23)]  <- 4
duration[(site_id==1) & (quarter_id==24)]  <- 5
duration[(site_id==1) & (quarter_id==25)]  <- 6
duration[(site_id==1) & (quarter_id==26)]  <- 7
duration[(site_id==1) & (quarter_id==27)]  <- 8
duration[(site_id==1) & (quarter_id==28)]  <- 9

duration[(site_id==2) & (quarter_id>=1 & quarter_id<=18)]  <- 0
duration[(site_id==2) & (quarter_id==19)]  <- 1
duration[(site_id==2) & (quarter_id==20)]  <- 2
duration[(site_id==2) & (quarter_id==21)]  <- 3
duration[(site_id==2) & (quarter_id==22)]  <- 4
duration[(site_id==2) & (quarter_id==23)]  <- 5
duration[(site_id==2) & (quarter_id==24)]  <- 6
duration[(site_id==2) & (quarter_id==25)]  <- 7
duration[(site_id==2) & (quarter_id==26)]  <- 8
duration[(site_id==2) & (quarter_id==27)]  <- 9
duration[(site_id==2) & (quarter_id==28)]  <- 10

duration[(site_id==3) & (quarter_id>=1 & quarter_id<=23)]  <- 0
duration[(site_id==3) & (quarter_id==24)]  <- 1
duration[(site_id==3) & (quarter_id==25)]  <- 2
duration[(site_id==3) & (quarter_id==26)]  <- 3
duration[(site_id==3) & (quarter_id==27)]  <- 4
duration[(site_id==3) & (quarter_id==28)]  <- 5

duration[(site_id==4) & (quarter_id>=1 & quarter_id<=28)]  <- 0

duration[(site_id==5) & (quarter_id>=1 & quarter_id<=22)]  <- 0
duration[(site_id==5) & (quarter_id==23)]  <- 1
duration[(site_id==5) & (quarter_id==24)]  <- 2
duration[(site_id==5) & (quarter_id==25)]  <- 3
duration[(site_id==5) & (quarter_id==26)]  <- 4
duration[(site_id==5) & (quarter_id==27)]  <- 5
duration[(site_id==5) & (quarter_id==28)]  <- 6

duration[(site_id==6) & (quarter_id>=1 & quarter_id<=10)]  <- 0
duration[(site_id==6) & (quarter_id==11)]  <- 1
duration[(site_id==6) & (quarter_id==12)]  <- 2
duration[(site_id==6) & (quarter_id==13)]  <- 3
duration[(site_id==6) & (quarter_id==14)]  <- 4
duration[(site_id==6) & (quarter_id==15)]  <- 5
duration[(site_id==6) & (quarter_id==16)]  <- 6
duration[(site_id==6) & (quarter_id==17)]  <- 7
duration[(site_id==6) & (quarter_id==18)]  <- 8
duration[(site_id==6) & (quarter_id==19)]  <- 9
duration[(site_id==6) & (quarter_id==20)]  <- 10
duration[(site_id==6) & (quarter_id==21)]  <- 11
duration[(site_id==6) & (quarter_id==22)]  <- 12
duration[(site_id==6) & (quarter_id==23)]  <- 13
duration[(site_id==6) & (quarter_id==24)]  <- 14
duration[(site_id==6) & (quarter_id==25)]  <- 15
duration[(site_id==6) & (quarter_id==26)]  <- 16
duration[(site_id==6) & (quarter_id==27)]  <- 17
duration[(site_id==6) & (quarter_id==28)]  <- 18

duration[(site_id==7) & (quarter_id>=1 & quarter_id<=19)]  <- 0
duration[(site_id==7) & (quarter_id==20)]  <- 1
duration[(site_id==7) & (quarter_id==21)]  <- 2
duration[(site_id==7) & (quarter_id==22)]  <- 3
duration[(site_id==7) & (quarter_id==23)]  <- 4
duration[(site_id==7) & (quarter_id==24)]  <- 5
duration[(site_id==7) & (quarter_id==25)]  <- 6
duration[(site_id==7) & (quarter_id==26)]  <- 7
duration[(site_id==7) & (quarter_id==27)]  <- 8
duration[(site_id==7) & (quarter_id==28)]  <- 9

duration[(site_id==8) & (quarter_id>=1 & quarter_id<=10)]  <- 0
duration[(site_id==8) & (quarter_id==11)]  <- 1
duration[(site_id==8) & (quarter_id==12)]  <- 2
duration[(site_id==8) & (quarter_id==13)]  <- 3
duration[(site_id==8) & (quarter_id==14)]  <- 4
duration[(site_id==8) & (quarter_id==15)]  <- 5
duration[(site_id==8) & (quarter_id==16)]  <- 6
duration[(site_id==8) & (quarter_id==17)]  <- 7
duration[(site_id==8) & (quarter_id==18)]  <- 8
duration[(site_id==8) & (quarter_id==19)]  <- 9
duration[(site_id==8) & (quarter_id==20)]  <- 10
duration[(site_id==8) & (quarter_id==21)]  <- 11
duration[(site_id==8) & (quarter_id==22)]  <- 12
duration[(site_id==8) & (quarter_id==23)]  <- 13
duration[(site_id==8) & (quarter_id==24)]  <- 14
duration[(site_id==8) & (quarter_id==25)]  <- 15
duration[(site_id==8) & (quarter_id==26)]  <- 16
duration[(site_id==8) & (quarter_id==27)]  <- 17
duration[(site_id==8) & (quarter_id==28)]  <- 18


SW  <- data.frame(site_id, quarter_id, pat_id, Ntotal_siteqtr, n_siteqtr, 
         referral01, impmain01, quarter, site_num, duration)

# Adding labels to factor variables

SW$quarter_id  <- ordered(SW$quarter_id,
             levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28),
             labels = c("Q1FY16","Q2FY16","Q3FY16","Q4FY16","Q1FY17","Q2FY17","Q3FY17","Q4FY17","Q1FY18","Q2FY18","Q3FY18","Q4FY18",
                  "Q1FY19","Q2FY19","Q3FY19","Q4FY19","Q1FY20","Q2FY20","Q3FY20","Q4FY20","Q1FY21","Q2FY21","Q3FY21","Q4FY21",
                  "Q1FY22","Q2FY22","Q3FY22","Q4FY22"))
SW$site_id  <- factor(SW$site_id,
           levels = c(1,2,3,4,5,6,7,8),
           labels = c("Greater Los Angeles","Loma Linda", "Long Beach", "New Mexico", 
                 "Northern Arizona", "Phoenix", "San Diego", "Tuscon"))


SW$site_num <- factor(SW$site_num,
            levels = c(1,2,3,4,5,6,7,8),
            labels = c("Tuscon", "San Diego", "Phoenix", "Northern Arizona",
                 "New Mexico", "Long Beach", "Loma Linda", "Greater Los Angeles"))




# keep only the relevant variables 
SW  <- SW[c(site_id, quarter_id, pat_id, Ntotal_siteqtr,n_siteqtr , 
      referral01,impmain01, duration, quarter, site_num)]

SW  <- SW[c(1:3,7:8,13,15:18)]

write.csv(SW,"C:/Users/Alexis K Huynh/OneDrive/Desktop/R/Datasets/steppedwedge.csv", row.names = FALSE) 

