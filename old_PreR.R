# Loading -----------------------------------------------------------------
source("d:/_R/_Fnc/dsKim_library.R")
File = c('20210224_IBKbranch.csv'); ibk.bch <- read_csv(
  File, skip = 3, col_names = T,
  locale = locale(encoding = guess_encoding(File)[1,1] %>% as.character)
  ); ibk.bch.00 <- ibk.bch; ibk.bch.00

# ibk.bch %>% names #-----------------------------------------------------
# [1] "지역본부"        "지점명"          "지점장"          "개점일"         
# [5] "개인수신"        "기업수신"        "개인여신"        "기업여신"       
# [9] "총여수신"        "영업이익"        "영업이익_공통비" "인원"           
# [13] "자가여부"        "취득가"          "보증금"          "월세"           
# [17] "면적"            "층수"            "경평17위"        "경평17중"       
# [21] "경평18위"        "경평18중"        "경평19위"        "경평19중"       
# [25] "경평20위"        "경평20중"       
#--------------------------------------------------------------------------

# Pretreatments -----------------------------------------------------------

ibk.bch.01 <- ibk.bch.00 %>% 
  mutate(
    across(dplyr::everything(), str_replace_all, ',', ''), # Remove ','
    across(dplyr::everything(), str_replace_all, '\\s*\\([^\\)]+\\)', '') # Remove '(*)'
    ); ibk.bch.01

# Type Setting ------------------------------------------------------------
## Integer : 인원, 층수, 경평17위 ~ 경평20중 
## Double  : 개인수신, 기업수신, 개인여신, 기업여신, 총여수신, 
#            영업이익, 영업이익_공통비, 취득가, 보증금, 월세, 면적
## Factor  : 지역본부, 자가여부
## Date    : 개점일
#--------------------------------------------------------------------------

ibk.bch.02 <- ibk.bch.01 %>%
  mutate(
    across(
      c(인원, 층수, 경평17위:경평20중), as.integer
      ), 
    across(
      c(개인수신:영업이익_공통비, 취득가, 보증금:면적), as.double
      ),
    across(
      c(지역본부, 자가여부), as.factor
      ),
    across(
      개점일, as.Date
      ), 
    자가여부 = factor(
      자가여부, levels = c('자가', '임차', '혼합')
      ),
    지점구분 = ifelse(
      str_detect(지점명, '_출|_가'), '기타', '지점'
      ),
    지점구분 = factor(
      지점구분, levels = c('지점', '기타')
      )
    ) %>%
  rename(
    영업이익_차감전 = 영업이익_공통비, 
    FY17.rnk = 경평17위, FY17.tot = 경평17중, 
    FY18.rnk = 경평18위, FY18.tot = 경평18중, 
    FY19.rnk = 경평19위, FY19.tot = 경평19중, 
    FY20.rnk = 경평20위, FY20.tot = 경평20중); ibk.bch.02

# 출장소가변점포 제외, 경평순위 - 상위 % 베이스 ----------------------

ibk.bch.03 <- ibk.bch.02 %>%
  filter(지점구분 == '지점') %>% select(-지점구분) %>% 
  mutate(
    FY17 = (FY17.rnk / FY17.tot) %>% round(digits = 2),
    FY18 = (FY18.rnk / FY18.tot) %>% round(digits = 2),
    FY19 = (FY19.rnk / FY19.tot) %>% round(digits = 2),
    FY20 = (FY20.rnk / FY20.tot) %>% round(digits = 2)
  ) %>% 
  select(-(FY17.rnk:FY20.tot)); ibk.bch.03 %>% str

# Divide Two Sets ---------------------------------------------------------

## Full Set
ibk.bch <- ibk.bch.03; ibk.bch %>% names

## Compact Set
ibk.bch.cpt <- ibk.bch %>% 
  select(
    지역본부, 지점명, 지점장, 개인수신, 기업수신, 개인여신, 기업여신, 
    영업이익, 인원, FY20
    ); ibk.bch.cpt %>% names
  
# Saving ------------------------------------------------------------------

ibk.bch.txt <- c("Made from '_20210219_IBKbranches'")

ibk.bch.description <- c("
ibk.bch : 
    지역본부, 지점명, 지점장, 개점일, 
    개인수신, 기업수신, 개인여신, 기업여신, 총여수신, 
    영업이익, 영업이익_차감전, 인원, 
    자가여부, 취득가, 보증금, 월세, 면적, 층수,
    (경평상위%) FY17, FY18, FY19, FY20
ibk.bch.abb : 
    지역본부, 지점명, 지점장, 개인수신, 기업수신, 개인여신, 기업여신, 
    영업이익, 인원, FY20
                     ")

save(
  list = c('ibk.bch.txt', 'ibk.bch.description', 
           'ibk.bch', 'ibk.bch.cpt'), 
  file = 'd:/_R/_Data/ibk.bch.rda'
  )

paste(format(Sys.Date(), '%Y년 %m월 %d일 %A'), format(Sys.time(), '%H시 %M분'))
