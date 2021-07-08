
# Loading -----------------------------------------------------------------

source("d:/_R/_Fnc/dsKim_library.R")

load('d:/_R/_Data/ibk.bch.rda') 

cat(ibk.bch.txt); cat(ibk.bch.des); ibk.bch; ibk.bch.cpt

# ibk.bch %>% names #-----------------------------------------------------
# [1] "지역본부"        "지점명"          "지점장"          "개점일"         
# [5] "개인수신"        "기업수신"        "개인여신"        "기업여신"       
# [9] "총여수신"        "영업이익"        "영업이익_차감전" "인원"           
# [13] "자가여부"        "취득가"          "보증금"          "월세"           
# [17] "면적"            "층수"            "FY17"            "FY18"           
# [21] "FY19"            "FY20"         

# ibk.bch.cpt %>% names #-------------------------------------------------
# [1] "지역본부" "지점명"   "지점장"   "개인수신" "기업수신" "개인여신" "기업여신"
# [8] "영업이익" "인원"     "FY20" 
# ------------------------------------------------------------------------

# 영업이익 = which(개인수신, 기업수신, 개인여신, 기업여신)? --------------

dta.mdl <- # Modeling
  ibk.bch.cpt %>% 
  pivot_longer(cols = 개인수신:기업여신) %>% 
  select(지점명, 영업이익, name, value) %>% 
  group_by(name) %>% nest(.) %>% 
  mutate(
    mdl = data %>% map(~lm(영업이익 ~ value, data = .x)),
    prd = data %>% map2(mdl, modelr::add_predictions), 
    rsq = mdl %>% map(glance) %>% map_dbl('r.squared'),
    slp = mdl %>% map(tidy) %>% map('estimate') %>% map_dbl(~.x[2]),
    aug = mdl %>% map(augment)
    ); dta.mdl

dta.mdl %>% # Graphs
  select(name, prd) %>% unnest(prd) %>% 
  ggplot(aes(x = value, color = name)) +
  geom_point(aes(y = 영업이익)) + geom_line(aes(y = pred)) + 
  facet_wrap(~name, nrow = 2, scale = 'free') + 
  theme(legend.position = 'none') + 
  scale_x_continuous(labels = scales::comma)

dta <- dta.mdl %>% # Plot Corp. Loans
  select(name, prd) %>% filter(name == '기업여신') %>% unnest(prd); dta

dta %>% # Who's Who?
  ggplot(aes(x = value, y = 영업이익, label = 지점명)) + 
  geom_point(color = 'blue') + geom_line(aes(y = pred), color = 'red') + 
  geom_text_repel(
    data = dta %>% filter(영업이익 > 75), segment.color = 'blue'
  )

##################### OK Here #################################

# 지역본부별 : 기업여신 vs 영업이익 --------------------------------------

dta.mdl <- 
  ibk.bch.cpt %>% 
  select(지역본부, 지점명, 영업이익, 기업여신) %>% 
  group_by(지역본부) %>% nest(.) %>% #jsonedit(.)
  mutate(
    mdl = data %>% map(~lm(영업이익 ~ 기업여신, data = .x)),
    rsq = mdl %>% map(glance) %>% map_dbl('r.squared'),
    slp = mdl %>% map(tidy) %>% map('estimate') %>% map_dbl(~.x[2]),
    aug = mdl %>% map(augment)
  ) %>% arrange(rsq); dta.mdl

dta.mdl %>% 
  select(지역본부, aug) %>% 
  filter(지역본부 %in% c('서부', '경기남부', '강서제주', '중부')) %>% 
  unnest(aug) %>% 
  ggplot(aes(x = 기업여신, group = 지역본부, color = 지역본부)) +
  geom_point(aes(y = 영업이익)) + geom_line(aes(y = .fitted)) + 
  facet_wrap(~지역본부, nrow = 2, scale = 'free') + 
  theme(legend.position = 'none') + 
  scale_x_continuous(labels = scales::comma)


# 지역본부별 생산성 : 1인당영업이익 -------------------------------------

ibk.bch.cpt %>% 
  select(지역본부, 지점명, 영업이익, 인원, FY20) %>% 
  mutate(
    생산성 = 영업이익 / 인원,
    지본명 = fct_reorder(지역본부, 생산성, .fun = median)
    ) %>% 
  ggplot(aes(x = 지본명, y = 생산성)) + geom_boxplot()

ibk.bch.cpt %>% # Who's Who?
  filter(지역본부 %>% str_detect('호남'), 영업이익/인원 > 4)




################ OK Here ###################################






# FY20 1인당생산성 톱5 점포의 경평순위 변화 (FY17~FY20) ------------------

dTa.04 %>% 
  filter(공통비 == '포함') %>% 
  left_join(dTa.02, by = '지점명') %>% 
  select(지점명, 영업이익, 인원, 지점장) %>% 
  mutate(생산성 = 영업이익 / 인원) %>% 
  top_n(n = 5, wt = 생산성) %>% arrange(desc(생산성)) %>% # 1인당생산성 톱5
  left_join(dTa.05.01, by = '지점명') %>% # 경평순위 변화 
  ggplot(aes(x = 연도, y = 경평상위, color = 지점명)) + 
  geom_line(aes(group = 지점명)) + geom_point() + theme_bw() + 
  scale_y_continuous(trans = "reverse")


# Q) 지역본부 별 총인원 및 영업이익 합계

dTa.04 %>% 
  filter(공통비 == '포함') %>% 
  left_join(dTa.02, by = '지점명') %>% 
  select(지역본부, 지점명, 인원, 영업이익) %>% 
  group_by(지역본부) %>% 
  summarise(
    총원 = sum(인원),
    이익 = sum(영업이익)
  ) %>% # 1인당 영업이익, 생산성을 비교해야 공정하다.
  mutate(생산성 = 이익 / 총원) %>% arrange(desc(생산성)) %>% 
  ggplot(aes(x = fct_reorder(지역본부,생산성), y = 생산성)) + 
  geom_bar(stat = 'identity') + 
  coord_flip()

# Q) 개인여수신 톱5 점포와 기업여수신 톱5 점포의 영업이익 비교

### 1) 개인여수신 톱5 점포 영업이익

dTa.03 %>% 
  filter(고객 == '개인') %>% 
  spread(key = '거래', value = '계수') %>% 
  rowwise() %>% mutate(개인거래 = sum(c(수신, 여신))) %>% ungroup() %>% 
  select(지점명, 개인거래) %>% top_n(n=5, wt=개인거래) %>% arrange(desc(개인거래)) %>% 
  left_join(dTa.04, by = '지점명') %>% filter(공통비 == '포함') %>% select(-공통비)

### 2) 기업여수신 톱5 점포 영업이익

dTa.03 %>% 
  filter(고객 == '기업') %>% 
  spread(key = '거래', value = '계수') %>% 
  rowwise() %>% mutate(기업거래 = sum(c(수신, 여신))) %>% ungroup() %>% 
  select(지점명, 기업거래) %>% top_n(n=5, wt=기업거래) %>% arrange(desc(기업거래)) %>% 
  left_join(dTa.04, by = '지점명') %>% filter(공통비 == '포함') %>% select(-공통비)

# Q) 총여수신이 많으면 영업이익도 많은가?

dTa.03 %>% arrange(지점명, 고객, 거래)

dmy <- dTa.03 %>% 
  group_by(지점명) %>% 
  summarise(총여수신 = sum(계수, na.rm = T)) %>% 
  left_join(dTa.04, by = '지점명') %>%
  filter(공통비 == '제외') %>% 
  left_join(dTa.01, by = '지점명') %>% 
  select(지역본부, 지점명, 총여수신, 영업이익) %>% 
  filter(총여수신 < 20000) # 데이터 왜곡발생! 총여수신 2조원 미만 점포만 추리자.

dmy %>% 
  ggplot(aes(x = 총여수신, y = 영업이익)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F)
  
dmy %>% 
  lm(영업이익 ~ 총여수신, data = .) %>% 
  summary

### C) 총여수신 8조원? 어디니?
dmy %>% filter(총여수신 > 20000)

# Q) 지점을 고를 수 있다면....어느지점 가면 좋나? wrt 경평!

dTa.05.01 # 경평이 4년치다. 어떻게 스칼라로 만들까?

### 연도별 경평에 가중치를 두자 : FY20=4, 19=3, 18=2, 17=1

wgt <- tibble( # 4개년 경평자료를 '최근것 우선' 가중평균하자.
  연도 = c('FY20', 'FY19', 'FY18', 'FY17'),
  가중치 = c(0.4, 0.3, 0.2, 0.1)); wgt

dmy <- dTa.05.01 %>% # 가중된 경평을 '누적경평'으로 놓고 점수를 매기자.
  left_join(wgt, by = '연도') %>% 
  mutate(가중경평 = 경평상위 * 가중치) %>% 
  group_by(지점명) %>% 
  summarise(누적경평 = sum(가중경평)); dmy

dmy.low <- dmy %>% top_n(n=5, wt=누적경평) %>% mutate(구분 = 'Bad'); dmy.low # 꼴찌점포 5개 
dmy.top <- dmy %>% top_n(n=5, wt=-누적경평) %>% mutate(구분 = 'Good'); dmy.top # 1등점포 5개 

rbind(dmy.low, dmy.top) %>% # 꼴찌와 1등을 묶자.
  ggplot(aes(x = fct_reorder(지점명, 누적경평), y = 누적경평, fill = 구분)) + 
  geom_bar(stat = 'identity') + geom_point() + theme_bw() + 
  scale_y_continuous(trans = "reverse")
  
### C) 좀 심한데... 실제 지점의 경평등수 확인 

target <- dmy.top$지점명 

dTa.05.01 %>%
  left_join(dTa.01, by = '지점명') %>% 
  select(지역본부, 지점명, 지점장, 연도, 경평상위) %>% 
  filter(지점명 %in% target) %>% 
  ggplot(aes(x = 연도, y = 경평상위, group = 지점명, color = 지점명)) +
  geom_line() + geom_point() + scale_y_continuous(trans = "reverse")


