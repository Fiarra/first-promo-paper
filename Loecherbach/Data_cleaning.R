library(tidyverse)
library(formattable)
library(reshape2)
library(ggplot2)

raw_data = read.csv('Systematic Literature Review_March 14, 2019_15.00.csv')
data = raw_data %>% slice(3:n()) %>% select(18:2754)
data %>% mutate_if(is.factor, as.character) -> data

data$div = ifelse(grepl("Diversity", data$Q5), "yes", "no")
data$ser = ifelse(grepl("Serendipity", data$Q5), "yes", "no")
data$plu = ifelse(grepl("Pluralism", data$Q5), "yes", "no")
data$fil = ifelse(grepl("Filter Bubble", data$Q5), "yes", "no")
data$ech = ifelse(grepl("Echo Chamber", data$Q5), "yes", "no")
data$oth = ifelse((data$Q5_7_TEXT == "" | is.na(data$Q5_7_TEXT)), "no", "yes")
data$no = ifelse(grepl("No diversity-related concept", data$Q5), "yes", "no")

data = data %>%
  gather(key = 'concept', value = 'answer', 7:2736) %>%
  extract(concept, c("loop_concept", "question"), "(X.*)_(Q.*)") %>%
  spread(question, answer)

data = data %>% filter((div == 'yes' & loop_concept == 'X1') | (ser == 'yes' & loop_concept == 'X2') | (plu == 'yes' & loop_concept == 'X3') | (fil == 'yes' & loop_concept == 'X4') | (ech == 'yes' & loop_concept == 'X5') | (oth == 'yes' & loop_concept == 'X7') | (no == 'yes' & loop_concept == 'X9'))

subdata = data %>% select('div':'ech')
apply(subdata, 2, table)

no_concept = data %>% filter(no == 'yes')
data = data %>% filter(no == 'no')
data = data[(colSums(!is.na(data)) > 0) & (colSums(data != '') != 0)]

empirical_data = data %>% filter(Q2 == 'Yes')
empirical_data = empirical_data[(colSums(!is.na(empirical_data)) > 0) & (colSums(empirical_data != '') != 0)]
colnames(empirical_data)
not_media_related = empirical_data %>% filter(Q9== 'No')
empirical_data = empirical_data %>% filter(Q9== 'Yes')
empirical_data$Q5 <- ifelse((empirical_data$oth == "yes"), paste(empirical_data$Q5, empirical_data$Q5_7_TEXT, sep=", "), empirical_data$Q5)
empirical_data$Q10 <- ifelse((empirical_data$Q10 == "Other, namely (please fill in)"), empirical_data$Q10_6_TEXT, empirical_data$Q10)
empirical_data$Q11 <- ifelse((empirical_data$Q11 == "Other (fill in)"), empirical_data$Q11_3_TEXT, empirical_data$Q11)
empirical_data$Q111 <- ifelse((empirical_data$Q111 == "Other"), empirical_data$Q111_3_TEXT, empirical_data$Q111)
empirical_data$Q112 <- ifelse((empirical_data$Q112 == "Other (not mentioned or qualitative study, please fill in how the authors judge the influence)"), empirical_data$Q112_3_TEXT, empirical_data$Q112)
empirical_data$Q113 <- ifelse((empirical_data$Q113 == "Other (please fill in)"), empirical_data$Q113_4_TEXT, empirical_data$Q113)
empirical_data$Q114 <- ifelse((empirical_data$Q114 == "Other"), empirical_data$Q114_5_TEXT, empirical_data$Q114)
empirical_data$Q116 <- ifelse((empirical_data$Q116 == "Other"), empirical_data$Q116_3_TEXT, empirical_data$Q116)
empirical_data$Q117 <- ifelse((empirical_data$Q117 == "Other (not mentioned or qualitative study, please fill in how the authors judge the influence)"), empirical_data$Q117_3_TEXT, empirical_data$Q117)
empirical_data$Q118 <- ifelse((empirical_data$Q118 == "Other (please fill in)"), empirical_data$Q118_4_TEXT, empirical_data$Q118)
empirical_data$Q121 <- ifelse((empirical_data$Q121 == "Other"), empirical_data$Q121_3_TEXT, empirical_data$Q121)
empirical_data$Q122 <- ifelse((empirical_data$Q122 == "Other (not mentioned or qualitative study, please fill in how the authors judge the influence)"), empirical_data$Q122_3_TEXT, empirical_data$Q122)
empirical_data$Q123 <- ifelse((empirical_data$Q123 == "Other (please fill in)"), empirical_data$Q123_4_TEXT, empirical_data$Q118)
empirical_data$Q127 <- ifelse((empirical_data$Q127 == "Other (not mentioned or qualitative study, please fill in how the authors judge the influence)"), empirical_data$Q127_3_TEXT, empirical_data$Q127)
empirical_data$Q138 <- ifelse((empirical_data$Q138 == "Other (not mentioned or qualitative study, please fill in how the authors judge the influence)"), empirical_data$Q138_3_TEXT, empirical_data$Q138)
empirical_data$Q139 <- ifelse((empirical_data$Q139 == "Other (please fill in)"), empirical_data$Q139_4_TEXT, empirical_data$Q139)
empirical_data$Q17 <- ifelse((empirical_data$Q17 == "Other"), empirical_data$Q17_9_TEXT, empirical_data$Q17)
empirical_data$Q23 <- ifelse((empirical_data$Q23 == "Other, namely (please fill in)"), empirical_data$Q23_6_TEXT, empirical_data$Q23)
empirical_data$Q26 <- ifelse((empirical_data$Q26 == "Other, namely:"), empirical_data$Q26_7_TEXT, empirical_data$Q26)
empirical_data$Q30 <- empirical_data$Q30_1_TEXT
empirical_data$Q31 <- empirical_data$Q31_1_TEXT
empirical_data$Q32 <- empirical_data$Q32_1_TEXT
empirical_data$Q35 <- ifelse((empirical_data$Q35 == "Other, namely:"), empirical_data$Q35_7_TEXT, empirical_data$Q35)
empirical_data$Q38 <- ifelse((empirical_data$Q38 == "Other measure"), empirical_data$Q38_5_TEXT, empirical_data$Q38)
empirical_data$Q39 <- empirical_data$Q39_1_TEXT
empirical_data$Q40 <- empirical_data$Q40_1_TEXT
empirical_data$Q41 <- empirical_data$Q41_1_TEXT
empirical_data$Q44 <- ifelse((empirical_data$Q44 == "Other, namely:"), empirical_data$Q44_7_TEXT, empirical_data$Q44)
empirical_data$Q38 <- ifelse((empirical_data$Q38 == "Other measure"), empirical_data$Q38_5_TEXT, empirical_data$Q38)
empirical_data$Q48 <- empirical_data$Q48_1_TEXT
empirical_data$Q49 <- empirical_data$Q49_1_TEXT
empirical_data$Q52 <- ifelse((empirical_data$Q52 == "Other, namely (please fill in)"), empirical_data$Q52_6_TEXT, empirical_data$Q52)
empirical_data$Q55 <- ifelse((empirical_data$Q55 == "Other, namely:"), empirical_data$Q55_7_TEXT, empirical_data$Q55)
empirical_data$Q59 <- empirical_data$Q59_1_TEXT
empirical_data$Q60 <- empirical_data$Q60_1_TEXT
empirical_data$Q61 <- empirical_data$Q61_1_TEXT
empirical_data$Q64 <- ifelse((empirical_data$Q64 == "Other, namely:"), empirical_data$Q64_7_TEXT, empirical_data$Q64)
empirical_data$Q69 <- empirical_data$Q69_1_TEXT
empirical_data$Q70 <- empirical_data$Q70_1_TEXT
empirical_data$Q7 <- empirical_data$Q7_1_TEXT
empirical_data$Q77 <- empirical_data$Q77_1_TEXT
empirical_data$Q78 <- empirical_data$Q78_1_TEXT
empirical_data$Q81 <- ifelse((empirical_data$Q81 == "Other, namely (please fill in)"), empirical_data$Q81_6_TEXT, empirical_data$Q81)
empirical_data$Q84 <- ifelse((empirical_data$Q84 == "Other, namely:"), empirical_data$Q84_7_TEXT, empirical_data$Q84)
empirical_data$Q87 <- empirical_data$Q87_1_TEXT
empirical_data$Q88 <- empirical_data$Q88_1_TEXT
empirical_data$Q89 <- empirical_data$Q89_1_TEXT
empirical_data$Q98 <- empirical_data$Q98_1_TEXT
empirical_data = empirical_data %>% select(-Q98_1_TEXT, -Q89_1_TEXT, -Q88_1_TEXT, -Q87_1_TEXT, -Q84_7_TEXT, -Q81_6_TEXT, -Q78_1_TEXT,
                          -Q77_1_TEXT, -Q7_1_TEXT, -Q70_1_TEXT, -Q69_1_TEXT, -Q64_7_TEXT, -Q61_1_TEXT,
                          -Q60_1_TEXT,-Q59_1_TEXT,-Q55_7_TEXT, -Q52_6_TEXT, -Q49_1_TEXT, -Q48_1_TEXT, -Q44_7_TEXT,
                          -Q40_1_TEXT, -Q41_1_TEXT,-Q39_1_TEXT, -Q38_5_TEXT, -Q35_7_TEXT, -Q32_1_TEXT, -Q31_1_TEXT, -Q30_1_TEXT, -Q26_7_TEXT,
                          -Q23_6_TEXT, -Q17_9_TEXT, -Q139_4_TEXT,-Q138_3_TEXT, -Q127_3_TEXT, -Q122_3_TEXT,
                          -Q117_3_TEXT, -Q112_3_TEXT, -Q123_4_TEXT, -Q118_4_TEXT, -Q121_3_TEXT, -Q116_3_TEXT, 
                          -Q114_5_TEXT, -Q113_4_TEXT, -Q111_3_TEXT, -Q11_3_TEXT, -Q10_6_TEXT, -Q5_7_TEXT, -Q25_6_TEXT)


empirical_data = empirical_data %>% rename(stat_sig.1 = Q112, stat_sig.2 = Q117, stat_sig.3 = Q122, stat_sig.4 = Q127, stat_sig.5 = Q132)
empirical_data = empirical_data %>% rename(stat_test.1 = Q111, stat_test.2 = Q116, stat_test.3 = Q121, stat_test.4 = Q126, stat_test.5 = Q131)
empirical_data = empirical_data %>% rename(stat_posneg.1 = Q113, stat_posneg.2 = Q118, stat_posneg.3 = Q123, stat_posneg.4 = Q128, stat_posneg.5 = Q133)
empirical_data = empirical_data %>% rename(stat_group_sig_dep.1 = Q138, stat_group_sig_dep.2 = Q140, stat_group_sig_dep.3 = Q142, stat_group_sig_dep.4 = Q144, stat_group_sig_dep.5 = Q146)
empirical_data = empirical_data %>% rename(stat_posneg_dep.1 = Q139, stat_posneg_dep.2 = Q141, stat_posneg_dep.3 = Q143, stat_posneg_dep.4 = Q145)
empirical_data = empirical_data %>% rename(stat_group_sig.1 = Q114, stat_group_sig.2 = Q119)
empirical_data = empirical_data %>% rename(stat_ind.1 = Q110_1, stat_ind.2 = Q110_2, stat_ind.3 = Q110_3, stat_ind.4 = Q110_4, stat_ind.5 = Q110_5)
empirical_data = empirical_data %>% rename(stat_dep.1 = Q137_1, stat_dep.2 = Q137_2, stat_dep.3 = Q137_3, stat_dep.4 = Q137_4, stat_dep.5 = Q137_5)
empirical_data = empirical_data %>% mutate(stat_grouptext.1 = paste(Q115_1_TEXT, Q115_2_TEXT, Q115_3_TEXT, Q115_4_TEXT, Q115_5_TEXT, Q115_6_TEXT, Q115_7_TEXT, sep = ", "))
empirical_data = empirical_data %>% select(-Q115_1_TEXT, -Q115_2_TEXT, -Q115_3_TEXT, -Q115_4_TEXT, -Q115_5_TEXT, -Q115_6_TEXT, -Q115_7_TEXT, -Q115_1, -Q115_2, -Q115_3, -Q115_4, -Q115_5, -Q115_6, -Q115_7)
empirical_data = empirical_data %>% mutate(stat_grouptext.2 = paste(Q120_1_TEXT, Q120_2_TEXT, Q120_3_TEXT, Q120_4_TEXT, Q120_5_TEXT, Q120_6_TEXT, sep = ", "))
empirical_data = empirical_data %>% select(-Q120_1_TEXT, -Q120_2_TEXT, -Q120_3_TEXT, -Q120_4_TEXT, -Q120_5_TEXT, -Q120_6_TEXT, -Q120_1, -Q120_2, -Q120_3, -Q120_4, -Q120_5, -Q120_6, -Q120_7)

empirical_data = empirical_data %>%
  gather(key = 'concept', value = 'answer', starts_with('stat')) %>%
  extract(concept, c("question", "loop_stat_variable"), "(.*)\\.(.*)") %>%
  spread(question, answer)

empirical_data = empirical_data %>% 
  filter(((Q109 == 'No') & (Q136 == 'No') & (loop_stat_variable == '1')) | (stat_ind != '') | (stat_dep != '') )

viewpoint = "Related to viewpoints or perspectives \\(also includes: frames; whether it includes conservative or liberal positions\\)"
actor = "Related to actors or entities \\(e.g. the diversity of politicians mentioned in the media, diversity of organisations\\)"
topic = "Related to topics \\(e.g. sports or politics or economics\\)"
outlet= "Related to outlets\\/sources \\(e.g. number of different media sources\\)"
other = "Other, namely:"
columns = c('Q12', 'Q25', 'Q34', 'Q43', 'Q54', 'Q63', 'Q72', 'Q83', 'Q91')

empirical_data = empirical_data %>% mutate_at(vars(columns), funs(gsub(viewpoint,"viewpoint", .)))
empirical_data = empirical_data %>% mutate_at(vars(columns), funs(gsub(actor,"actor", .)))
empirical_data = empirical_data %>% mutate_at(vars(columns), funs(gsub(topic,"topic", .)))
empirical_data = empirical_data %>% mutate_at(vars(columns), funs(gsub(outlet,"outlet", .)))
empirical_data = empirical_data %>% mutate_at(vars(columns), funs(gsub(other,"other", .)))

empirical_data = empirical_data %>% separate(col = Q12, into = c('dim1', 'dim2', 'dim3'), sep = ',')
empirical_data = empirical_data %>% separate(col = Q29, into = c('first', 'second', 'third', 'fourth'), sep = ',')

empirical_data$first = ifelse((empirical_data$first == "Other measure"), empirical_data$Q29_5_TEXT, empirical_data$first)
empirical_data$first = ifelse((empirical_data$first == "Precision"), paste(empirical_data$first, empirical_data$Q29_1_TEXT, sep = ', '), empirical_data$first)
empirical_data$first = ifelse((empirical_data$first == "F1 Score"), paste(empirical_data$first, empirical_data$Q29_3_TEXT, sep = ', '), empirical_data$first)

empirical_data$second = ifelse((empirical_data$second == "Other measure"), empirical_data$Q29_5_TEXT, empirical_data$second)
empirical_data$second = ifelse((empirical_data$second == "Recall"), paste(empirical_data$first, empirical_data$Q29_2_TEXT, sep = ', '), empirical_data$second)

empirical_data$third = ifelse((empirical_data$third == "F1 Score"), paste(empirical_data$third, empirical_data$Q29_3_TEXT, sep = ', '), empirical_data$third)
empirical_data$third = ifelse((empirical_data$third == "Other measure"), empirical_data$Q29_5_TEXT, empirical_data$third)

empirical_data$fourth = ifelse((empirical_data$fourth == "Accuracy"), paste(empirical_data$fourth, empirical_data$Q29_4_TEXT, sep = ', '), empirical_data$fourth)

empirical_data = empirical_data %>% mutate_if(is_character, funs(na_if(.,"")))

empirical_data = empirical_data %>% mutate(dim_1.metr = paste(first, second, third, fourth, sep = "; "))
empirical_data = empirical_data %>% select(-first, -second, -third, -fourth, -Q29_5_TEXT, -Q29_2_TEXT, -Q29_3_TEXT, -Q29_4_TEXT, -Q29_1_TEXT)


empirical_data = empirical_data %>% rename_at(vars(c('Q12_1_TEXT', 'Q12_2_TEXT', 'Q12_3_TEXT', 'Q12_6_TEXT', 'Q12_9_TEXT')), 
                                           ~ c('actor_TEXT','viewpoint_TEXT', 'topic_TEXT', 'other_TEXT', 'outlet_TEXT'))

empirical_data = empirical_data %>% rename_at(vars(c('Q26', 'Q27', 'Q28', 'Q30', 'Q31', 'Q32', 'Q33')), 
                             ~ c('dim_1.method', 'dim_1.autom', 'dim_1.metr2', 'dim_1.measurecit', 'dim_1.math', 'dim_1.qual', 'dim_1.procedure'))
empirical_data = empirical_data %>% rename_at(vars(c('Q35', 'Q36', 'Q37', 'Q38', 'Q39', 'Q40', 'Q41', 'Q42')),
                             ~ c('dim_2.method', 'dim_2.autom', 'dim_2.metr', 'dim_2.metr2', 'dim_2.measurecit', 'dim_2.math', 'dim_2.qual', 'dim_2.procedure'))
empirical_data = empirical_data %>% rename_at(vars(c('Q44', 'Q45', 'Q46', 'Q48', 'Q49', 'Q50', 'Q51')),
                             ~ c('dim_3.method', 'dim_3.autom', 'dim_3.metr', 'dim_3.measurecit', 'dim_3.math', 'dim_3.qual', 'dim_3.procedure'))

empirical_data = empirical_data %>%
  gather(key = 'concept', value = 'answer', starts_with('dim_')) %>%
  extract(concept, c("dimension", "question"), "(.*)\\.(.*)") %>%
  spread(question, answer)

empirical_data = empirical_data %>% filter((dimension == 'dim_2' & dim2 != 'NA') | (dimension == 'dim_3' & dim3 != 'NA') | (dimension == 'dim_1'))
empirical_data$dimension = ifelse((empirical_data$dimension == "dim_1"), empirical_data$Q25, empirical_data$dimension)
empirical_data$dimension = ifelse((empirical_data$dimension == "dim_2"), empirical_data$Q34, empirical_data$dimension)
empirical_data$dimension = ifelse((empirical_data$dimension == "dim_3"), empirical_data$Q43, empirical_data$dimension)

empirical_data = empirical_data %>% select(-dim1, -dim2, -dim3, -Q25, -Q34, -Q43)

empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X1"), 'div', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X2"), 'ser', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X3"), 'plu', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X4"), 'fil', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X5"), 'ech', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X7"), 'oth', empirical_data$loop_concept)
empirical_data$loop_concept = ifelse((empirical_data$loop_concept == "X9"), 'no', empirical_data$loop_concept)

empirical_data = empirical_data %>% select(-div, -ser, -plu, -fil, -ech, -oth, -no, -Q5, -Q9)
colnames(empirical_data)
empirical_data = empirical_data %>% select('Q1':'Q136','Q17','Q14','Q15':'Q52','Q6','Q7', 'Q8', 'Q81', 'loop_stat_variable':'qual', 'Q148')

oldnames = c('Q1', 'Q2', 'Q3', 'Q4', 'Q6', 'Q7', 'Q8' ,'Q10', 'Q11', 'Q13', 'Q14', 'Q15', 'Q16', 'Q17', 'Q18',
             'Q19', 'Q20_5', 'Q20_6', 'Q21', 'Q22', 'Q23', 'Q24','Q52', 'Q81','Q109', 'Q136', 'Q148')
newnames = c('ID', 'empirical', 'n_studies', 'n_study', 'def_yn', 'defcit', 'def', 'level', 'perspective', 'ca_yn', 'n_coders', 'n_coders2', 'rel_yn', 
             'rel_meth', 'rel_criterion', 'rel_morethanone', 'sample_size', 'rel_size', 'rel_n_coders', 'rel_n_coders2', 'med1', 'measured_yn', 'med2', 'med3', 'dep_yn', 'ind_yn', 'comment')
empirical_data = empirical_data %>% rename_at(vars(oldnames), ~ newnames)
colnames(empirical_data)

media_gen = "'The news' or 'the media' in general without any further specifications"
media_1sec = "More than one media outlet \\(all in one sector\\)"
media_2sec = "More than one media outlet \\(in different sectors; this also includes aggregating platforms such as Google News\\)"
media_out = "One media outlet \\(i.e. one newspaper, one TV channel, one social media platform\\)"
media_mark = 'The media market overall \\(e.g. not related to specific media outlets\\)'
media_art = 'One media article \\(i.e. one newspaper article\\)'

empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_gen,"media_gen", .)))
empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_1sec,"media_1sec", .)))
empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_2sec,"media_2sec", .)))
empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_out,"media_out", .)))
empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_mark,"media_mark", .)))
empirical_data = empirical_data %>% mutate_at('level', funs(gsub(media_art,"media_art", .)))

exposure = 'Exposure perspective \\(i.e. centered on the \\$\\{lm\\:\\/\\/Field\\/1\\} consumed by the audience, i.e. what they read, view, click on; more focussed on what is displayed to the individual\\)'
supply = 'Supply perspective  \\(i.e. centered on the \\$\\{lm\\:\\/\\/Field\\/1\\} provided by media outlets, platforms or aggregators to the general public\\)'


empirical_data = empirical_data %>% mutate_at('perspective', funs(gsub(exposure,"exposure", .)))
empirical_data = empirical_data %>% mutate_at('perspective', funs(gsub(supply,"supply", .)))

citations = empirical_data %>% group_by(ID, loop_concept) %>% summarize(defcit = first(defcit), def_yn = first(def_yn))

div_def = citations %>% filter(loop_concept == 'div')
fil_def = citations %>% filter(loop_concept == 'fil')
ech_def = citations %>% filter(loop_concept == 'ech')
ser_def = citations %>% filter(loop_concept == 'ser')
plu_def = citations %>% filter(loop_concept == 'plu')
oth_def = citations %>% filter(loop_concept == 'oth')

div_def %>% summarise(div = paste(defcit, collapse = ';'))

fil_def

#How many do give a definition?
#67 give div definition (of 125, 53.6%), 9 give filter bubble definition (of 10, 90%), 11 give echo chamber definition (of 12, 91.6%), 2 give serendip definition (of 2, 100%), 14 give pluralism definition (of 18, 77.8%)
table(div_def$def_yn)
table(fil_def$def_yn)
table(ech_def$def_yn)
table(ser_def$def_yn)
table(plu_def$def_yn)

#How many (do not) use a citation?
#20 of 67 go not give div citation (29.8%), all give citation for filter bubble, 3 of 11 do not give echo chamber citation (27%), 1 does not give serendip citation (50%), 4 of 14 do not give pluralism citation (28.6%)
def_given = div_def %>% filter(def_yn == 'Yes')
def_given_fil = fil_def %>% filter(def_yn == 'Yes')
def_given_ech = ech_def %>% filter(def_yn == 'Yes')
def_given_ser = ser_def %>% filter(def_yn == 'Yes')
def_given_plu = plu_def %>% filter(def_yn == 'Yes')
sum(is.na(def_given))
sum(is.na(def_given_fil))
sum(is.na(def_given_ech))
sum(is.na(def_given_ser))
sum(is.na(def_given_plu))

level_perspective = empirical_data %>% group_by(ID, loop_concept) %>% summarize(level = first(level), perspective = first(perspective))
#
div_lev = level_perspective %>% filter(loop_concept == 'div')
fil_lev = level_perspective %>% filter(loop_concept == 'fil')
ech_lev = level_perspective %>% filter(loop_concept == 'ech')
ser_lev = level_perspective %>% filter(loop_concept == 'ser')
plu_lev = level_perspective %>% filter(loop_concept == 'plu')
oth_lev = level_perspective %>% filter(loop_concept == 'oth')
 
#Overall levels (172, multiple possible)
#media in one sector (56, 32.6%), media outlet (28, 16.3%), media in multiple sectors (28, 16.3%), media market (22, 12.7%), media general (12, 6.9%), article level (3, 1.7%), other
sort(table(level_perspective$level))
length(level_perspective$level)
#Diversity levels
#Several media outlets in one sector (47 of 125, 37.6%), media outlets in more than one sector (22, 17.5%), single media outlets (21, 16.8%), media 
#market (12, 9.6%), media in general (9, 7.2%), media article (5, 4%), three articles address more than one level; 9 have something else (usually: recommender system or individual browsing history)
#So in total market, media outlets in multiple or one sector are 81 (64.8%) while single media outlets, article level are 24 (19.2%), 72:26 if you count the media in general as external and the something else as internal
length(div_lev$level)
sort(table(div_lev$level))
#Filter Bubble: 3 out of 10 look at single outlet, 2 at multiple in multiple sectors, 2 at multiple in one sector, 1 at media in general and 2 at search results
#so it would be 50:50 with media in general as external and search results as internal (are only in one outlet)
length(fil_lev$level)
sort(table(fil_lev$level))

#Echo chamber: 7 in outlets, 2 in more than one sector, one media in general, one in one sector, one browsing history
#so it would be 66 (internal):33 (external) if browsing history internal and media in general external
length(ech_lev$level)
sort(table(ech_lev$level))

#Serendipity: Media in one sector and online news in general
sort(table(ser_lev$level))
#Pluralism: 9 about the media market, 3 in one sector, 2 in two sectors, two in both one and two sectors, 1 in media general, one internal
#Apart from one that specified that they are looking at internal everything is external
sort(table(plu_lev$level))
#Other: Everything external (2 one sector, 1 multiple sectors, media market)
sort(table(oth_lev$level))

#Overall (taking into account one article can appear multiple times, n = 172)
#116 supply, 46 exposure, 2 both, 7 not specified (mostly about perceived diversity)
table(level_perspective$perspective)
length(level_perspective$perspective)
#Div perspective: 92 of 125 look at supply (73.6%), 25 of 125 look at exposure (20%), 2 look at both, 5 are not specified (perceived diversity and links shared on Twitter)
table(div_lev$perspective)
#Filter bubble perspective: 9 exposure, 1 supply
table(fil_lev$perspective)
#echo chamber: 9 exposure, 2 supply, 1 not specified
table(ech_lev$perspective)
#Serendipity: 2 exposure
table(ser_lev$perspective)
#Pluralism: 17 supply, 1 exposure
table(plu_lev$perspective)
#Other: 4 supply, 1 perceived
table(oth_lev$perspective)


#empirical_data %>% group_by(Q1) %>% summarise(count = n())
#no_concept %>% group_by(Q1) %>% summarise(count = n())
#not_media_related %>% group_by(Q1) %>% summarise(count = n())
#data %>% group_by(Q1) %>% summarise(count = n())
#raw_data %>% group_by(Q1) %>% summarise(count = n())
#data %>% group_by(Q1) %>% summarise(Q9 = paste(Q9, collapse=','))
measured = empirical_data %>% filter(measured_yn == 'Yes')

div_def %>% select(ID, defcit)
div_def$ID
div_def %>% filter(ID == '000532@2018.nl.pdf')
test = div_def$defcit
test <- test[!is.na(test)]
test = strsplit(test, "\\\n")
test = unlist(test)
test = strsplit(test, ";")
test = unlist(test)
test <- test[test != " "]
test[23] = paste(test[23], test[24], sep = ' ')
test[40] = paste(test[40], test[41], sep = ' ')
test[42] = paste(test[42], test[43], sep = ' ')
test[44] = paste(test[44], test[45], test[46], sep = ' ')
test[51] = paste(test[51], test[52], sep = ' ')
test[80] = paste(test[80], test[81], sep = ' ')
test[82] = paste(test[82], test[83], sep = ' ')
test = test[-c(24, 41, 43, 45, 46, 52, 81, 83)]
test[28] = "Baden and Springer 2015; Voakes et al. 1996"
test[67] = 'Napoli (2009); Napoli (2011)'
test = strsplit(test, ";")
test = unlist(test)
test

doi = c('https://doi.org/10.1177/001654928303100301', 
        'https://doi.org/10.1177/1464884915605028', 
        'https://doi.org/10.1016/j.poetic.2009.09.002', 
        'https://doi.org/10.1177/107769909607300306', 
        'https://doi.org/10.1080/1461670x.2012.748516', 
        'https://doi.org/10.1016/j.chb.2014.05.028', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'Van Cuilenburg, J. (2007). Media diversity, competition and concentration: Concepts and theories. Media between Culture and Commerce, 4, 25-54.', 
        'https://doi.org/10.1017/cbo9781139034326', 
        'https://doi.org/10.1177/0267323114545712', 
        'https://doi.org/10.2139/ssrn.1121229', 
        'Pritchard, D. (2002). Viewpoint diversity in cross-owned newspapers and television stations: A study of news coverage of the 2000 presidential campaign. FCC Media Ownership Working Group Working Paper.',
        'https://doi.org/10.1080/1369118x.2016.1271900', 'Sørensen, J. K., & Schmidt, J. H. (2016). An Algorithmic Diversity Diet?: Questioning Assumptions behind a Diversity Recommendation System for PSM.',
        'https://doi.org/10.1515/commun-2017-0017', 
        'https://doi.org/10.1057/9781137304308_8', 
        'https://doi.org/10.1177/14614449922225555', 
        'https://doi.org/10.5422/fordham/9780823245123.001.0001', 
        'https://doi.org/10.1057/9781137304308_17', 
        'Pariser, E. (2011). The filter bubble: What the Internet is hiding from you. Penguin UK.',
        'https://doi.org/10.1016/j.infoecopol.2013.10.002',
        'https://doi.org/10.1146/annurev.soc.27.1.307', 
        'https://doi.org/10.1093/oxfordhb/9780199545636.003.0002', 
        'Van Dijk, J. A. G. M. "The evolution of the digital divide: The digital divide turns to inequality of skills and usage." Digital enlightenment yearbook 2012 (2012): 57-75.',
        'Hindman, M. (2008). The myth of digital democracy. Princeton University Press.', 
        'https://doi.org/10.5325/jinfopoli.1.2011.0246', 
        'https://doi.org/10.1017/cbo9780511790867', 
        'https://doi.org/10.4324/9781410614407', 
        'https://doi.org/10.1177/1464884915605028',
        'https://doi.org/10.1177/1464884910388228',
        'https://doi.org/10.1177/107769909607300306',
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x', 
        'Wisdom Fund. (1997, July 25). Broadcasting fairness doctrine promised balanced coverage. Retrieved from http://www.twf.org/News/Y1997/Fairness.html', 
        'https://doi.org/10.5325/jinfopoli.1.2011.0246',
        'Barron, J. A. (1999). Structural regulation of the media and the diversity rationale. Fed. Comm. LJ, 52, 555.', 
        'https://doi.org/10.1207/s15327736me1702_5', 
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'Kwoka Jr, J. E. (1985). The Herfindahl index in theory and practice. Antitrust Bull., 30, 915.', 
        'Dimmick, J. W. (2002). Media competition and coexistence: The theory of the niche. Routledge.',
        'Hill, B. C. (2006). Measuring media market diversity: Concentration, importance, and pluralism. Fed. Comm. LJ, 58, 169.',
        'Noam, E. (2004). How to measure media concentration. Financial Times.', 
        'https://doi.org/10.1177/14614449922225555', 
        'Griswold, W. F. (1999). Shaping the news mirror: Community structure, reporter specialization and content diversity. Mass media, social control, and social change: A macrosocial perspective, 183-196.',
        'https://doi.org/10.1177/107769909607300306', 
        'https://doi.org/10.1177/0267323105058253', 
        'https://doi.org/10.1016/j.poetic.2009.09.002',
        'https://doi.org/10.1287/mnsc.32.5.554',
        'https://doi.org/10.1016/j.compedu.2010.03.005',
        'https://doi.org/10.1177/107769907705400304',
        'https://doi.org/10.1080/23808985.2007.11679073', 
        'https://doi.org/10.1177/0093650210384985', 
        'https://doi.org/10.1093/ijpor/15.1.44', 
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.', 
        'https://doi.org/10.1093/poq/nfr052', 
        'https://doi.org/10.1177/107769909607300306', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
        'https://doi.org/10.1093/poq/nfr052',
        'https://doi.org/10.1207/s15327736me1404_2', 
        'https://doi.org/10.5422/fordham/9780823245123.001.0001',
        'Alia, V. (2005). Media and ethnic minorities. Edinburgh University Press.',
        'https://doi.org/10.1177/1464884906065512',
        'https://doi.org/10.1080/13691830500058943',
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x',
        'https://doi.org/10.1177/107769909907600111', 
        'Van Cuilenburg, J. (2005). On monitoring media diversity, media profusion, and media performance: Some regulator\'s notes. Communications, 30(3), 301-308.',
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
        'Van Cuilenburg, J. (2000). On measuring media competition and media diversity: Concepts, theories and methods. Measuring media content, quality and diversity. Approaches and issues in content research, 51-84.',
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x', 
        'https://doi.org/10.1177/107769909607300306', 
        'https://doi.org/10.1080/07393180701560864',
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
        'https://doi.org/10.5325/jinfopoli.1.2011.0246', 
        'https://doi.org/10.1145/290941.291025',
        'https://doi.org/10.1145/1390334.1390446', 
        'Drosou, M., & Pitoura, E. (2009). Diversity over Continuous Data. IEEE Data Eng. Bull., 32(4), 49-56.',
        'https://doi.org/10.1145/1060745.1060754', 
        'https://doi.org/10.5406/j.ctv1nhr0v', 
        'McQuail, D. (1983). Mass communication theory. Beverly Hills.', 
        'Tuchman, Gaye. 1978. Making News. A Study in the Construction of Reality. New York: Free Press.', 
        'https://doi.org/10.1111/j.1460-2466.1985.tb02451.x', 
        'https://doi.org/10.1145/1454008.1454030', 
        'https://doi.org/10.1177/0267323103018002002', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'Karppinen, K. (2009). Rethinking media pluralism and communicative abundance. Observatorio (OBS*), 3(4).', 
        'https://doi.org/10.1177/001654928303100301',
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x', 
        'https://www.fcc.gov/document/federal-communications-commission-biennial-regulatory-review-2002-consumer',
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'https://doi.org/10.1080/08838158709386645', 
        'https://doi.org/10.1016/j.poetic.2009.09.002', 
        'https://doi.org/10.1177/107769909607300306',
        'Druckman, J. N. (2009). Competing frames in a political campaign. In Winning with words (pp. 113-132). Routledge.', 
        'https://doi.org/10.1111/j.1460-2466.1985.tb02454.x', 
        'Donati PR (1992) Political discourse analysis. In: Diani M and Eyerman R (eds) Studying Collective Action. London: SAGE, pp. 136–167.',
        'Wetherell, M., & Potter, J. (1988). Discourse analysis and the identification of interpretative repertoires. Analysing everyday explanation: A casebook of methods, 1688183.',
        'https://doi.org/10.1111/j.1460-2466.2008.00384.x', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
        'https://doi.org/10.1177/0267323101016002003',
        'https://doi.org/10.1207/s15327736me1404_2', 
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.', 
        'Van Cuilenburg, J. (2002, December). The media diversity concept and European perspectives. In Media Economics, Content and Diversity Seminar, Finnish Academy of Sciences. Helsinki.', 
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
        'https://doi.org/10.1080/23808985.2007.11679073',
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.', 
        'https://doi.org/10.1207/s15327736me1004_4',
        'https://doi.org/10.1080/23808985.2007.11679073', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
        'https://doi.org/10.1177/001654928303100301',
        'Public Broadcasting Service. (2005). PBS editorial standards & policies. Retrieved from http:// www.pbs.org/aboutpbs/aboutpbs_standards.html', 
        'https://doi.org/10.1007/s10824-007-9037-8', 
        'https://doi.org/10.1177/107769909607300306',
        'Project for Excellence in Journalism (1999) ‘What Works, What Flops, and Why: What is a “Good” Newscast. Local TV News Project 1998’, 1 March, URL (consulted 15 March 2007): http://www.journalism.org/node/377',
        'https://doi.org/10.1177/0093650202239026', 
        'https://doi.org/10.1038/163688a0', 
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
        'https://doi.org/10.1080/23808985.2007.11679073', 
        'https://doi.org/10.1207/s15327736me1404_2',
        'https://doi.org/10.1177/0267323101016002003', 
        'https://doi.org/10.1080/08838158709386645',
        'https://doi.org/10.1177/009365088015001002', 
        'https://doi.org/10.1093/ijpor/7.4.353',
        'https://doi.org/10.1177/107769907705400304',
        'https://doi.org/10.1177/107769907705400304',
        'https://doi.org/10.4135/9781446262467', 
        'https://doi.org/10.1177/1527476402250683',
        'Steemers, J. (2001) ‘Onlineaktivitäten der BBC. Gratwanderung zwischen Public-Service-Verpflichtungen und kommerziellen Zielen’ (BBC online activities: Between public service committments and commercial objectives), Media Perspektiven 3(March): 126–32.',
        'https://doi.org/10.1207/s15327736me1702_5',
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
        'https://doi.org/10.1207/s15327736me1004_4',
        'Bourdieu, P. (1998). On television. New York: The New Press.',
        'Shoemaker, P. J., & Reese, S. D. (1996). Mediating the message (pp. 781-795). White Plains, NY: Longman.',
        'https://doi.org/10.1007/978-1-349-16939-9', 
        'Bagdikian, B. H. (2000). The media monopoly. Boston, MA: Beacon Press.', 
        'https://doi.org/10.1017/cbo9780511613227', 
        'https://doi.org/10.1207/s15327736me1702_5',
        'https://doi.org/10.1177/0267323101016002003', 
        'Ishikawa, S. (1994). Diversity in television programming. Studies of Broadcasting, 30, 155-170.',
        'https://doi.org/10.1177/107769909807500410',
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
        'https://doi.org/10.1080/08838158709386645', 
        'https://doi.org/10.1177/107769909607300306',
        'https://doi.org/10.1177/004912418000800305', 
        'https://doi.org/10.1177/0093650202239026', 
        'https://doi.org/10.1111/j.1467-9450.1994.tb00929.x', 
        'Pielou, Evelyn C. 1975. Ecological Diversity. New York: Wiley.', 
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'Freedman, D. (2008). The politics of media policy. Polity.', 
        'Doyle G (2002) Media Ownership. The Economics and Politics of Convergence and Concentration in the UK and European Media. London: Sage.', 
        'Dahl, R. A. (1971). Polyarchy. New Haven and London.',
        'https://doi.org/10.1177/0093650202239026',
        'https://doi.org/10.1207/s15327736me1702_3', 
        'Chem, C., et al. (2003). New journalism. Taipei, Taiwan: Wu-Lam Co.', 
        'Johnson, T. J., & Wanta, W. (1993). Newspaper competition and message diversity in an urban market. Mass Communication Review, 20(3-4), 136-147.',
        'https://doi.org/10.1177/107769908706400201', 
        'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.', 
        'https://doi.org/10.1177/107769909607300306',
        'https://www.fcc.gov/document/federal-communications-commission-biennial-regulatory-review-2002-consumer',
        'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
        'Sunstein, C. R. (1993). Democracy and the problem of free speech. New York The Free Press.',
        'https://doi.org/10.1111/j.1460-2466.1992.tb00765.x', 
        'Rogovin, W. M. (1992). The Regulation of Televison in the Public Interest: On Creating a Parallel Universe in Which Minorities Speak and Are Heard. Cath. UL Rev., 42, 51.', 
        'https://doi.org/10.1177/107769909607300306', 
        'https://doi.org/10.1111/j.1460-2466.1978.tb01600.x',
        'https://doi.org/10.1177/107769907705400304',
        'Culbertson, H. M. (1987). American and Filipino Editorials on the 1986 Philippine Election and Revolution. Philippine Communication Journal, 1, 1-14.')
doi

#https://doi.org/10.1177/1464884915605028
#https://doi.org/10.1177/0093650202239026
#https://doi.org/10.1177/0267323101016002003

df <- melt(data.frame(test, doi))
df
sort(table(doi))
length(table(doi))
sum(sapply(gregexpr("Napoli", test, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Voakes", test, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("McQuail", test, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Cuilenburg", test, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Benson", test, fixed = TRUE), function(x) sum(x > -1)))

#For definitions diversity:
#Napoli (1999) cited 11 times, Voakes et al (1996) 10 times, McQuail (1992) 8 times, Entman & Wildman (1992) 6 times, Chaffee & Wilson (1977)/Hellman(2001)/Roessler(2007) cited 4 times
#In total 105 different sources (in 49 articles that named sources for the definitions they used); 23% cited Napoli, 21% cited Voakes, 17% cited McQuail
#Looking at persons: Napoli cited 18 times, Voakes 12 times, McQuail 14 times, van Cuilenburg 11 times
length(table(doi))
table(!is.na(div_def$defcit))

#For definitions filter bubble:
#All cite Pariser (2011), in two articles additionally Sunstein (2006, 2017, 2001) and Negroponte (1996)
#For definitions echo chamber: 
#All but one cite Sunstein (although in different years, 3x2001, 2002, 2006, 2x2009, 2017); other sources: Jamieson & Cappella, 2008; Morozov, 2011; Pariser, 2011; Shirky, 2003;Adamic and Glance, 2005; Boutet et al., 2013

#For definitions pluralism: 
#Most cite Halin and Manchini (2004, 5x), otherwise no clear pattern (all only mentioned once)

#For definitions serendipity: 
#Only one article uses citation: Merton & Barber, 2004; Makri & Blandford, 2012
measure_citations = measured %>% group_by(ID, loop_concept) %>% summarize(measurecit = first(measurecit), 
                                                                          method = first(method), autom = first(autom), 
                                                                          procedure = first(procedure), metrics = first(metr),
                                                                          reliability = first(rel_yn), rel_criterion = first(rel_criterion),
                                                                          content_analysis = first(ca_yn),
                                                                          rel_method = first(rel_meth), sample_size = first(sample_size), rel_size = first(rel_size),
                                                                          rel_n_coders = first(rel_n_coders), rel_n_coders2 = first(rel_n_coders2),
                                                                          rel_morethanone = first(rel_morethanone), n_coders = first(n_coders), rel_n_coders = first(rel_n_coders), 
                                                                          math = first(math))
measures = measured %>% group_by(ID, loop_concept) %>% summarize(measurecit = first(measurecit), 
                                                                          method = first(method), autom = first(autom), 
                                                                          procedure = first(procedure),math = first(math))
measures = arrange(measures, by=loop_concept)
write.csv(measures, 'measures.csv')
div_def_m = measure_citations %>% filter(loop_concept == 'div')
fil_def_m = measure_citations %>% filter(loop_concept == 'fil')
ech_def_m = measure_citations %>% filter(loop_concept == 'ech')
ser_def_m = measure_citations %>% filter(loop_concept == 'ser')
plu_def_m = measure_citations %>% filter(loop_concept == 'plu')
oth_def_m = measure_citations %>% filter(loop_concept == 'oth')

#How many give a citation for the measurement?
#Diversity: 40 of 110 (49.4%), 2 of 8 for filter bubble (25%), 3 of 8 for echo chambers (43%), 0 of 1 for serendipity, 4 of 15 for pluralism (23.1%)
sum(!is.na(div_def_m$measurecit))
sum(!is.na(fil_def_m$measurecit))
sum(!is.na(ech_def_m$measurecit))
sum(!is.na(ser_def_m$measurecit))
sum(!is.na(plu_def_m$measurecit))

as.data.frame(div_def_m)

test_m = div_def_m$measurecit
test_m <- test_m[!is.na(test_m)]
test_m = strsplit(test_m, "\\\n")
test_m = unlist(test_m)
test_m = strsplit(test_m, ";")
test_m = unlist(test_m)
test_m[1] = 'Simpson (1949)'
test_m[2] = 'Shannon and Weaver (1949); Jennings et al (2011); John and Jennings (2010); Jones and Baumgartner (2005)'
test_m = test_m = strsplit(test_m, ";")
test_m = unlist(test_m)
test_m = test_m[-14]

doi_m = c('https://doi.org/10.1038/163688a0',
          'Shannon, C.E. & Weaver, W. (1949) The Mathematical Theory of Communication. University of Illinois Press, Urbana.',
          'https://doi.org/10.1177/0010414011405165',
          'https://doi.org/10.1017/s0007123409990068', 
          'Jones, B. D., & Baumgartner, F. R. (2005). The politics of attention: How government prioritizes problems. University of Chicago Press.',
          'https://doi.org/10.1093/qje/qjr044',
          'https://doi.org/10.1126/science.aaa1160',
          'https://doi.org/10.1093/poq/nfw006',
          'Dworak, Bryan, John Lovett, and Frank R. Baumgartner. 2014. “The Diversity of Internet Media: Utopia or Dystopia?” In Midwest political science association, Chicago, April 3-6.',
          'https://doi.org/10.1177/0267323101016002003',
          'https://doi.org/10.1080/01402382.2013.799312',
          'https://doi.org/10.1177/0267323104042911',
          'https://doi.org/10.1016/j.tpb.2006.06.003',
          'George, Lisa M., and Felix Oberholzer Gee. 2011. “Diversity in Local Television News.” US Federal Communications Commission Quadrennial Review, Washington, DC, July. Accessed 3 December 2017. http://www. fcc. gov/encyclopedia/2010-media-ownership-studies.',
          'https://doi.org/10.3115/974557.974585',
          'https://doi.org/10.1111/jcom.12141',
          'https://doi.org/10.1177/0267323114545712',
          'Salgado S, Nienstedt H-W and Schneider L (2015) Consensus or discussion? An analysis of plurality and consonance in coverage. In: Picard RG (ed.) The Euro Crisis in the Media: Journalistic Coverage of Economic Crisis and European Institutions. London; New York: I.B. Tauris, pp. 103–124.',
          'https://doi.org/10.1038/163688a0',
          'https://doi.org/10.2307/2095977',
          'https://doi.org/10.2307/270810',
          'https://doi.org/10.1177/0093650202239026',
          'https://doi.org/10.3115/v1/p14-5010',
          'https://doi.org/10.1177/073953290602700302',
          'https://doi.org/10.1080/1461670052000328212',
          'Gans HJ (1979) Deciding What’s News: A Study of CBS Evening News, NBC Nightly News, Newsweek and Time. New York, NY: Vintage Books.',
          'https://doi.org/10.1177/107769909607300306',
          'https://doi.org/10.1007/s11109-013-9222-8',
          'https://doi.org/10.1007/978-3-642-10871-6_7',
          'Balahur, A.; Steinberger, R.; Kabadjov, M. A.; Zavarella, V.; Van Der Goot, E.; Halkia, M.; Pouliquen, B.; and Belyaeva, J. 2010. Senti- ment Analysis in the News. 
          In Proceedings of the International Conference on Language Resources and Evaluation (LREC 2010). Paris: European Language Resources Association.',
          'Blei, D. M., Ng, A. Y., & Jordan, M. I. (2003). Latent dirichlet allocation. Journal of machine Learning research, 3(Jan), 993-1022.',
          'Xuan-Hieu Phan and Cam-Tu Nguyen. 2007. GibbsLDA++: A C/C++ Implementation of Latent Dirichlet Allocation (LDA). Retrieved from http://gibbslda.sourceforge.net/.',
          'Altheide DL (1996) Qualitative Media Analysis. Thousand Oaks, CA: Sage.',
          'https://doi.org/10.1145/2487575.2487672',
          'https://doi.org/10.1177/0267323112449097',
          'https://doi.org/10.1145/1454008.1454030',
          'https://doi.org/10.1177/1748048512439812',
          'https://doi.org/10.1111/j.1460-2466.2011.01616.x',
          'Shannon, C.E. & Weaver, W. (1949) The Mathematical Theory of Communication. University of Illinois Press, Urbana.',
          'van Cuilenburg, J., & van der Wurff, R. (2007). Toward easy-to-measure media diversity indicators. In E. D. Bens (Ed.), Media between culture and commerce (pp. 258). Intellect Books.',
          'https://doi.org/10.1162/003355305775097542',
          'https://doi.org/10.1145/2380718.2380750',
          'https://doi.org/10.3982/ecta7195',
          'Denscombe, M. (2007), The Good Research Guide, New York, NY: McGraw Hill.',
          'Smith, P. and Bell, A. (2007), ‘Unraveling the web of discourse analysis’, in E. Devereux (ed.), Media Studies: Key Issues & Debates, Thousand Oaks, CA: Sage, pp. 78–100.',
          'Smythe, S. (2006), ‘The good mother: A critical discourse analysis of literacy advice to mothers in the 20th century’, Ph.D. thesis, Canada: University of British Columbia.',
          'Anthony Giddens. (1984). The constitution of society: Outline of the theory of structuration. Univ of California Press.',
          'Boltanski, L., & Thévenot, L. (2006). On justification: Economies of worth (Vol. 27). Princeton University Press.',
          'Druckman JN (2010) Competing frames in a political campaign. In: Schaffner B and Sellers P (eds) Winning with Words: The Origins and Impact of Political Framing. New York: Routledge, pp. 101–120.',
          'https://doi.org/10.1111/j.1460-2466.1985.tb02454.x',
          'https://doi.org/10.1111/j.1460-2466.1993.tb01304.x',
          'https://doi.org/10.1017/cbo9780511628108.009',
          'https://doi.org/10.1207/s15327736me0701_4',
          'https://doi.org/10.1177/0267323101016002003',
          'Kleinnijenhuis J (2003) Het publiek volgt media die de politiek volgen [The public follows media who follow politics]. In: RMO Medialogica. Over het Krachtenveld tussen Burgers, Media en Politiek. [Media Logic. About the Force between Citizens, Media and Politics]. Den Haag: Sdu.',
          'https://doi.org/10.1177/0267323186001001005',
          'https://doi.org/10.1093/hcr/30.3.411',
          'https://doi.org/10.1177/0093650202239026',
          'https://doi.org/10.1080/10584600490481389',
          'Voltmer K (2000) Structures of diversity of press and broadcasting systems: The institutional context of public communication in western democracies. Discussion Paper FS III 00-201, Wissenschaftszentrum Berlin fu ̈r Sozialforschung, Berlin.',
          'https://doi.org/10.1016/j.jce.2003.08.005',
          'https://doi.org/10.1080/10584600590908410',
          'https://doi.org/10.1038/163688a0',
          'https://doi.org/10.1177/0093650202239026',
          'De Ridder, J. A. (1984). Van Tekst naar informatie: ontwikkeling en toetsing van een inhoudsanalyse-instrument [From text to information: development and testing of a content analysis instrument]. Doctoral dissertation, University of Amsterdam, Amsterdam.',
          'Kleinnijenhuis J (2003) Het publiek volgt media die de politiek volgen [The public follows media who follow politics]. In: RMO Medialogica. Over het Krachtenveld tussen Burgers, Media en Politiek. [Media Logic. About the Force between Citizens, Media and Politics]. Den Haag: Sdu.',
          'https://doi.org/10.1177/0267323101016002003',
          'https://doi.org/10.1007/bf00430638',
          'https://doi.org/10.1002/bs.3830010302',
          'Scholten, O., & Ruigrok, N. (2005). Politiek en politici in het nieuws in vijf landelijke dagbladen [Politics and politicians in the news in five national newspapers]. First report of the Netherlands News Monitor, April 2006, retrieved October 2, 2008, from http://www.nieuwsmonitor.net/continu/politiek_2005.pdf.',
          'https://doi.org/10.1177/1750635208097048',
          'Kleinnijenhuis J (2003) Het publiek volgt media die de politiek volgen [The public follows media who follow politics]. In: RMO Medialogica. Over het Krachtenveld tussen Burgers, Media en Politiek. [Media Logic. About the Force between Citizens, Media and Politics]. Den Haag: Sdu.',
          'https://doi.org/10.1177/107769907705400304',
          'Shannon, C.E. & Weaver, W. (1949) The Mathematical Theory of Communication. University of Illinois Press, Urbana.',
          'https://doi.org/10.1007/s10824-005-0490-y',
          'https://doi.org/10.1207/s15506878jobem5003_10',
          'Bohrnstedt, G., & Knoke, D. (1994). Statistics for social data analysis, 3rd edn. E. Itasca, IL: FE Peacock Publishers.',
          'Shannon, C.E. & Weaver, W. (1949) The Mathematical Theory of Communication. University of Illinois Press, Urbana.',
          'Cuilenburg, J. van. (). On measuring media competition and media diversity: Concepts, theories and methods. In R. G. Picard (Ed.), Measuring media content, quality, and diversity: Approaches and issues in content research (pp. –), Turku, Finland: Turku School of Economics and Business Administration.',
          'https://doi.org/10.1086/269491',
          'https://doi.org/10.1038/163688a0',
          'https://doi.org/10.1177/0093650202239026',
          'https://doi.org/10.1207/s15327736me1702_3',
          'https://doi.org/10.1111/j.1460-2466.1993.tb01304.x',
          'https://doi.org/10.1207/s15327736me1702_3',
          'https://doi.org/10.2307/2095977',
          'https://doi.org/10.2307/270810',
          'https://doi.org/10.1207/s15327736me0701_3'
          )


#For definitions diversity:
#McDonald and Dimmick (2003) cited 5 times, Shannon & Weaver (1949) cited 4 times, Kleinnijenhuis (2003) and Hellman (2001) cited 3 times
#In total 71 different sources (in 40 articles that named sources for the definitions they used); 12.8% cited McDonald and Dimmick, 10% cited Shannon & Weaver (1949), 7.7% cited Kleinnijenhuis (2003) and Hellman (2001)
#Looking at people: None of the most cited in theory are cited in the measurement a lot (Napoli and McQuail not at all, Voakes 1, Cuilenburg 4); most people only with one publication
sort(table(doi_m))
length(table(doi_m))
sum(sapply(gregexpr("Napoli", test_m, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Voakes", test_m, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("McQuail", test_m, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Cuilenburg", test_m, fixed = TRUE), function(x) sum(x > -1)))


#For definitions filter bubble:
#None cite Pariser (gave no measurement), only two give citations for measurement (one with two and one with 5), no overlaps

#For definitions echo chamber: 
#None of the theory citations in the methods, only three give citations (two with one, one with 3), no overlaps

#For definitions pluralism: 
#None of the theory citations in the methods, Only two (first: Hellman, 2001; second: Dowd et al, 2002; Batman, 2006)

#For definitions serendipity: 
#No citations

#Citations (diversity) that appear both in the definition and the measurement
#Voakes (1996), van Hoof et al (2014), McDonald & Dimmick (2003), Hellman (2001), Chaffee & Wilson (1977), Zhang & Hurley (2008), Herman (2006), Simpson (1949), McDonald & Lin (2004)
intersect(doi, doi_m)


methods_a = measure_citations %>% filter(grepl("another|secondary|already includes|not mentioned",tolower(method)))
methods_b = measure_citations %>% filter(grepl("economic|market",tolower(method)))
methods_c = measure_citations %>% filter(grepl("linguistic|entity mining|topic probabilit|lda|cosine|similarity|recommend",tolower(method)))
methods_d= measure_citations %>% filter(grepl("content analysis \\(quantitative",tolower(method)))
methods_e= measure_citations %>% filter(grepl("survey",tolower(method)))
methods_f= measure_citations %>% filter(grepl("qualitative|semi-structured",tolower(method)))
methods_g= measure_citations %>% filter(grepl("simulation",tolower(method)))
methods_h= measure_citations %>% filter(grepl("experiment",tolower(method)))
methods_i= measure_citations %>% filter(grepl("network",tolower(method)))

measure_citations %>%
  filter(!(ID %in% methods_a$ID) & !(ID %in% methods_b$ID) & !(ID %in% methods_c$ID) & !(ID %in% methods_d$ID) & !(ID %in% methods_e$ID) & !(ID %in% methods_f$ID) & !(ID %in% methods_g$ID) & !(ID %in% methods_h$ID) & !(ID %in% methods_i$ID)) 
methods_a %>% group_by(ID) %>% summarize(method = first(method))
methods_b %>% group_by(ID) %>% summarize(method = first(method))
methods_c %>% group_by(ID) %>% summarize(method = first(method))
methods_d %>% group_by(ID) %>% summarize(method = first(method))
methods_e %>% group_by(ID) %>% summarize(method = first(method))
methods_f %>% group_by(ID) %>% summarize(method = first(method))
methods_g %>% group_by(ID) %>% summarize(method = first(method))
methods_h %>% group_by(ID) %>% summarize(method = first(method))
methods_i %>% group_by(ID) %>% summarize(method = first(method))

table(measure_citations$autom)
autom_all = measure_citations %>% group_by(ID) %>% summarize(autom = first(autom), method = first(method), procedure = first(procedure), metrics = first(metrics), content_analysis = first(content_analysis),
                                                             reliability = first(reliability), rel_criterion = first(rel_criterion),
                                                             rel_method = first(rel_method), rel_size = first(rel_size), sample_size = first(sample_size), rel_n_coders = first(rel_n_coders), rel_n_coders2 = first(rel_n_coders2),
                                                             rel_morethanone = first(rel_morethanone), n_coders = first(n_coders), rel_n_coders = first(rel_n_coders), math = first(math)
                                                             )

table(autom_all$autom)
autom_which = autom_all %>% filter(autom == 'Yes')
autom_which %>% filter(metrics != 'NA; NA; NA; NA')
autom_which$method
autom_which$metrics
content_analysis = autom_all %>% filter(content_analysis == 'Yes')
table(content_analysis$reliability)
rel_method = content_analysis %>% filter(reliability == 'Yes')
sort(table(rel_method$rel_method))
sort(table(rel_method$n_coders))
sort(table(rel_method$rel_n_coders))
sort(table(rel_method$rel_morethanone))
colnames(empirical_data)
autom_all %>% select(ID, method, procedure)
sum(is.na(autom_all$math))
math = autom_all %>% filter(math != 'NA') %>% select(ID, method, math, procedure)
math$ID

HHI = math %>% filter(grepl('HHI', math))
Shannon = math %>% filter(grepl('Shannon|entropy|page 409', math))
Simpson = math %>% filter(grepl('Simpson', math))

math_2 = math %>%
  filter(!(ID %in% HHI$ID) & !(ID %in% Shannon$ID) & !(ID %in% Simpson$ID) & !(ID %in% other_div_d$ID) & !(ID %in% other_div_e$ID) & !(ID %in% other_div_f$ID)) 
math_2$procedure

theory_data = data %>% filter(Q2 == 'No')
theory_data = theory_data[(colSums(!is.na(theory_data)) > 0) & (colSums(theory_data != '') != 0)]

theory_data$Q5 <- ifelse((theory_data$oth == "yes"), paste(theory_data$Q5, theory_data$Q5_7_TEXT, sep=", "), theory_data$Q5)
theory_data$Q7 <- theory_data$Q7_1_TEXT
theory_data$Q11 <- ifelse((theory_data$Q11 == "Other (fill in)"), theory_data$Q11_3_TEXT, theory_data$Q11)
theory_data$Q10 <- ifelse((theory_data$Q10 == "Other, namely (please fill in)"), theory_data$Q10_6_TEXT, theory_data$Q10)

theory_data = theory_data %>% mutate_at('Q12', funs(gsub(viewpoint,"viewpoint", .)))
theory_data = theory_data %>% mutate_at('Q12', funs(gsub(actor,"actor", .)))
theory_data = theory_data %>% mutate_at('Q12', funs(gsub(topic,"topic", .)))
theory_data = theory_data %>% mutate_at('Q12', funs(gsub(outlet,"outlet", .)))
theory_data = theory_data %>% mutate_at('Q12', funs(gsub(other,"other", .)))

theory_data = theory_data %>% separate(col = Q12, into = c('dim1', 'dim2', 'dim3', 'dim4', 'dim5'), sep = ',')
theory_data = theory_data %>% rename_at(vars(c('Q12_1_TEXT', 'Q12_2_TEXT', 'Q12_3_TEXT', 'Q12_6_TEXT', 'Q12_9_TEXT')), 
                                        ~ c('actor_TEXT','viewpoint_TEXT', 'topic_TEXT', 'other_TEXT', 'outlet_TEXT'))

theory_not_media_related = theory_data %>% filter(Q9== 'No')
theory_data = theory_data %>% filter(Q9 == 'Yes')

theory_data = theory_data %>% select(-Q5, -Q5_7_TEXT, -Q13, -Q9, -div, -ser, -plu, -fil, -ech, -oth, -no, -Q11_3_TEXT, -Q7_1_TEXT, -Q10_6_TEXT)

oldnames = c('Q1', 'Q2', 'Q6', 'Q7', 'Q8' ,'Q10', 'Q11', 'Q148')
newnames = c('ID', 'empirical', 'def_yn', 'defcit', 'def', 'level', 'perspective', 'comment')

theory_data = theory_data %>% rename_at(vars(oldnames), ~ newnames)

exposure = 'Exposure perspective \\(i.e. centered on the \\$\\{lm\\:\\/\\/Field\\/1\\} consumed by the audience, i.e. what they read, view, click on; more focussed on what is displayed to the individual\\)'
supply = 'Supply perspective  \\(i.e. centered on the \\$\\{lm\\:\\/\\/Field\\/1\\} provided by media outlets, platforms or aggregators to the general public\\)'

media_gen = "'The news' or 'the media' in general without any further specifications"
media_1sec = "More than one media outlet \\(all in one sector\\)"
media_2sec = "More than one media outlet \\(in different sectors; this also includes aggregating platforms such as Google News\\)"
media_out = "One media outlet \\(i.e. one newspaper, one TV channel, one social media platform\\)"
media_mark = 'The media market overall \\(e.g. not related to specific media outlets\\)'
media_art = 'One media article \\(i.e. one newspaper article\\)'

theory_data = theory_data %>% mutate_at('level', funs(gsub(media_gen,"media_gen", .)))
theory_data = theory_data %>% mutate_at('level', funs(gsub(media_1sec,"media_1sec", .)))
theory_data = theory_data %>% mutate_at('level', funs(gsub(media_2sec,"media_2sec", .)))
theory_data = theory_data %>% mutate_at('level', funs(gsub(media_out,"media_out", .)))
theory_data = theory_data %>% mutate_at('level', funs(gsub(media_mark,"media_mark", .)))
theory_data = theory_data %>% mutate_at('level', funs(gsub(media_art,"media_art", .)))

theory_data = theory_data %>% mutate_at('perspective', funs(gsub(exposure,"exposure", .)))
theory_data = theory_data %>% mutate_at('perspective', funs(gsub(supply,"supply", .)))

theory_data$loop_concept = ifelse((theory_data$loop_concept == "X1"), 'div', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X2"), 'ser', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X3"), 'plu', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X4"), 'fil', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X5"), 'ech', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X7"), 'oth', theory_data$loop_concept)
theory_data$loop_concept = ifelse((theory_data$loop_concept == "X9"), 'no', theory_data$loop_concept)

theory_data %>% group_by(ID) %>% summarise(count = n())

citations_t = theory_data %>% group_by(ID, loop_concept) %>% summarize(defcit = first(defcit), def_yn = first(def_yn))

div_def_t = citations_t %>% filter(loop_concept == 'div')
fil_def_t = citations_t %>% filter(loop_concept == 'fil')
ech_def_t = citations_t %>% filter(loop_concept == 'ech')
ser_def_t = citations_t %>% filter(loop_concept == 'ser')
plu_def_t = citations_t %>% filter(loop_concept == 'plu')
oth_def_t = citations_t %>% filter(loop_concept == 'oth')

div_def_t %>% summarise(div = paste(defcit, collapse = ';'))

#How many do give a definition?
#38 give div definition (of 46, 82%), 2 give filter bubble definition (of 2, 100%), no echo chamber, 1 give serendip definition (of 1, 100%),  19 give pluralism definition (of 29, 65.5%)
table(div_def_t$def_yn)
table(fil_def_t$def_yn)
table(ser_def_t$def_yn)
table(plu_def_t$def_yn)

#How many (do not) use a citation?
#9 of 38 go not give div citation (23.6%), all give citation for filter bubble, all give serendip citation, 8 of 19 do not give pluralism citation (42%)
def_given_t = div_def_t %>% filter(def_yn == 'Yes')
def_given_fil_t = fil_def_t %>% filter(def_yn == 'Yes')
def_given_ser_t = ser_def_t %>% filter(def_yn == 'Yes')
def_given_plu_t = plu_def_t %>% filter(def_yn == 'Yes')
sum(def_given_t == "")
sum(def_given_fil_t == "")
sum(def_given_ser_t == "")
sum(def_given_plu_t == "")

test_t = div_def_t$defcit
test_t <- test_t[test_t != '']
test_t = strsplit(test_t, "\\\n")
test_t = unlist(test_t)
test_t = strsplit(test_t, ";")
test_t = unlist(test_t)
test_t <- test_t[test_t != '']
test_t[3] = paste(test_t[3], test_t[4], sep = ' ')
test_t[6] = paste(test_t[6], test_t[7], sep = ' ')
test_t[39] = paste(test_t[39], test_t[40], sep = ' ')
test_t = test_t[c(-4, -7, -40)]

citations_t = def_given_t %>% filter(defcit != "")
citations_t_plu = def_given_plu_t %>% filter(defcit != "")
citations_t$defcit
citations_t_plu$defcit
citation_list = c('Baker, 2006', 
                  'Van Cuilenburg, 2000', 
                  'https://doi.org/10.5325/jinfopoli.1.2011.0287',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0441', 
                  ' https://doi.org/10.5325/jinfopoli.1.2011.0217',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0246', 
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'https://doi.org/10.5235/175776312802483880',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0287',
                  'https://doi.org/10.5422/fordham/9780823245123.001.0001',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'Neuberger, C., & Lobgis, F. (2010). Die Bedeutung des Internets im Rahmen der Vielfaltssicherung. Berlin: Vistas.',
                  'Valcke, P. (2004). Digitale Diversiteit – Convergentie van Media-, Telecommunicatie- en Mededinginsrecht. Brussel: Larcier.',
                  'https://doi.org/10.1093/0198297556.001.0001',
                  'https://doi.org/10.1007/978-3-319-29562-6_16',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0441',
                  'Korthals, M. (2013). De Overheid als Verleidster. Ethische vragen rond gedragsbeinvloeding door de overheid ten gunste van duurzemere gedragspartonen en leefstijlen, in ALI – Raad voor de leefomgeving en instrastructuur (eds.), Raad voor de leefomgeving en infrastructuur, Essays Duurzame Gedragspartronen, Augustus 2013, 31–48.',
                  'Rieder, B. (2009). Democratizing search? From critique to society-oriented design. In K. Becker, & F. Stalder (Eds.), Deep search. The politics of search beyond google (pp. 133–151). Innsbruck: StudienVerlag.',
                  'https://doi.org/10.1111/j.1460-2466.2011.01582.x',
                  'Song, Sonya. 2016. “Examining Digital Consumption Trends during Breaking News Events.” A Chartbeat Study. Metrics that Matter Special Series. http://lp.chartbeat.com/rs/062- HAC-076/images/Chartbeat_Study_SONYASONG.pdf.',
                  'Napoli, P. M. (2001). Foundations of communications policy: Principles and process in the regulation of electronic media. Hampton Press.',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0246',
                  'https://doi.org/10.1177/107769909607300306',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'https://doi.org/10.1016/j.poetic.2009.09.002',
                  'https://doi.org/10.1080/10584600903502615',
                  'https://doi.org/10.1177/1940161213497595',
                  'https://doi.org/10.1016/j.soscij.2008.12.014',
                  'https://doi.org/10.1023/A:1016284431021',
                  'Holcomb J, Rosenstiel T, Mitchell A, et al. (2011) Non-Profit News: Assessing a New Landscape in Journalism. Washington, DC: Pew Research Center.',
                  'https://doi.org/10.1080/10584600903502615',
                  'https://doi.org/10.1111/j.1460-2466.1990.tb02265.x',
                  'https://doi.org/10.1177/1940161211407334',
                  'https://doi.org/10.1080/10584600903502615', 
                  'https://doi.org/10.1177/0267323114538724',
                  'https://doi.org/10.1111/j.1460-2466.2008.00384.x', 
                  'https://doi.org/10.5771/9783845202938-41',
                  'Hagel, J., & Brown, J. S. (2009, April 8). The next wave of open innovation. Business Week.',
                  'van Cuilenburg, J. (1998). Diversity revisited: Towards a critical rational model of media diversity. In K. Brants, J. Hermes, & L. van Zoonen (Eds.), The Media in Question: Popular cultures and public interests (pp. 38–49). Thousand Oaks, CA: SAGE Publications.',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'https://doi.org/10.1177/0267323104042911',
                  'Council of Europe, 2003',
                  'Craufurd Smith, R. (2004), ‘Rethinking European Union competence in the field of media ownership: The internal market, fundamental rights and European citizenship’, European Law Review, 29: 5, pp. 652–72.',
                  'https://doi.org/10.5040/9781472559975',
                  'https://doi.org/10.1080/17577632.2010.11427355',
                  'https://doi.org/10.5235/175776312802483880',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0246',
                  'https://doi.org/10.4135/9781446219942',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'https://doi.org/10.1177/14614449922225555',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0246',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0287',
                  'https://doi.org/10.3390/d2081059',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0246',
                  'https://doi.org/10.5325/jinfopoli.1.2011.0287',
                  'Napoli, P. M. (2012). Persistent and emergent diversity policy concerns in an evolving media environment. In S. Pager & A. Candeub (Eds.), Transnational culture in the Internet age (pp. 167–181). Cheltenham, UK: Edward Elgar.',
                  'https://doi.org/10.1057/9781137304308.0010',
                  'Nordenstreng, Kaarle, and Tapio Varis. 1974. Television Traffic—A One-Way Street? A Survey and Analysis of the International Flow of Television Programme Material. Reports and Papers on Mass Communication, No. 70. Paris: UNESCO.',
                  'https://doi.org/10.1080/08838158709386645',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x',
                  'FCC, 2003',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'Schlesinger, P., Murdock, G., & Elliot, P. (1983). Televising ‘terrorism’: Political violence in popular culture. London: Comedia.',
                  'FCC, 2003',
                  'FCC, 2003',
                  'https://doi.org/10.1177/001654928303100301',
                  'Van Cuilenburg, J. 2007. Media diversity, competition and concentration: Concepts and theories. In Media between culture and commerce: An introduction, ed. E. de Bens, 25–54. London: Intellect.',
                  'McQuail, D. (2005). Mass communication theory. London: Sage.',
                  'Napoli, P.M. (2003). Foundations of communication policy: Principles and processes in regulation of electronic media. Cresskill, NJ: Hampton Press.',
                  'T Mendel & E Salomon, Freedom of Expression and Broadcasting Regulation (Rio de Janeiro: UNESCO, 2011)',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
                  'https://doi.org/10.4135/9781446219942',
                  'https://doi.org/10.4135/9781446219942',
                  'Council of Europe, 1994',
                  'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x', 
                  'https://doi.org/10.1207/s15506878jobem5004_6',
                  'UNESCO, 1995',
                  'https://doi.org/10.1017/cbo9780511804557.001',
                  'Organisation for Economic Cooperation and Development, 2007',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'Cento Veljanovski, The Media in Britain Today (London:News International plc, 1990).',
                  'https://doi.org/10.1080/08838158709386645',
                  'https://doi.org/10.1177/107769907805500328',
                  'McLennan, Gregor (1995) Pluralism. Buckingham: Open University Press.',
                  'van Cuilenburg, J. (1998). Diversity revisited: Towards a critical rational model of media diversity. In K. Brants, J. Hermes, & L. van Zoonen (Eds.), The Media in Question: Popular cultures and public interests (pp. 38–49). Thousand Oaks, CA: SAGE Publications.',
                  'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.',
                  'Gibbons, Thomas (2000) ‘Pluralism, Guidance and the New Media’, in C. Marsden (ed) Regulating the Global Information Society. London: Routledge, pp. 304-15',
                  'https://doi.org/10.1177/0267323104042911',
                  'https://doi.org/10.1111/j.1460-2466.1976.tb01360.x',
                  'In the Matter of 1998 Biennial Regulatory Review, Notice of Inquiry (1998)',
                  'In the Matter of 2002 Biennial Review, Report and Order and Notice of Proposed Rulemaking (2003)')                                                                                                                                                                                 

length(citation_list)
sum(sapply(gregexpr("Napoli", citation_list, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("McQuail", citation_list, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Cuilenburg", citation_list, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Helberger", citation_list, fixed = TRUE), function(x) sum(x > -1)))
sum(sapply(gregexpr("Valcke", citation_list, fixed = TRUE), function(x) sum(x > -1)))
sort(table(citation_list))
length(table(citation_list))

add_info = read.csv('coding_raw.csv')
add_info %>% mutate_if(is.factor, as.character) -> add_info
add_info = add_info %>% filter(IPAddress != '146.50.68.108')
theory_empirical = full_join(theory_data, empirical_data)
theory_empirical$ID = gsub(".pdf", "", theory_empirical$ID)
add_info = add_info %>% filter(Q3 == 6 | Q3 == 4) %>% select(ID = RecipientEmail, abstract, author, doi, outlet, title_full, year)
theory_empirical = left_join(theory_empirical, add_info, by = 'ID')

data %>% group_by(Q1) %>% summarise(count = n())
empirical_data %>% group_by(ID) %>% summarise(count = n())
theory_data %>% group_by(ID) %>% summarise(count = n())
no_concept %>% group_by(Q1) %>% summarise(count = n())
not_media_related$Q1 = gsub(".pdf", "", not_media_related$Q1)
not_media_related %>% group_by(Q1) %>% filter(!Q1 %in% empirical_data$ID) %>% summarise(count = n())
theory_not_media_related %>% group_by(Q1) %>% summarise(count = n())
theory_empirical %>% group_by(ID) %>% summarise(count = n())


per_year = theory_empirical %>% group_by(ID, year) %>% summarise(count = first(ID)) %>% group_by(year) %>% summarise(count = n())
#per_year = per_year[-nrow(per_year),]
per_year$year = as.numeric(per_year$year)
per_year[nrow(per_year) + 1,] = list(2001,0)
per_year = per_year %>% arrange(year)
per_year %>% filter(year < 2013) %>% summarize(value = sum(count))
per_year %>% filter(year >= 2013) %>% summarize(value = sum(count))
per_year %>% summarize(value = sum(count))



ggplot(data=per_year, aes(x=year, y=cumsum(count), group = 1)) +
  geom_line() +
  geom_point()

exposure = theory_empirical %>% filter(perspective == 'exposure') #%>% filter(loop_concept == 'div')
by_year =  exposure %>% group_by(ID, year) %>% summarize(perspective = first(perspective))
by_year = by_year %>% group_by(year) %>% summarise(exposure = n())
by_year$year = as.double(by_year$year)
by_year <- by_year %>%
  complete(year = 1999:2018, 
           fill = list(incidents = 0)) %>%
  as.data.frame()
by_year[is.na(by_year)] <- 0
by_year$value_cumsum =cumsum(by_year$exposure)

by_year_a = by_year %>% filter(year < 2011) %>% summarize(value = sum(exposure))
by_year_a = by_year %>% filter(year >= 2011) %>% summarize(value = sum(exposure))


supply = theory_empirical %>% filter(perspective == 'supply') #%>% filter(loop_concept == 'div')
by_year_s =  supply %>% group_by(ID, year) %>% summarize(perspective = first(perspective))
by_year_s = by_year_s %>% group_by(year) %>% summarise(supply = n())
by_year_s$year = as.double(by_year_s$year)
by_year_s <- by_year_s %>%
  complete(year = 1999:2018, 
           fill = list(incidents = 0)) %>%
  as.data.frame()
by_year_s[is.na(by_year_s)] <- 0
by_year_s$value_cumsum_s =cumsum(by_year_s$supply)
both = full_join(x = by_year, y = by_year_s)
all_data = full_join(x = both, y = per_year)

df <- melt(both[,c("year","value_cumsum", "value_cumsum_s")], id.vars = 1)
df = df %>% filter(year != 2019)
write_csv(df, 'data_visualization.csv')

diversity_per_year = theory_empirical %>% filter(loop_concept == 'div')
diversity_per_year =  diversity_per_year %>% group_by(ID, year) %>% summarise(count = first(ID)) %>% group_by(year) %>% summarise(count = n())
diversity_per_year = diversity_per_year[-nrow(diversity_per_year),]
ggplot(data=diversity_per_year, aes(x=year, y=cumsum(count), group = 1)) +
  geom_line() +
  geom_point() + 
  geom_line(data=pluralism_per_year, aes(x=year, y=cumsum(count), group = 1)) +
  geom_line(data=filter_per_year, aes(x=year, y=cumsum(count), group = 1))

pluralism_per_year = theory_empirical %>% filter(loop_concept == 'plu')
pluralism_per_year =  pluralism_per_year %>% group_by(ID, year) %>% summarise(count = first(ID)) %>% group_by(year) %>% summarise(count = n())

filter_per_year = theory_empirical %>% filter(loop_concept == 'ech' | loop_concept == 'fil')
filter_per_year =  filter_per_year %>% group_by(ID, year) %>% summarise(count = first(ID)) %>% group_by(year) %>% summarise(count = n())


per_year$value = per_year$count
ggplot(df,aes(x = year,y =value)) + 
  geom_bar(aes(fill = factor(variable,labels=c("Exposure","Supply"))), stat = "identity", position = "stack") +
  #geom_point(data = per_year, aes(y=cumsum(value))) +
  #geom_line(data = per_year, aes(x = year, y = cumsum(value), linetype = ""), inherit.aes = F, group = 1) +
  labs(fill = "Perspective",
      linetype = "Overall") +
  ylab('Number of articles (cumulative)') + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("black","grey")) +
  scale_x_continuous("Year", labels = as.character(df$year), breaks = df$year)


  

definitions = empirical_data %>% filter(def != 'NA') %>% filter(loop_concept == 'div') %>% group_by(ID) %>% summarize(def = first(def))
definitions = definitions %>% select(ID, def)
write.csv(definitions, 'definitions.csv')

colnames(theory_empirical)
actor_div = theory_empirical %>% filter(actor_TEXT != 'NA') %>% filter(actor_TEXT != '') %>% group_by(ID) %>% summarize(text = first(actor_TEXT), author = first(author), year = first(year))
actor_div_a = actor_div %>% filter(grepl("actor|person|group|companies|entit|part",tolower(text)))
actor_div_b = actor_div %>% filter(grepl("source",tolower(text)))
actor_div_c = actor_div %>% filter(grepl("voice",tolower(text)))
actor_div_d = actor_div %>% filter(grepl('gender|minority', tolower(text)))
actor_div %>%
  filter(!(ID %in% actor_div_a$ID) & !(ID %in% actor_div_b$ID) & !(ID %in% actor_div_c$ID) & !(ID %in% actor_div_d$ID))

topic_div = theory_empirical %>% filter(topic_TEXT != 'NA') %>% filter(topic_TEXT != '') %>% group_by(ID) %>% summarize(text = first(topic_TEXT))
topic_div$text
topic_div_a = topic_div %>% filter(grepl("issue",tolower(text)))
topic_div_b = topic_div %>% filter(grepl("topic|them|subject",tolower(text)))
topic_div_c = topic_div %>% filter(grepl("program|genr|formats",tolower(text)))
topic_div_d = topic_div %>% filter(grepl('agenda', tolower(text)))
topic_div %>%
  filter(!(ID %in% topic_div_a$ID) & !(ID %in% topic_div_b$ID) & !(ID %in% topic_div_c$ID) & !(ID %in% topic_div_d$ID))


viewpoint_div = theory_empirical %>% filter(viewpoint_TEXT != 'NA') %>% filter(viewpoint_TEXT != '') %>% group_by(ID) %>% summarize(text = first(viewpoint_TEXT))
viewpoint_div$text

viewpoint_div_a = viewpoint_div %>% filter(grepl("viewpoint|persp|idea|view",tolower(text)))
viewpoint_div_b = viewpoint_div %>% filter(grepl("ideolog",tolower(text)))
viewpoint_div_c = viewpoint_div %>% filter(grepl("opinion|stance",tolower(text)))
viewpoint_div_d = viewpoint_div %>% filter(grepl('partis|far-right|cross-cutt', tolower(text)))
viewpoint_div_e = viewpoint_div %>% filter(grepl('frame', tolower(text)))
viewpoint_div_f = viewpoint_div %>% filter(grepl('polit', tolower(text)))

viewpoint_div %>%
  filter(!(ID %in% viewpoint_div_a$ID) & !(ID %in% viewpoint_div_b$ID) & !(ID %in% viewpoint_div_c$ID) & !(ID %in% viewpoint_div_d$ID) & !(ID %in% viewpoint_div_e$ID) & !(ID %in% viewpoint_div_f$ID))


outlet_div = theory_empirical %>% filter(outlet_TEXT != 'NA') %>% filter(outlet_TEXT != '') %>% group_by(ID) %>% summarize(text = first(outlet_TEXT))
outlet_div$text

outlet_div_a = outlet_div %>% filter(grepl("own",tolower(text)))
outlet_div_b = outlet_div %>% filter(grepl("source",tolower(text)))
outlet_div_c = outlet_div %>% filter(grepl("channel|outlet",tolower(text)))
outlet_div_d = outlet_div %>% filter(grepl("struct|mark|conc",tolower(text)))
outlet_div %>%
  filter(!(ID %in% outlet_div_a$ID) & !(ID %in% outlet_div_b$ID) & !(ID %in% outlet_div_c$ID) & !(ID %in% outlet_div_d$ID)) 


other_div = theory_empirical %>% filter(other_TEXT != 'NA') %>% filter(other_TEXT != '') %>% group_by(ID) %>% summarize(text = first(other_TEXT))
other_div$text

other_div_a = other_div %>% filter(grepl("cultur",tolower(text)))
other_div_b = other_div %>% filter(grepl("regio|geograph|count",tolower(text)))
other_div_c = other_div %>% filter(grepl("content",tolower(text)))
other_div_d = other_div %>% filter(grepl("ling|lexic|token|cosine",tolower(text)))
other_div_e = other_div %>% filter(grepl("hyper|url|audio|link",tolower(text)))
other_div_f = other_div %>% filter(grepl("experience|interpret|received",tolower(text)))

other_div %>%
  filter(!(ID %in% other_div_a$ID) & !(ID %in% other_div_b$ID) & !(ID %in% other_div_c$ID) & !(ID %in% other_div_d$ID) & !(ID %in% other_div_e$ID) & !(ID %in% other_div_f$ID)) 

level_perspective_all = theory_empirical %>% group_by(ID, loop_concept) %>% summarize(level = first(level), perspective = first(perspective))
#
div_lev = level_perspective_all %>% filter(loop_concept == 'div')
fil_lev = level_perspective_all %>% filter(loop_concept == 'fil')
ech_lev = level_perspective_all %>% filter(loop_concept == 'ech')
ser_lev = level_perspective_all %>% filter(loop_concept == 'ser')
plu_lev = level_perspective_all %>% filter(loop_concept == 'plu')
oth_lev = level_perspective_all %>% filter(loop_concept == 'oth')

sort(table(level_perspective_all$level))
length(level_perspective_all$level)
level_perspective_all_g = level_perspective_all %>% group_by(ID) %>% summarize(level = first(level), perspective = first(perspective))
sort(table(level_perspective_all_g$level))
length(level_perspective_all_g$level)


sort(table(level_perspective_all_g$perspective))
length(level_perspective_all_g$perspective)
comments = theory_empirical %>% group_by(ID) %>% summarise(comment = first(comment))
comments_freq = comments %>% filter(grepl('freque|percent|propor|real', tolower(comment)))
comments_syn = comments %>% filter(grepl('interchange|synony', tolower(comment)))
comments_freq$comment
comments_syn$comment
frequencies = theory_empirical %>% filter(ID %in% comments_freq$ID) %>% group_by(ID) %>% summarize(method = first(method)) %>% filter(method != "NA")
frequencies$method

colnames(theory_empirical)
definitions = theory_empirical %>% filter(def_yn == 'Yes') %>% group_by(ID, loop_concept) %>% summarise(def = first(def), cit = first(defcit), actor = first(actor_TEXT),
                                                                                                   viewpoint = first(viewpoint_TEXT), topic = first(topic_TEXT), 
                                                                                                   outlet = first(outlet_TEXT), other = first(other_TEXT) 
                                                                                              )
definitions
definitions = arrange(definitions, by=loop_concept)
write.csv(definitions, 'definitions.csv')
definitions$def
definitions_all = definitions %>% group_by(ID) %>% summarise(def = paste(def, collapse = "|||||||||||||"), n = n())
definitions_all = definitions_all %>% filter(n > 1)
definitions_all$ID
definitions_all
comments_syn$ID
comments_syn$comment
one = theory_empirical %>% group_by(ID, loop_concept) %>% summarize(n = n())
table(one$loop_concept)

two = one %>% group_by(ID) %>% summarize(n = n(), concepts = paste(loop_concept, collapse = ','))
table(two$concepts)


citations_all = theory_empirical %>% group_by(ID, loop_concept) %>% summarize(defcit = first(defcit), def_yn = first(def_yn))

div_def_all = citations_all %>% filter(loop_concept == 'div')
fil_def_all = citations_all %>% filter(loop_concept == 'fil')
ech_def_all = citations_all %>% filter(loop_concept == 'ech')
ser_def_all = citations_all %>% filter(loop_concept == 'ser')
plu_def_all = citations_all %>% filter(loop_concept == 'plu')
oth_def_all = citations_all %>% filter(loop_concept == 'oth')

div_def_all %>% filter(def_yn == 'Yes') %>% filter(!is.na(defcit) & defcit != "")
plu_definitions = plu_def_all %>% filter(def_yn == 'Yes') %>% filter(!is.na(defcit) & defcit != "")
plu_definitions$defcit
table(div_def_all$def_yn)
table(plu_def_all$def_yn)
colnames(empirical_data)

dep_ind = empirical_data %>% group_by(ID, loop_concept, loop_stat_variable) %>% summarize(dep_yn = first(dep_yn), ind_yn = first(ind_yn), stat_dep = first(stat_dep),  stat_ind = first(stat_ind), stat_group_sig = first(stat_group_sig),
                                                                      stat_test = first(stat_test), stat_grouptext = first(stat_grouptext), stat_posneg = first(stat_posneg), 
                                                                      stat_group_sig_dep = first(stat_group_sig_dep), dimension = paste(dimension, collapse = ";"))
dep_ind_d = dep_ind %>% filter(loop_concept == 'div')
dep_ind_d %>% group_by(ID) %>% summarize(dep_yn = first(dep_yn)) %>% filter(dep_yn == 'Yes')
dep_ind_d %>% group_by(ID) %>% summarize(ind_yn = first(ind_yn)) %>% filter(ind_yn == 'Yes')
dep_ind_d %>%  group_by(ID) %>% summarize(ind_yn = first(ind_yn), dep_yn = first(dep_yn)) %>% filter(dep_yn == "Yes", ind_yn == 'Yes')
variables_dep = dep_ind_d %>% filter(ind_yn == "Yes") %>% group_by(ID) %>% summarize(stat_dep = paste(stat_dep, collapse = ";"), dimension = paste(dimension, collapse = ";")) 
variables_ind = dep_ind_d %>% filter(dep_yn == "Yes") %>% group_by(ID) %>% summarize(stat_ind = paste(stat_ind, collapse = ";"))
dep_ind_d$stat_dep
variables_dep %>% arrange(dimension)
a = dep_ind %>% filter(!is.na(stat_dep))
a$stat_dep


b = dep_ind_d  %>% filter(!is.na(stat_ind)) %>% select(ID, stat_ind)
b$stat_ind
write.csv(b, 'b.csv')

table(dep_ind_d$ID)

dep_ind_p = dep_ind %>% filter(loop_concept == 'plu')
table(dep_ind_p$dep_yn)
table(dep_ind_p$ind_yn)
dep_ind_p %>% filter(dep_yn == "Yes", ind_yn == 'Yes')

dep_ind_f = dep_ind %>% filter(loop_concept == 'fil')
table(dep_ind_f$dep_yn)
table(dep_ind_f$ind_yn)
dep_ind_f %>% filter(dep_yn == "Yes", ind_yn == 'Yes')

dep_ind_e = dep_ind %>% filter(loop_concept == 'ech')
table(dep_ind_e$dep_yn)
table(dep_ind_e$ind_yn)
dep_ind_e %>% filter(dep_yn == "Yes", ind_yn == 'Yes')

dep_ind %>% filter(loop_concept == 'ser')

dep_ind_d

theory_empirical %>% filter(grepl("copy",tolower(comment)))
year_autom = theory_empirical %>% filter(autom == 'Yes') %>% group_by(ID) %>% summarise(year = first(year),  procedure = first(procedure), outlet = first(outlet), method = first(method), metr = first(metr))
table(year_autom$year)
year_autom %>% filter(year < 2014)                                                              
table(year_autom$method)
colnames(theory_empirical)
year_autom %>% filter(method == 'Content Analysis (quantitative, manual or (semi-)annotated )')
table(year_autom$metr)
theory_empirical$loop_stat_variable
sort(table(c(doi, citation_list)))

theor_emp = theory_empirical %>% filter(loop_concept == 'div'|loop_concept == 'plu') %>% group_by(ID) %>% summarize(empirical = first(empirical), def = first(def_yn), defcit = first(defcit))
table(sort(theor_emp$empirical))
table(sort(theor_emp$def))
table(sort(theor_emp$defcit))
emp = theor_emp %>% filter(empirical == "Yes")
table(sort(emp$def))
theor = theor_emp %>% filter(empirical == "No")
table(sort(theor$def))
#-----------------------------------VISUALIZATIONS: GRAPH OF CITATIONS---------------------------------------------------------------------------
graph_citations = theory_empirical %>% select(ID, def_yn, defcit, year, empirical, loop_concept, author, title_full, doi) %>% filter(def_yn == 'Yes') %>% filter(loop_concept == 'div') %>% group_by(ID) %>% summarise(defcit = first(defcit), year = first(year), empirical = first(empirical), author = first(author), doi = first(doi), title = first(title_full)) %>% arrange(desc(empirical))
graph_citations = graph_citations %>% filter(!is.na(defcit)) %>% filter(defcit != '') %>% separate(defcit, c("a", "b", "c", "d", "e", "f", "g", 'h', 'i', 'j', 'k', 'l', 'm'), "; |\\n|;")
graph_citations_test = graph_citations %>%
  gather(key = 'citation_round', value = 'citation', a:m)
graph_citations_test = graph_citations_test %>% filter(!is.na(citation)) %>% filter(citation != "") %>% arrange(ID, citation_round, desc(empirical))
graph_citations_test[15,]$citation = "Valcke, P.: Looking for the user in media pluralism regulation: the potential and limits of regulating exposure diversity. J. Inf. Policy 1, 287–320 (2011)"
graph_citations_test[18,]$citation = "Hitchens, L.: Media regulatory frameworks in the age of broadband: securing diversity. J. Inf. Policy 1, 217–240 (2011)"
graph_citations_test[31,]$citation = "Neuman, Bimber, & Hindman, 2011"
graph_citations_test[35,]$citation = "Voakes et al, 1996"
graph_citations_test[63,]$citation = "JOHN W. DIMMICK, MEDIA COMPETITION AND COEXISTENCE: THE THEORY OF NICHE (2003)"
graph_citations_test[65,]$citation = "Brian C. Hill, Measuring Media Market Diversity: Concentration, Importance,and Pluralism, 58 FED. COMM. L.J. 169 (2006)"
graph_citations_test[67,]$citation = "Eli Noam, How to Measure Media Concentration,FT.COM, Aug. 30, 2004, http://www.ft.com/cms/s/da3Obf5e-fa9d-1ld8-9a71-00000e251lc8.html."
graph_citations_test[74,]$citation = "Benson (2009)"
graph_citations_test[142,]$citation = "Drosou M and Pitoura E. Diversity over continuous data. IEEE Data(base) Engineering Bulletin – DEBU 2009 32: 49–56."
graph_citations_test[144,]$citation = "Ziegler C-N, McNee SM, Konstan JA and Lausen G. Improving recommendation lists through topic diversification. In:Proceedings of the 14th ınternational conference on World Wide Web, Chiba, Japan. New York: ACM, 2005, pp. 22–32."
graph_citations_test[271,] = graph_citations_test[116,]
graph_citations_test[271,]$citation_round = 'b'
graph_citations_test[116,]$citation = 'Napoli, 2009'
graph_citations_test[271,]$citation = 'Napoli, 2011'
graph_citations_test[272,] = graph_citations_test[35,]
graph_citations_test[272,]$citation_round = 'c'
graph_citations_test[35,]$citation = 'Baden and Springer, 2015'
graph_citations_test[272,]$citation = 'Voakes et al 2016'
graph_citations_test[273,] = graph_citations_test[81,]
graph_citations_test[273,]$citation_round = 'b'
graph_citations_test[81,]$citation = 'Voakes et al 2016'
graph_citations_test[273,]$citation = 'Napoli 1999'
graph_citations_test[274,] = graph_citations_test[137,]
graph_citations_test[274,]$citation_round = 'f'
graph_citations_test[137,]$citation = 'Napoli 2011'
graph_citations_test[274,]$citation = 'Napoli 2012'

graph_citations_test = graph_citations_test %>% filter(!(row_number() %in% c(16, 19, 32, 64, 66, 68, 69, 75, 143, 145))) %>% arrange(desc(empirical), ID, citation_round)
all_citations = c(doi, citation_list)
graph_citations = cbind(graph_citations_test, all_citations)
graph_citations = graph_citations %>% arrange(ID)
graph_citations$ID
graph_citations[144,]$doi = 'https://doi.org/10.1177/1329878x1415200109'
graph_citations[211,]$doi = 'https://doi.org/10.1093/ijpor/15.1.44'
graph_citations[213,]$doi = 'https://doi.org/10.2139/ssrn.253054'
graph_citations[226,]$doi = 'https://doi.org/10.25200/bjr.v12n2.2016.863'
graph_citations[240,]$doi = 'https://doi.org/10.4337/9780857931344.00013'
graph_citations[241,]$doi = 'https://doi.org/10.4337/9780857931344.00013'
graph_citations[242,]$doi = 'https://doi.org/10.4337/9780857931344.00013'
graph_citations[243,]$doi = 'https://doi.org/10.4337/9780857931344.00013'
graph_citations[244,]$doi = 'https://doi.org/10.4337/9780857931344.00013'
graph_citations[253,]$doi = 'https://doi.org/10.1515/nor-2017-0230'
graph_citations[254,]$doi = 'https://doi.org/10.1515/nor-2017-0230'
graph_citations[255,]$doi = 'https://doi.org/10.1515/nor-2017-0230'
graph_citations[256,]$doi = 'https://doi.org/10.1515/nor-2017-0230'
graph_citations[257,]$doi = 'https://doi.org/10.1515/nor-2017-0230'
graph_citations[261,]$doi = 'https://doi.org/10.1177/073953290002100305'
graph_citations[262,]$doi = 'https://doi.org/10.1177/073953290002100305'
graph_citations[263,]$doi = 'https://doi.org/10.1177/073953290702800104'
graph_citations[264,]$doi = 'https://doi.org/10.1177/073953290702800104'

graph_citations_div = graph_citations
graph_citations_plu = theory_empirical %>% select(ID, def_yn, defcit, year, empirical, loop_concept, author, title_full, doi) %>% filter(def_yn == 'Yes') %>% filter(loop_concept == 'plu') %>% group_by(ID) %>% summarise(defcit = first(defcit), year = first(year), empirical = first(empirical), author = first(author), doi = first(doi), title = first(title_full)) %>% arrange(desc(empirical))
graph_citations_plu= graph_citations_plu %>% filter(!is.na(defcit)) %>% filter(defcit != '') %>% separate(defcit, c("a", "b", "c", "d", "e", "f", "g", 'h', 'i', 'j', 'k', 'l', 'm'), "; |\\n|;")
graph_citations_test_plu = graph_citations_plu %>%
  gather(key = 'citation_round', value = 'citation', a:m)
graph_citations_test_plu = graph_citations_test_plu %>% filter(!is.na(citation)) %>% filter(citation != "") %>% arrange(ID, citation_round, desc(empirical))
graph_citations_test_plu[2,]$citation = "Raeijmaekers and Maeseele (2015)"
graph_citations_test_plu[36,]$citation = "Doyle (2002)"
graph_citations_test_plu[38,] = graph_citations_test_plu[36,]
graph_citations_test_plu[38,]$citation_round = 'b'
graph_citations_test_plu[38,]$citation = 'Council of Europe (1994)'
graph_citations_test_plu = graph_citations_test_plu %>% filter(!(row_number() %in% c(3, 21))) %>% arrange(desc(empirical), ID, citation_round)
citation_list_plu = c('https://doi.org/10.5325/jinfopoli.1.2011.0287',
                      'https://doi.org/10.1177/0163443715591670',
                      'https://doi.org/10.1057/9781137304308_6',
                      'https://doi.org/10.5422/fordham/9780823245123.001.0001',
                      'https://doi.org/10.1057/9781137304308',
                      'Aslama, M., De Bens, E., Van Cuilenburg, J., Nordenstreng,K., Schulz, W., & Van der Wurff, R. (2007).
                      Measuring and assessing empirical media diversity:
                      Some European cases. In E. De Bens (Ed.), Media between
                      culture and commerce (pp. 55–98). Chicago, IL:
                      Intellect Ltd.',
                      'Picard, R. G. (Ed.). (2000). Measuring media content, quality, and diversity. Turku: Turku School of Economics
                      and Business Administration. Retrieved from
                      http://www.robertpicard.net/files/Measuring_Media
                      _Content_Quality_Diversity_Book.pdf',
                      'https://doi.org/10.1016/b978-0-444-53776-8.00014-3',
                      'https://doi.org/10.1017/cbo9780511790867', 
                      'https://doi.org/10.1017/cbo9780511790867', 
                      'https://doi.org/10.1017/cbo9780511790867',
                      'Council of Europe. (1994). 4ème Conférence Ministérielle européenne sur la politique des communications
de masse. Prague, 7–8 décembre 1994. “Les média dans une société démocratique” [4th
                      European Ministerial Conference on Mass Media Policy. Prague, 1994, 7th–8th December: The
                      media in a democratic society]. MCM (94) 5.',
                      'https://doi.org/10.1080/10584600802426965',
                      'https://doi.org/10.1017/cbo9780511790867',
                      'Gibbons T (1998) Regulating the Media. London: Sweet and Maxwell.',
                      'https://doi.org/10.1017/cbo9780511790867',
                      'https://doi.org/10.7829/9789639776739klimkiewicz',
                      'https://doi.org/10.4135/9781446219942',
                      'DAHL, Robert. Polyarchy, New Haven, Yale University Press, 1971.',
                      'Ofcom. 2015. Measurement Framework for Media Plurality. November. London: Ofcom.',
                      'Centre for Media Pluralism and Media Freedom (2013)',
                      '6K Lefever, E Wauters, and P Valcke, Media Pluralism in the EU – Comparative analysis
of measurements systems in Europe and US (Steunpunt Media, 2013) 12.',
                      'K Lefever, E Lievens, and P Valcke, ‘Risk-Based Regulation in the Media Sector: To
Measure Is To Know’ (2012) 41 JAIIO – SID 1 at 3.',
                      'https://doi.org/10.1080/14241277.2005.9669413',
                      'van Cuilenburg, J. (1998). Diversity revisited: Towards a critical rational model of media diversity. In K. Brants, J. Hermes, & L. van Zoonen (Eds.), The Media in Question: Popular cultures and public interests (pp. 38–49). Thousand Oaks, CA: SAGE Publications.',
                      'https://doi.org/10.5422/fordham/9780823245123.001.0001',
                      'McGonagle, T. (2011). Minority rights, freedom of expression and of the media: Dynamics and dilemmas. Antwerpen, BE: Intersentia.',
                      'Council of Europe (1999)',
                      'Independent Study on Indicators for Media Pluralism in the Member States – Towards a Risk-Based Approach (2009) Prepared for the European Commission DG Information Society and Media by K.U. Leuven – ICRI, Jönköping International Business School –32 MMTC and Ernst & Young Consultancy Belgium, Preliminary Final Report, Leuven, April.',
                      'https://doi.org/10.1007/s11293-015-9455-5',
                      'https://doi.org/10.1016/s0014-2921(01)00139-8',
                      'article 29 of the 1986 Act (French legislation)',
                      'https://doi.org/10.4135/9781446219942',
                      'https://doi.org/10.4135/9781446219942',
                      'Council of Europe. (1994). 4ème Conférence Ministérielle européenne sur la politique des communications
de masse. Prague, 7–8 décembre 1994. “Les média dans une société démocratique” [4th
                      European Ministerial Conference on Mass Media Policy. Prague, 1994, 7th–8th December: The
                      media in a democratic society]. MCM (94) 5.',
                      'Mouffe, Chantal (2000) The Democratic Paradox. London: Verso.'
                      )
graph_citations_plu = cbind(graph_citations_test_plu, citation_list_plu)
graph_citations_plu = graph_citations_plu %>% select(ID = ID, year = year, empirical = empirical, author = author, doi = doi, title = title, citation_round = citation_round, citation = citation, all_citations = citation_list_plu)

graph_citations = rbind(graph_citations, graph_citations_plu)

number_citations = graph_citations %>% group_by(ID) %>% summarize(n = n())
prop.table(table(sort(number_citations$n)))
graph_citations %>% group_by(all_citations)

#graph_citations_filech = theory_empirical %>% select(ID, def_yn, defcit, year, empirical, loop_concept, author, title_full, doi) %>% filter(def_yn == 'Yes') %>% filter(loop_concept == 'ech'|loop_concept == 'fil') %>% group_by(ID) %>% summarise(defcit = first(defcit), year = first(year), empirical = first(empirical), author = first(author), doi = first(doi), title = first(title_full)) %>% arrange(desc(empirical))
#graph_citations_filech = graph_citations_filech %>% filter(!is.na(defcit)) %>% filter(defcit != '') %>% separate(defcit, c("a", "b", "c", "d", "e", "f", "g", 'h', 'i', 'j', 'k', 'l', 'm'), "; |\\n|;")
#graph_citations_test_filech = graph_citations_filech %>%
  #gather(key = 'citation_round', value = 'citation', a:m)
#graph_citations_test_filech = graph_citations_test_filech %>% filter(!is.na(citation)) %>% filter(citation != "") %>% arrange(ID, citation_round, desc(empirical))
#graph_citations_test_filech[2,]$citation = "Pariser (2011)"
#graph_citations_test_filech[25,] = graph_citations_test_filech[2,]
#graph_citations_test_filech[26,] = graph_citations_test_filech[2,]
#graph_citations_test_filech[25,]$citation = 'Sunstein (2017)'
#graph_citations_test_filech[26,]$citation = 'Sunstein (2001)'
#graph_citations_test_filech[25,]$citation_round = 'b'
#graph_citations_test_filech[26,]$citation_round = 'c'
#graph_citations_test_filech[12,]$citation = "Sunstein (2001)"
#graph_citations_test_filech[27,] = graph_citations_test_filech[12,]
#graph_citations_test_filech[27,]$citation = "Sunstein (2006)"
#graph_citations_test_filech[27,]$citation_round = "e"
#graph_citations_test_filech = graph_citations_test_filech %>% arrange(desc(empirical), ID, citation_round)
#citation_list_filech = c("https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.3139/9783446431164",
#                         'Sunstein, Cass. 2017. #Republic: Divided Democracy in the Age of Social Media. Princeton, NJ:Princeton University Press.', 
#                         'Sunstein, Cass. 2001. Echo Chambers: Bush v. Gore, Impeachment, and Beyond. Princeton, NJ:Princeton University Press.',
#                         "https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.3139/9783446431164",
#                         "Negroponte N (1996) Being Digital. London: Coronet Books.",
#                         "Sunstein CR (2006) Infotopia: How Many Minds Produce Knowledge. Oxford: Oxford University Press.",
#                         "https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.3139/9783446431164",
#                          "https://doi.org/10.3139/9783446431164",
#                         'Sunstein, Cass. 2001. Echo Chambers: Bush v. Gore, Impeachment, and Beyond. Princeton, NJ:Princeton University Press.',
#                         'Morozov, E. (2011). The net delusion: The dark side of Internet freedom. Philadelphia, PA: Public Affairs.',
#                         "https://doi.org/10.3139/9783446431164",
#                         "Shirky, C. (2003, February 10). Power laws, weblogs, and inequality. Clay Shirky’s Writings about the Internet: Economics & Culture, Media & Community. Retrieved from http://www.shirky.com/writings/powerlaw_weblog.html",
#                         "Sunstein CR (2006) Infotopia: How Many Minds Produce Knowledge. Oxford: Oxford University Press.",
#                         "Sunstein, C. R. (2009). Republic.com 2.0. Princeton University Press, Princeton, NJ.",
#                         "https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.1145/1134271.1134277",
#                         "https://doi.org/10.1109/asonam.2012.32",
#                         "https://doi.org/10.1111/1467-9760.00148",
#                         'Sunstein, Cass. 2001. Echo Chambers: Bush v. Gore, Impeachment, and Beyond. Princeton, NJ:Princeton University Press.',
#                         "https://doi.org/10.3139/9783446431164",
#                         'Sunstein, Cass. 2001. Echo Chambers: Bush v. Gore, Impeachment, and Beyond. Princeton, NJ:Princeton University Press.',
#                         "https://doi.org/10.3139/9783446431164",
#                         "https://doi.org/10.3139/9783446431164")

#graph_citations_filech = cbind(graph_citations_test_filech, citation_list_filech)
#graph_citations_filech = graph_citations_filech %>% select(ID = ID, year = year, empirical = empirical, author = author, doi = doi, title = title, citation_round = citation_round, citation = citation, all_citations = citation_list_filech)

#graph_citations = rbind(graph_citations, graph_citations_filech)

relationships = graph_citations %>% select(ID, cited = all_citations)
actors = graph_citations %>% select(ID = ID, year = year, empirical = empirical, orig_author = author, doi = doi) %>% group_by(ID) %>% summarize(year = first(year), empirical = first(empirical), orig_author = first(orig_author), doi = first(doi))
actors$ID
fill = rep('NA', each=300)
new_dataframe = data.frame(ID = graph_citations$all_citations, year = fill, empirical = fill, orig_author = fill, doi = fill) %>% group_by(ID) %>% summarize(year = first(year), empirical = first(empirical), orig_author = first(orig_author), doi = first(doi))
actors = rbind(actors, new_dataframe)


relationships_div = graph_citations_div %>% select(ID, cited = all_citations)
actors_div = graph_citations_div %>% select(ID = ID, year = year, empirical = empirical, orig_author = author, doi = doi) %>% group_by(ID) %>% summarize(year = first(year), empirical = first(empirical), orig_author = first(orig_author), doi = first(doi))
fill = rep('NA', each=264)
new_dataframe_div = data.frame(ID = graph_citations_div$all_citations, year = fill, empirical = fill, orig_author = fill, doi = fill) %>% group_by(ID) %>% summarize(year = first(year), empirical = first(empirical), orig_author = first(orig_author), doi = first(doi))
actors_div = rbind(actors_div, new_dataframe_div)



library(igraph)
library(rcrossref)
library(bib2df)
df <- bib2df(Parthasarathi)
actors$doi
actors$bibtex = cr_cn(dois = actors$doi, format = 'bibtex')
cr_cn(dois = "10.1177/1329878x1415200109", format = "bibtex")



g1 <- graph_from_data_frame(relationships_div, directed=TRUE, vertices=actors_div)
print(g1, e=TRUE, v=TRUE)
is.simple(g1)
g1 <- simplify(g1, remove.loops = FALSE)
vcount(g1)
ecount(g1)
is.directed(g1)
graph.density(g1)
L <- layout_nicely(g1)
A = layout.fruchterman.reingold(g1)
C= layout.davidson.harel(g1)
D = layout.graphopt(g1, spring.length = 5, spring.constant = 0.5)
indeg=degree(g1, mode="in")
outdeg=degree(g1, mode="out")
ln1 = order(order(indeg, decreasing = T))
names(ln1) = names(indeg)
V(g1)$label <- ln1

                                                                                                                                                                                                                                                                                   
plot(g1, layout=L, vertex.size = outdeg, vertex.label=NA, edge.arrow.size = 0.2, edge.color = 'grey', vertex.color = 'blue')


g <- graph_from_data_frame(relationships, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
is.simple(g)
g <- simplify(g, remove.loops = FALSE)
vcount(g)
ecount(g)
is.directed(g)
graph.density(g)
L <- layout_nicely(g)
A = layout.fruchterman.reingold(g)
C= layout.davidson.harel(g)
D = layout.graphopt(g, spring.length = 5, spring.constant = 0.5)
indeg=degree(g, mode="in")
outdeg=degree(g, mode="out")
ln1 = order(order(indeg, decreasing = T))
names(ln1) = names(indeg)
V(g)$label <- ln1
sort(indeg)
sort(decreasing = TRUE, ln1)
plot(g, layout=L, vertex.size = indeg, vertex.label = ifelse(degree(g, mode="in") > 4, V(g)$label, NA), vertex.label.color = 'black', vertex.frame.color="black", vertex.label.cex = 0.7, edge.arrow.size = 0.2, edge.color = 'grey', vertex.color = adjustcolor("white", alpha.f = 0.8), rescale = TRUE, asp = 0.8, xlim = c(-0.7,0.7), ylim = c(-0.7,0.7),  vertex.label.font = 2)
plot(g, layout=L, vertex.size = outdeg, vertex.label=NA, edge.arrow.size = 0.2, edge.color = 'grey', vertex.color = 'blue')


graph_citations = theory_empirical %>% select(ID, def_yn, defcit, year, empirical, loop_concept, author, title_full, doi) %>% filter(def_yn == 'Yes') %>% filter(loop_concept == 'div'|loop_concept == 'plu') %>% group_by(ID)
graph_citations %>% filter(all_citations == 'https://doi.org/10.1111/j.1460-2466.1999.tb02815.x') %>% arrange(empirical)
graph_citations %>% filter(all_citations == 'McQuail, D. (1992). Media performance: Mass communication and the public interest. Sage.') %>% arrange(empirical)
graph_citations %>% filter(all_citations == 'https://doi.org/10.1177/107769909607300306') %>% arrange(empirical)
graph_citations %>% filter(all_citations == 'https://doi.org/10.5325/jinfopoli.1.2011.0246')

#-----------------------------------VISUALIZATIONS: STACKED BAR GRAPHS---------------------------------------------------------------------------
theory_empirical = theory_empirical %>% mutate(dimension = case_when(actor_TEXT != '' & empirical == 'No' ~ "actor", 
                                                                                viewpoint_TEXT != '' & empirical == 'No' ~ "viewpoint",
                                                                                topic_TEXT != '' & empirical == 'No' ~ "topic",
                                                                                outlet_TEXT != ''& empirical == 'No' ~ "outlet",
                                                                                other_TEXT != ''& empirical == 'No' ~ "other",
                                                                                empirical == 'No' & dim1 != 'outlet' & dim2 != 'outlet' & dim3 != 'outlet' & dim4 != 'outlet' ~ "No dimensions",
                                                                                empirical == 'No' ~ "outlet",
                                                                                empirical == 'Yes' ~ dimension))
theory_empirical$dimension

test = theory_empirical %>% group_by(ID, loop_concept, dimension) %>% summarize(actor = paste(actor_TEXT, collapse = ""), viewpoint = paste(viewpoint_TEXT, collapse = ""), 
                                                                topic = paste(topic_TEXT, collapse = ""), other = paste(other_TEXT, collapse = ""), outlet = paste(outlet_TEXT, collapse = ""), author = first(author), year = first(year))
test = test %>% filter(!is.na(dimension))
all_dimensions = test %>% group_by(ID) %>% summarize(dimension = paste(dimension, collapse = " "), author = first(author), year = first(year), actor = first(actor), viewpoint = first(viewpoint), 
                                                                                             topic = first(topic), other = first(other), outlet = first(outlet)
                                                                                             )
test$dim1
colnames(empirical_data)
test$dimension = as.factor(test$dimension)
test$dimension = factor(test$dimension , levels = c("viewpoint", "actor", "topic", "outlet", "other", "No dimensions"))
test$loop_concept = recode(test$loop_concept, ech = 'ech_fil', fil = 'ech_fil', ser = 'oth')
g = ggplot(test, aes(dimension))
g + geom_bar(aes(fill = test$loop_concept)) +
  scale_fill_grey(start = 0.2, end = 0.7) + 
  theme_bw() + 
  labs(fill = 'Concept', x = 'Dimension', y = 'Number of articles')


test_level = theory_empirical %>% group_by(ID, loop_concept, level) %>% summarize(author = first(author), year = first(year))
test_level$level = as.factor(test_level$level)
top_categories = c('media_gen', 'media_2sec', 'media_out', 'media_mark', 'media_1sec')
levels(test_level$level)[which(!levels(test_level$level)%in%top_categories)] <- "other"
test_level$loop_concept = recode(test_level$loop_concept, ech = 'ech_fil', fil = 'ech_fil', ser = 'oth')
test_level$level = factor(test_level$level, levels = c('media_gen', 'media_mark', 'media_2sec', 'media_1sec', 'media_out', 'other'))
g = ggplot(test_level, aes(level))
g + geom_bar(aes(fill = test_level$loop_concept)) + 
  scale_fill_grey(start = 0.1, end = 0.7) + 
  theme_bw() + 
  labs(fill = 'Concept', x = 'Level', y = 'Number of articles') + 
  scale_x_discrete(breaks=c('media_gen', 'media_mark', 'media_2sec', 'media_1sec', 'media_out', 'other'),
                     labels=c("General Media", "Media Market", "Media 2 Sectors", 'Media 1 Sector', 'Media Outlet', 'Other'))

test_level
test_level_grouped = test_level %>% group_by(ID) %>% summarize(level = paste(level, collapse=" "), author = first(author), year = first(year))
colnames(empirical_data)
theory_empirical$actor_TEXT
sort(table(empirical_data$method))


test_method = empirical_data %>% group_by(ID) %>% filter(!is.na(method)) %>% mutate(method = case_when(
  method == 'Content Analysis (quantitative, manual or (semi-)annotated )' ~ "Content analysis",
  method == 'Survey' ~ 'Survey',
  method == 'Qualitative Approach' ~ 'Qualitative',
  ID %in% methods_b$ID ~ 'Market analysis',
  ID %in% methods_c$ID ~ 'Linguistic analysis',
  method != "" ~ 'Other'
)) %>% summarize(method = first(method), author = first(author), year = first(year))
test_method
g = ggplot(test_method, aes(method))
g + geom_bar()

test = theory_empirical %>% group_by(ID) %>% summarize(author = first(author), year = first(year))
sort(test$author)
test$ID
test$author

write.csv(theory_empirical, 'dataset.csv')


test_method = theory_empirical %>% group_by(ID) %>% filter(!is.na(method)) %>% mutate(method = case_when(
  method == 'Content Analysis (quantitative, manual or (semi-)annotated )' ~ "Content analysis",
  method == 'Survey' ~ 'Survey',
  method == 'Qualitative Approach' ~ 'Qualitative',
  ID %in% methods_b$ID ~ 'Market analysis',
  ID %in% methods_c$ID ~ 'Linguistic analysis',
  method != "" ~ 'Other'
)) %>% summarize(method = first(method), author = first(author), year = first(year))

theory_empirical_new = theory_empirical %>% group_by(ID) %>% summarize(method = paste(method, collapse=" "), author = first(author), year = first(year), autom = first(autom))
methods_a = theory_empirical_new %>% filter(grepl("another|secondary|already includes|not mentioned",tolower(method)))
methods_b = theory_empirical_new %>% filter(grepl("economic|market",tolower(method)))
methods_c = theory_empirical_new %>% filter(grepl("linguistic|entity mining|topic probabilit|lda|cosine|similarity|recommend",tolower(method)))
methods_d= theory_empirical_new %>% filter(grepl("content analysis \\(quantitative",tolower(method)))
methods_e= theory_empirical_new %>% filter(grepl("survey",tolower(method)))
methods_f= theory_empirical_new %>% filter(grepl("qualitative|semi-structured",tolower(method)))
methods_g= theory_empirical_new %>% filter(grepl("simulation",tolower(method)))
methods_h= theory_empirical_new %>% filter(grepl("experiment",tolower(method)))
methods_i= theory_empirical_new %>% filter(grepl("network",tolower(method)))

rest = theory_empirical %>%
  filter(!(ID %in% methods_a$ID) & !(ID %in% methods_b$ID) & !(ID %in% methods_c$ID) & !(ID %in% methods_d$ID) & !(ID %in% methods_e$ID) & !(ID %in% methods_f$ID) & !(ID %in% methods_g$ID) & !(ID %in% methods_h$ID) & !(ID %in% methods_i$ID) & (empirical == 'Yes')) 
methods_a %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_b %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_c %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
content_analysis = methods_d %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_e %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_f %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_g %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_h %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
methods_i %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))
rest %>% group_by(ID) %>% summarize(method = first(method), author = first(author), year = first(year), autom = first(autom))

content_analysis

author_list = theory_empirical %>% select(ID, author, year)

actor_div_a = all_dimensions %>% filter(grepl("actor|person|group|companies|entit|part",tolower(actor)))
actor_div_b = all_dimensions %>% filter(grepl("source",tolower(actor)))
actor_div_c = all_dimensions %>% filter(grepl("voice",tolower(actor)))
actor_div_d = all_dimensions %>% filter(grepl('gender|minority', tolower(actor)))
actor_other = actor_div %>%
  filter(!(ID %in% actor_div_a$ID) & !(ID %in% actor_div_b$ID) & !(ID %in% actor_div_c$ID) & !(ID %in% actor_div_d$ID))
actor_all = all_dimensions %>% select(ID, year, author, actor)
issue_all = all_dimensions %>% select(ID, year, author, topic)
viewpoint_all = all_dimensions %>% select(ID, year, author, viewpoint)
outlet_all = all_dimensions %>% select(ID, year, author, outlet)

colnames(theory_empirical)
all_pluralism_def = theory_empirical %>% filter(loop_concept == 'plu' & def_yn == 'Yes') %>% group_by(ID) %>% summarize(def = first(def), author = first(author), outlet = first(outlet), year = first(year), empirical = first(empirical))

ids = theory_empirical %>% group_by(ID) %>% summarize() %>% arrange(ID) %>% select(ID)
write.csv(ids, 'ids.csv')
write.csv(definitions, 'definitions_ids.csv')

colnames(theory_empirical)
normative_democracy = read.csv('normative_democracy_mentioned_cleaned.csv', sep = ';')
theory_empirical_normative = theory_empirical
theory_empirical_normative$ID = gsub(".nl", "", theory_empirical_normative$ID)
theory_empirical_normative = theory_empirical_normative %>% filter(ID %in% normative_democracy$docname)
%>% group_by(ID) %>% summarize(empirical = first(empirical), dimension = paste(dimension, collapse = ', ') )
table(theory_empirical_normative$empirical)
normative_democracy$length <- nchar(as.character(normative_democracy$pre_post))
normative_democracy_sorted = normative_democracy %>% arrange(length)
write.csv(normative_democracy_sorted, 'normative_democracy_sorted.csv')
graph_citations$all_citations
