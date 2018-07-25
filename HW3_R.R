library(engsoccerdata)
library(ggplot2)
library(dplyr)
library(highcharter)
data(package = "engsoccerdata")
fdb = as.tbl(spain)


#Q1
fdb %>% select(Date, team = home, opp = visitor, Season, MG = hgoal, OG = vgoal, tier, HT) -> one
fdb %>% select(Date, team = visitor, opp = home, Season, MG = vgoal, OG = hgoal, tier, HT) -> two
q1 = rbind(one, two)
q1 %>% group_by(team) %>% summarise(s = sum(MG > OG)) -> q11
q11 %>% arrange(s) -> q11
q11 %>% hchart(type = "column", hcaes(x = team, y = s), name = "winned games")
ggplot(data = q11, aes(x = team, y = s)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = -75, vjust = 0.5))

#Q2
## the most boring league
q1 %>% 
  filter(MG == 0 & OG == 0) %>% 
  group_by(Season) %>% summarise(count = n()) %>% arrange(desc(count)) -> q2

most_boring_league = q2[1, 1]
## the most boring team
q1 %>% group_by(team) %>% mutate(total = n()) -> q1
q1 %>% 
  filter(MG == 0, OG == 0) %>% 
  group_by(team) %>% summarise(count = n()/mean(total)) %>% arrange(desc(count))->q22
most_boring_team = q22$team[1]
## 10 most boring leagues
q2[1:10, ] %>% hchart(type = "bar", hcaes(x = Season, y = count), name = "boring games")
ggplot(data = q2[1:10, ], aes(x = Season, y = count, colour = "pink")) + geom_bar(stat = "identity")
## 10 most boring teams
q22[1:10, ] %>% hchart(type = "bar", hcaes(x = team, y = count), name = "boring games") 
ggplot(data = q22[1:10, ], aes(x = team, y = count, colour = "pink")) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = -75, vjust = 0.5))+ coord_flip() 


#Q3

#####
q1 %>% 
  mutate(GD = MG - OG) %>% 
  group_by(team) %>% 
  arrange(team, Date) %>% 
  mutate(W = ifelse(GD > 0,1,0),
         D = ifelse(GD == 0,1,0),
         L = ifelse(GD < 0,1,0)) %>% group_by(team) %>% 
  mutate(play = row_number(Season)) -> season
season %>% group_by(team) %>% 
  mutate(goalsF = cumsum(MG),
         goalsA = cumsum(OG),
         goaldif = cumsum(GD),
         W = cumsum(W),
         D = cumsum(D),
         L = cumsum(L)
  ) %>% 
  mutate(score = W*3 + D) %>% arrange(team,play) -> thistory
thistory$Date = substr(as.character(thistory$Date), 1, 7)

thistory %>% select(team, score, Date, Season) %>% group_by(Date, team) %>% summarise(score = max(score), Season = mean(Season))->  thistory
thistory %>% group_by(Date) %>% mutate(rank = row_number(-score)) %>% arrange(Date, rank) ->f

f %>% group_by(Season) %>% mutate(count = n(), satr = row_number(Season), remainders = count - satr) ->f1
### ghahremane nim fasl
f1 %>% filter(satr <= remainders)-> y
y %>% group_by(Season) %>% summarise(nimche_ghahreman = team[which.max(score)], score = max(score)) -> nimche
### ghahremane fasl
f1 %>% group_by(Season) %>% summarise(ghahreman = team[which.max(score)], score= max(score)) -> ghahreman


a = merge(nimche, ghahreman, by = "Season", all = TRUE)


a %>% summarise(darsad = 100*sum(ghahreman == nimche_ghahreman)/n()) -> answer
print(answer)


#Q4
q1 %>% group_by(team) %>% summarise(s = sum(MG > OG), n = n()) %>% mutate(shakhi = 100*s/n) %>% arrange(desc(shakhi))-> q4
#View(q4)
best = q4[1:20, ]

q1 %>% filter(Season>2001 & Season<2010) %>% filter(team %in% best$team) %>% mutate(GD = MG-OG) %>% filter(GD <= 0)->q4
View(q4)

q4 %>% group_by(team, opp) %>% summarise(count = n()) -> q44
#View(q44)
q44 %>% group_by(team) %>% arrange(team, desc(count)) ->test

test %>% group_by(team) %>% summarise(gorbe = opp[1]) -> q444
View(q444)

#Q5 

####
q1 %>% 
  mutate(GD = MG - OG) %>% 
  group_by(team) %>% 
  arrange(team, Date) %>% 
  mutate(W = ifelse(GD > 0,1,0),
         D = ifelse(GD == 0,1,0),
         L = ifelse(GD < 0,1,0)) %>% group_by(team) %>% 
  mutate(play = row_number(Season)) -> season
season %>% group_by(team) %>% 
  mutate(goalsF = cumsum(MG),
         goalsA = cumsum(OG),
         goaldif = cumsum(GD),
         W = cumsum(W),
         D = cumsum(D),
         L = cumsum(L)
  ) %>% 
  mutate(score = W*3 + D) %>% arrange(team,play) -> thistory
thistory$Date = substr(as.character(thistory$Date), 1, 7)

thistory %>% select(team, score, Date, Season) %>% group_by(Date, team) %>% summarise(score = max(score), Season = mean(Season))->  thistory
thistory %>% group_by(Date) %>% mutate(rank = row_number(-score)) %>% arrange(Date, rank) ->f

f %>% group_by(Season) %>% mutate(count = n(), satr = row_number(Season), remainders = count - satr) ->f1
####
f1 %>% group_by(Date) %>% filter(rank <= 2) %>% mutate(a = score - lead(score)) -> f
f %>% mutate(b = ifelse(a > 3*remainders,1, 0)) -> f
f %>% filter(b == 1) ->f2
print(f2$team[which.max(f2$remainders)])
# teami ra moshakhas kardim ke bishtarin zoodtar az hame bordesh moshakhas shode(hanuz kolli bazi moonde boodan!)

  
##moghtaderane tarin ghahremani(winning with the highest difference with the second team)

#####
f1 %>% filter(rank <= 2) %>% group_by(Season) %>% filter(remainders < min(remainders) + 2) %>%  mutate(tafazol = score - lead(score)) -> teee
teee %>% arrange(desc(tafazol)) ->q55
print(q55$team[1])

# Q6
q1 %>% 
  mutate(GD = MG - OG) %>% 
  group_by(team) %>% 
  arrange(team, Date) %>% 
  mutate(W = ifelse(GD > 0,1,0),
         D = ifelse(GD == 0,1,0),
         L = ifelse(GD < 0,1,0)) %>% mutate(bord1 = 0, bakht = 0, mosavi = 0) -> M
for(c in 1:20){
M$bord1 <- with(M, ifelse(W == 1, ifelse(W-lag(W) > 0, 1, ifelse(team == lag(team) ,lag(bord1, default = 0) + 1 , 1)), 0 ) )
}

for(c in 1:20){
  M$bakht <- with(M, ifelse(L == 1, ifelse(L-lag(L) > 0, 1, ifelse(team == lag(team) ,lag(bakht, default = 0) + 1 , 1)), 0 ) )
}

for(c in 1:20){
  M$mosavi <- with(M, ifelse(D == 1, ifelse(D-lag(D) > 0, 1, ifelse(team == lag(team) ,lag(mosavi, default = 0) + 1 , 1)), 0 ) )
}

View(M)
M %>% filter(bord1 == max(M$bord1)) -> Toolanitarin_navare_piruzi
M %>% filter(bakht == max(M$bakht, na.rm = T)) -> Toolanitarin_navare_bakht
M %>% filter(mosavi == max(M$mosavi, na.rm = T)) -> Toolanitarin_navare_mosavi


Toolanitarin_navare_piruzi_teams = Toolanitarin_navare_piruzi$team
Toolanitarin_navare_bakht_teams = Toolanitarin_navare_bakht$team
Toolanitarin_navare_mosavi_teams = Toolanitarin_navare_mosavi$team

#Q7
f1 %>% group_by(Date) %>% filter(rank == (max(rank) - 3)| rank ==(max(rank) - 4)) %>% mutate(a = score - lead(score)) -> ff
ff %>% mutate(b = ifelse(a > 3*remainders,1, 0)) -> ff
ff %>% filter(b == 1) ->f22
print(f22$team[which.max(f22$remainders)])


#Q8

###########
q1 %>% filter(Season == 1998) %>% arrange(Date) %>% mutate(GD = MG - OG) -> q8
q8 %>% 
  mutate(GD = MG - OG) %>% 
  group_by(team) %>% 
  arrange(team, Date) %>% 
  mutate(W = ifelse(GD > 0,1,0),
         D = ifelse(GD == 0,1,0),
         L = ifelse(GD < 0,1,0)) %>% group_by(team) %>% 
  mutate(play = row_number(Season)) -> season
season %>% group_by(team) %>% 
  mutate(goalsF = cumsum(MG),
         goalsA = cumsum(OG),
         goaldif = cumsum(GD),
         W = cumsum(W),
         D = cumsum(D),
         L = cumsum(L)
  ) %>% 
  mutate(score = W*3 + D) %>% arrange(team,play) -> thistory
thistory %>% filter(team == "Real Madrid" | team == "Atletico Madrid" | team == "Athletic Bilbao" | team == "Espanyol Barcelona" | team == "FC Barcelona" | team == "Real Valladolid " | team == "Deportivo La Coruna " | team =="CD Tenerife"| team == "RCD Mallorca" | team == "Villarreal CF " | team == "CD Alves" ) -> this

this$Date = substr(as.character(this$Date), 1, 7)

this %>% select(team, score, Date, Season) %>% group_by(Date, team) %>% summarise(score = max(score), Season = mean(Season))->  this

this %>% group_by(Date) %>% mutate(rank = row_number(-score)) %>% arrange(Date, rank) ->f

f %>% hchart(type = "line", hcaes(x = Date, y = rank, group = team))


# Q9
ggplot(data = q1, aes(team, opp)) + 
  geom_tile(aes(fill = HT)) + 
  geom_text(aes(label = HT))

p = ggplot(data = fdb[1:10,], aes(x = home, y = visitor)) 
p + geom_tile(aes(x = home, y = visitor, fill = FT)) + geom_text(aes(label = FT)) + theme(axis.text.x = element_text(angle = -90, vjust = 0.5))

# Q10
## amareye aval: chegalie borde 5 teame aval
q1 %>% 
  filter(team == "Real Madrid" | team == "FC Barcelona" | team == "Atletico Madrid" | team == "Valencia CF" | team == "Athletic Bilbao" ) %>% 
  group_by(team) %>% summarise(winning_density  = sum(MG > OG)/n()) -> q10
q10 %>% hchart(type = "pie", hcaes(x = team, y = winning_density), name = "winning_density") %>% 
  hc_add_theme(hc_theme_db())
ggplot(data = q10, aes(x = team, y = winning_density)) + geom_bar(stat = "identity") + coord_flip()
## amareye dovom: kola bord haye khane/kole bord ha
q1 %>% 
  filter(team == "Real Madrid" | team == "FC Barcelona" | team == "Atletico Madrid" | team == "Valencia CF" | team == "Athletic Bilbao" ) %>% 
  group_by(team) %>% summarise(total_wins = sum(MG>OG)) -> bordha
fdb %>%
  filter(home == "Real Madrid" | home == "FC Barcelona" | home == "Atletico Madrid" | home == "Valencia CF" | home == "Athletic Bilbao" ) %>% 
  group_by(home) %>% 
  summarise(home_wins = sum(hgoal>vgoal)) -> khangei
colnames(khangei) <- c("team", "khanegi")
p = merge(bordha, khangei, by = "team", all = TRUE)
p %>% mutate(darsad = khanegi/total_wins) -> p
p %>% hchart(type = "column", hcaes(x = team, y = 100*darsad), name = "chance of winning at home if winning")
ggplot(data = p, aes(x = team, y = 100*darsad)) + geom_bar(stat = "identity") + coord_flip()

## amareye sevom : ehtemale borde har team 
p1 = merge(p, q10, by = "team", all = TRUE)

p1 %>% mutate(chance_of_win_home = 2*darsad*winning_density, chance_of_win_nonhome = 2*(1-darsad)*winning_density) -> p1 
p1 %>% hchart(type = "column",hcaes(x = team, y = 100*chance_of_win_home),name = "chance of winning at home in any play")
ggplot(data = p1, aes(x = team, y = 100*chance_of_win_home)) + geom_bar(stat = "identity") + coord_flip()









