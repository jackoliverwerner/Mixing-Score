############
# Preamble #
############

library(plyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/jack.werner1/Documents/BB")
#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Scraping Files")

# Read data
pitch <- read.csv(file = "pitch_data_2016.csv")

####################
# Reference tables #
####################

# At-bat results
simpleResults <- data.frame(event = as.character(sort(unique(pitch$event))),
                            simple_event = c("Out", "Out", "Out", "Out", "HBP",
                                             "Hit", "Out", "Hit", "Out", "Out",
                                             "Out", "Out", "Out", "Out", "Out",
                                             "HBP", "Hit", "BB", "Out", "Out",
                                             "Out", "Out", "Out", "Out", "Out",
                                             "Hit", "K", "K", "Hit", "Out", "BB"),
                            stringsAsFactors = F)

# Pitch classifications
simplePitches <- data.frame(pitch_type = sort(as.character(unique(pitch$pitch_type))),
                            simple_pitch_type = c("UN", "UN",  "CH", "CU", "CH", "FC", "FF", 
                                                  "PO", "SI", "FT", "UN", "CU", "KN", "PO",
                                                  "UN", "SI", "SL", "UN"),
                            fastball = c("UN", "UN", "O", "O", "O", "F", "F", "O", "F",
                                         "F", "UN", "O", "O", "O", "UN", "F", "O", "UN")
)

# Player names/IDs
pitcher_names <- read.csv("playerid_list.csv") %>%
  mutate(name = paste0(FIRSTNAME, " ", LASTNAME), id = MLBCODE) %>%
  select(name, id)


######################
# Manipulate dataset #
######################

# Add Simple Event, Simple Pitch Type, Fastball, Player Names
pitch_2 <- pitch %>% left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
pitch_3 <- pitch_2 %>% 
  mutate(hand_match = b_hand == p_throws) %>%     # Handedness match
  group_by(gid, ab_num) %>%
  mutate(finalCount = paste0(b, "-", s),     # Count on last pitch
         last = row_number() == n(),
         next_balls = pmin(cumsum(type == "B"), 3), next_strikes = pmin(cumsum(type == "S"), 2),
         next_count = ifelse(last, simple_event, paste0(next_balls, "-", next_strikes)),
         count = lag(as.character(next_count), default = "0-0"),
         balls = lag(as.character(next_balls), default = "0"),
         strikes = lag(as.character(next_strikes), default = "0")) %>% 
  ungroup()


pbk <- pitch_3 %>% select(pitcher, pitcher_name, count, balls, strikes, p_throws, hand_match, pitch_type, 
                          type, fastball, simple_pitch_type)


############
# Fastball #
############

# Group by pitcher and count
fastballs <- pbk %>% filter(simple_pitch_type != "UN", simple_pitch_type != "PO") %>%
  group_by(pitcher) %>%
  mutate(sinker = sum(simple_pitch_type == "SI")/n()>.1,
        twoseam = sum(simple_pitch_type == "FT")/n()>.1,
        sinkfast = sum(simple_pitch_type == "FS")/n()>.1,
        cutter = sum(simple_pitch_type == "FC")/n()>.1,
        fourseam = sum(simple_pitch_type == "FF")/n()>.1,
        diffs = sinker + twoseam + sinkfast + cutter + fourseam,
        num_total = n(),
        ff_p_total = sum(fastball == "F")/n()) %>%
  ungroup() %>%
  group_by(pitcher, pitcher_name, count, balls, strikes) %>%
  summarize(ff_p = sum(fastball == "F")/n(), num_count = n(),
            sinker = mean(sinker),
            twoseam = mean(twoseam),
            sinkfast = mean(sinkfast),
            cutter = mean(cutter),
            fourseam = mean(fourseam),
            diffs = mean(diffs),
            num_total = mean(num_total),
            ff_p_total = mean(ff_p_total)) %>%
  ungroup()


# Explore
# a <- fastballs %>% group_by(pitcher, pitcher_name) %>% 
#   summarize(diffs = mean(diffs), num_total = mean(num_total), ff_p_total = mean(ff_p_total)) %>% 
#   ungroup()
# 
# table(a$diffs)
# a %>% filter(diffs == 0)
# 
# 
# ggplot(data = a, aes(x = num_total, y = ff_p_total)) + geom_point()
# 
# ggplot(data = filter(a, num_total > 1500), aes(x = num_total, y = ff_p_total, color = factor(diffs))) + geom_point()
# 
# ggplot(data = filter(a, num_total > 1500), aes(x = num_total, y = ff_p_total)) + 
#   facet_grid(.~diffs) +
#   geom_point()


# Difference from overall mean
leaguewide_counts <- fastballs %>% group_by(count) %>% summarize(total = sum(num_count)) %>% ungroup() %>%
  mutate(freq = total/sum(total)) %>% select(-total)

fastballs_2 <- fastballs  %>% 
  left_join(leaguewide_counts, by = "count") %>% 
  group_by(pitcher, pitcher_name) %>% 
  mutate(ff_p_total_adj = sum(ff_p*freq)) %>%
  filter(sum(num_count) > 1500) %>% 
  ungroup() %>%
  mutate(ff_p_diff = ff_p - ff_p_total_adj,
         weighted_diff = abs(ff_p_diff)*num_count,
         weighted_diff_2 = abs(ff_p_diff)*freq,
         sq_diff = ff_p_diff^2 * num_count)

ggplot(data = fastballs_2, aes(x = num_count, y = ff_p_diff)) + 
  facet_grid(balls~strikes) + geom_point()

ggplot(data = fastballs_2, aes(x = num_count, y = ff_p_diff, color = balls)) + geom_point()
ggplot(data = fastballs_2, aes(x = num_count, y = ff_p_diff, color = strikes)) + geom_point()

ggplot(data = filter(fastballs_2, count == "3-0"), aes(x = ff_p_total, y = ff_p)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")




fastballs_scores <- fastballs_2 %>% 
  group_by(pitcher, pitcher_name) %>%
  summarize(mean_abs_diff = sum(weighted_diff)/sum(num_count), 
            sq_diff = sum(sq_diff)/sum(num_count),
            mean_abs_diff_2 = sum(weighted_diff_2),
            ff_p_total = mean(ff_p_total), diffs = mean(diffs)) %>%
  ungroup()
  

# Examine scores

arrange(fastballs_scores, desc(mean_abs_diff))[1:10,] # I like these better
arrange(fastballs_scores, mean_abs_diff)[1:10,] # I like these better

arrange(fastballs_scores, desc(mean_abs_diff_2))[1:10,] # I like these the best
arrange(fastballs_scores, mean_abs_diff_2)[1:10,] # I like these the best

arrange(fastballs_scores, desc(sq_diff))[1:10,]
arrange(fastballs_scores, sq_diff)[1:10,]

ggplot(data = fastballs_scores, aes(x = mean_abs_diff, y = sq_diff)) + geom_point()
ggplot(data = fastballs_scores, aes(x = ff_p_total, y = mean_abs_diff, color = factor(diffs))) + geom_point(size = 3)


#########################
# FF Freq visualization #
#########################

name <- "Clayton Kershaw"
name <- "Johnny Cueto"

table(filter(pbk, pitcher_name == name)$pitch_type)[table(filter(pbk, pitcher_name == name)$pitch_type) > 150]

pitcher <- fastballs_2 %>% filter(pitcher_name == name) %>% as.data.frame()

table.df <- data.frame(point_ind = 1:4,
                       count = rep(pitcher$count, each = 4),
                       balls = rep(as.numeric(pitcher$balls), each = 4), 
                       strikes = rep(as.numeric(pitcher$strikes), each = 4),
                       ff_p = 1 - rep(pitcher$ff_p, each = 4)) %>%
  mutate(x = ifelse(point_ind <= 2, strikes, strikes + 1),
         y = ifelse(point_ind %in% c(2, 3), balls, balls + 1))

labels.df <- data.frame(x = as.numeric(pitcher$strikes) + .5,
                        y = as.numeric(pitcher$balls) + .65,
                        txt = paste0(round(100 - pitcher$ff_p*100), "%"),
                        ff_p = 1)

labels.df.2 <- data.frame(x = as.numeric(pitcher$strikes) + .5,
                        y = as.numeric(pitcher$balls) + .35,
                        txt = paste0("(", pitcher$num_count, " pitches)"),
                        ff_p = 1)


labels.df.3 <- data.frame(x = c(-.15, -.15, -.15, -.15, .5, 1.5, 2.5, -.5, 1.5),
                          y = c(.5, 1.5, 2.5, 3.5, -.3, -.3, -.3, 2, -.75),
                          txt = c("0", "1", "2", "3", "0", "1", "2", "Balls", "Strikes"),
                          ff_p = 1,
                          adj = c(0, 0, 0, 0, 0, 0, 0, 90, 0)
)

ggplot(data = table.df, aes(x = x, y = y, fill = ff_p, group = count)) + 
  geom_polygon(color = "grey30", size = .5) +
  coord_cartesian(xlim = c(-.5, 3), ylim = c(-1, 4)) +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", midpoint = .5, limits = c(0, 1)) +
  geom_text(data = labels.df, aes(x = x, y = y, label = txt, group = NA), 
            hjust = "center", size = 4) +
  geom_text(data = labels.df.2, aes(x = x, y = y, label = txt, group = NA), 
            hjust = "center", size = 4) +
  geom_text(data = labels.df.3, aes(x = x, y = y, label = txt, group = NA, angle = adj), size = 7) +
  labs(title = paste0(name, " Offspeed %")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.title = element_text(size=18, face = "bold", hjust = .65))







# Johnny
jc_all <- filter(pbk, pitcher_name == name) %>%
  filter(pitch_type %in% c("CH", "FC", "FF", "FT", "SL")) %>%
  mutate(pitch_type = as.character(pitch_type))

cueto_counts <- table(jc_all$count, jc_all$pitch_type) %>% as.matrix() %>%
  apply(1, function(x){x/sum(x)}) %>% t() %>% as.data.frame()

jc_all <- filter(pbk, pitcher_name == name) %>%
  filter(pitch_type %in% c("FC", "FF", "FT")) %>%
  mutate(pitch_type = as.character(pitch_type))

cueto_counts <- table(jc_all$count, jc_all$pitch_type) %>% as.matrix() %>%
  apply(1, function(x){x/sum(x)}) %>% t() %>% as.data.frame()






