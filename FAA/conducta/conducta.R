################################################################################
###################     Fisiología animal aplicada     #########################
################### Práctica de la conducta de ratones #########################
################################################################################
library(tidyverse)
library(DescTools)
library(readxl)
library(ggthemes)
library(lawstat)
library(survival) ####
library(coin)     #### Permite usar el test de Wilcoxon cuando hay datos repetidos, p valor exacto
library(googledrive)

# Importar de un excel con el data frame

conducta <- read_csv("C:/Users/jcge9/Desktop/FAA/conducta/conducta_ratones.csv")

conducta %>% str()

conducta <- read_excel("conducta.xlsx", col_types = c("numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "text", "text"));view(conducta)

#------------------------------------------------------------------------------#
#                               OPEN FIELD                                     #
#------------------------------------------------------------------------------#

of <- conducta %>% select(starts_with("o_"), "v_j")
of %>% summarise(n = n())

view(of)

###########################
# Normalidad de los datos #
###########################

tapply(of$o_heces, of$v_j, shapiro.test)            # p > 0.05
tapply(of$o_ambext, of$v_j, shapiro.test)           # p > 0.05
tapply(of$o_ambmed, of$v_j, shapiro.test)           # p > 0.05      
tapply(of$o_ambint, of$v_j, shapiro.test)           # p > 0.05
tapply(of$o_grooming, of$v_j, shapiro.test)         # p < 0.05
tapply(of$o_up, of$v_j, shapiro.test)               # p > 0.05



####################
# Prueba de Levene #
####################

    
levene.test(of$o_heces,of$v_j, location = "mean")         # p > 0.05
levene.test(of$o_ambext,of$v_j, location = "mean")        # p > 0.05
levene.test(of$o_ambmed,of$v_j, location = "mean")        # p < 0.05
levene.test(of$o_ambint,of$v_j, location = "mean")        # p > 0.05
levene.test(of$o_grooming,of$v_j)                         # p > 0.05
levene.test(of$o_up,of$v_j, location = "mean")            # p > 0.05

colnames(of)
of
viejos <- of %>% 
  filter(v_j %in% "Viejos") 
var(viejos$o_ambext)
viejos <- of %>% 
  filter(v_j %in% "Jovenes") 


##############################################
# Test parametrico; todos excepto "grooming" #
##############################################

# t-student y Welch para dos grupos (ambulaciones Intermedias)

t.test(o_heces~v_j, data = of, paired = F, var.eq = T)  # p > 0.05
t.test(o_ambext~v_j, data = of, paired = F, var.eq = T) # p > 0.05
t.test(o_ambmed~v_j, data = of, paired = F, var.eq = F) # p > 0.05
t.test(o_ambint~v_j, data = of, paired = F, var.eq = T) # p > 0.05
t.test(o_up~v_j, data = of, paired = F, var.eq = T)     # p > 0.05

# Wilcoxon

library(survival)
library(coin)
wilcox.test(o_grooming~as.factor(v_j), data = of, paired = F) # p > 0.05
wilcox_test(o_grooming~as.factor(v_j), data = of)

# gráficos de las ambulaciones

comp_of_amb <- data.frame(ambulaciones = c(78,73,170,75,79,71,60,68,87,77,54,92,
                                           22,22,12,57,25,13,17,11,11,39,20,11,
                                           2,2,3,6,3,1,2,2,2,4,3,0),
                          tipo = c(rep("amb/ext", 12), rep("amb/Int", 12), rep("amb/int", 12)),
                          v_j = c("Viejos","Viejos","Jovenes","Jovenes",
                                  rep("Viejos", 4), rep("Jovenes", 4))); view(comp_of_amb)

# Gráfico de ambulaciones
amb_ext <- filter(comp_of_amb, tipo %in% "amb/ext")
levene.test(amb_ext$ambulaciones, amb_ext$v_j, location = "mean")

comp_of_amb %>% 
  group_by(v_j, tipo) %>% 
  mutate(media1 = mean(ambulaciones),
            sd1 = sd(ambulaciones)) %>% 
  ggplot(aes(v_j, media1, fill = tipo)) +
  #geom_jitter(pch = 21, position = position_jitterdodge(.1)) +
  #stat_summary(fun = mean, geom = "crossbar", position = position_dodge(.65),
   #            width = .6) +
  geom_bar(stat = "identity", position = position_dodge(0.5), width = .5, col = "black") +
  geom_errorbar(aes(ymin=media1+sd1, ymax=media1-sd1), width = .3, position = position_dodge(.5)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,220),
                     breaks = seq(0,220, 40)) +
  labs(title = "Open field, comparación de ambulaciones",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Ambulaciones",
       x = "Grupo de ratones",
       fill = "Ambulación") +
  theme_tufte() +
  theme(axis.line = element_line(),
        legend.background = element_rect(color = "black"),
        legend.position = c(.9,.7),
        legend.text = element_text(size = 13),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  geom_line(data = tibble(x = c(.9,2.1), y = c(155,155)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 165),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5) +
  scale_fill_manual(values = c("red", "yellow", "blue"))

# Comparación heces

of %>% 
  group_by(v_j) %>% 
  summarise(media2 = mean(o_heces), sd2 = sd(o_heces)) %>% 
  ggplot(aes(v_j, media2, fill = v_j)) +
  geom_errorbar(aes(ymin = media2+sd2, ymax = media2-sd2), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,3),
                     breaks = seq(0,3, .5)) +
  labs(title = "Open Field, nº de defecaciones",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de defecaciones",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(2.2,2.2)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 2.4),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

# Nº de ups

of %>% 
  group_by(v_j) %>% 
  summarise(media3 = mean(o_up), sd3 = sd(o_up)) %>% 
  ggplot(aes(v_j, media3, fill = v_j)) +
  geom_errorbar(aes(ymin = media3+sd3, ymax = media3-sd3), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,80),
                     breaks = seq(0,80, 20)) +
  labs(title = "Open Field, nº de Saltos",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº Saltos",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(55,55)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 60),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)
# Nº Grooming

of %>% 
  group_by(v_j) %>% 
  ggplot(aes(v_j, o_grooming, fill = v_j)) +
  geom_jitter(pch = 21, position = position_jitterdodge(.5, seed = 20101997),
              show.legend = F) +
  geom_boxplot(width = .5, alpha = .7, show.legend = F) +
  labs(title = "Open Field, nº de Grooming",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de grooming",
       x = "Grupo de ratones") +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,2)) +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(8.5,8.5)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 9),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

#------------------------------------------------------------------------------#
#                                  Holes maze                                  #
#------------------------------------------------------------------------------#

hb <- conducta %>% select(starts_with("h_"), "v_j")

######################
# Estudio Normalidad #
######################

view(hb)

tapply(hb$h_cabeza, hb$v_j, shapiro.test)        # p > 0.05
tapply(hb$h_amb, hb$v_j, shapiro.test)           # p > 0.05
tapply(hb$h_t_oler, hb$v_j, shapiro.test)        # p > 0.05
tapply(hb$h_grooming, hb$v_j, shapiro.test)      # No se puede comparar todos lo x son =
tapply(hb$h_up, hb$v_j, shapiro.test)            # p > 0.05

####################
# Prueba de Levene #
####################

levene.test(hb$h_cabeza, hb$v_j, location = "mean")    # p > 0.05
levene.test(hb$h_amb, hb$v_j, location = "mean")       # p > 0.05
levene.test(hb$h_t_oler, hb$v_j, location = "mean")    # p > 0.05
levene.test(hb$h_grooming, hb$v_j, location = "mean")  # p < 0.05
levene.test(hb$h_up, hb$v_j, location = "mean")        # p > 0.05

################
# Test-Student #
################

t.test(h_cabeza~v_j, data = hb, var.equal = T, paired = F)    # p < 0.05
t.test(h_amb~v_j, data = hb, var.equal = T, paired = F)       # p > 0.05
t.test(h_t_oler~v_j, data = hb, var.equal = T, paired = F)    # p > 0.05
t.test(h_grooming~v_j, data = hb, var.equal = F, paired = F)  # p > 0.05
t.test(h_up~v_j, data = hb, var.equal = T, paired = F)        # p > 0.05

############
# Gráficos #
############

hb %>% 
  group_by(v_j) %>% 
  summarise(media = mean(h_cabeza), sd = sd(h_cabeza)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,80),
                     breaks = seq(0,80, 20)) +
  labs(title = "H.B., nº de veces que mete la cabeza",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de veces que mete la cabeza",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(60,60)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 62),
            aes(x = x, y = y),inherit.aes = F, label = "**", size = 10)

hb %>% 
  group_by(v_j) %>% 
  summarise(media = mean(h_amb), sd = sd(h_amb)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,120),
                     breaks = seq(0,120, 20)) +
  labs(title = "H.B., Nº de ambulaciones",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de Ambulaciones",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(80,80)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 85),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

hb %>% 
  group_by(v_j) %>% 
  summarise(media = mean(h_t_oler), sd = sd(h_t_oler)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,70),
                     breaks = seq(0,70, 15)) +
  labs(title = "H.B., Tiempo de olisqueo",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Tiempo (s)",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(55,55)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 60),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

hb %>% 
  group_by(v_j) %>% 
  summarise(media = mean(h_grooming), sd = sd(h_grooming)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  #scale_y_continuous(expand = expansion(0),
   #                  limits = c(0,70),
    #                 breaks = seq(0,70, 15)) +
  labs(title = "H.B., Grooming",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Veces de Grooming",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13)) +
  scale_fill_manual(values = c("red", "blue")) #+
  #geom_line(data = tibble(x = c(1,2), y = c(55,55)),
   #         aes(x = x, y = y),inherit.aes = F) +
  #geom_text(data = tibble(x = 1.5, y = 60),
   #         aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

hb %>% 
  group_by(v_j) %>% 
  summarise(media = mean(h_up), sd = sd(h_up)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                    limits = c(0,45),
                   breaks = seq(0,45, 10)) +
  labs(title = "H.B., Nº de saltos",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de Saltos",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(35,35)),
         aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 38),
         aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)

#------------------------------------------------------------------------------#
#                                 P.Maze                                       #
#------------------------------------------------------------------------------#

pm <- conducta %>% select(starts_with("p_"), "v_j")

##############
# Normalidad #
##############

tapply(pm$p_t_cto, pm$v_j, shapiro.test)     # p < 0.05
tapply(pm$p_t_ba, pm$v_j, shapiro.test)      # p < 0.05
tapply(pm$p_t_bc, pm$v_j, shapiro.test)      # p < 0.05
tapply(pm$p_cto, pm$v_j, shapiro.test)       # p > 0.05
tapply(pm$p_grooming, pm$v_j, shapiro.test)  #
tapply(pm$p_up, pm$v_j, shapiro.test)        # p > 0.05

####################
# Homocedasticidad #
####################

levene.test(pm$p_t_cto, pm$v_j)                        # p > 0.05
levene.test(pm$p_t_ba, pm$v_j)                         # p > 0.05
levene.test(pm$p_t_bc, pm$v_j)                         # p > 0.05
levene.test(pm$p_cto, pm$v_j, location = "mean")       # p < 0.05
levene.test(pm$p_up, pm$v_j, location = "mean")        # p > 0.05



#####################
# Test Paramétricos #
# T-Student; Welch  #
#####################

t.test(p_cto~v_j, data = pm, var.equal = F, paired = F) # p < 0.05 *
t.test(p_up~v_j, data = pm, var.equal = T, paired = F)  # p > 0.05



########################
# Test No paramétricos #
#     Wilcoxon         #
########################


wilcox_test(p_t_cto~as.factor(v_j), data = pm)    # p > 0.05
wilcox_test(p_t_ba~as.factor(v_j), data = pm)     # p > 0.05
wilcox_test(p_t_bc~as.factor(v_j), data = pm)     # p > 0.05



############################
# Gráficos No paramétricos #
############################

# Data frame para los análisis

comb_pm_t <- data.frame(tb = c(163,95,18,39,11,8.38,4.70,0,15.15,95,46,80,
                               0,32,0,134,0,0,0,3,0,8.1,3,0,
                               17,53,162,7,170,172,175.3,177,164,77,131,100),
                        tipo = c(rep("t.Centro", 12),rep("t.Brazo abierto", 12),
                                 rep("t.Brazo cerrado", 12)),
                        v_j = rep(c("Viejos","Viejos","Jovenes","Jovenes",
                                    rep("Viejos",4), rep("Jovenes",4)), 3));view(comb_pm_t)


comb_pm_t%>% 
  group_by(v_j, tipo) %>% 
  ggplot(aes(v_j, tb, fill = tipo)) +
  geom_boxplot(alpha = .7, width = .4, position = position_dodge(.75)) +
  geom_jitter(pch = 21, position = position_jitterdodge(.2)) +
  scale_y_continuous(limits = c(0,320),
                     breaks = seq(0,320, 40)) +
  labs(title = "P.Maze, Tiempo de permanencia brazo/centro",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Tiempo (s)",
       x = "Grupo de ratones",
       fill = "Zona") +
  theme_tufte() +
  theme(axis.line = element_line(),
        legend.background = element_rect(color = "black"),
        legend.position = c(.9,.9),
        legend.text = element_text(size = 13),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  geom_line(data = tibble(x = c(.75,2.25), y = c(200,200)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 215),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5) +
  scale_fill_manual(values = c("red", "yellow", "blue"))


#########################
# Gráficos paramétricos #
#########################

pm %>% 
  group_by(v_j) %>% 
  summarise(media = mean(p_cto), sd = sd(p_cto)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,20),
                     breaks = seq(0,20, 5)) +
  labs(title = "P.Maze, Nº veces en el centro",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Frecuencia absoluta",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(15,15)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 16),
            aes(x = x, y = y),inherit.aes = F, label = "*", size = 10)

pm %>% 
  group_by(v_j) %>% 
  summarise(media = mean(p_up), sd = sd(p_up)) %>% 
  ggplot(aes(v_j, media, fill = v_j)) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .1) +
  geom_bar(stat = "identity", show.legend = F, width = .3, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,45),
                     breaks = seq(0,45, 10)) +
  labs(title = "P.Maze., Nº de saltos",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de Saltos",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(35,35)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 37),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)


####################################
# P.Maze comparación brazos/centro #
####################################

pm_brazos <- data.frame(paso = c(2,3,3,5,5,4,0,2,9,9,12,4,
                                 0,2,7,0,0,0,1,0,0,2,1,0,
                                 1,4,3,2,6,5,4,5,10,8,11,7),
                        brazo = c(rep("Centro", 12), rep("B.abierto", 12),
                                  rep("B.cerrado",12)),
                        v_j = rep(c("Viejos","Viejos","Jovenes","Jovenes",
                                    rep("Viejos",4), rep("Jovenes",4)), 3));view(pm_brazos)

pm_cto <- filter(pm_brazos, brazo %in% "Centro")
pm_ba <- filter(pm_brazos, brazo %in% "B.abierto")
pm_bc <- filter(pm_brazos, brazo %in% "B.cerrado")

# Normalidad

tapply(pm_cto$paso, pm_cto$v_j, shapiro.test)   # p > 0.05
tapply(pm_ba$paso, pm_ba$v_j, shapiro.test)     # p < 0.05
tapply(pm_bc$paso, pm_bc$v_j, shapiro.test)     # p > 0.05

# Homocedasticidad

levene.test(pm_cto$paso, pm_cto$v_j, location = "mean") # p < 0.05
levene.test(pm_ba$paso, pm_ba$v_j, location = "median") # p > 0.05
levene.test(pm_bc$paso, pm_bc$v_j, location = "mean")   # p > 0.05

# Análisis paramétricos

t.test(paso~v_j, data = pm_cto, paired = F, var.eq = F)  # p < 0.05
t.test(paso~v_j, data = pm_bc, paired = F, var.eq = T)   # p > 0.05

# Análisis no paramétrico

wilcox_test(paso~as.factor(v_j), data = pm_ba)           # p > 0.05


# gráficos
pm_cto %>% filter(v_j %in% "Jovenes")%>%  summarise(media = mean(paso))
pm_bc %>% filter(v_j %in% "Jovenes") %>% summarise(media = mean(paso))

pm_brazos %>% 
  filter(brazo %in% c("Centro", "B.cerrado")) %>% 
  group_by(v_j, brazo) %>% 
  mutate(media1 = mean(paso),
         sd1 = sd(paso)) %>% 
  ggplot(aes(v_j, media1, fill = brazo)) +
  geom_errorbar(aes(ymin=media1+sd1, ymax=media1-sd1), width = .2, position = position_dodge(.6)) +
  geom_bar(stat = "identity", position = position_dodge(.6), width = .5, col = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,20),
                     breaks = seq(0,20, 5)) +
  labs(title = "P.Maze, veces que pasan por B.cerrado/Centro",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de trámites",
       x = "Grupo de ratones",
       fill = "Zona") +
  theme_tufte() +
  theme(axis.line = element_line(),
        legend.background = element_rect(color = "black"),
        legend.position = c(.9,.7),
        legend.text = element_text(size = 13),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  geom_text(data = tibble(x = .85, y = 11.5),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5) +
    geom_text(data = tibble(x = 1.15, y = 11.2),
              aes(x = x, y = y),inherit.aes = F, label = "*", size = 10) +
    geom_text(data = tibble(x = 1.85, y = 7),
              aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5) +
    geom_text(data = tibble(x = 2.15, y = 5.3),
              aes(x = x, y = y),inherit.aes = F, label = "*", size = 10) +
  scale_fill_manual(values = c("red", "blue"))

pm_ba
pm_ba %>% 
  ggplot(aes(v_j, paso, fill = v_j)) +
  geom_boxplot(width = .5, alpha = .7, show.legend = F) + 
  geom_point(alpha = .5, show.legend = F) +
  scale_y_continuous(limits = c(-2,12),
                     breaks = seq(0,12, 2)) +
  labs(title = "P.Maze, Nº veces que pasa por el B.abierto",
       subtitle = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Nº de trámites",
       x = "Grupo de ratones") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 13)) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_line(data = tibble(x = c(1,2), y = c(9,9)),
            aes(x = x, y = y),inherit.aes = F) +
  geom_text(data = tibble(x = 1.5, y = 9.5),
            aes(x = x, y = y),inherit.aes = F, label = "n.s.", size = 5)
  
