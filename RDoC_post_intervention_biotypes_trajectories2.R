# Install packages, Get data, & Load libraries:
load.lib<-c("gapminder","ggplot2","gganimate", "gifski","plyr","transformr")
install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

# Make a dataframe, rename variables
df <- gapminder
names(df)[names(df) == "pop"] <- "PROMHBQ"
names(df)[names(df) == "continent"] <- "Biotype"
names(df)[names(df) == "lifeExp"] <- "CC"
names(df)[names(df) == "gdpPercap"] <- "SP"
levels(df$Biotype)[levels(df$Biotype)=="Africa"] <- "1"
levels(df$Biotype)[levels(df$Biotype)=="Americas"] <- levels(df$Biotype)[levels(df$Biotype)=="Asia"] <- "2"
levels(df$Biotype)[levels(df$Biotype)=="Europe"] <- levels(df$Biotype)[levels(df$Biotype)=="Oceania"] <- "3"
df$patient=df$country
levels(df$patient)[levels(df$patient) != "Italy"] <- ""
levels(df$patient)[levels(df$patient) == "Italy"] <- "You"

#rescale Variables
df$SP=scale(df$SP)
df$CC= df$CC/85.5 
df$day=round(((df$year-1951)*2/1.12),digits=0)
df$PROMHBQ[df$Biotype=="1"]=df$day*runif(568, min=0, max=0.0005)
df$PROMHBQ[df$Biotype=="2"]=df$day*runif(568, min=0.003, max=0.005)
df$PROMHBQ[df$Biotype=="3"]=df$day*runif(568, min=0.007, max=0.01)

#Get rid of unecessary colunms
df <- subset(df, select = -c(year,country))

# gganimate specific bits:
my.animation <- ggplot(df, aes(SP, CC, size = PROMHBQ , color = Biotype)) +
    geom_point(shape=1) + #scale_x_log10() + xlim(25, 100) + ylim(70, 83) +
    stat_smooth(formula = y ~ log10(x), se = FALSE, size = 0.7, linetype="solid") +
    theme_bw() +
    labs(title = '{round((frame_time),digits=0)} Days Post-Intervention', x = 'Low-Function    <-     Social Processes Score     ->    High-Function', y = 'Low-Function    <-    Cognitive Control Score     ->    High-Function') +
    theme(legend.position=c(0.87,0.23)) +
    transition_time(day) +
    guides(color = guide_legend(order=1), size = guide_legend(order=2))+
    view_follow(fixed_y = TRUE)+ #shadow_wake(wake_length = 0.1, alpha = FALSE) +
    geom_text(aes(label = patient), nudge_y = +0.015, show.legend = FALSE)+
    ease_aes('linear')

# Animate & Save at gif:
res = 800
animate(my.animation, height = res, width = res)
anim_save("biotypeanimated.gif")