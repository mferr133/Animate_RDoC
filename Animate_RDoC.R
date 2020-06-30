load.lib<-c("ggplot2","gganimate", "gifski","plyr","transformr")  # Install & Load Packages & libraries:
install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

df <- read.csv(file = 'data.csv') # Loads the dataset
df$Biotype <- as.character(df$Biotype)
Animate_RDoC <- ggplot(df, aes(SP, CC, size = PROMHBQ , color = Biotype)) + # Set up ggplot & animate specific bits:
    geom_text(aes(label = patient), nudge_y = +0.015, show.legend = FALSE) +
    stat_smooth(formula = y ~ log10(x), se = FALSE, size = 0.7, linetype="solid") +  #these are the 3 colored fitting lines
    labs(title = '{round((frame_time),digits=0)} Days Post-Intervention', x = 'Low-Function    <-     Social Processes Score     ->    High-Function', y = 'Low-Function    <-    Cognitive Control Score     ->    High-Function') +
    guides(color = guide_legend(order=1), size = guide_legend(order=2)) + 
    view_follow(fixed_y = TRUE) + 
    theme_bw() + 
    transition_time(day) + 
    ease_aes('linear') + 
    geom_point(shape=1) + 
    theme(legend.position=c(0.9,0.15))  
animate(Animate_RDoC, height = 800, width = 800)
anim_save("Animate_RDoC.gif")