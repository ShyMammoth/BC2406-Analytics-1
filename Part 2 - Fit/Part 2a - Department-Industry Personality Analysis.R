# ==============================================================================================================
# Part 2a : Fit
# Department/Industry Personality Analysis
# ==============================================================================================================
setwd('C:/Users/limji/Documents/University/19-20 Y2 Sem 1/BC2406 Analytics 1 - Visual & Predictive Techniques/Group Projects')
library(data.table)
personality.dt <- fread("Personality.csv")

# ==============================================================================================================
# Extraversion
# ==============================================================================================================

# Extraversion Scores across Departments
# Mean
means_extraversion_dept <- round(tapply(personality.dt$Extraversion, personality.dt$Department, mean), digits = 2)
means_extraversion_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Extraversion)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Extraversion across Departments") 
#ANOVA
aov_extraversion_dept <- aov(personality.dt$Extraversion~personality.dt$Department)
summary(aov_extraversion_dept)

# Extraversion Scores across Industries
# Mean
means_extraversion_industry <- round(tapply(personality.dt$Extraversion, personality.dt$Industry, mean), digits = 2)
means_extraversion_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Extraversion)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Extraversion across Industries") 
# ANOVA
aov_extraversion_industry <- aov(personality.dt$Extraversion~personality.dt$Industry)
summary(aov_extraversion_industry)

# ==============================================================================================================
# Agreeableness
# ==============================================================================================================

# Agreeableness Scores across Departments
# Mean
means_agreeableness_dept <- round(tapply(personality.dt$Agreeableness, personality.dt$Department, mean), digits = 2)
means_agreeableness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Agreeableness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Agreeableness across Departments") 
#ANOVA
aov_agreeableness_dept <- aov(personality.dt$Agreeableness~personality.dt$Department)
summary(aov_agreeableness_dept)

# Agreeableness Scores across Industries
# Mean
means_agreeableness_industry <- round(tapply(personality.dt$Agreeableness, personality.dt$Industry, mean), digits = 2)
means_agreeableness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Agreeableness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Agreeableness across Industries") 
# ANOVA
aov_agreeableness_industry <- aov(personality.dt$Agreeableness~personality.dt$Industry)
summary(aov_agreeableness_industry)

# ==============================================================================================================
# Openness
# ==============================================================================================================

# Openness Scores across Departments 
# Mean
means_openness_dept <- round(tapply(personality.dt$Openness, personality.dt$Department, mean), digits = 2)
means_openness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Openness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Openness across Departments") 
# ANOVA
aov_openness_dept <- aov(personality.dt$Openness~personality.dt$Department)
summary(aov_openness_dept)

# Openness Scores across Industries
# Mean
means_openness_industry <- round(tapply(personality.dt$Openness, personality.dt$Industry, mean), digits = 2)
means_openness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Openness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Openness across Industries") 
# ANOVA
aov_openness_industry <- aov(personality.dt$Openness~personality.dt$Industry)
summary(aov_openness_industry)

# ==============================================================================================================
# Neuroticism
# ==============================================================================================================

# Neuroticism Scores across Departments
# Mean
means_neuroticism_dept <- round(tapply(personality.dt$Neuroticism, personality.dt$Department, mean), digits = 2)
means_neuroticism_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Neuroticism)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Neuroticism across Departments") 
# ANOVA
aov_neuroticism_dept <- aov(personality.dt$Neuroticism~personality.dt$Department)
summary(aov_neuroticism_dept)

# Neuroticism Scores across Industries 
# Mean
means_neuroticism_industry <- round(tapply(personality.dt$Neuroticism, personality.dt$Industry, mean), digits = 2)
means_neuroticism_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Neuroticism)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Neuroticism across Industries") 
# ANOVA
aov_neuroticism_industry <- aov(personality.dt$Neuroticism~personality.dt$Industry)
summary(aov_neuroticism_industry)

# ==============================================================================================================
# Conscientiousness
# ==============================================================================================================

# Conscientiousness Scores across Departments
# Mean
means_conscientiousness_dept <- round(tapply(personality.dt$Conscientiousness, personality.dt$Department, mean), digits = 2)
means_conscientiousness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Conscientiousness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Conscientiousness across Departments") 
# ANOVA
aov_conscientiousness_dept <- aov(personality.dt$Conscientiousness~personality.dt$Department)
summary(aov_conscientiousness_dept)

# Conscientiousness Scores across Industries
# Mean
means_conscientiousness_industry <- round(tapply(personality.dt$Conscientiousness, personality.dt$Industry, mean), digits = 2)
means_conscientiousness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Conscientiousness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Conscientiousness across Industries") 
# ANOVA
aov_conscientiousness_industry <- aov(personality.dt$Conscientiousness~personality.dt$Industry)
summary(aov_conscientiousness_industry)