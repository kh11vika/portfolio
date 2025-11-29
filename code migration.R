library(readxl)
library(sandwich)
library(stargazer)
library(ivreg)

df = read_excel("data migration.xlsx")


m1 <- lm(migration_net ~ gdp_ppp + gdp_growth + health_expend + educ_expend + unemployment + inflation,
         data = df)
summary(m1, vcov. = vcovHC(m1, "HC4"))


m2 <- lm(health_expend ~ gdp_ppp + gdp_growth + educ_expend + unemployment + inflation + corruption,
         data = df)
summary(m2)


m3 <- ivreg(migration_net ~ gdp_ppp + gdp_growth + educ_expend + unemployment + inflation |
              health_expend | corruption,
            data=df)
summary(m3)

stargazer(m1, m3, type="html", out="миграция модели.doc")
stargazer(m3, type="html", out="миграция с инстр.doc")




