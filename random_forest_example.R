library("VSURF")
data("toys")
set.seed(3101318)

toys.vsurf <- VSURF(x = toys$x, y = toys$y, mtry = 100)

names(toys.vsurf)
summary(toys.vsurf)

plot(toys.vsurf)


cov.vsurf <- VSURF(x = covmerged_red, y = covmerged$ev_gesamt_fac, mtry = 100)

library("varSelRF")
cov.vs1 <- varSelRF(covmerged_red[1:30], covmerged$ev_gesamt_fac, ntree = 500, ntreeIterat = 300,
                   vars.drop.frac = 0.2)

library("randomForest")
# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/importance
cov.rf <- randomForest(ev_gesamt_fac ~ ., data=covmerged, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(cov.rf)

imp_cov <- as.data.frame(importance(cov.rf))
imp_cov %>% arrange(desc(MeanDecreaseGini)) %>% slice(1:20)
