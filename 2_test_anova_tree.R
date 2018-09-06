mystate <- data.frame(state.x77, region=state.region)
names(mystate) <- casefold(names(mystate)) #make character lowercase
#custom options our anova decision tree
ulist <- list(eval = etemp, split = stemp, init = itemp)
# tell rpart to create our anova tree
fit1 <- rpart(murder ~ population + illiteracy + income + life.exp +
                hs.grad + frost + region, data = mystate,
                method = ulist, minsplit = 10)
# use rpart built in anova tree
fit2 <- rpart(murder ~ population + illiteracy + income + life.exp +
                hs.grad + frost + region, data = mystate,
                method = 'anova',
                minsplit = 10, xval = 0)

# check our implementation is correct
all.equal(fit1$frame, fit2$frame)
all.equal(fit1$splits, fit2$splits)
all.equal(fit1$csplit, fit2$csplit)
all.equal(fit1$where, fit2$where)
all.equal(fit1$cptable, fit2$cptable)
