source("BayesianNetworks-templateZinnia.r")
riskFactor = read.csv("RiskFactors.csv") 
varNames = colnames(riskFactor)
riskCPT = createCPT.fromData(riskFactor, colnames(riskFactor))

i = createCPT.fromData(riskFactor, c("income"))
s.i = createCPT.fromData(riskFactor, c("smoke", "income"))
e.i = createCPT.fromData(riskFactor, c("exercise", "income"))
bmi.e.i = createCPT.fromData(riskFactor, c("bmi", "exercise", "income"))
bp.s.i.e = createCPT.fromData(riskFactor, c("bp", "smoke", "income", "exercise"))
c.s.i.e = createCPT.fromData(riskFactor, c("cholesterol", "smoke", "income", "exercise"))
d.bmi = createCPT.fromData(riskFactor, c("diabetes", "bmi"))
s.bmi.bp.c = createCPT.fromData(riskFactor, c("stroke", "bmi", "bp", "cholesterol"))
attack.bmi.bp.c = createCPT.fromData(riskFactor, c("attack", "bmi", "bp", "cholesterol"))
angina.bmi.bp.c = createCPT.fromData(riskFactor, c("angina", "bmi", "bp", "cholesterol"))

riskNet = list(i, s.i, e.i, bmi.e.i, bp.s.i.e, c.s.i.e, d.bmi, s.bmi.bp.c, attack.bmi.bp.c, angina.bmi.bp.c)

# infer(riskNet, setdiff(varNames, list("diabetes", "smoke", "excercise")), c("smoke", "excercise"), c(2, 1))
# infer(riskNet, setdiff(varNames, "bp"), NULL, NULL)
# infer(riskNet, setdiff(varNames, c("attack", "smoke")), "smoke", NULL)
# infer(riskNet, setdiff(varNames, c("diabetes", "income")), c("income"), c(8))
infer(riskNet, NULL, setdiff(varNames, "bp"), rep(1, 9))

