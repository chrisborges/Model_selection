## Best model selection function
---
A data dredge function to select the best models given a global model (i.e. x ~ a + b + c). 
The function works through an exhaustive combination of parameters and outputs a dataframe where models are ranked by model selection methods, such as AIC or WAic. 

Output includes: 
- log-likelihood "LL"; 
- number of parameters "k";
- Akaike Information Criterion "AICc";
- the difference between the two AIC values being compared "∆AIc"; 
- Watanabe Akaike Information Criterion "WAic".
