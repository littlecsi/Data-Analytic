# Data Analytic

## Crime data in Seattle

Seattle의 범죄 데이터를 다양한 관점에서 분석하여 범죄율과 관련된 요인들을 찾고 범죄율의 변화와 향후 예측을 하는 프로젝트

***

## Well Known Function

### 선형 회귀 공식 도출 함수 

```R
lm2equation=function(mydata,xvar,yvar,parse=FALSE){
    fit=eval(parse(text=paste0("lm(",yvar,"~",xvar,",data=",mydata,")")))
    intercept=round(coef(fit)[1],1)
    slope=round(coef(fit)[2],1)
    if(parse) equation=paste0("y==",slope,"*x",ifelse(intercept>=0,'+','-'),abs(intercept))
    else equation=paste0("y = ",slope,"x",ifelse(intercept>=0,' + ',' - '),abs(intercept))
    p=round(summary(fit)$coeff[2,4],3)
    if(p==0) equation=paste(equation,"(p < 0.001)")
    else equation=paste(equation,"(p =",p,")")
    equation
}
```