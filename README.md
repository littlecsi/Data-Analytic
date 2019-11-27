# Data Analytic

## Crime data in Seattle

Seattle의 범죄 데이터를 다양한 관점에서 분석하여 범죄율과 관련된 요인들을 찾고 범죄율의 변화와 향후 예측을 하는 프로젝트

***

## File Description


- [data_yearly.R](https://github.com/littlecsi/Data-Analytic/blob/master/script/data_yearly.R)
  - 연도별 시애틀 범죄 빈도수 꺾은선 그래프
  - 선형 회귀 직선을 통해 범죄율의 추세를 볼 수 있음
- [months_each_year.R](https://github.com/littlecsi/Data-Analytic/blob/master/script/months_each_year.R)
  - 시애틀에서 특정 달(month)에 따른 범죄 빈도수 그래프 제공
- [seasonal_crime_trends.R](https://github.com/littlecsi/Data-Analytic/blob/master/script/seasonal_crime_trends.R)
  - 시애틀의 범죄 빈도를 계절별로 구분하여, 비교를 할 수 있음
  - 연도에 따른 각 계절의 범죄 빈도(Frequency)와 비율(Proportion) 그래프 제공

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

- Reference Link : [선형회귀식](https://rpubs.com/cardiomoon/98019)