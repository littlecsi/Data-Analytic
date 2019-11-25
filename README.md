# Data Analytic

## Crime data in Seattle

Seattle의 범죄 데이터를 다양한 관점에서 분석하여 범죄율과 관련된 요인들을 찾고 범죄율의 변화와 향후 예측을 하는 프로젝트

***

## Well Known Function

class, mode를 한번에 출력

```R
viewClassNMode <- function(data) {
  cat('class:', class(data))
  cat('\n','mode:', mode(data))
}
```