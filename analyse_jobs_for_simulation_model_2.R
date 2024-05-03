# Script para calcular os dados para o modelo 2 de simulação do emprego público.
# Neste modelo assumimos que há uma percentagem das pessoas que pertencem a uma
# faixa etária f_i que passam para a faixa etária seguinte f_{i+1}. Esta
# percentagem é proporcional à divisão entre a duração do intervalo de tempo e a
# diferença de idades máxima e mínima da faixa etária f_i.

library (package = "data.table")
library (package = "ggplot2")

el.data <- fread (file="posto-trabalho-por-admin-cargo-genero-data.csv")

el.data  [
  ,
  `:=` (
    time_number = ano + (mês - 6) / 12.0
  )
]
el.data [
  ,
  `:=` (
    dia = NULL,
    mês = NULL,
    ano = NULL,
    idade_i
    
  )
]

