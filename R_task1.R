# вспомогательные функции
Horsepower_to_KWatt <- function(x) {
  return (round(x / 1.36, digits = 2))
}

count_models_by_type <- function(df, Makename) { 
  counts <- table(df[df$Make == Makename,]$Type)
  return (paste(names(counts), "=", counts, collapse = ", "))
}

get_power <- function(df, Makename) { 
  filtered_df <- df[df$Make==Makename,]
  power_min <- aggregate(filtered_df$Horsepower ~ filtered_df$Type, data = filtered_df, FUN = min)
  colnames(power_min) <- c('Type','Horsepower')
  return (paste(power_min$Type, "=", Horsepower_to_KWatt(power_min$Horsepower), 'KWatt', collapse = ", "))
}

get_power_by_type <- function(line, type) {
  # Разделение строки по запятой для получения отдельных элементов
  entries <- strsplit(line, ", ")[[1]]
  
  # Инициализация переменной для хранения мощности
  power_value <- NA
  
  # Итерация по каждому элементу
  for (entry in entries) {
    # Разделение элемента на тип кузова и значение мощности
    parts <- strsplit(entry, " = ")[[1]]
    
    # Проверка, соответствует ли тип кузова запрашиваемому
    if (trimws(parts[1]) == type) {
      # Извлечение значения мощности, удаление ' KWatt' и преобразование в числовой формат
      power_value <- as.numeric(gsub(" KWatt", "", trimws(parts[2])))
      break
    }
  }
  
  return(power_value)
}


# импортируем файл
df <- read.csv('cars.csv')

#Создаём новый класс и объект этого класса
cls1 <- setClass('cls1', contains = 'data.frame')
cars <- new('cls1', df)

#Создаём метод, который сохраняет результат задания 1
setMethod('print', 'cls1', function(x) 
{
  producers = unique(x$Make)
  producers_with_country = unique(paste(x$Make, "[", x$Origin, "]", sep = ""))
  result <- c()
  for (i in 1:length(producers))
  { 
    result <- c(result,producers_with_country[i],count_models_by_type(x,producers[i]),get_power(x,producers[i]))
  }
  return (result)
})

number_of_models_data <- print(cars)

#Создаём метод, который сохраняет результат задания 2
setMethod('write.csv', 'cls1', function(x, file = "output.csv", ...){

origins <- unique(cars$Origin)
types <- unique(cars$Type)
result <- data.frame(matrix(ncol = length(origins), nrow = length(types)))
colnames(result) <- origins
rownames(result) <- types
for (origin in origins)
{
  for (type in types)
  {
    vector_of_minpower = c()
    for (i in seq(1,length(number_of_models_data),3))
    {
      if (grepl(origin,number_of_models_data[i])) {vector_of_minpower <- c(vector_of_minpower, get_power_by_type(number_of_models_data[i+2],type))}
    }
    vector_of_minpower <- na.omit(vector_of_minpower)
    if (length(vector_of_minpower) != 0) {result[type, origin] = min(vector_of_minpower)}
  }
}
write.csv(result, file)
}
)
write.csv(cars)