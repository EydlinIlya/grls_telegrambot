library(telegram.bot)
library(fs)
library(here)
library(dplyr)
library(logger)

log_appender(appender_file(here("log.out")))
# создаём экземпляр класса Updater
updater <- Updater('5007043387:AAGJyRLBU0rcacapMuS47JrYnbDUW1NHzP0')

# Пишем метод для приветсвия
## команда приветвия
start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Привет, %s! Отправь мне номер протокола КИ, название препарата или комании-спонсора, и я найду для тебя информацию по исследованию.", update$message$from$first_name))
  log_info('user {update$message$from$first_name} started')
}

#Контроллер команды приветствия
start_handler <- CommandHandler("start", start)

echo <- function(bot, update){
  log_info('user {update$message$from$first_name} searched for {update$message$text}')
  search <- URLencode(update$message$text)
  url <- paste0("https://clinline.ru/sohranenie-rezultatov-v-csv.html?type=ki&show-all=&search=", search, "&fio=&name=&status%5B%5D=%D0%9F%D1%80%D0%BE%D0%B2%D0%BE%D0%B4%D0%B8%D1%82%D1%81%D1%8F&status%5B%5D=%D0%97%D0%B0%D0%B2%D0%B5%D1%80%D1%88%D0%B5%D0%BD%D0%BE&status%5B%5D=%D0%9F%D1%80%D0%B8%D0%BE%D1%81%D1%82%D0%B0%D0%BD%D0%BE%D0%B2%D0%BB%D0%B5%D0%BD%D0%BE&status%5B%5D=%D0%9F%D1%80%D0%B5%D0%BA")
  destfile <- file_temp(ext = "csv")
  download.file(url, destfile)
  if (file.info(destfile)$size < 10 ) {
    log_info('request for user {update$message$from$first_name} returned empty file from clinline')
    bot$sendMessage(chat_id = update$message$chat_id, text = "Такого исследования не нашлось( Попробуй изменить запрос.")
  }
  else {
    csv <- read.csv2(destfile)
    csv[,1] <- NULL
    csv[is.na(csv)] <- "Нет информации"
    names(csv) <- paste0("V", seq(1, ncol(csv)))
    csv <- csv %>% 
      filter(grepl(update$message$text, V1, ignore.case=TRUE) | 
               grepl(update$message$text, V12, ignore.case=TRUE) | 
               grepl(update$message$text, V7, ignore.case=TRUE))
    log_info('request for user {update$message$from$first_name} returned {nrow(csv)} rows after filtering')
    if (nrow(csv) == 0 ) {
      bot$sendMessage(chat_id = update$message$chat_id, text = "Такого исследования не нашлось( Попробуй изменить запрос.")
    }
    else {
      csv[1, 1] <- paste0("Номер протокола: ", csv[1, 1], "\n")
      csv[1, 2] <- paste0("Название протокола: ", csv[1, 2], "\n")
      csv[1, 3] <- paste0("Терапевтическая область: ", csv[1, 3], "\n")
      csv[1, 4] <- paste0("Заболевания: ", csv[1, 4], "\n")
      csv[1, 5] <- paste0("Период проведения КИ: ", csv[1, 5], "\n")
      csv[1, 6] <- paste0("Номер и дата РКИ: ", csv[1, 6], "\n")
      csv[1, 7] <- paste0("Спонсор: ", csv[1, 7], "\n")
      csv[1, 8] <- paste0("КИО: ", csv[1, 8], "\n")
      csv[1, 9] <- paste0("Страна: ", csv[1, 9], "\n")
      csv[1, 10] <- paste0("Исследователи: ", csv[1, 10], "\n")
      csv[1, 11] <- paste0("Центры: ", csv[1, 11], "\n")
      csv[1, 12] <- paste0("Препарат: ", csv[1, 12], "\n")
      csv[1, 13] <- paste0("Города проведения: ", csv[1, 13], "\n")
      csv[1, 14] <- paste0("Фаза: ", csv[1, 14], "\n")
      csv[1, 15] <- paste0("Пациентов: ", csv[1, 15], "\n")
      csv[1, 16] <- paste0("Центров открыто: ", csv[1, 16], "\n")

      string <- paste0(csv[1, 6], "\n",
                      csv[1, 12], "\n",
                      csv[1, 7], "\n",
                      csv[1, 1], "\n",
                      csv[1, 2], "\n",
                      csv[1, 5], "\n",
                      csv[1, 14], "\n",
                      csv[1, 13], "\n"
                      )
      bot$sendMessage(chat_id = update$message$chat_id, text = string) 
      }
    }}

updater <- updater + MessageHandler(echo, MessageFilters$text) + start_handler

# запускаем бота
tryCatch(

  # запускаем пуллинг
  expr = updater$start_polling(),

  # действия при ошибке пуллинга
  error = function(err) {

    # очищаем полученный апдейт бота, который вызвал ошибку
    updater$bot$clean_updates()
    log_error('error occured. clean updates')


  },
  # действия которые будут выполненны в любом случае
  finally = {

    # останавливаем пулинг
    updater$stop_polling()
    log_info('stop polling, restart bot')
    # перезапускаем скрипт бота
    source(here('bot.R'))

  }
)
