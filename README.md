Обязательно замените параметр перед запуском скрипта.

> [!IMPORTANT]
> **Инструкция для запуска:**
> В первой строчке кода ниже замените `my_data_2.fq.gz` на название вашего файла. Остальной код менять не нужно — просто скопируйте его целиком.

```bash
# 1. Укажите имя вашего файла вместо my_data_2.fq.gz
MY_FILE="my_data_2.fq.gz"

# 2. Скачивание адаптеров
curl -O -# https://githubusercontent.com

# 3. Фильтрация и очистка
bbduk.sh -Xmx1G in=reads_1.fq.gz in2=reads_1.fq.gz out=reads_1_trim.fq.gz out2=\$MY_FILE ktrim=r k=23 mink=11 hdist=1 ref=adapters.fa
```

Продолжение вашей обычной документации...
