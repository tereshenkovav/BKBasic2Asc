Проект предназначен для конвертации файла с программой на Бейсике для
компьютера БК-0010-01 в цепочку ASC-файлов для загрузки в эмуляторе GID.
Реализован на ObjectPascal, поддерживается сборка как в виде проекта
для Delphi, так и файла для FreePascalCompiler.

Возможности:
* Удаление пустых строк и строк-комментариев (начинающихся с одиночной кавычки)
* Обрезка пустых пробелов в начале и конце строки
* Преобразование из UTF8, WIN1251 и OEM866 в KOI-8R
* Возможность автоматической нумерации строк при указании параметра
* Возможность использования символьных меток при автоматической нумерации
строк с заменой их на номера строк в операторах RESTORE,GOTO,GOSUB.
Метки являются регистронезависимыми, могут включать буквы, цифры и
символ нижнего подчеркивания.

В каталоге `bin` содержатся файлы-примеры для конвертации.

# Инструкция по применению:

Выходные файлы класть в подкаталог `bin` эмулятора. Для упрощения запуска
в подкаталоге `scripts` можно создать файл `load.script`

```
NEW
|#LOAD "prog"
|#RUN
```
где `prog` - имя файла ASC без расширения, и загружать готовые ASC-файлы командой 

`BK.exe /s load.script`
 
# Параметры запуска

* /basiccodepage=кодовая страница - позволяет задать кодовую страницу для
исходного файла, возможные значения: utf8,win1251,koi8r,oem866. По умолчанию,
используется utf8
* /autonumlines=true|false - позволяет включить режим автонумерации строк
исходного текста программы. По умолчанию отключена. Использование символьных
меток возможно только в этом режиме.
* savepreparedsource=true|false - позволяет сохранить подготовленный исходный
текст (с удаленными строками, удаленными комментариями и перенумерованными
строками при соответствующем режиме работы) во временный выходной файл.

# Пример преобразования исходного кода при использовании режима автонумерации

Исходный файл:
``` BASIC
' Просто комментарий в UTF-8
PRINT "Basic program with auto line nums"

' Блоки со ссылками для тестирования
LabData: DATA 1,2,3
Label1: PRINT "Line with LABEL1"
LABEL2: PRINT "Line with LABEL2"
LaBeL3: PRINT "Line with LABEL3"
label4: PRINT "Line with LABEL4"

' Использование ссылок во всех конструкциях
RESTORE LabData
GOTO  Label1
GOSUB Label2
ON I% GOSUB LABEL1,label2
ON I% GOTO LABEL3, label4
IF I%=1 THEN GOTO Label1   ELSE  GOSUB LABEL4

' Конец программы и пара пустых строк


END
```

Подготовленный файл для сохранения в цепочку ASC-файлов:
``` BASIC
10 PRINT "Basic program with auto line nums"
20 DATA 1,2,3
30 PRINT "Line with LABEL1"
40 PRINT "Line with LABEL2"
50 PRINT "Line with LABEL3"
60 PRINT "Line with LABEL4"
70 RESTORE 20
80 GOTO 30
90 GOSUB 40
100 ON I% GOSUB 30,40
110 ON I% GOTO 50,60
120 IF I%=1 THEN GOTO 30 ELSE  GOSUB 60
130 END
```
