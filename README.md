Проект предназначен для конвертации файла с программой на Бейсике для
компьютера БК-0010-01 в цепочку ASC-файлов для загрузки в эмуляторе GID
или файл WAV для использования с магнитофонным входом компьютера БК.
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
* Поддержка директив условной трансляции IFDEF/ELSE/ENDIF, с передачей
имен директив через аргументы командной строки /define. В данной версии,
многоуровневые директивы условной трансляции не поддерживаются.
* Поддержка включаемых файлов через директиву INCLUDE. Вложенные файлы также
могут содержать директивы включения.

В каталоге `bin` содержатся файлы-примеры для конвертации.

# Инструкция по применению:

Выходные bin-файлы класть в подкаталог `bin` эмулятора. Для упрощения запуска
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
* /savepreparedsource=true|false - позволяет сохранить подготовленный исходный
текст (с удаленными строками, удаленными комментариями и перенумерованными
строками при соответствующем режиме работы) во временный выходной файл.
* /makewav=true|false - указывает создать WAV-файл вместо цепочки бинарных файлов.
По умолчанию, создаются цепочки бинарных файлов.
* /define=name - установка имени для директивы IFDEF. Может использоваться несколько раз, задавая много имен.

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

# Пример использования условных директив вместе с автонумерацией

Исходный файл:
``` BASIC
'$IFDEF BK10
PRINT "BK10 is defined"
'$ENDIF

'$IFDEF BK11
PRINT "BK11 is defined"
'$ELSE
PRINT "BK11 is not defined"
'$ENDIF
```
Подготовленный файл для сохранения в цепочку ASC-файлов при передаче
в аргументах команды /define=BK10
``` BASIC
10 PRINT "BK10 is defined"
20 PRINT "BK11 is not defined"
```

# Пример использования включаемых файлов вместе с автонумерацией

Исходный файл:
``` BASIC
PRINT "Test included file"
'$INCLUDE: 'beep3.bi'
PRINT "Test OK"
```

Включаемый файл beep3.bi:
``` BASIC
BEEP
BEEP
BEEP
```

Подготовленный файл для сохранения в цепочку ASC-файлов:
``` BASIC
10 PRINT "Test included file"
20 BEEP
30 BEEP
40 BEEP
50 PRINT "Test OK"
```

# Инструкция по сборке

Версия для Delphi компилируется через открытие проекта. Для сборки версии
FreePascal, нужно запустить пакетный файл из каталога `build`.
Для компиляции проекта нужно добавить в каталог `src` файл версии `Version.pas`.
При работе из репозитория git данный файл создаётся вызовом скрипта
`scripts\update_version.bat`, в противном случае его нужно создать самому по примеру:

```PASCAL
unit Version ;
interface
type TGitVersion = class
const COMMIT = 'd6aac25e';
const BRANCH = 'main';
const TAG = 'v1.0.0';
end ;
implementation
end.
```
