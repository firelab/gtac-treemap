@echo off
REM .bat file statement to keep cmd open even if an error is encountered
REM https://stackoverflow.com/a/33929280
IF NOT DEFINED in_subprocess (cmd /k SET in_subprocess=y ^& %0 %*) & EXIT )

REM Connect to Rscript executable located in machin to run R script
@echo LAUNCHING EVAL APP...
CALL "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "run.R"