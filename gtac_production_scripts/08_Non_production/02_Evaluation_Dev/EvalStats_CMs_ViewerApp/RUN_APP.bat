@echo off
REM .bat file statement to keep cmd open even if an error is encountered
REM https://stackoverflow.com/a/33929280
IF NOT DEFINED in_subprocess (cmd /k SET in_subprocess=y ^& %0 %*) & EXIT )

REM --- SET PARAMETER HERE ---
SET R_PATH="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"

REM if R_PATH does not exist, warn user
IF EXIST %R_PATH% (
	
	REM Connect to Rscript executable located in machin to run R script
	@echo LAUNCHING EVAL APP...
	CALL %R_PATH% "run.R"
	
) ELSE (

	@echo ------------------------------------------------------
	@echo WARNING: PLEASE OPEN THE .BAT FILE AND SET THE R PATH!
	@echo ------------------------------------------------------
	
)