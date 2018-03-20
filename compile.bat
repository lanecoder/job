@echo off
rem please check your erl path to make sure that the 'erl' command
rem can be use correctly 
erl -pa mmake/ -s mod_make all
pause