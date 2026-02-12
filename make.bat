@echo off
REM Check if "out" directory exists
if not exist out (
    mkdir out
)
echo Compiling Haunted House
if exist out\haunted.bin del out\haunted.bin
bin\dasm.exe src\haunted.asm -Isrc -sout\haunted.sym -T1 -Lout\haunted.lst -f3 -oout\haunted.bin

REM Compare output binary with original
if exist out\orighaunt.bin (
    fc /b out\haunted.bin out\orighaunt.bin >nul
    if errorlevel 1 (
        echo WARNING: out\haunted.bin differs from out\orighaunt.bin!
    ) else (
        echo ROMs are identical.
    )
) else (
    echo WARNING: Reference ROM out\orighaunt.bin not found!
)
