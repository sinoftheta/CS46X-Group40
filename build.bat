:: 
:: This script assumes that MinGW is installed
:: and PATH has been set up to point to 
:: the required dlls and commands
:: 

:: find dlls
set dllsDir=""
if exist D:\MinGW\bin (
     set dllsDir="D:\MinGW\bin"
) else (
    if exist C:\MinGW\bin (
         set dllsDir="C:\MinGW\bin"
    ) else (
        echo "gsl dlls not found!"
        exit
    )
)

set pythonDir=".\python"
set csourceDir=".\c-source"

set distDir=".\dist"

:: build libgs2.so
cd %csourceDir%
make clean
make
make library
cd ..

:: build python
cd %pythonDir%
call .\env\Scripts\activate.bat
pip install -r requirements.txt
pyinstaller main.py --onefile
call .\env\Scripts\deactivate.bat
cd ..

:: bundle app
md "%distDir%\lib"
md "%distDir%\config"
md "%distDir%\res"

copy "%pythonDir%\config\prod-config.ini" "%distDir%\config\config.ini"
copy "%pythonDir%\dist\main.exe" "%distDir%\app.exe"
copy "%csourceDir%\build\libgs2.so" "%distDir%\lib\libgs2.so"
copy "%csourceDir%\res\example1.csv" "%distDir%\res\example.csv"

copy "%dllsDir%\libgsl-25.dll" "%distDir%\libgsl-25.dll"


:: clean up build artifacts
rmdir /s /q "%pythonDir%\build"
rmdir /s /q "%pythonDir%\dist"
del "%pythonDir%\main.spec"

cd %csourceDir%
make clean