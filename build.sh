#!/bin/bash

pythonDir="./python"
csourceDir="./c-source"

distDir="./dist"

# build libgs2.so
cd $csourceDir
make clean
make
make library
cd ..

# build python
cd $pythonDir
source env/bin/activate
pip install -r requirements.txt
pyinstaller main.py --onefile
deactivate
cd ..

# bundle
mkdir -p "$distDir/lib/" "$distDir/config/"
cd $distDir
cp "../$pythonDir/dist/main" "./main"
cp "../$csourceDir/build/libgs2.so" "./lib/libgs2.so"
cp "../$pythonDir/config/prod-config.ini" "./config/config.ini"

# clean up artefacts
