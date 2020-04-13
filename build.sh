#!/bin/bash


# look for gsl and gslcblas libraries
gslLibPath=""
gslLibName=""
cblasLibPath=""
cblasLibName=""
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    if [[ -e "/usr/local/lib/libgsl.so.25.0.0" ]]; then
        gslLibPath="/usr/local/lib/libgsl.so.25.0.0"
        gslLibName="libgsl.so.25"

        cblasLibPath="/usr/local/lib/libgslcblas.so.0.0.0"
        cblasLibName="libgslcblas.so.0"
    else
        echo "Could not find libgsl.so.25.0.0"
        exit
    fi
else
    # assume mac
    if [[ -e "/usr/local/lib/libgsl.dylib" ]]; then
        gslLibPath="/usr/local/lib/libgsl.25.dylib"
        gslLibName="libgsl.25.dylib"

        cblasLibPath="/usr/local/lib/libgslcblas.0.dylib"
        cblasLibName="libgslcblas.0.dylib"
    else
        echo "Could not find libgsl.dylib"
        exit
    fi
fi


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
pyinstaller --hidden-import pkg_resources.py2_warn main.py --onefile
deactivate
cd ..

# bundle
mkdir -p "$distDir/lib/" "$distDir/config/" "$distDir/res/"
cd $distDir
cp "../$pythonDir/config/prod-config.ini" "./config/config.ini"
cp "../$pythonDir/dist/main" "./main"
cp "../$csourceDir/build/libgs2.so" "./lib/libgs2.so"
cp "../$csourceDir/res/example1.csv" "./res/example.csv"

cd ..


# copy in gsl and cblas
cp "$gslLibPath" "$distDir/$gslLibName"
cp "$cblasLibPath" "$distDir/$cblasLibName"

# clean up artifacts

rm -r "$pythonDir/build/"
rm -r "$pythonDir/dist/"
rm -r "$pythonDir/main.spec"

cd $csourceDir
make clean
