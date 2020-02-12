

## Virtual Env
Setting up a virtual environment is something that each developer needs to do. This is done by moving into the python directory and running:

```$ python3 -m venv env```

This creates a directory called `env`. Note that this directory is in the `.gitignore`.

After creating this directory the following workflow should be used:
```
# enable vitual env
# on mac/linux
$ source env/bin/activate

# on windows
$ .\env\Scripts\activate.bat

# make sure the virtual env has all the required libraries
$ pip install -r requirements.txt

# program as one normally would

# when finisihed programming, make sure any new libraries are added to the requirements.txt
$ pip freeze > requirements.txt

# deactivate env
$ deactivate

# deactivate env on windows
$ .\env\Scripts\deactivate.bat

# do git stuff
$ git add/commit/push

```


## Building on Linux/Mac

To build on Linux and presumably Mac, just:

```
$ bash build.sh
```

Running `build.sh` creates a top-level directory, `dist`. In dist there is an executable `main`. Running main should launch the application.

## Building on Windows

Just run the `build.bat` file.

## Compiling on Windows

Install MinGW by following the instructions [here](http://www.mingw.org/wiki/Getting_Started). 
Assuming MinGW is installed to `C:\MinGW`, make sure to add `C:\MinGW\bin` to your path.
After adding it to your path, open up a power shell and run:
```
$ mingw-get install gcc g++ mingw32-make
```

Install MSYS. A link to it can be found [here](http://www.mingw.org/wiki/MSYS).
I put mine in `D:\MinGW\msys\`. Then add `D:\MinGW\msys\1.0\bin` to your path.

At this point, move on to the Installing GSL section and return here after installing GSL.

WIP.


## Installing GSL

Main website [here](https://www.gnu.org/software/gsl/). On the downloads page look for version 2.5.

### Linux/Mac

Download `gsl-2.6.tar.gz`.
```
$ tar -xf gsl-2.6.tar.gz
$ cd gsl-2.6.tar.gz
$ ./configure
$ make
$ sudo make install
```

### Windows

Download `gsl-2.6.tar.gz` into `C:/MinGW`.

```
$ # using msys launch a bash shell
$ bash
$ tar -xf gsl-2.6.tar.gz
$ cd gsl-2.6.tar.gz
$ ./configure
$ make
```

At this point I think it is easiest to manually copy the libs to the correct location.

move `gsl-2.6/.libs/libgsl.dll.a` to `MinGW/lib/libgsl.a`.

move `gsl-2.6/.libs/libgsl-25.dll` to `MinGW/bin/libgsl-25.dll`.

move `gsl-2.6/gsl/` to `MinGW/include/gsl`.

Should then be good to go.



