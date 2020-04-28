
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

## Extra Notes On Windows

working on windows is a pain, I recommend using either linux or mac. Client has a mac, so windows comes 2nd or 3rd.

## Installing GSL

Main website [here](https://www.gnu.org/software/gsl/). On the downloads page look for version 2.6.

### Linux/Mac

Download `gsl-2.6.tar.gz`.
```
$ tar -xf gsl-2.6.tar.gz
$ cd gsl-2.6
$ ./configure
$ make
$ sudo make install
```

### Windows

Windows is a touch more involved. Using msys2 as a build tool has worked the best.

Follow [these](https://github.com/orlp/dev-on-windows/wiki/Installing-GCC--&-MSYS2) instructions for installing msys2 and c build tools. You do not need to install the boost library. You will, however, need to install GSL.

In msys2 shell:
```
pacman -Ss gsl
```
Gives a list avalible gsl distros to install, find gsl-2.6 and install that one.

## Compiling Standalone C Code

Our C Code relies on the GSL library, so make sure that was installed. Note that the standalone C code exists as a means to debug the C library in C. 

### Linux / Mac

CD into the c-source directory and run the Makefile. To execute the code, run `make run`.

### Windows

Presumably the steps in the `Installing GSL` section were followed, so all tools should be avalible.

CD into the c-source directory and run the Makefile using `mingw32-make.exe`. You might encouter an issue with the mkdir command. Its a dirty fix, but comment it out and create the following directory structure under c-source
```
c-source/
    build/objs/src/
        capstone/
        gs2/
```

## Building Standalone Application

Running one of the build scripts creats a top-level directory, `dist`. In dist there is an executable `main`. Running main should launch the application.

### Linux / Mac

Running `$ bash build.sh` should be enough to produce an application.

### Windows

Running the `build.bat` file should be enough to produce an application.


## Running Python by itself

running `main.py` works just fine. It uses a different configuration file though.