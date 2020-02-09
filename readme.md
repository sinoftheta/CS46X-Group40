

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
$ \env\Scripts\activate.bat

# make sure the virtual env has all the required libraries
$ pip install -r requirements.txt

# program as one normally would

# when finisihed programming, make sure any new libraries are added to the requirements.txt
$ pip freeze > requirements.txt

# deactivate env
$ deactivate

# do git stuff
$ git add/commit/push

```


## Installing GSL

Main website [here](https://www.gnu.org/software/gsl/). On the downloads page look for version 2.6.

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

Building GSL on windows is slightly different from Linux/Mac. This [stackoverflow](https://stackoverflow.com/questions/30015656/using-gnu-scientific-library-gsl-under-windows-x64-with-mingw) post should shed some light on the matter.





