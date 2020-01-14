

## Virtual Env
Setting up a virtual environment is something that each developer needs to do. This is done by moving into the python directory and running:

```$ python3 -m venv env```

This creates a directory called `env`. Note that this directory is in the `.gitignore`.

After creating this directory the following workflow should be used:
```
# enable vitual env
$ source env/bin/activate

# program as one normally would

# when finisihed programming, make sure any new libraries are added to the requirements.txt
$ pip freeze > requirements.txt

# deactivate env
$ deactivate

# do git stuff
$ git add/commit/push

```