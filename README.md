# CP-Seeker application

## How to install run this application

### Windows
For Windows users, we created one file to click to be able to run our application.
So you just have to click on the file `CP-Seeker.bat`, then the application will run normaly, opening a new internet window.
If you want it directly on your desk, right click on this file and create a new shortcut on it.

### Ubuntu/Linux
For Ubuntu or Linux users, you will need to open a terminal and write some command lines :
```{bash}
cd my_path_of_CPSeeker
./R-Portable/bin/R.exe
```

This will open to you the R command line tool from our R portable folder which is directly in CP-Seeker. You now need to run the application by writing :
```{R}
source("./utils/manager.R")
```

Now all the application should run and open in a new internet window.