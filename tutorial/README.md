# MSA 2024 Internship Tutorial

* Supervisor: Richard Wen <rrwen.dev@gmail.com>
* Last Updated: January 21, 2024

## Introduction

Welcome to the Centre for Global Health Research (CGHR) at Unity Health Toronto!

As a Spatial Data Engineering Intern, you will work in the data science team 2 days a week to create a data processing pipeline that can help us spatially aggregate our data on openmortality.org for analytical use.

This tutorial will help you get started on your internship by teaching you how to install and setup a Python 3 development environment using a package manager, Git, and Visual Studio Code (VSCode), while briefing you on how to read/write and transform spatial data with Python.

When you're ready, move onto the next section and begin installing some software!

## Step 1: Installation

The first step is to install all the software needed for the internship, which includes:

* Package Manager (either Chocolatey for Windows or Homebrew for MacOS)
* Python 3 Programming Language and Visual Studio Code (VSCode) Editor

### Step 1.1: Package Manager

First, you will need a package manager.

Install one of the following depending on your Operating System (OS):

* Windows: [Chocolatey](https://chocolatey.org/)
* MacOS: [Homebrew](https://brew.sh/)

If it all went well, you should open up a command line terminal (see [here](https://en.wikiversity.org/wiki/Command_Prompt/Open#:~:text=Open%20the%20Start%20menu%20or,Press%20Enter.) for windows, and [here](https://support.apple.com/en-ca/guide/terminal/apd5265185d-f365-44cb-8b09-71a064a42125/mac#:~:text=Terminal%20for%20me-,Open%20Terminal,%2C%20then%20double%2Dclick%20Terminal.) for MacOS) and run the following command:

*Windows (Note: Remember to Run as Administrator)*

```bat
choco --version
```

*Mac OS*

```sh
brew --version
```

If you see the version information and not an error, then you all set to go on the next step!

### Step 1.2: Installing Python 3, Git, and VSCode

For this step, you will need to install [Python 3](https://www.python.org), followed by [Git](https://git-scm.com/), and finally [Visual Studio Code](https://code.visualstudio.com), using the package manager you just installed in the previous step.

To do this, make sure you are in a command line terminal, and enter the following commands:

*Windows*

```bat
choco install python
choco install git
choco install vscode
```

*Mac OS*

```sh
brew install python
brew install git
brew install --cask visual-studio-code
```

If the installation went well, you should be able to enter the following command and display the version information for Python and Git:

*Windows*

```bat
python --version
git --version
```

*Mac OS*

```sh
python --version
git --version
```

For VSCode, you should be able to launch a program called `Visual Studio Code` using either the Windows [Start Menu](https://support.microsoft.com/en-us/windows/open-the-start-menu-4ed57ad7-ed1f-3cc9-c9e4-f329822f5aeb) or the Mac OS [Launchpad](https://support.apple.com/en-ca/guide/mac-help/mh35840/mac).

**Note**: If the installation errors at any point, a common solution is try uninstalling and reinstalling the packages that are giving you errors.

This can be simply done using the `uninstall` and `reinstall` commands in `choco` or `brew`:

*Windows*

```bat
choco install python
choco install git
choco install vscode

choco uninstall python
choco uninstall git
choco uninstall vscode
```

*Mac OS*

```sh
brew uninstall python
brew uninstall git
brew uninstall --cask visual-studio-code

brew install python
brew install git
brew install --cask visual-studio-code
```

### Step 1.3: Installing the Python Extension in VSCode

In order to use Python in VSCode, we need the [Python Extension](https://marketplace.visualstudio.com/items?itemName=ms-python.python) installed in VSCode if it has not been installed already.

Launch the `Visual Studio Code` program, on the left hand sidebar (with the icons), hover over the icons and you will see one named `Extensions`.

Click on `Extensions`, look up `python`, find the `Python` extension from Microsoft, and click `Install` if it has not yet been installed.

This extension allows VSCode to interact with your installed Python software.

## Step 2: Setup

The second step is to setup a development environment with Git and VSCode by:

1. Cloning (copying) the version controlled (by Git) internship folder and files into your desktop
2. Opening the internship folder with VSCode
3. Switching to your `dev` branch (a copy of the folder that tracks your changes only)

### Step 2.1: Clone the Internship Repository

First, open a command line terminal and use the `cd` change directory command to move to your desktop:

*Windows*

```bat
cd C://Users/NAME/Desktop
```

**Note**: Replace `C://Users/NAME/Desktop` with the actual path of to your desktop (e.g. `C://Users/Richard/Desktop`)

*Mac OS*

```sh
cd Desktop
```

Next, clone the internship repository, entering your Github credentials when prompted:

```sh
git clone https://github.com/cghr-toronto/msa-2024-internship
```

You should now see a folder named `msa-2024-internship` on your desktop.

**IMPORTANT**: At this point, if you are using a cloud sync service such as OneDrive or Google Drive, you should omit the `msa-2024-internship` folder from being synced to the cloud as the Python packages to be installed will cause issues from the large amount and size of files.

### Step 2.2: Open the Folder in VSCode

For this step, open `Visual Studio Code`, and you should see a code editor pop up.

In VSCode, go to `File --> Open Folder...`, then select the `msa-2024-internship` folder.

You should now see that VSCode has opened the `msa-2024-internship` folder, and the files inside will be on the left pane, while any files you open will presented on the right pane.

Let's now create a branch in the next step!

### Step 2.3: Switch to Your Branch

In VSCode with your `msa-2024-internship` folder open, go to `Terminal --> New Terminal` and you should see a command line terminal popup at the bottom pane of the code editor.

A branch called `dev` has already been created for you to work on.

Inside of this command line terminal, switch to your branch with the following command:

```sh
git checkout dev
```

You can check which branch you are on with the following command:

```sh
git branch
```

Congrats - you have just installed and setup your development environment!

Feel free to take a break here or move onto the next section.

## Step 3: Installing Python Packages

This step will have you installing Python packages into a virtual environment and creating a list of installed packages with exact versions for anyone to reproduce your Python development environment, which involves:

* Creating a Python virtual environment
* Installing Python packages into the virtual environment
* Creating a list of packages to reproduce your package installation
* Initiating a Jupyter Notebook file in VSCode

### Step 3.1: Creating a Python Virtual Environment

First, we need to create a Python [virtual environment](https://docs.python.org/3/library/venv.html) - an isolated Python folder of files that keeps package installations separate from your main Python installation.

The reason we do this is to avoid software conflicts and to also ensure that our software is reproducible when we want someone else to use it. It also makes a great sandbox environment for us to break and experiment with things without worry!

Go to your terminal opened in VSCode, and create a python 3.11 environemnt in the `tmp/venv` folder:

*Windows*

```bat
py -m venv tmp/venv
```

*Mac OS*

```sh
python3 -m venv tmp/venv
```

Once it is successfully created, you should see a folder `tmp` and a subfolder `venv` inside of it.

Now activate your virtual environment!:

*Windows*

```bat
tmp\venv\Scripts\activate.bat
```

*Mac OS*

```sh
source tmp/venv/bin/activate
```

You should see a `(venv)` text next to all your prompts showing you that you activated your Python environment.

Now we need to let VSCode know that our virtual environment has been created.

To do this, right click on the left pane and create a new folder called `.vscode`, then create a file called `settings.json` with the following contents:

*Windows*

```json
{
    "python.defaultInterpreterPath": "tmp/venv/Scripts/python",
}
```

*Mac*

```json
{
    "python.defaultInterpreterPath": "tmp/venv/bin/python",
}
```

If you did this correctly, you should have a file in `.vscode/settings.json`, which allows VSCode to see our virtual environment.

To test this, in VSCode, go to `View --> Command Palette...`, find and click on `Python: Select Interpreter`.

You should see a recommended interpreter similar to `Python 3.XX.X ('venv': venv) ./tmp/venv/bin/python` (`3.XX.X` is the version of Python in your virtual environment and varies depending on which one you installed) - you do not need to set this yet, but it is a good test to see that VSCode has found your virtual environment.

If you do not see this, close and restart VSCode, and try to find the interpreter again.

### Step 3.2: Installing Python Packages

Python packages from the Python Package Index (https://pypi.org/) can be installed with `pip` in a command line terminal.

For this tutorial, we will install the following Python packages:

* `jupyter`: a package for interactive Python notebooks
* `pandas`: a package for manipulating data
* `geopandas`: a package for extending `pandas` to handle spatial data
* `matplotlib`: a package for extending plotting data

In VSCode, go to your terminal (Note: Ensure that you activated your virtual environment, indicated by the `(venv)` next to all your prompts) and enter the following commands to install the above packages:

```sh
pip install jupyter
pip install pandas
pip install geopandas
pip install matplotlib
```

The Python packages should install successfully without too many issues.

### Step 3.3: Creating a Reproducible List of Python Packages

Inside the same terminal from the previous step, go ahead and create a list of packages with exact versions to reproducible your development environment:

```sh
pip freeze > requirements.txt
```

You will see a file called `requirements.txt` that lists all the required packages for your development environment based on the packages you installed (dependent packages included).

### Step 3.4: Create a Jupyter Notebook

Now that you have installed the `jupyter` library, you should be able to create interactive Python notebooks with Jupyter.

In VSCode, go to `View --> Command Palette` and search for `Crate: New Jupyter Notebook`, then click it.

This will create a new notebook in VSCode on the right pane.

Click on your new notebook and save it `File --> Save As...` as `main.ipynb` under your `tutorial` folder.

If you have done this right, you should have a file under `tutorial/main.ipynb`.

### Step 3.5: Checkpoint! Saving your Results to Github

Now that you have made it this far, its time to save all your hard work to Github!

First let's make sure you are in the right branch.

In a VSCode terminal, check that you are in the right branch (the `*` should be on `dev`):

```sh
git branch
```

Once you are in the right branch, lets add all the local files you have currently in the `msa-2024-internship` folder (your repository):

```sh
git add .
```

Next we commit all the local changes on your computer you currently have:

```sh
git commit -a -m "Tutorial checkpoint - step 3.5"
```

And then we finally send these changes to the remote repository:

```sh
git push
```

If this went through, you should be able to see an exact copy of your files and changes you made at:

https://github.com/cghr-toronto/msa-2024-internship/tree/dev

Great job! You have now not only created your development environment, but sent in your changes the remote repository so that anyone on the team can refer to your code, collaborate with you, or help you out.

Feel free to take a break here or if you're up for it, dive straight into the next step!

## Step 4: Having Fun with Data!

Now that you have created a development environment like a pro and installed the required software, we can start the fun stuff!

The [pandas](https://pandas.pydata.org/) package allows you to read, write, visualize, and manipulate non-spatial data, while the [geopandas](https://geopandas.org/en) package extends these functions over to spatial data.

### Step 4.1: Loading Python Packages

In your `tutorial/main.ipynb` file, add a code cell (the plus symbol `+ Code`) with the following Python code to load the required packages:

```py
import pandas as pd
import geopandas as gpd
```

Click on the run button (triangle play icon) next to the cell to run the Python code and load the packages (on first run, it may take a little bit to finish loading, but on subsequent runs it should be faster).

### Step 4.2: Loading Spatial Data

Create another code cell to load the tutorial data:

* `toronto-nbh.geojson`: Polygon file of Toronto neighborhood boundaries
* `toronto-rlc.geojson`: Point file of Toronto red light camera intersections

In this cell, add the following code:

```py
# Store the read files in nbh and rlc variables
nbh = gpd.read_file('toronto-nbh.geojson')
rlc = gpd.read_file('toronto-rlc.geojson')
```

Run this cell and ensure that the files are loaded into variables `nbh` and `rlc`.

#### Some background on Spatial Data Structures

Geopandas (`gpd`) will load these files as [GeoDataFrames](https://geopandas.org/en/stable/docs/reference/geodataframe.html), which are table structured objects in Python that can perform the same functions as a pandas (`pd`) [DataFrame](https://pandas.pydata.org/docs/reference/frame.html), but with extended spatial coordinates and functionality.

Each column in a geopandas GeoDataFrame is either a [GeoSeries](https://geopandas.org/en/stable/docs/reference/geoseries.html) or pandas [Series](https://pandas.pydata.org/docs/reference/series.html).

Thus, a GeoDataFrame is a collection of Series (non-spatial) and GeoSeries (spatial).

### Step 4.2: Viewing Spatial Data

Create another code cell and preview the neighborhood data non-spatially:

```py
nbh
```

Also, create another code cell and preview the red light camera data non-spatially:

```py
rlc
```

Now create a code cell and preview the neighborhood data spatially by plotting a map using the [plot](https://geopandas.org/en/stable/docs/reference/api/geopandas.GeoDataFrame.plot.html) method:

```py
nbh.plot()
```

And again create a cell with the red light camera data:

```py
rlc.plot()
```

Finally, let's map the red light camera data on top the neighborhood data in another cell:

```py
# First plot the nbh data
base = nbh.plot(
    color = 'white',
    edgecolor = 'black'
)

# Then plot the rlc data on top of the nbh
rlc.plot(
    ax = base,
    color = 'red',
    markersize = 5,
    legend = True
)
```

#### Checking the API Reference Documentation

Here we used some options for the `plot` method such as `color`, `ax`, `legend`, but how do we know what the options are?

If the package is properly documented, thats easy!

We check the API documentation for the `plot` function from its reference page:

https://geopandas.org/en/stable/docs/reference/api/geopandas.GeoDataFrame.plot.html

You will see many other interesting options such as `cmap` and `scheme`.

### Step 4.3: Joining Spatial Data

Next, we will do some data manipulation and spatially join the red light camera data to the neighborhood data.

This will give us the ability to count the number of red light traffic camera intersections for each neighborhood.

Add a cell with the following code to join spatial data (Notice that comments are code that are not executed, and these are denoted by the `#`):

```py
# Spatially join rlc to nbh by intersecting rule
join = nbh.sjoin(rlc, predicate='intersects')

# With the joined data, count the number of rlcs in each nbh
counts = join.groupby(join.index).size()

# Name the counts column
counts.name = 'rlc_counts'

# Then add the joins to the nbh data
nbh_rlc = nbh.join(counts)

# Fill in any empty values as 0
nbh_rlc['rlc_counts'] = nbh_rlc['rlc_counts'].fillna(0)
```

After you run the code above and process the join, lets map the red light traffic intersection counts:

```py
nbh_rlc.plot(
    column = 'rlc_counts',
    cmap='coolwarm',
    legend = True,
    legend_kwds={
        'label': 'Red Light Camera Intersections',
        'orientation': 'horizontal'
    },
)
```

#### Other Spatial Tools

In addition to spatial joins, there are a variety of other handy tools that you can use to do spatial processing, such as `clip`, `overlay`, and `sjoin_nearest`:

https://geopandas.org/en/stable/docs/reference/tools.html

And similarly, non-spatial processing tools from pandas such as `concat` and `merge`:

https://pandas.pydata.org/docs/user_guide/merging.html

And selecting and filtering data:

https://pandas.pydata.org/docs/user_guide/indexing.html

These are quite a lot to learn in a short period of time!

Luckily, we have had advances in Artificial Intelligence (AI) and Large Langugage Models (LLM) like ChatGPT can accelerate you ahead and help you get the syntax right on these - of course you will need to double check the logic and see if the code is actually doing what you want it to!

### Step 4.4: Writing Spatial Data

Finally, we'll save the joined data into the tutorials folder in another cell:

```py
nbh_rlc.to_file('toronto-nbh-rlc.geojson', driver='GeoJSON')
```

You should see a file called `toronto-nbh-rlc.geojson` in the `tutorial` folder.

Don't forget to save your `tutorial/main.ipynb` opened on the right pane!

Click on it, and go to `File -> Save` to save your `main.ipynb` file before proceeding to the next step.

### Step 4.5a: Checkpoint! Saving Once Again to Github

> Good habits make time your ally. Bad habits make time your enemy.

- James Clear, Atomic Habits

Congratulations! You now have all the basic tools to get started with this internship!

Always remember to make a good habit of saving your code to Github with relatively good commit messages once you've made significant strides in your work (I recommend at least at the end of the day!):

```sh
git add .
git commit -a -m "Finished tutorial step 4.5!"
git push
```

Now check that they are properly saved to Github:

https://github.com/cghr-toronto/msa-2024-internship/tree/dev

You should see a very recent timestamp next to `X commits`.

### Step 4.5b: Checking Developer Documentation

If you haven't already, check out (quickly glance and perhaps have some questions) the documentation pages for the pandas and geopandas packages, which will give you more details on the particular functions we used above:

* `pandas`: https://pandas.pydata.org/docs/reference/index.html
* `geopandas`: https://geopandas.org/en/stable/docs/reference.html

## Step 5: Restoring Your Development Environment

Now what happens when you lose your files on your computer or worse, your computer is broken!

As long as you made a good habit of saving your code to Github, you can likely restore them (so as long as Github still exists!).

Let's try this out by opening a command line terminal on your computer.

Navigate to a location that is not the Desktop with `cd`.

```sh
cd some/location/not/desktop
```

Then clone the repository (ensuring you have `git` installed):

```sh
git clone https://github.com/cghr-toronto/msa-2024-internship
```

All your files are now in the folder, but wait - you still need to restore your virtual environment for things to work (ensure that you have Python 3 installed):

*Windows*

```bat
cd msa-2024-internship
python3.11 -m venv tmp/venv
tmp\venv\Scripts\activate.bat
pip install -r requirements.txt
```

*Mac Os*

```sh
cd msa-2024-internship
python3.11 -m venv tmp/venv
source tmp/venv/bin/activate
pip install -r requirements.txt
```

What this does is move into your cloned folder, create the Python virtual environment, activate it, then install the required packages to make your code work with the `requirements.txt` file you previously created.

Now we can open this folder in VSCode, and proceed work as usual (and of course, always push your changes to Github when you've done some work!).

## Step 6: Pat Yourself on the Back

Now that you have completed this tutorial, you are in a good position to further your knowledge in spatial data engineering!

Great Job! You should now be able to:

* Install software on your OS with a package manager through the command line
* Setup a Python development environment linked to Github for version control
* Create and work with interactive Jupyter Notebooks in VSCode
* Read, write, plot, and join spatial data

During the course of the internship, you will begin to work and explore independently under my supervision and advice to figure out how to tackle certain tasks with the power of the internet (Google and ChatGPT really.. and likely lots and lots of trial and error).

I hope you learned a lot and are excited to work with the data science team to improve Open Mortality - helping the living, study the dead!

## Some Helpful Commands for Your Journey

Reminder 1: Use `Run as Administrator` on your command line terminal on Windows (if you are not admin).

Reminder 2: Commit, commit, commit! Remember to always save your local changes and push them to your remote Github repository once you have made some progress in your work.

### OS Package Manager (Chocolatey or Homebrew)

Install software for your OS (replace `<package` with the software name):

*Windows*

```bat
choco install <package>
```

*Mac OS*

```sh
brew install <package>
brew install --cask <package>
```

List software installed for your OS:

*Windows*

```bat
choco list
```

*Mac OS*

```sh
brew list
brew list --cask
```

Uninstall software for your OS (replace `<package` with the software name):

*Windows*

```bat
choco uninstall <package>
```

*Mac OS*

```sh
brew uninstall <package>
brew uninstall --cask <package>
```

### Python

Activating your virtual environment:

*Windows*

```bat
tmp\venv\Scripts\activate.bat
```

*Mac OS*

```sh
source tmp/venv/bin/activate
```

Deactivating your virtual environment:

```sh
deactivate
```

Installing a package (replace `<package>` with a package name to install):

```sh
pip install <package>
```

Note: Remember to `pip freeze` like below if you install or remove any new packages in your virtual environment.

Saving a list of your currently installed packages:

```sh
pip freeze > requirements.txt
```

Installing packages from a previously saved list:

```sh
pip install -r requirements.txt
```

### Git

Cloning (copying) a remote repository to your local computer (URL can be replaced for another repository):

```sh
git clone https://github.com/cghr-toronto/msa-2024-internship
```

Checking branches:

```sh
git branch
```

Switching branches (replace `<branch>` with your branch name):

```sh
git checkout <branch>
```

**Saving your local changes and pushing to remote Github repository:**

```sh
git add .
git commit -a -m "Some meaningful message for changes"
git push
```

Note: Replace `"Some meaningful message for changes"` with something meaningful.
