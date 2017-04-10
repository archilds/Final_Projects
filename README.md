# PHP 2560: Statistical Programming with R


## Class Shiny Apps

- On this page you will find shiny apps for all of the previous PHP 2560 classes.
    - [Class 01: Spring 2016](https://github.com/php2560/Final_Projects/tree/master/01_Spring_2016)
    - [Class 02: Fall 2016](https://github.com/php2560/Final_Projects/tree/master/02_Fall_2016)
- Note you will most likely need to look at the repository and code first to make sure you have all the necessary packages installed.
- There are a couple of ways to do this
- Follow the instructions below.


```R
# First clone the repository with git. If you have cloned it into
# ~/Final Projects, first go to that directory, then use runApp().
setwd("~01_Spring_2016/01_Draft_Kings/")
runApp()
```


To run a Shiny app from a subdirectory in the repo or zip file, you can use the `subdir` argument. This repository happens to contain another copy of the app in `01_Spring_2016/01_Draft_Kings/`.

```R
runGitHub("Final_Projects", "php2560", subdir = "01_Spring_2016/01_Draft_Kings")

```