## Game of Life

Game of Life boards are available in the `.rle` file. The tool `rle`
compiles `.rle` files into boolean vector data suitable for Futhark to eat:

    bash-3.2$ ./rle --flat < ex.rle
    [15,15]
    [0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0]
    Done!

To get more boards, please consult the [wikilife page](http://www.conwaylife.com/wiki/Category:Patterns).

First generate the futhark program:

    bash-3.2$ (cd ..; make fut_lifein)

Then, try to run the executable `fut_lifein` with some input:

    bash-3.2$ make ex.futout
    ../fut_lifein < ex.futin ex.futout
    Assertion assert_arg_9673 at lifein.fut:196:97-196:97 failed.
    /bin/sh: line 1: 79233 Abort trap: 6           ../fut_lifein ex.futout < ex.futin
    make: *** [ex.futout] Error 134
    bash-3.2$

The output may be compared with the result of running the program using the `aplt` interpreter:

    bash-3.2$ make withaplt
    ~/gits/apltail/aplt ~/gits/apltail/lib/prelude.apl ../lifein.apl
    [Reading file: /Users/mael/gits/apltail/lib/prelude.apl]
    [Reading file: ../lifein.apl]
    Evaluating


       ooo     ooo  





       ooo     ooo  








    Result is [](1.0)
    