# linsys

This is a small Haskell project that I wrote to better familiarise myself with the Parsec library and with git.

## Usage
You may either directly enter the input from `stdin` (the program terminates when it reaches an `EOF` character). or give a filename as an argument. The output will be given in `stdout`. You can find a pre-built executable in the build folder. To try it out, run the command `stack run -- input.txt`

## Input Format
Each variable is preceded by a dollar sign. The input consists of a sequence of equalities (one on each line), separated by `;` characters (see `input.txt` for an example).

## Output Format
If the system is inconsistent, the program will output `No solution`. If the system is dependent, then the variables which can take any values are listed in the output as equaling themselves. For example, for the input
```
3*$x+9*$y=-6;
-4*$x-12*$y=8;
```
The program will output:
$x = -3.0*$y-2.0
$y = $y