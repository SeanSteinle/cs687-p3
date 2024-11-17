# INTRO:

Hello! I made some organizational changes because it made things easier for me. If you don't like them, feel free to keep developing in the way you were and just copy my changes over.

# ORGANIZATION/HOW TO RUN

Because this is a very big project and I work better with smaller code modules, I split up the large assignment file into smaller files:
    - `instances/` - The instance-specific code to solve each of the 4 main instances we have to solve. For example, has the setup functions and evaluation functions.
    - `core_ec.lisp` - For EC code that spans multiple instances. For example, contains the `evolve()` function, tournament selection, and the genetic programming functions.
    - `tests.lisp` - Very simple unit-test style checks for each of the files. I am horrible at debugging Lisp so I wrote a ton, feel free to ignore most. I use this for test-driven development.
    - `utils.lisp` - Small functions that the professor provided.
    - `instructions.txt` - The original instructions provided.

I run the code by making whatever changes I'd like, writing a test for those changes, then calling `(load "tests.lisp")` from the SBCL repl. This automatically pulls updates from the other files (see the top of `tests.lisp`) and runs the test. Plus, you know have all the variables in scope that you need if you want to debug on the command line.

Because the professor likes everything in a single file to submit, I'll copy all the lisp code back together and run it once we're completely done.

# NEXT STEPS:

Here's the rest of the development path:

1. Finish `gp-modifier()` in `core_ec.lisp`. This will allow us to write the code for each of the 2 harder instances, symbolic regression and artifical ants. To finish this, do:
    - Write `nth-subtree-parent()`. This is the crucial function for `gp-modifier()`.
    - I stubbed out the logic for `gp-modifier()`, you just have to translate it to Lisp.
2. Symbolic Regression specific functions, mostly symbolic-regression-evaluate (Sean).
3. Artificial Ant specific functions (Ray).
4. Add the rest of the boolean-vector cases (pack 1's, etc.).
5. Evaluate all of our results and write a report.

If you can take (1) I would be glad to do (4) and (5)! I don't know why but the `nth-subtree-parent()` has really taken me out.

