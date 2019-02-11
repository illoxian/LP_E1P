LittleManComputer - by 829474 Sgro Mattia
Lisp implementation
(possibily following the ANSI CL)


Built with Emcas, LispWorks when needed, with much but less love than prolog. 
By Mattia Sgro,
badge number 829474, student at Universita' degli Studi di Milano - Bicocca



- Features -
A perfect working and functional lmc simulator, parser included!
(Bugs may be present more there than the prolog version, 
just let's see them as hidden features)

- Tests made -
Tested with all *.lmc assembly files provided at 
https://elearning.unimib.it/course/view.php?id=19227

- How to run it -
If you are under windows you can *try* to use LispWorks personal edition,
compiling al loading
    > lmc.lisp
    > lmc_run('assembly.lmc', your_input_queue, Out)
your input queue must be a list with your inputs (in any requested by your as-
sembly file)
If you are now following the path to be what I'll never be - a Jedi Master -
you can use SBCL for your *nix distribution.

- Function Specification -
I won't ensure anything, but the code is (uncommented) less than 300 lines.
I belive that such things have to be underlined!

is-istr (string) => T / NIL
    true if given input is an instruction of the LMC instruction set
is-label (string)
    true if given input is a legal label
is-digit (string)
    true if given input is a number between 0 and 999
is-legal (string)
    true if given input is a number between 0 and 99


replace-in-list (list item-to-insert index) => newlist
    replace an item in a list at given index, with item-to-insert


parse-comments (string)
    pareses comments
fill-no-op (list)
    fills instruction with no argument with argument 0 (to simplify code)
no-space (string)
    to remove empty line codes

(those iteration are made TAIL-RECURSIVE)
listing-istr (list)
    iterates no-space on the MEMORY
listing-fill-no-op (list)
    iterates


labeler (list)
    return a label if in first position of instruction
list-labeler (list)
    filling the list of labels iterating labeler

elider (list)
    removes labels in front of an instruction
list-elider
    iterates

linker (list)
    if has a label in position 2, matches it with the one in listOfLabels and
    substitutes index
list-linker (list)
    iterates

assembler (list)
    assemble an instruction
list-assembler (list)
    iterates on memory

lmc-load (filename) => memory
    loads a filename and pulls out a memory of MACHINE codes

loadfile (stream)
    to work with the input file stream

fill-memory  (lista)
    fills the memory if needed, fails on memory greater than 100 istr

parserino (list)
    this name hasn't been changed 'cause seemd quite funny
    parses a memory, calling once and in chain
    fill-memory    list-fill-no-op    list-linker    list-elider
    list-labeler   listing-istr       list-parse-comments


check-mem (state) => T/NIL
    checks the memory to be exactly 100 istr (if not given from the parser)
    checks the first state to be STATE and not HALTED-STATE

execution-loop (state) => output 
    stops and gives output on halted state,
    fails on failed input among other things, and gives current output
    iterates one-instruction until stops

one-instruction (state) => newState
    "a cond to divide them all"  -semicit.
    with the use of cond, switches between instruction and outputs the newState



Released under MIT License © by Mattia Sgro, badge number 829474.
