LittleManComputer - by 829474 Sgro Mattia
Prolog Implementation


Built with Emacs and swipl, with much love.
By Mattia Sgro,
badge number 829474, student at Universita' degli Studi di Milano - Bicocca



- Features -
A perfect working and functional lmc simulator, parser included!
(Bugs may be present, just let's see them as hidden features)

- Tests made -
Tested with all *.lmc assembly files provided at 
https://elearning.unimib.it/course/view.php?id=19227

- How to run it -
Just open the Prolog interpreter (swipl.exe or swipl in any *nix distribution
available), consult the file, with
    > 'LMC.pl'           or
    > consult('LMC.pl')
and load your assembly file with
    > lmc_run('assembly.lmc', your_input_queue, Out)
your input queue must be a list with your inputs (in any requested by your as-
sembly file)

- Predicate Specification -
Many different predicates are used to ensure a faster and easier debug of the
code during the build process; any code refactoring as be performed, nor heavy
optimizations (i.e. something as been surely made simple for faster load time)

is_slash/1 
is_doubleSlash/1
    these two methos are used to find "//" pattern i.e. beginning of a comment

parsing/2
parseMem/2
    parsing basicaly removes a comment from a string, parseMem iterates all over
    the memory

to_string/2
    lowers the digit of the memory, converts atoms to strings (for clearify the
    process later on)
delete_white_in_front/2
    needed this to remove extra blank characters before each instruction

is_istr/1
    true if given input is an instruction of the LMC instruction set
is_label/1
    true if given input is a legal label
is_digit/3
    is_legalN/1
        checks a number between 0 and 999 included
    is_legal/1
        checks a number between 0 and 99 included
replace/4
    needed this for STA operation (to replace a memory entry with ACC)


labeler/3
    makes a list of labels used in the file (just those to jump to), and removes
    them from the memory
assemble/3
    assembles memory instruction from TEXT STRING to MACHINE CODE
substituteLabels/3
    uses the list of labels made by labeler to substitute labels in instruction
    with respective addresses
fill_trail_zeros/2
    fills the memory until it's made by 100 cells
    (possibly, launch this right after the comment parser to fail in case of
    memories greater than 100)


lmc_load/2
    standard predicate given in the implementation: loads and ASM file and pulls
    out a memory of 100 instructions 
lmc_run/3
    calls lmc_load and runs LMC SIMULTOR 
execution_loop/2
    given an instruction, iterates one_instruction/2 until finds an HALTED_STATE
one_instruction/2
    given an instruction, executes it (one program counter step). One or more
    goals for each ASM instruction.




Released under MIT License © by Mattia Sgro, badge number 829474.
