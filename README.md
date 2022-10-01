Basic-x86 is a basic interpreter developed completely in assembly language and designed for IBM compatible computers,
which therefore use a processor belonging to the x86 family.
The interpreter does not want to be a professional and really usable interpreter (also because who still programs in BASIC?) But a simple project to improve my skills as a programmer.
The BASIC interpreter is neither efficient nor pleasant to use, it does not make use of all the information about compilers, but it uses a part of it and limits itself to sketching out 
the functioning of a real interpreter.
it does not need any operating system as it was designed to run in bare metal, for its execution you will therefore need an IBM compatible computer emulator such as QEMU or BOCHS

![Schermata del 2022-02-28 13-55-40](https://user-images.githubusercontent.com/74959879/155987072-e2b84be2-a4b5-41a4-b819-d3c1be605b5d.png)

# Feauters

All the interpreter features:

   • all high-level logical structures (IF, FOR, WHILE, GOTO).
  
   • input/output instructions (print, input).
  
   • instruction to arbitrarily access memory (write, read, machc).
  
   • instruction to arbitrarily access hardware ports (inp, outp).
  
   • mathematical and logical operators (+, -, *, /, %, ABS, |, &).
   
   • operators for comparison (==, !=, <, <=, >, >=).
  
# Source

The source consists of 9 modules, plus 1 header file containing the information of the initial screen.

• main.asm - the main module of the program, makes calls to the main procedures of the secondary modules.
   
• utils.asm - module containing general utility procedures.

• lexer.asm - contains the procedures necessary to carry out the syntactic analysis of the program.

• parser.asm - it contains the procedures necessary to carry out the semantic analysis of the program.

• code_gen.asm - module containing the procedures necessary for the generation of the machine language instructions.

• code_gen_lib.asm - support module of the code_gen.asm module.

• graphics.asm - contains the procedures for managing the interpreter's graphics.

• subroutine.asm - contains all the subroutines activated by the interpreter, such as the one for comparing two strings.

• expression.asm - contains the procedures for the management of algebraic expressions.

• BASIC.INC - contains all the information for the start screen.

command line commands are as follows:
	
• RUN - to run the programs in multi-line mode.
	
• SHVT - to show th variable table.
	
• TIME - to show the current time.
	
• RESET - to restart the basic interpreter.
	
• CLS - to clear the screen.


Then there is a module for loading the BASIC interpreter in RAM memory, that is the one for the interpreter bootloader
(boot.asm).

# Execution

Since the program was designed to run in bare metal, the image of the virtual disk containing the instructions of the interpreter is provided, the name of this file is "basic.img", it is strongly recommended not to modify the contents of the file, a modification could lead to a malfunction of the interpreter that would not start anymore, or would crash apparently meaningless.

To run the program via QEMU type from the terminal:

	qemu-system-i386 -rtc base=localtime -hda basic.img
	




  

