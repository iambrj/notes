A text stream consists of a sequence of lines; each line ends with a newline
character. If the system doesn't operate that way, the library does whatever is
necessary to make it appear as if it does (convert carriage return to newline
etc)

Programs are oblivious to I/O redirection; in particular, strings like
"<infile" are not included in the command-line arguments in argv.


