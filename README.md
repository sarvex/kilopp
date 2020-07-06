Kilopp
===

[Kilo](https://github.com/antirez/kilo) is a small text editor in less than 1K lines of C code.

This project is an ongoing attempt to implement it in C++ 17. It is made for educational purposes and you probably won't get any benefits using it instead of the original Kilo editor.

Feel free to open pull requests or issues suggesting other improvements.

## Goals
* No external dependencies, just like Kilo.
* No fancy build system or use of multiple files.
* No new features should be added to kilopp.
* Avoid using raw pointers, C arrays and C strings as much as possible.
* Resource management should be performed using RAII.
* Error reporting should be done using exceptions instead of return values.