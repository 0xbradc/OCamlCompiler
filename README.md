# OCaml Compiler Project
Developed by Brad Campbell in the Spring of 2021.



# Getting Started
1. Install [Homebrew](http://brew.sh).

2. Run the following for needed dependencies.
  ```
  % brew update
  % brew install xquartz
  % brew install pkg-config
  % brew install gpatch
  % brew install opam
  % opam init -a
  % opam update
  % opam switch create 4.11.1
  % opam switch 4.11.1
  % opam install -y graphics
  % opam install -y ocamlbuild
  % opam install -y ocamlfind
  % opam install -y ocamlnet
  % opam install -y yojson
  % opam install -y merlin
  % opam install -y utop
  % opam install -y menhir
  % opam pin add CS51Utils https://github.com/cs51/utils.git -y
  % eval $(opam env)
  ```



# Running the Compiler
1. Open the project to the root directory and run the following command.
  ```
  ocamlbuild -use-ocamlfind miniml.byte
  ```
  
2. Interact with the compiler!



# Resources
This repository contains some starter code from Harvard's
CS 51 class, Abstraction and Design in Computation. For more information, see [https://cs51.io](https://cs51.io). 
