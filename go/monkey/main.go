package main

import (
	"fmt"
	"io"
	"log"
	"monkey/repl"
	"os"
	"os/user"

	"monkey/evaluator"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	in := os.Stdin
	out := os.Stdout

	if len(os.Args) == 1 {
		fmt.Printf("Hello %s! This is the Monkey programming language!\n",
			user.Username)
		fmt.Printf("Feel free to type in commands\n")
		repl.Start(in, out)
	} else if len(os.Args) == 2 {

		file_name := os.Args[1]

		file_input, err := os.ReadFile(file_name)

		if err != nil {
			log.Fatal("couldnt read file ", file_name)
		}

		env := object.NewEnvironment()

		l := lexer.New(string(file_input))
		p := parser.New(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			repl.PrintParserErrors(out, p.Errors())
			return
		}

		evaluated := evaluator.Eval(program, env)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}
	}
}
