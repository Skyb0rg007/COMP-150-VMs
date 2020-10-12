
    # This is a comment
    # Registers are specified via '%n', where n is an integer
    # So register 1 is '%1'
    # Each line is its own instruction, unless separated with ';'
    %1 := %2 + %3; print %1
    # Labels may be placed on their own lines, or on the same line as an instruction
lbl: print %1
    another-label:
    print %1

    # Arithmetic
    %1 := %2 + %3 # Addition
    %1 := %2 - %3 # Subtraction
    %1 := %2 * %3 # Multiplication
    %1 := %2 / %3 # Division
    %1 := %2 % %3 # Modulo
    %1 := %2 // %3 # Integer division
    %1 := hash %2 # Not really arithmetic I know

    # Comparisons
    %1 := %2 = %3
    %1 := %2 < %3
    %1 := %2 > %3
    %1 := %2 <= %3
    %1 := %2 >= %3

    # Type predicates
    %1 := function? %2
    %1 := pair? %2
    %1 := symbol? %2
    %1 := number? %2
    %1 := boolean? %2
    %1 := null? %2
    %1 := nil? %2

    # List functions
    %1 := car %2
    %1 := cdr %2
    %1 := cons %2 %3
    
    # Control flow
    halt
    error %1
    goto -2 # Jumps by that offset
    goto example-label # Jumps to that label
    example-label: # Label, attached to the following line (note that '-' is valid in labels)
    if %1
    
    # Printing
    print %1
    printu %1
    println %1
    
    # Literals
    %1 := "string literal" # Strings
    %1 := 12 # Ints
    %1 := 3.14159 # Float literals are supported
    %1 := 3.14159e10 # With extended syntax too
    %1 := true # More literals
    %1 := false
    %1 := emptylist
    %1 := nil
    
    # Loading functions
    # The number following 'function' is the arity
    %1 := function 2 {
        # Note that you cannot jump from to a label defined outside the function
        local-label: print %3
        goto -2
        goto local-label
    }


