# Affordable 2nd-Class Values for Fun and (Co-)Effect

## Key points
- "[Second class functions] always have to appear in person and can never be represented by a variable or expression (except in the case of a formal parameter)."

## Review
The main idea of this paper is that while first-class objects give a language expressiveness and flexibility (leading to a number of desired PL effects) demoting certain values to second class can actually give the program more static guarantees and readability. It is an example of constraining the programmer to give them more safety.

### First and second-class values
A second-class value is a value that cannot be stored in a variable or be returned from a function. It CAN however be passed as the actual parameters of a function call. 

### Usage
In this paper, they describe their extension of the Scala language that allows an annotation to consider a value a second-class one in the definition of a function. If a parameter is marked as `@local` then the argument is considered to be second-class and can therefore not be assigned to another variable or returned from a function call. This very simple revocation of the privileges of a value allow for static guarantees that can be checked at compile time. 
The example they gave was for opening a file and then acting on it with a call-back. Making the File a second-class value guarantees that the File object does not escape the scope of the callback. After all, file operations are time-sensitive and a read or write must happen after an `open` and before a `close`. Confining the reference to the File to its usage inside the callback guarantees that it is not used in a way that would violate its time-sensitive constraints and thus cause a run-time error. Instead, a compile-time error protects us from potential run-time crashes. 

### Principle
The principle behind this constraining of usage of certain values is similar to Rust's ownership/borrowing system as well as linear types as we read earlier. In each case, the constraint may remove some expressiveness and flexibility (which is a feature desired in many situations) for safety, and compile-time guarantees (which are desirable in many other situations). It seems to me, that in cases where one is building a large enterprise-scale application with a decently large team, the key to soundness of the application is compile-time checks and guarantees made possible by the disciplined *constraints* imposed by the language (as frustrating as they may seem to some.).

My question now is: what is the difference between first and second-class values in implementation? 