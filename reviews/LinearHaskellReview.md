# Linear Haskell

## The gist
What I understood from the blog post and paper on Linear Haskell is that by putting in the type definition a guarantee (enforced by the compiler) that a value will be consumed in the function exactly once there are certain features and performance improvements that can occur. It reminded me of the statement made at the PL lunch: "Constraints liberate, liberations constrain". 

## Similar to Rust
This principle of limiting the programmer as a means to improve the programmer's life is seen in languages like Rust. In Rust, the compiler is notoriously strict, but with strictness comes far more runtime guarantees (namely memory safety and no nulls). With this philosophy, the compiler is doing more work in service of the programmer and not the other way around. 

The great thing about making the programmer adhere to strict compiler rules is that the code's intent becomes clearer as well. In Rust, when you declare a variable as mutable, you not only allow yourself to mutate it but express the *expectation* that it *will* be mutated at some point in the program execution. Otherwise, what is the point of labelling it so. 

## What LH does for the programmer
Linear Haskell makes some compile-time guarantees. For example, a linear value that is allocated for the beginning of a function call, can safely be freed after being consumed within the function. This puts less work on the GC and therefore gives us more uptime performance. 

The other benefits as described in the article were more difficult for me to understand. But generally, I understood that the use of linear typing allows us to have simpler function and type definitions and not have to rely so much on monads for side-effects. The paper talks about not needing all the complexity of the ST monad in order to use random access Arrays. With linear typing, operations on the arrays can be safe without monadic complexity. For example, updating the array can be done by mutation under the hood but will return a logically new array. I'm still not quite sure how this ensures purity. 

The blog post shows a nice example of a client and server. But making a function definition of a function sent to the server linear, it guarantees that the function will be called and exactly once. This means that the server can't hang and never respond to the client and it cannot respond twice. 

The backwards-compatibility of LH is very cool. It is not often that you can add a comprehensive feature to a language to improve it but make it work just as well with old code-bases. 