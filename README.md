# JHM - Java Haskell Machine

A very simple JVM implementation in Haskell.

## Working code example

Below is a sample code that can be executed by this virtual machine.

```java
public class Main {
    public static int fibonacci(int n) {
        if (n < 2) return n;
        else return fibonacci(n-1) + fibonacci(n-2);
    }

    public static void main(String[] args) {
        System.out.println("Fibonacci(9) = ");
        System.out.println(fibonacci(9));
    }
}
```

This can be compiled with `javac Main.java`, which will output a
`Main.class` file, which then can be read by the JVM.

## Build instructions

### Prerequisites

 - `ghc` - Haskell compiler
 - `cabal` - Haskell build system
 - `javac` - Java compiler, only to produce the class file

### Build and run

To build and run the application use this:

```bash
cabal run -- jhm -- <ClassName>
```

this will look for a file named `<ClassName>.class`, and execute its
main method, similar to `java` command.
