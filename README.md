# JHM - Java Haskell Machine

A very simple JVM implementation in Haskell.

## Working code example

Below is a sample code that can be executed by this virtual machine.

```java
public class Main {
    public static int doSomething(int a, int b) {
        int c = a + b;
        return c;
    }

    public static void main(String[] args) {
        System.out.println("Hello world!");

        int a = 20;
        a -= 12;

        int res = doSomething(a, 10);
        System.out.println(res);
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

To build and run the application, simply run:

```bash
cabal run
```
