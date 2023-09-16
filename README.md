# JHM - Java Haskell Machine

A very simple JVM implementation in Haskell.

## Working code example

Below is a sample code that can be executed by this virtual machine.

```java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello world!");
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
