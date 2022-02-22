## μC

A reference implementation of the μC language for the [NI-APR](https://courses.fit.cvut.cz/NI-APR/) course.

## Requirements

- Scala 2.13.7
- SBT 1.5.8

## Building

The following will produce a fat JAR that includes all the code and dependencies.

```sh
sbt assembly
```

Alternatively, you can build a docker image:

```sh
sbt docker
```