## μC

A reference implementation of the μC language for the [NI-APR](https://courses.fit.cvut.cz/NI-APR/) course.

## Requirements

- OpenJDK 11 (please check `java -version`)
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

## Testing (experimental)

A tiny, super simple testing framework is in `tests`. It is a shell script that runs the uc (or uc-docker) command with
various input and compare the output and exit code. If the output / exit code is different it will show a diff using
either `diff` or [delta](https://github.com/dandavison/delta).

For example, the following

```sh
cd tests
./test.sh
```

will run it the main test suite using the set of example files from the `examples` folder.

See the `./test.sh --help` for more info.