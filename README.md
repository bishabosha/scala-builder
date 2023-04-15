Scala Builder
==============

The new way to glue scala-cli modules

## Use Cases
- [full stack app](examples/1-full-stack-app/builder.toml) with Scala.js front-end and JVM/Native/Node.js server

## Building Scala Builder

on macOS/Linux

1. add `~/.local/bin` to the `PATH`
2. package `scala-builder` command with
```bash
scala --power package -f -o ~/.local/bin/scala-builder --workspace . modules/scala-builder
```
3. change to `examples/1-full-stack-app` directory, and then run

```bash
cd examples/1-full-stack-app
scala-builder run webapp
```

## Setting up project

This is a Scala CLI project, meaning that before working on any of the modules, you should initialise
Scala CLI for the module you wish to work on (currently there is only one).

For VS Code + Metals extension, run the following command at the root of this repository

```bash
scala setup-ide --workspace . modules/scala-builder
```

then when opening any scala file from `modules/scala-builder`, Metals will activate and correctly type the code.
