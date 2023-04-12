Scala Builder
==============

The new way to glue scala-cli modules

## Use Cases
- full stack app with Scala.js front-end and JVM/Native/Node.js server

## Setting up project

This is a Scala CLI project, meaning that before working on any of the modules, you should initialise
Scala CLI for the module you wish to work on (currently there is only one).

For VS Code + Metals extension, run the following command at the root of this repository

```bash
scala setup-ide --workspace . modules/scala-builder
```

then when opening any scala file from `modules/scala-builder`, Metals will activate and correctly type the code.
