# Morganey Index

Morganey index is a file called `morganey-index` that is located at
the root of every Morganey module container. Morganey module container
is a place that is discoverable via JVM classpath mechanism (jar,
folder, etc) and contains Morganey modules. Morganey interpreter can
load multiple modules at once, and each of them should contain its own
Index file. For example, here is the structure of a jar which is a
Morganey module container:

```
foo.jar
 |--META-INF/...
 |--morganey-index
 |--a.mgn
 |--b.mgn
 +--foo/
     |--x.mgn
     +--y.mgn
```

Morganey index contains the full list of modules inside of a
correponding module container. For the example above the Morganey
index (the content of `morganey-index` file) would look like:

```
a.mgn
b.mgn
foo/x.mgn
foo/y.mgn
```

Morganey index is required for module REPL autocompletion to work
correctly.
