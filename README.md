# Nak

Authors: 
* **Jason Baldridge** (jasonbaldridge@gmail.com)
* **Dan Garrette** (dhg@cs.utexas.edu)

## Introduction

Nak is a library for machine learning and related tasks, with a focus on having an easy to use API for some standard algorithms. It is formed from the OpenNLP Maxent package, and the intent is to evolve it as a Scala library with further capabilities. It will be developed in particular with the natural language processing library [Chalk](https://github.com/scalanlp/chalk) in mind. Use of [Breeze](https://github.com/scalanlp/breeze) is likely in Nak's future.

Like Chalk, the name Nak comes from one of Jason's son's stuffed elephants. (He really likes elephants.)

## What's inside

The latest stable release of Nak is 1.1.0. It includes:
* The classification code from the OpenNLP Maxent package, slightly reorganized.
* The k-means clustering code from Scalabha.

## Using Nak

In SBT:

    libraryDependencies += "org.scalanlp" % "nak" % "1.1.0"

In Maven:

    <dependency>
       <groupId>org.scalanlp</groupId>
       <artifactId>nak</artifactId>
       <version>1.1.0</version>
    </dependency>

**Note**: There is one dependency that won't get pulled along: pca_transform-0.7.2.jar in the lib directory is not available on any repository, so you'll need to add that to your classpath by hand if (and only if) you want to be able to use PCA transformations for input to k-means.

Note that the domain has changed from `com.jasonbaldridge` (v1.0) to `org.scalanlp` now.

There is no dedicated documentation for Nak as yet, but you can see some use of the k-means clustering code in [homework three](https://github.com/utcompling/applied-nlp/wiki/Homework3) for Jason's [Applied NLP course](https://github.com/utcompling/applied-nlp). Future homeworks will cover classification and more, using Nak.

## Requirements

* Version 1.6 of the Java 2 SDK (http://java.sun.com)

## Configuring your environment variables

The easiest thing to do is to set the environment variables `JAVA_HOME`
and `NAK_DIR` to the relevant locations on your system. Set `JAVA_HOME`
to match the top level directory containing the Java installation you
want to use.

Next, add the directory `NAK_DIR/bin` to your path. For example, you
can set the path in your `.bashrc` file as follows:

	export PATH=$PATH:$NAK_DIR/bin

Once you have taken care of these three things, you should be able to
build and use Nak.


## Building the system from source

Nak uses SBT (Simple Build Tool) with a standard directory
structure.  To build Nak, type (in the `$NAK_DIR` directory):

	$ ./build update compile

This will compile the source files and put them in
`./target/classes`. If this is your first time running it, you will see
messages about Scala being downloaded -- this is fine and
expected. Once that is over, the Nak code will be compiled.

To try out other build targets, do:

	$ ./build

This will drop you into the SBT interface. To see the actions that are
possible, hit the TAB key. (In general, you can do auto-completion on
any command prefix in SBT, hurrah!)

To make sure all the tests pass, do:

	$ ./build test

Documentation for SBT is at <http://www.scala-sbt.org/>

Note: if you have SBT already installed on your system, you can
also just call it directly with "sbt" in `NAK_DIR`.

# Questions or suggestions?

Email Jason Baldridge: <jasonbaldridge@gmail.com>

Or, create an issue: <https://github.com/jasonbaldridge/nak/issues>


