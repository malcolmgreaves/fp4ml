==========================================================================================
Using sbt for building, tests, running programs, and interative shell.
==========================================================================================

We recommend using the following SBT options:
	 SBT_OPTS="-Xmx2G -XX:MaxPermSize=724M  -XX:+UseConcMarkSweepGC  -XX:+CMSClassUnloadingEnabled

./sbt test
	Downloads dependencies, compiles, packages, and runs all unit tests" 

./sbt
	Interactive sbt shell

[TODO] Unified running programs
[TODO] Interative spark shell with all project jars in classpath

==========================================================================================
To compile all dependencies into bytecode and pacakge into a single jar
==========================================================================================

$ sbt assembly
