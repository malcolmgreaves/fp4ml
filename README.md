# fp4ml
[![Build Status](https://travis-ci.org/malcolmgreaves/fp4ml.svg?branch=master)](https://travis-ci.org/malcolmgreaves/fp4ml) [![Coverage Status](https://coveralls.io/repos/malcolmgreaves/fp4ml/badge.svg?branch=master&service=github)](https://coveralls.io/github/malcolmgreaves/fp4ml?branch=master)
 [![Codacy Badge](http://api.codacy.com:80/project/badge/7a4fbaf2cbe6449993224d6eb4df0f13)](https://www.codacy.com/app/greavesmalcolm/fp4ml) [![Stories in Ready](https://badge.waffle.io/malcolmgreaves/fp4ml.png?label=ready&title=Ready)](https://waffle.io/malcolmgreaves/fp4ml)  [![Join the chat at https://gitter.im/malcolmgreaves/fp4ml](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/malcolmgreaves/fp4ml?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.malcolmgreaves/fp4ml-scala_2.11/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.malcolmgreaves/fp4ml-scala_2.11)

Machine learning for functional programmers.

# Project Structure

This repository is split into subprojects:

* [fp4ml-main](https://github.com/malcolmgreaves/fp4ml/tree/master/fp4ml-core)
  * The meat and potatoes of the fp4ml project. Includes:
    * learning algorithms
    * abstractions
    * data structures
    * experiment frameworks
    * evaluation metrics
    * model definitions, formats
  * Depends on 3rd party libraries including:
    * [`data-tc`](https://github.com/malcolmgreaves/data-tc)
    * `shapeless`
    * `spire`
  
* [fp4ml-spark](https://github.com/malcolmgreaves/fp4ml/tree/master/fp4ml-spark)
  * An extension of `fp4ml-main` to use elements from the Apache Spark ecosystem.

# Legal

The original author retains copyright over all material contained within this repository. Use of this code is governed under the terms of the Apache 2.0 open source software license. See the [LICENSE](./LICENSE) file for more details.

