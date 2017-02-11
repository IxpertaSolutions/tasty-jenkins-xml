# tasty-jenkins-xml

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)

[![Hackage](http://img.shields.io/hackage/v/tasty-jenkins-xml.svg)](https://hackage.haskell.org/package/tasty-jenkins-xml)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/tasty-jenkins-xml.svg)](http://packdeps.haskellers.com/reverse/tasty-jenkins-xml)
[![Build](https://travis-ci.org/IxpertaSolutions/tasty-jenkins-xml.svg?branch=master)](https://travis-ci.org/IxpertaSolutions/tasty-jenkins-xml)

## Description

**An ingredient transformer version of [tasty-ant-xml][tasty-ant-xml].**

This package implements a [tasty][] ingredient transformer that makes it
possible to output test results as JUnit XML **in addition to** other output
ingredient, e.g. a `consoleTestReporter`. Internally it invokes the
[tasty-ant-xml][] ingredient.

To be practically useful, it implements two additions:

 * `--jxml` alias for `--xml` for [test-framework][] compatibility,

 * `--exit-success` to distinguish between _failed_ and _unstable_ builds in
   Jenkins CI.

[tasty]: https://hackage.haskell.org/package/tasty
[tasty-ant-xml]: https://hackage.haskell.org/package/tasty-ant-xml
[test-framework]: https://hackage.haskell.org/package/test-framework

## Usage

Example:

```haskell
import Test.Tasty
import Test.Tasty.Runners.JenkinsXML (jenkinsXMLTransformer)

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [listingTests, jenkinsXMLTransformer [consoleTestReporter]]
```

Alternatively `jenkinsXMLTransformer` may be applied directly to
`defaultIngredients`.

For comparison, here's a `main` that uses [tasty-ant-xml][] instead (and can't
output to console and XML at the same time):

```haskell
import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [listingTests, antXMLRunner, consoleTestReporter]
```

## Contributing

Contributions are welcome! Documentation, examples, code, and feedback - they
all help.

## License

The BSD 3-Clause License, see [LICENSE][] file for details.

[LICENSE]: https://github.com/IxpertaSolutions/tasty-jenkins-xml/blob/master/LICENSE
