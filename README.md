# tasty-jenkins-xml

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)

[![Hackage](http://img.shields.io/hackage/v/tasty-jenkins-xml.svg)](https://hackage.haskell.org/package/tasty-jenkins-xml)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/tasty-jenkins-xml.svg)](http://packdeps.haskellers.com/reverse/tasty-jenkins-xml)
[![Build](https://travis-ci.org/IxpertaSolutions/tasty-jenkins-xml.svg?branch=master)](https://travis-ci.org/IxpertaSolutions/tasty-jenkins-xml)

## Description

An extension of [tasty-ant-xml][] that also outputs to console and implements
two additions to be more practically useful:

 * `--jxml` alias for `--xml` for [test-framework][] compatibility,

 * `--exit-success` to distinguish between _failed_ and _unstable_ builds in
   Jenkins CI.

[tasty-ant-xml]: https://hackage.haskell.org/package/tasty-ant-xml
[test-framework]: https://hackage.haskell.org/package/test-framework

## Usage

Example:

```haskell
import Test.Tasty
import Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [listingTests, jenkinsXMLRunner]
```

## Contributing

Contributions are welcome! Documentation, examples, code, and feedback - they
all help.

## License

The BSD 3-Clause License, see [LICENSE][] file for details.

[LICENSE]: https://github.com/IxpertaSolutions/tasty-jenkins-xml/blob/master/LICENSE
