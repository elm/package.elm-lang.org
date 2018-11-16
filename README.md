# [package.elm-lang.org][pkg]

The source code for Elm's package website.

The [package website][pkg] hosts hundreds of open source projects. All are created by the Elm community to share useful bits of code with everyone else.

[pkg]: http://package.elm-lang.org/

## Automatic Semantic Versioning

It is best practice to use [semantic versioning](http://semver.org/) when choosing version numbers. Humans can mess this up though, so Elm figures out all version numbers automatically based on your API. Every package starts with 1.0.0 and then Elm figures out what the next version should be based on how the API changes!


## Reliability

All community packages are written entirely in Elm, so all the things that make your apps reliable are helping make packages reliable.

A small set of packages provide access to [The Web Platform](https://platform.html5.org/). These packages are managed by @elm-lang to ensure that (1) the APIs are exposed in a way that makes sense for Elm and (2) they are carefully vetted to make sure the underlying JS code is stable. We cover a decent amount of The Web Platform now, but you can always use [ports][] if something is not covered yet!

[ports]: https://guide.elm-lang.org/interop/javascript.html#ports
