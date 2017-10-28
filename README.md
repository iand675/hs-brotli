# Brotli compression for Haskell

Brotli is a compression format that can achieve higher compression
ratios and compress faster than gzip and deflate. It is supported
natively by [all modern browsers](https://caniuse.com/#search=brotli).

This monorepo contains:

- Primitive FFI bindings to Google's official C API
- Higher level strict, lazy, and streaming compression and decompression
  interfaces.
- Convenience wrappers for the popular Conduit, Pipes libraries
- WAI middleware to quickly drop in support for Haskell web
  applications.

All of these projects are still alpha quality, so use at your own risk.

## Installation

These libraries are not currently uploaded to Hackage, you can add them
to your stack.yaml:

``` yaml

packages:
- location:
    git: https://github.com/iand675/brotli
    commit: PRESENT_COMMIT_HERE
  subdirs:
  - brotli
  - brotli-conduit
  - wai-middleware-brotli
  extra-dep: true

```

## Roadmap

All of the base `brotli` package high level bindings are functional, but
it's possible that error cases could presently cause memory leaks.
Any contributions that find and/or fix these situations would be very
welcome.

More documentation examples are always welcome.

`streaming-brotli` and `pipes-brotli` aren't really actually implemented
yet.

It would be great to have benchmarks in place to compare compression
performance with Haskell's gzip bindings.

## Contributing

Please use `hint` and `hindent` on pull requests prior to opening them
up. If fixing any bugs, please implement regression test cases.

If you have node installed locally, you can run `npm install` or `yarn
install` to set up shared git hooks to to automate these validations.

Please respect and follow the [Code of Conduct](docs/CODE_OF_CONDUCT.md)

