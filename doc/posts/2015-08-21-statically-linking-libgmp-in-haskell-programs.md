---
title: Statically linking libgmp in Haskell programs
author: Sharif Olorin
tags: haskell, linux
---

Dynamic linking is usually a good idea, but occasionally it's useful
to be able to build a Haskell program which runs on both older and
newer GNU/Linux systems[^gnulinux]. The primary impediment to doing
this in most cases is [libgmp](http://gmplib.org/); there are two
versions of `libgmp`'s ABI in common use today, `libgmp.so.3` and
`libgmp.so.10`, the former being found on older distributions still
common in the wild like Centos 6, and the latter being found on newer
distributions like Centos 7 or 2015 Arch Linux. 

Adding `-optl -static` to `ghc-options` in the Cabal file will build a
fully statically-linked executable. This probably won't do what you
want, as statically linking `glibc` will result in the individual
libraries `glibc` loads dynamically being restricted to the versions
present on the build system.

So, can we use Cabal to build an ELF which includes `libgmp` but isn't
entirely statically-linked? I gave up trying to make Cabal generate
linker options for some system libraries but not others and resorted
to abusing the `-pgml` option (which allows the user to specify a
linker executable to call in place of `gcc`) by writing a shell script
which looked like this:

```
#! /bin/sh -eux

CCARGS=$(echo "$@" | sed -e 's/-lgmp//g')

# This path is for Centos 6 with the `gmp-static` package; the path
# might be different on other systems.
STATIC_GMP="/usr/lib64/libgmp.a"

gcc $CCARGS $STATIC_GMP
```

Call that something like `link-with-gmp.sh` and save it somewhere in
your `$PATH`, add `-pgml link-with.gmp.sh` to the `ghc-options`
section in your Cabal file and build.

Keep in mind that this won't work if you're building on a system with
a `glibc` that's much more recent than the one with which the program
will be run; `glibc` is backwards-compatible but not
forwards-compatible, so this is most effective if used with an older
distribution as the build system (I used Centos 6, which should cover
anything released in the last few years).

[^gnulinux]: I use "GNU/Linux" deliberately here as `glibc` is relevant. `musl`-based systems are probably different.
