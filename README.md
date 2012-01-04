Haspin: Haskell Pen Spinning Toolkit
====================================

Haspin aims to provide a set of tools to parse and manipulate descriptions of
pen spinning tricks and combos (hereafter "breakdowns")

Some of the goals are:

* Writing formal definitions for several languages used for breakdowns, among
  which:
  * The standard, international notation used in occidental communities
  * The breakdown language used by Japanese pen spinners (see
    http://www47.atwiki.jp/penspinorder/ for examples)
  * The experimental formal languages suggested by
    [Skatox](http://thefpsb.penspinning.fr/tricks/systeme-de-breakdown-bas-niveau-decomposer-les-tricks-t12320.html)
    and
    [Lindor](http://thefpsb.penspinning.fr/tricks/un-langage-informatique-descriptif-des-combos-t12383.html)
    in the FPSB lab
* At least one Haskell-hosted DSL that is yet to be defined
* Provide both strict parsers and lenient ones; the lenient ones should try to
  successfully understand most breakdowns out there, possibly detecting the
  language.
* Implement extensions to the standard syntax from the UPSB and FPSB labs.
* Supporting conversions between some of these languages
* Supporting printing the "most explicit" and "most implicit" version of a breakdown

Things that I don't know and that may be useful to this project:

* Are there labs similar to the UPSB lab and the FPSB lab in other communities?
* I think the Korean and Thai use their own notation too? I would like to
  support them too, but I don't speak their language so I would need external
  info.

The parsers are written with the Parsec library. The code does not make use of
any GHC extension for the time being, but this could change.

This is an early work in progress. Nothing really interesting yet.

