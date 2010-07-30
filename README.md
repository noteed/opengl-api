# openg-api

Represent and parse spec files from the OpenGL [registry][].

[registry]: http://www.opengl.org/registry/#specfiles

## Current state

`Text.OpenGL.Spec.hs` has code to completely parse and represent the content of
`enumext.spec`. (Probably there is not much left to do to support `enum.spec`.).
The representation includes comments, blank lines, and passthru lines.

## To do

- Implement a parser for `gl.tm`.
- Implement a parser for `gl.spec`.
- Offer a simplified representation of `[EnumLine]` with no comment, no blank
  line, no passthru, maybe references already resolved, and a single number
  format.
- Offer a parser for `enum.spec`.
