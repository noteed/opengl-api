# openg-api

Represent and parse spec files from the OpenGL [registry][].

[registry]: http://www.opengl.org/registry/#specfiles

## Directory layout

- `spec-files` contains the `.spec` and `.tm` files provided by [opengl.org][].
- `results` contains modified `glext.h` file. This is a `glext.h` header
  provided by [opengl.org][] with one line modified (the one with the last
  update date). It is used to ensure that opengl-api still generated the same
  results after refactoring.

[opengl.org]: http://www.opengl.org/registry/#specfiles

## Current state

`Text.OpenGL.Spec` has code to completely parse and represent the content of
`enumext.spec` (Probably there is not much left to do to support `enum.spec`.),
`gl.tm` and `gl.spec`.
The representations includes comments, blank lines, and passthru lines.

The representations are nearly feature complete, the only missing important bit
is the text defining the array size (in the description of function
parameters).

`Text.OpenGL.Api` has code to create higher-level data structures from the
one in Spec.

`Text.OpenGL.ExtHeader` is able to perfectly recreate the `glext.h` file
from the spec files.

`Text.OpenGL.GenChecks` is able to generate macros to call glGetError after
every GL commands.

## To do

- Offer a simplified representation of `[EnumLine]` with no comment, no blank
  line, no passthru, maybe references already resolved, and a single number
  format.
- Offer a parser for `enum.spec`.
- State in the doc that there is some doc about the spec file format at the
  good old Haskell-OpenGL [page][]!

[page]: http://www.haskell.org/HOpenGL-old/spec_explained.html
