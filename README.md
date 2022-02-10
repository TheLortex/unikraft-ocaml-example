## unikraft-ocaml-hello

This is an ocaml 5 program running with https://github.com/TheLortex/unikraft-ocaml. 
It requires an opam switch with the `ocaml-variants.5.00.0+trunk` compiler. In this switch you need the `dune` build system.

The `unix` lib is not usable for now because `unikraft-ocaml` isn't building it. 

## Notes

I plan to make the example reproducible but I need to get more familiar with the `kraft` tool.