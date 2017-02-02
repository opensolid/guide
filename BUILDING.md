For example, for the frames guide:

```
elm make Frames/RelativeTo.elm Frames/PlaceIn.elm --output Frames/main.js
asciidoctor Frames/index.adoc
cp Frames/index.html Frames/main.js ../../opensolid.github.io/guides/frames
```
