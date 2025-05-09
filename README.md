# zig-svg

zig-svg provides abstractions for SVG attributes.

Note that this library only parses specific SVG attributes that need parsing
outside of general XML, such as paths, colors, or co-ordinate specifications.

For XML parsers, here are a couple:

 * https://github.com/ianprime0509/zig-xml
 * https://github.com/nektro/zig-xml

## Parsing

All useful primitives will have a parse function that you can call to perform
the parsing. An example for path is below:

```zig
const Path = @import("svg").Path;
var path = try Path.parse(alloc, data);
defer path.deinit();
for (path.nodes) |n| {
  ...
}
```

## Errors

Only errors in allocation are reported as errors. All other errors are
non-fatal and are reported in the `err` attribute instead. This allows for
partial SVG processing as per the
[spec](https://www.w3.org/TR/SVG11/implnote.html#ErrorProcessing).

```zig
const io = @import("std").io;
const log = @import("std").log;
const Path = @import("svg").Path;
var path = try Path.parse(alloc, data);
defer path.deinit();
if (path.err) |err| {
    const errWriter = io.getStdErr();
    buf.format(errWriter, "error processing SVG: ");
    try path.fmtErr(errWriter);
}

for (path.nodes) |n| {
  ...
}
```

## License

This project is licensed MPL 2.0. See LICENSE for a copy of the MPL.
