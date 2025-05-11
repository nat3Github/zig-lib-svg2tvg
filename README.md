# svg2tvg - convert svg to tvg and render tvg to image!

- written in zig 0.14.0
- dependency name: svg2tvg, module name: svg2tvg

## usage:

```zig
test "convert to tvg and render" {
    const svg2tvg = @import("svg2tvg");
    const gpa = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    const Wrapper = struct {
        width: i64,
        height: i64,
        img: *MyImage,
        pub fn setPixel(self: *@This(), x: i64, y: i64, color: [4]u8) void {
            const pix: MyPixel = .init_from_u8_slice(&color);
            self.img.set_pixel(@intCast(x), @intCast(y), pix);
        }
    };

    const width_and_height = 200;

    var myImage = try MyImageType.init(alloc, width_and_height, width_and_height);
    const my_svg_icon_data = icons.svg.feather.activity;

    var w = std.ArrayList(u8).init(alloc);
    try svg2tvg.tvg_from_svg(alloc, w.writer(), svg_bytes);

    var image_wrapper = Wrapper{
        .img = img,
        .width = @intCast(img.get_width()),
        .height = @intCast(img.get_height()),
    };

    var fb = std.io.fixedBufferStream(w.items);
    try svg2tvg.renderStream(alloc, &image_wrapper, fb.reader(), .{}); <-- you can use options here

    ...
}
```

## Credit:

- Chris Marchesi: https://github.com/vancluever/z2d (used to render tvg as image)
- Ian Johnson: https://github.com/ianprime0509/zig-xml (used for parsing xml)
- Chris Marchesi: https://github.com/vancluever/zig-svg (as inspiration for svg attribute parsing but currently not in use due to special cases not beeing handled)
- https://github.com/TinyVG/sdk/tree/main (used for writing tvg)

## Licence

- this library is under MIT
- svg.zig from https://github.com/vancluever/zig-svg is MPL licenced
- z2d is MPL licenced
- zig-xml is under 0BSD
- tinyVG/sdk is under MIT
