const math = @import("math.zig").math;
const mat = @import("mat.zig");
const imaginary = @import("imaginary.zig");

test "root: run reference all files" {
    @import("std").testing.refAllDecls(@This());
}
