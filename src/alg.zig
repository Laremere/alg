pub const math = @import("math.zig").math;
pub const mat = @import("mat.zig");
pub const imaginary = @import("imaginary.zig");
pub const fixed = @import("fixed.zig");

test "root: run reference all files" {
    @import("std").testing.refAllDecls(@This());
}
