const std = @import("std");
const math = @import("math.zig").math;
const shr = std.math.shr;

const expectEqual = @import("std").testing.expectEqual;

const D64 = struct {
    v: i64,
    const offset = 32;
    const lowBits = 0xFFFFFFFF;

    pub fn lit(comptime value: comptime_float) D64 {
        // TODO: Once bug is fixed, this should be handled at comptime: https://github.com/ziglang/zig/issues/10592
        var scaled: f128 = comptime value * @as(comptime_float, 1 << 32);
        var r = @floatToInt(i64, std.math.round(scaled));
        return D64{ .v = r };
    }

    pub fn add(self: D64, other: D64) D64 {
        // a*2^f + b*2^f = (a + b)*2^f
        return D64{ .v = self.v + other.v };
    }

    pub fn mul(self: D64, other: D64) D64 {
        // a*2^f * b*2^f = a*b*2^(2f)
        const selfLow = self.v & lowBits;
        const selfHigh = shr(i64, self.v, 32);
        const otherLow = other.v & lowBits;
        const otherHigh = shr(i64, other.v, 32);

        return D64{ .v = shr(i64, selfLow * otherLow, 32) + (selfLow * otherHigh) + (selfHigh * otherLow) + ((selfHigh * otherHigh) << 32) };
    }

    pub fn toF32(self: D64) f32 {
        return @intToFloat(f32, self.v) / (1 << 32);
    }
};

test "lit" {
    const a = D64.lit(2);
    try expectEqual(@as(f32, 2.0), a.toF32());

    const b = D64.lit(-2);
    try expectEqual(@as(f32, -2.0), b.toF32());
}

test "mul" {
    const a = D64.lit(2);
    try expectEqual(@as(f32, 4.0), math("a * a", .{ .a = a }).toF32());

    const b = D64.lit(0.5);
    try expectEqual(@as(f32, 0.25), math("b * b", .{ .b = b }).toF32());

    const c = D64.lit(2.5);
    try expectEqual(@as(f32, 6.25), math("c * c", .{ .c = c }).toF32());

    const d = D64.lit(-2.5);
    try expectEqual(@as(f32, 6.25), math("d * d", .{ .d = d }).toF32());
    try expectEqual(@as(f32, -6.25), math("d * c", .{ .c = c, .d = d }).toF32());
    try expectEqual(@as(f32, -6.25), math("c * d", .{ .c = c, .d = d }).toF32());
}
