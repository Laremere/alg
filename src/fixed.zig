const std = @import("std");
const math = @import("math.zig").math;
const shr = std.math.shr;
const shl = std.math.shl;

const expectEqual = @import("std").testing.expectEqual;

const D64 = struct {
    v: i64,
    const offset = 32;
    const lowBits = 0xFFFFFFFF;

    pub fn lit(comptime str: [:0]const u8) D64 {
        if (str.len == 0) {
            @compileError("Empty fixed point literal.");
        }

        // Parse as a fixed 128 with 64 fractal bits, then shift
        // to D64.
        const parseOne = 1 << 64;
        comptime var r: i128 = 0;

        comptime var i: usize = 0;
        const negative = str[0] == '-';
        if (negative) {
            i += 1;
        }

        parse: {
            inline while (true) {
                if (i >= str.len) {
                    break :parse;
                }
                const c = str[i];
                i += 1;
                if (c >= '0' and c <= '9') {
                    r *= 10;
                    r += parseOne * @intCast(i128, c - '0');
                } else if (c == '.') {
                    break;
                } else {
                    @compileError("Not a valid fixed point literal: " ++ str);
                }
            }

            comptime var divisor: i128 = 1;
            inline while (true) {
                if (i >= str.len) {
                    break :parse;
                }
                divisor *= 10;
                const c = str[i];
                i += 1;
                if (c >= '0' and c <= '9') {
                    r += @divFloor(parseOne, divisor) * @intCast(i128, c - '0');
                } else {
                    @compileError("Not a valid fixed point literal: " ++ str);
                }
            }
        }

        if (negative) {
            r *= -1;
        }
        return comptime D64{ .v = @intCast(i64, shr(i128, r, 32)) };
    }

    pub fn add(self: D64, other: D64) D64 {
        // a*2^f + b*2^f = (a + b)*2^f
        return D64{ .v = self.v + other.v };
    }

    pub fn mul(self: D64, other: D64) D64 {
        // a*2^f * b*2^f = a*b*2^(2f)
        // (a*2^f * b*2^f) / 2^f = a*b*2^f
        const selfLow = self.v & lowBits;
        const selfHigh = shr(i64, self.v, 32);
        const otherLow = other.v & lowBits;
        const otherHigh = shr(i64, other.v, 32);

        return D64{ .v = shr(i64, selfLow * otherLow, 32) + (selfLow * otherHigh) + (selfHigh * otherLow) + ((selfHigh * otherHigh) << 32) };
    }

    pub fn div(self: D64, other: D64) D64 {
        // (a*2^f) / (b*2^f) = a/b
        // (a*2^f*2^f) / (b*2^f) = (a/b) * 2^f

        const sb = shl(i128, @intCast(i128, self.v), 32);
        const ob = @intCast(i128, other.v);

        const rb = @divFloor(sb, ob);

        return D64{ .v = @intCast(i64, rb) };
    }

    pub fn toF32(self: D64) f32 {
        return @intToFloat(f32, self.v) / (1 << 32);
    }
};

test "lit" {
    const a = D64.lit("2");
    try expectEqual(@as(f32, 2.0), a.toF32());

    const b = D64.lit("-2");
    try expectEqual(@as(f32, -2.0), b.toF32());
}

test "mul" {
    const a = D64.lit("2");
    try expectEqual(@as(f32, 4.0), math("a * a", .{ .a = a }).toF32());

    const b = D64.lit("0.5");
    try expectEqual(@as(f32, 0.25), math("b * b", .{ .b = b }).toF32());

    const c = D64.lit("2.5");
    try expectEqual(@as(f32, 6.25), math("c * c", .{ .c = c }).toF32());

    const d = D64.lit("-2.5");
    try expectEqual(@as(f32, 6.25), math("d * d", .{ .d = d }).toF32());
    try expectEqual(@as(f32, -6.25), math("d * c", .{ .c = c, .d = d }).toF32());
    try expectEqual(@as(f32, -6.25), math("c * d", .{ .c = c, .d = d }).toF32());
}

test "add" {
    const a = D64.lit("2");
    const b = D64.lit("0.5");
    try expectEqual(@as(f32, 2.5), math("a + b", .{ .a = a, .b = b }).toF32());
}

test "div" {
    const a = D64.lit("2");
    const b = D64.lit("0.5");
    try expectEqual(@as(f32, 4), math("a / b", .{ .a = a, .b = b }).toF32());
}
