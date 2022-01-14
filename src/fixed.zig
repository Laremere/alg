const std = @import("std");
const math = @import("math.zig").math;
const shr = std.math.shr;
const shl = std.math.shl;

const expectEqual = @import("std").testing.expectEqual;

pub const D64 = struct {
    v: i64,
    const offset = 32;
    const lowBits = 0xFFFFFFFF;

    pub const pi = lit("3.1415926535897932384626433");

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

    pub fn sub(self: D64, other: D64) D64 {
        // a*2^f - b*2^f = (a - b)*2^f
        return D64{ .v = self.v - other.v };
    }

    pub fn mul(self: D64, other: D64) D64 {
        // a*2^f * b*2^f = a*b*2^(2f)
        // (a*2^f * b*2^f) / 2^f = a*b*2^f
        // const selfLow = self.v & lowBits;
        // const selfHigh = shr(i64, self.v, 32);
        // const otherLow = other.v & lowBits;
        // const otherHigh = shr(i64, other.v, 32);

        // std.debug.print("--- {} * {}\n", .{ self.toF32(), other.toF32() });
        // std.debug.print("--- {b} = {b} + {b}\n", .{ self.v, selfHigh, selfLow });
        // std.debug.print("--- {b} = {b} + {b}\n", .{ other.v, otherHigh, otherLow });
        // return D64{ .v = shr(i64, selfLow * otherLow, 32) + (selfLow * otherHigh) + (selfHigh * otherLow) + ((selfHigh * otherHigh) << 32) };

        const rb = @intCast(i128, self.v) * @intCast(i128, other.v);

        return D64{ .v = @intCast(i64, shr(i128, rb, 32)) };
    }

    pub fn div(self: D64, other: D64) D64 {
        // (a*2^f) / (b*2^f) = a/b
        // (a*2^f*2^f) / (b*2^f) = (a/b) * 2^f

        const sb = shl(i128, @intCast(i128, self.v), 32);
        const ob = @intCast(i128, other.v);

        const rb = @divFloor(sb, ob);

        return D64{ .v = @intCast(i64, rb) };
    }

    pub fn mod(self: D64, other: D64) D64 {
        var sb = shl(i128, @intCast(i128, self.v), 32);
        if (self.v < 0) {
            sb |= 0xFFFFFFFF;
        }
        const ob = shl(i128, @intCast(i128, other.v), 32);
        const rb = @mod(sb, ob);
        return D64{ .v = @intCast(i64, shr(i128, rb, 32)) };
    }

    pub fn sin(self: D64) D64 {
        const twoPi = comptime pi.mul(lit("2"));
        const piOverTwo = comptime pi.div(lit("2"));
        const partial = self.mod(twoPi).div(piOverTwo); // 0 to 4.

        if (partial.lessThan(lit("1"))) {
            return zeroToOneSin(partial);
        }
        if (partial.lessThan(lit("2"))) {
            return zeroToOneSin(lit("2").sub(partial));
        }
        if (partial.lessThan(lit("3"))) {
            return lit("-1").mul(zeroToOneSin(partial.sub(lit("2"))));
        }
        return lit("-1").mul(zeroToOneSin(lit("4").sub(partial)));
    }

    pub fn cos(self: D64) D64 {
        const twoPi = comptime pi.mul(lit("2"));
        const piOverTwo = comptime pi.div(lit("2"));
        const partial = self.mod(twoPi).div(piOverTwo); // 0 to 4.

        if (partial.lessThan(lit("1"))) {
            return zeroToOneSin(lit("1").sub(partial));
        }
        if (partial.lessThan(lit("2"))) {
            return lit("-1").mul(zeroToOneSin(partial.sub(lit("1"))));
        }
        if (partial.lessThan(lit("3"))) {
            return lit("-1").mul(zeroToOneSin(lit("3").sub(partial)));
        }
        return zeroToOneSin(partial.sub(lit("3")));
    }

    fn zeroToOneSin(z: D64) D64 {
        // Is correct to within +-0.0075.  A better Taylor series would yield better accuracy.
        // Equation source: https://www.nullhardware.com/blog/fixed-point-sine-and-cosine-for-embedded-systems/
        const a = comptime math("four * ((three / pi) - (nine / sixteen))", .{
            .four = lit("4"),
            .three = lit("3"),
            .pi = pi,
            .nine = lit("9"),
            .sixteen = lit("16"),
        });
        const b = comptime math("(five / two) - (two * a)", .{
            .two = lit("2"),
            .a = a,
            .five = lit("5"),
        });
        const c = comptime math("a - (three / two)", .{
            .a = a,
            .three = lit("3"),
            .two = lit("2"),
        });
        const zSquared = math("z * z", .{ .z = z });
        const zCubed = math("zSquared * z", .{ .zSquared = zSquared, .z = z });
        const zFifth = math("zCubed * z", .{ .zCubed = zCubed, .z = z });
        return math("(a * z) + (b * zCubed) + (c * zFifth)", .{
            .a = a,
            .z = z,
            .b = b,
            .zCubed = zCubed,
            .c = c,
            .zFifth = zFifth,
        });
    }

    pub fn floor(self: D64) D64 {
        return D64{ .v = self.v & (~@as(i64, lowBits)) };
    }

    pub fn toF32(self: D64) f32 {
        return @intToFloat(f32, self.v) / (1 << 32);
    }

    pub fn lessThan(self: D64, other: D64) bool {
        return self.v < other.v;
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

test "mod" {
    const d = D64.lit;
    try expectEqual(d("1"), d("-5").mod(d("3")));
    try expectEqual(d("2"), d("5").mod(d("3")));
    try expectEqual(
        d("-5.5").sub(d("-5.5").div(d("3")).floor().mul(d("3"))),
        d("-5.5").mod(d("3")),
    );
    // The literal seems to be the wrong part here, so exact comparison is slightly off?
    try std.testing.expectApproxEqAbs(d("0.5").toF32(), d("-5.5").mod(d("3")).toF32(), 0.0075);
}

test "zeroToOneSin" {
    var i: usize = 0;
    const pointOne = D64.lit("0.01");
    var value = D64.lit("0");
    while (i <= 100) : (i += 1) {
        const calculated = value.zeroToOneSin().toF32();
        const groundTruth = std.math.sin(value.toF32() * std.math.pi / 2);
        try std.testing.expectApproxEqAbs(groundTruth, calculated, 0.0075);

        value = value.add(pointOne);
    }
}

test "sin and cos" {
    var i: usize = 0;
    const one = D64.lit("1");
    // var value = D64.lit("0");
    var value = D64.lit("-50");
    while (i <= 100) : (i += 1) {
        const sinExpected = std.math.sin(value.toF32());
        const sinActual = value.sin().toF32();
        try std.testing.expectApproxEqAbs(sinExpected, sinActual, 0.0075);

        const cosExpected = std.math.cos(value.toF32());
        const cosActual = value.cos().toF32();
        try std.testing.expectApproxEqAbs(cosExpected, cosActual, 0.0075);

        value = value.add(one);
    }
}

test "floor" {
    try expectEqual(D64.lit("1"), D64.lit("1.5").floor());
    try expectEqual(D64.lit("-1"), D64.lit("-0.5").floor());
}
