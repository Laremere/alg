const expectEqual = @import("std").testing.expectEqual;
const Vector = @import("std").meta.Vector;

const math = @import("math.zig").math;

pub fn Img(comptime T: type) type {
    return struct {
        v: Vector(2, T),

        pub fn init(scalar: T, imaginary: T) Img(T) {
            return Img(T){
                .v = [_]T{ scalar, imaginary },
            };
        }

        pub fn mul(self: Img(T), other: Img(T)) Img(T) {
            // I have no clue if this Vector version is faster than
            // just writing out the multiplcations.  Good practice
            // with Vectors, though.
            const scalarOnly: Vector(2, i32) = [_]i32{ 0, 0 };
            const imaginaryOnly: Vector(2, i32) = [_]i32{ 1, 1 };
            const flipped: Vector(2, i32) = [_]i32{ 1, 0 };
            const invertISquared: Vector(2, T) = [_]T{ -1, 1 };

            var otherS = @shuffle(T, other.v, undefined, scalarOnly);
            var otherI = @shuffle(T, other.v, undefined, imaginaryOnly);
            var selfFlipped = @shuffle(T, self.v, undefined, flipped);

            return Img(T){
                .v = (self.v * otherS) + (selfFlipped * otherI * invertISquared),
            };
        }

        pub fn add(self: Img(T), other: Img(T)) Img(T) {
            return Img(T){ .v = self.v + other.v };
        }
    };
}

test "imaginary" {
    const T = Img(f32);
    try expectEqual(T.init(1, 1), math("a + b", .{
        .a = T.init(0, 1),
        .b = T.init(1, 0),
    }));

    try expectEqual(T.init(-5, 10), math("a * b", .{
        .a = T.init(1, 2),
        .b = T.init(3, 4),
    }));
}
