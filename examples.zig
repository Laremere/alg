const expectEqual = @import("std").testing.expectEqual;
const alg = @import("src/alg.zig");

test "matrix and scaler multiplication" {
    var a = alg.mat.Matrix(f32, 2, 1).lit(.{
        1,
        2,
    });

    var b = alg.mat.Matrix(f32, 1, 2).lit(.{
        3, 4,
    });

    var c: f32 = 5;

    var result = alg.math("a * b * c", .{
        .a = a,
        .b = b,
        .c = c,
    });

    try expectEqual(alg.mat.Matrix(f32, 2, 2).lit(.{
        15, 20,
        30, 40,
    }), result);
}
