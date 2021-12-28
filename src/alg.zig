const std = @import("std");
const expectEqual = @import("std").testing.expectEqual;

// TODO:
//
// Types:
// - Floats
// - Integers
// - Comptime float and integers
// - Vectory / Array
// - Matrix
// - Affine Matrix
// - Geometic Algebra
// - Maybe custom functions?
// - Imaginary Numbers
// - Quaternion
//
// Operations
// Add, multiply, dot, etc.
//
// Built in values?
// e?
// pi?
// Identity matrix?  Is this useful?

// pub fn Matrix(comptime T: type, comptime rows: comptime_int, columns: comptime_int) type {
//     return struct {
//         // Values stored in column major order.
//         v: [rows * columns]T,
//     };
// }

test "math" {
    std.debug.print("\n{}\n", .{math("a", .{ .a = @as(i8, 7) })});
    try expectEqual(@as(i8, 5), math("a", .{ .a = @as(i8, 5) }));
}

// Using an allocator (even just a fixed buffer) doesn't work during comptime yet.
// Instead just allocate something on the stack at the entry point, and work
// from there.  If we could use a real allocator, it'd be better for ast to
// contain pointer to concrete types, instead of concrete types pointing to asts.
// This would more efficiently pack memory.  However this way is much simpler given
// the constraints.
const StupidAlloc = struct {
    asts: [100]Ast,
    index: isize,

    fn next(comptime self: *StupidAlloc) *Ast {
        var r = &self.asts[self.index];
        self.index += 1;
        return r;
    }
};

pub fn math(comptime eq: [:0]const u8, args: anytype) ReturnType(eq, @TypeOf(args)) {
    comptime var alloc = StupidAlloc{
        .asts = undefined,
        .index = 0,
    };
    comptime var rootAst = comptime parse(&alloc, eq, @TypeOf(args));

    return rootAst.eval(args);
}

pub fn ReturnType(comptime eq: [:0]const u8, argsType: type) type {
    comptime var alloc = StupidAlloc{
        .asts = undefined,
        .index = 0,
    };
    comptime var rootAst = comptime parse(&alloc, eq, argsType);

    return rootAst.ReturnType();
}

pub fn parse(comptime alloc: *StupidAlloc, comptime eq: [:0]const u8, argsType: type) *Ast {
    var tokenizer = std.zig.Tokenizer.init(eq);
    while (true) {
        comptime var token = comptime tokenizer.next();
        switch (token.tag) {
            .eof => {
                @compileError("eof");
            },
            .plus => {
                @compileError("plus");
            },
            .identifier => {
                const name = eq[token.loc.start..token.loc.end];
                return Identifier.init(alloc, name, argsType);
            },
            else => {
                @compileError("Invalid token for math equation.");
            },
        }
    }
}

const Ast = union(enum) {
    identifier: Identifier,
    // binaryOp: *BinaryOp,
    // unary op.

    fn eval(comptime ast: *Ast, args: anytype) ast.ReturnType() {
        return switch (ast.*) {
            .identifier => |v| v.eval(args),
        };
    }

    fn ReturnType(comptime ast: *Ast) type {
        return switch (ast.*) {
            .identifier => |v| v.ReturnType(),
        };
    }
};

const Identifier = struct {
    name: []const u8,
    return_type: type,

    fn init(comptime alloc: *StupidAlloc, name: []const u8, comptime argsType: type) *Ast {
        comptime var S = switch (@typeInfo(argsType)) {
            .Struct => |v| v,
            else => @compileError("math args must be a struct or touple."),
        };
        for (S.fields) |field| {
            if (std.mem.eql(u8, name, field.name)) {
                var r = alloc.next();
                r.* = Ast{
                    .identifier = Identifier{
                        .name = name,
                        .return_type = field.field_type,
                    },
                };
                return r;
            }
        } else {
            @compileError("Identifier in equation not found in passed info: " ++ name);
        }
    }

    fn eval(comptime ident: *const Identifier, args: anytype) ident.ReturnType() {
        return @field(args, ident.name);
    }

    fn ReturnType(comptime ident: *const Identifier) type {
        return ident.return_type;
    }
};

// const BinaryOp = struct {
//     lhs: Ast,
//     rhs: Ast,
//     op: Op,

//     const Op = enum {
//         addErr,
//         mulErr,
//     };
// };

// fn parseInt(comptime str: []const u8) comptime_int {
//     var r: comptime_int = 0;
//     // todo: non-base 10 integers?
//     for (str) |chr| {
//         switch (chr) {
//             '0' => r = r * 10,
//             '1' => r = r * 10 + 1,
//             '2' => r = r * 10 + 2,
//             '3' => r = r * 10 + 3,
//             '4' => r = r * 10 + 4,
//             '5' => r = r * 10 + 5,
//             '6' => r = r * 10 + 6,
//             '7' => r = r * 10 + 7,
//             '8' => r = r * 10 + 8,
//             '9' => r = r * 10 + 9,
//             else => @compileError("invalid integer"),
//         }
//     }
//     return r;
// }

// test "parse integer" {
//     try expectEqual(0, parseInt("0"));
//     try expectEqual(10, parseInt("010"));
//     try expectEqual(9876543210, parseInt("9876543210"));
// }

// fn isBuiltinScalar(comptime v: type) bool {
//     return switch (@typeInfo(v)) {
//         .Int => true,
//         .Float => true,
//         .ComptimeFloat => true,
//         .ComptimeInt => true,
//         else => false,
//     };
// }

// fn InvalidCombo(comptime a: type, comptime b: type, comptime op: []const u8) noreturn {
//     @compileError("Invalid combination of " ++ @typeName(a) ++ " and " ++ @typeName(b) ++ " for operation " ++ op ++ ".");
// }

// fn logStruct(s: anytype) void {
//     comptime var T = @TypeOf(s);
//     @compileLog(T);
//     @compileLog(s);
//     comptime var S = switch (@typeInfo(T)) {
//         .Struct => |v| v,
//         else => @compileError("log struct only takes structs."),
//     };
//     inline for (S.fields) |field| {
//         @compileLog(field.name);
//     }
// }
