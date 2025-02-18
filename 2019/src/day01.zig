const std = @import("std");
const advent = @import("advent");

const Solution = struct {
    part1: i64,
    part2: i64,

    pub fn print(self: Solution) !void {
        const stdout_file = std.io.getStdOut().writer();
        var bw = std.io.bufferedWriter(stdout_file);
        const stdout = bw.writer();

        try stdout.print("Part 1: {d}\nPart 2: {d}\n", .{ self.part1, self.part2 });

        try bw.flush();
    }
};

fn mass(fuel: i32) i32 {
    return @divFloor(fuel, 3) - 2;
}

fn mass_recursive(fuel: i32) i32 {
    const m = mass(fuel);
    if (m <= 0) {
        return 0;
    }
    return m + mass_recursive(m);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const contents = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024 * 10);
    defer allocator.free(contents);

    var numbers = std.mem.tokenizeScalar(u8, contents, '\n');
    var part1: i64 = 0;
    var part2: i64 = 0;
    while (numbers.next()) |number| {
        const n = try std.fmt.parseInt(i32, number, 10);
        part1 += mass(n);
        part2 += mass_recursive(n);
    }

    const sol = Solution{ .part1 = part1, .part2 = part2 };
    try sol.print();
}

test "mass calculation" {
    const input = [_]i32{ 12, 14, 1969, 100756 };
    const expected = [_]i32{ 2, 2, 654, 33583 };

    for (input, expected) |i, e| {
        try std.testing.expect(mass(i) == e);
    }
}

test "mass recursive calculation" {
    const input = [_]i32{ 14, 1969, 100756 };
    const expected = [_]i32{ 2, 966, 50346 };

    for (input, expected) |i, e| {
        try std.testing.expect(mass_recursive(i) == e);
    }
}
