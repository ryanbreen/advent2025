const std = @import("std");

const MAX_MODULES = 64;
const MAX_DESTINATIONS = 16;
const MAX_INPUTS = 16;
const QUEUE_SIZE = 4096;

const ModuleType = enum {
    broadcaster,
    flip_flop,
    conjunction,
    output,
};

const Module = struct {
    name: []const u8 = "",
    module_type: ModuleType = .output,
    destinations: [MAX_DESTINATIONS]u8 = undefined,
    num_destinations: u8 = 0,
    // Flip-flop state
    ff_state: bool = false,
    // Conjunction memory: input indices and their last pulse values
    input_indices: [MAX_INPUTS]u8 = undefined,
    input_memory: [MAX_INPUTS]bool = [_]bool{false} ** MAX_INPUTS,
    num_inputs: u8 = 0,
};

const Pulse = struct {
    source: u8,
    dest: u8,
    high: bool,
};

const Simulator = struct {
    modules: [MAX_MODULES]Module = [_]Module{.{}} ** MAX_MODULES,
    num_modules: u8 = 0,
    broadcaster_idx: u8 = 0,
    rx_input_idx: u8 = 0,
    has_rx: bool = false,

    // Queue for simulation
    queue: [QUEUE_SIZE]Pulse = undefined,
    queue_head: usize = 0,
    queue_tail: usize = 0,

    // For part 2: watch nodes
    watch_nodes: [MAX_INPUTS]u8 = undefined,
    num_watch_nodes: u8 = 0,
    high_sent: [MAX_INPUTS]bool = [_]bool{false} ** MAX_INPUTS,

    fn findModule(self: *Simulator, name: []const u8) ?u8 {
        for (0..self.num_modules) |i| {
            if (std.mem.eql(u8, self.modules[i].name, name)) {
                return @intCast(i);
            }
        }
        return null;
    }

    fn getOrCreateModule(self: *Simulator, name: []const u8) u8 {
        if (self.findModule(name)) |idx| {
            return idx;
        }
        const idx = self.num_modules;
        self.modules[idx].name = name;
        self.modules[idx].module_type = .output;
        self.modules[idx].num_destinations = 0;
        self.modules[idx].num_inputs = 0;
        self.num_modules += 1;
        return idx;
    }

    fn addConjunctionInput(self: *Simulator, module_idx: u8, input_idx: u8) void {
        const m = &self.modules[module_idx];
        // Check if already added
        for (0..m.num_inputs) |i| {
            if (m.input_indices[i] == input_idx) return;
        }
        m.input_indices[m.num_inputs] = input_idx;
        m.input_memory[m.num_inputs] = false;
        m.num_inputs += 1;
    }

    fn findInputIdx(self: *Simulator, module_idx: u8, input_idx: u8) ?u8 {
        const m = &self.modules[module_idx];
        for (0..m.num_inputs) |i| {
            if (m.input_indices[i] == input_idx) {
                return @intCast(i);
            }
        }
        return null;
    }

    fn parse(self: *Simulator, input: []const u8) void {
        var lines = std.mem.splitScalar(u8, input, '\n');

        // First pass: create all modules and set up destinations
        while (lines.next()) |line| {
            if (line.len == 0) continue;

            // Find " -> "
            var arrow_pos: usize = 0;
            for (0..line.len - 3) |i| {
                if (std.mem.eql(u8, line[i .. i + 4], " -> ")) {
                    arrow_pos = i;
                    break;
                }
            }
            if (arrow_pos == 0) continue;

            const name_part = line[0..arrow_pos];
            const dest_part = line[arrow_pos + 4 ..];

            var name: []const u8 = undefined;
            var module_type: ModuleType = undefined;

            if (std.mem.eql(u8, name_part, "broadcaster")) {
                name = "broadcaster";
                module_type = .broadcaster;
            } else if (name_part[0] == '%') {
                name = name_part[1..];
                module_type = .flip_flop;
            } else if (name_part[0] == '&') {
                name = name_part[1..];
                module_type = .conjunction;
            } else {
                continue;
            }

            const idx = self.getOrCreateModule(name);
            self.modules[idx].module_type = module_type;

            if (module_type == .broadcaster) {
                self.broadcaster_idx = idx;
            }

            // Parse destinations
            var dests = std.mem.splitSequence(u8, dest_part, ", ");
            while (dests.next()) |dest_name| {
                const dest_idx = self.getOrCreateModule(dest_name);
                self.modules[idx].destinations[self.modules[idx].num_destinations] = dest_idx;
                self.modules[idx].num_destinations += 1;

                // Check if this is rx
                if (std.mem.eql(u8, dest_name, "rx")) {
                    self.rx_input_idx = idx;
                    self.has_rx = true;
                }
            }
        }

        // Second pass: set up conjunction inputs
        for (0..self.num_modules) |i| {
            const m = &self.modules[i];
            for (0..m.num_destinations) |j| {
                const dest_idx = m.destinations[j];
                if (self.modules[dest_idx].module_type == .conjunction) {
                    self.addConjunctionInput(dest_idx, @intCast(i));
                }
            }
        }
    }

    fn reset(self: *Simulator) void {
        for (0..self.num_modules) |i| {
            self.modules[i].ff_state = false;
            for (0..self.modules[i].num_inputs) |j| {
                self.modules[i].input_memory[j] = false;
            }
        }
    }

    fn enqueue(self: *Simulator, source: u8, dest: u8, high: bool) void {
        self.queue[self.queue_tail] = Pulse{
            .source = source,
            .dest = dest,
            .high = high,
        };
        self.queue_tail = (self.queue_tail + 1) % QUEUE_SIZE;
    }

    fn dequeue(self: *Simulator) ?Pulse {
        if (self.queue_head == self.queue_tail) return null;
        const p = self.queue[self.queue_head];
        self.queue_head = (self.queue_head + 1) % QUEUE_SIZE;
        return p;
    }

    fn simulateButtonPress(self: *Simulator, low_count: *u64, high_count: *u64) void {
        self.queue_head = 0;
        self.queue_tail = 0;

        // Reset high_sent tracking
        for (0..self.num_watch_nodes) |i| {
            self.high_sent[i] = false;
        }

        // Button sends low to broadcaster (use 255 as "button" source)
        self.enqueue(255, self.broadcaster_idx, false);

        while (self.dequeue()) |p| {
            if (p.high) {
                high_count.* += 1;
            } else {
                low_count.* += 1;
            }

            // Track watched nodes sending high
            if (p.high) {
                for (0..self.num_watch_nodes) |i| {
                    if (p.source == self.watch_nodes[i]) {
                        self.high_sent[i] = true;
                    }
                }
            }

            const m = &self.modules[p.dest];

            switch (m.module_type) {
                .broadcaster => {
                    for (0..m.num_destinations) |i| {
                        self.enqueue(p.dest, m.destinations[i], p.high);
                    }
                },
                .flip_flop => {
                    if (!p.high) {
                        m.ff_state = !m.ff_state;
                        for (0..m.num_destinations) |i| {
                            self.enqueue(p.dest, m.destinations[i], m.ff_state);
                        }
                    }
                },
                .conjunction => {
                    // Update memory for the source
                    if (p.source != 255) {
                        if (self.findInputIdx(p.dest, p.source)) |input_idx| {
                            m.input_memory[input_idx] = p.high;
                        }
                    }

                    // Check if all inputs are high
                    var all_high = true;
                    for (0..m.num_inputs) |i| {
                        if (!m.input_memory[i]) {
                            all_high = false;
                            break;
                        }
                    }

                    const output = !all_high;
                    for (0..m.num_destinations) |i| {
                        self.enqueue(p.dest, m.destinations[i], output);
                    }
                },
                .output => {
                    // Do nothing
                },
            }
        }
    }

    fn part1(self: *Simulator) u64 {
        self.reset();

        var total_low: u64 = 0;
        var total_high: u64 = 0;

        for (0..1000) |_| {
            self.simulateButtonPress(&total_low, &total_high);
        }

        return total_low * total_high;
    }

    fn gcd(a: u64, b: u64) u64 {
        var x = a;
        var y = b;
        while (y != 0) {
            const t = y;
            y = x % y;
            x = t;
        }
        return x;
    }

    fn lcm(a: u64, b: u64) u64 {
        return (a / gcd(a, b)) * b;
    }

    fn part2(self: *Simulator) u64 {
        self.reset();

        if (!self.has_rx) return 0;

        // Set up watch nodes (inputs to rx_input)
        const rx_input = &self.modules[self.rx_input_idx];
        self.num_watch_nodes = rx_input.num_inputs;
        for (0..rx_input.num_inputs) |i| {
            self.watch_nodes[i] = rx_input.input_indices[i];
        }

        // Track cycle lengths
        var cycle_lengths: [MAX_INPUTS]u64 = [_]u64{0} ** MAX_INPUTS;
        var found: u8 = 0;

        var button_press: u64 = 0;
        while (found < self.num_watch_nodes) {
            button_press += 1;
            var low: u64 = 0;
            var high: u64 = 0;
            self.simulateButtonPress(&low, &high);

            for (0..self.num_watch_nodes) |i| {
                if (self.high_sent[i] and cycle_lengths[i] == 0) {
                    cycle_lengths[i] = button_press;
                    found += 1;
                }
            }
        }

        // Calculate LCM of all cycle lengths
        var result: u64 = 1;
        for (0..self.num_watch_nodes) |i| {
            result = lcm(result, cycle_lengths[i]);
        }

        return result;
    }
};

pub fn main() !void {
    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    var buf: [32768]u8 = undefined;
    const bytes_read = try file.readAll(&buf);
    const input = buf[0..bytes_read];

    var sim = Simulator{};
    sim.parse(input);

    const p1 = sim.part1();
    std.debug.print("Part 1: {}\n", .{p1});

    // Reset for part 2
    sim.reset();
    const p2 = sim.part2();
    std.debug.print("Part 2: {}\n", .{p2});
}
