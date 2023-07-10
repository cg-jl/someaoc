pub const Point = @Vector(2, usize);

pub const is_real_input = true;
pub const will_paint_table = true;

pub const input_file_name = if (is_real_input) "input.txt" else "test.txt";

// these constants were calculated with the help of `test.hs` script and GHCi.

pub const floor_position = if (!is_real_input) 9 + 2 else 173 + 2;

pub const objs_hz_span = if (!is_real_input) 10 else 57;
pub const objs_vt_spa = if (!is_real_input) 5 else 160;

pub const board_height = floor_position;
// (+2) because part2 assumes in the geometry part that there are fully filled
// lanes on the edges.
pub const board_width = objs_hz_span + 2;

// The (-1) here is to keep the values centered after (+2)
pub const board_left_corner = Point{ (if (!is_real_input) 494 else 449) - 1, 0 };

pub const drop_point = Point{ 500, 0 } - board_left_corner;
