# notes

optimizations for part2:
- [ ] opt #1: cut down the box so it's only up to filling it.
We can pre-calculate the area that we're cutting from the big triangle.
- [ ] opt #2:
We can use a different table to fetch easily which positions to settle on:
- get the first platform that our currently launched grain (P) will deposit to.
  Call it TY.
- if grains can go left, check for any platform on the left between (P.y + 1) and TY (upwards)
     For every platform that we encounter, launch a grain towards it and repeat this process.
- if grains can go right, check for the same thing on the right.
This way we can also avoid setting any grains on `blocked`, since each "ray
cast" that we're doing will fill completely its part, without overlap, which
removes the need to check for other grains.
