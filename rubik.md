- Cubie: single 1x1x1 cube
    * 26 total
    * 8 corner cubies - urf
    * 12 edge cubies - ur
    * 6 center cubies - f
- Oriented => urf != ruf != fur
- Unoriented => urf = ruf = fur
- Cubicles : spaces where cubies live, actions don't change cubicles only cubes
- Move names:
    * R : clockwise right
    * U : clockwise up
    * F : clockwise front
    * B : clockwise back
    * L : clockwise left
    * D : clockwise down
- Move are mod 4
- Cubicle position is invariant under moves : edge cubie cannot be moved into corner cubicle and vice versa
- Corner cubies configurations : 3^8 possible orientations * 8! cubicles
- Edge cubies configurations : 2^12 position orientations * 12! cubicles
- Total = 5.19 * 10^20
- Valid configuration : can be achieved by a sequence of moves

# Rubik's group
- Set = all possible moves
- Operation = move composition, i.e. M1 * M2 => do M1 then M2
- M(C) cubicle that the oriented cubie C ends up in after move M. E.g.
  R(ur) = br
- Commutator : [M1M2] = M1M2M1^-1M2^-1
- Valid configuration is determined by
    * Position of corner cubies : S8
    * Position of edge cubies : S12
    * Orientation of corner cubies
    * Orientation of edge cubies

- (1,2,3,4,5,6,7,8)
- (0,0,0,0,0,0,0,0)
