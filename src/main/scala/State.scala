/**
 * This class' main purpose is to represent a "state or states" that the
 * board needs to transition to after every given move. 'i' and 'j' will
 * be the indices in the Reversi board of a possible position for a move
 * taken by a player, 'direction' is a value between 1 and 8 that indicates 
 * all possible directions to update the disks on the board, while 'player'
 * indicates what disks will need to be updated. As mentioned, a player move 
 * may cause the board to update one or more "lines" of disks and this is
 * accomplished by having one or more states that will be used when updating
 * disks for the chosen move.
 **/
class State(
  val i: Int, val j: Int, val direction: Int, val player: Int
)

