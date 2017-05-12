
(**
 * Steps:
 * - Start the game
 *   - Get player count
 *   - Initialize board data (start.ml)
 *   - Get player names
 *   - Make game object
 * - Game loop, for each player
 *   - Display state of the world
 *   - Read action
 *   - Validate action against board + player state
 *     - Translate from relative indexes to concrete data (cards)
 *     - Read another action if invalid
 *   - Perform action
 *     - Update all relevant data structures
 *   - Check for cleanup
 *     - Claim nobles
 *     - Put back tokens
 * - After each full loop, check for end of game
 *)

