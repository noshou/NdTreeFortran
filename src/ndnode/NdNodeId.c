#include <stdint.h>
/*
 * A NodeId packs two 64-bit fields into 16 contiguous bytes:
 *
 *   node_id  -- stable unique identifier assigned at node creation;
 *               never changes for the lifetime of the node.
 *               Treat as read-only after construction.
 *
 *   pool_idx -- mutable hint to the node's current 1-based position
 *               in the tree's node pool. May go stale after any
 *               mutation (addNodes / rmvNodes) that triggers pool
 *               compaction or a rebuild.
 *
 * Lookup strategy (O(1) common case):
 *
 *   1. Use pool_idx as a direct index into the pool.
 *   2. If pool_idx >= pop, the hint is stale -- fall back to O(n) scan.
 *   3. If pool_idx is in range, compare pool[pool_idx].node_id == node_id.
 *      A match is an O(1) hit. A mismatch means the slot was reused --
 *      fall back to O(n) scan and update pool_idx if found.
 */
typedef struct {
    uint64_t node_id;   /* stable id -- do not modify after construction */
    uint64_t pool_idx;  /* mutable pool position hint; 0 = unknown       */
} NodeId;
