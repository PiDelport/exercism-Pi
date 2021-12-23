//! <https://exercism.org/tracks/rust/exercises/role-playing-game>
//!
//! TODO: Test the problem specification using proptest.

/// The level at which the player's mana pool unlocks.
const MANA_UNLOCK_LEVEL: u32 = 10;

/// How much health a player starts out with.
const FULL_HEALTH: u32 = 100;

/// How much mana a player starts out with.
const FULL_MANA: u32 = 100;

/// How much damage a spell deals relative to its mana cost.
const SPELL_DAMAGE_FACTOR: u32 = 2;

#[derive(Debug)] // core
pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    fn is_alive(&self) -> bool {
        0 < self.health
    }

    /// Return a full mana pool for this player, if the mana pool is unlocked.
    fn full_mana_pool(&self) -> Option<u32> {
        if self.level < MANA_UNLOCK_LEVEL {
            None
        } else {
            Some(FULL_MANA)
        }
    }

    /// Attempt to revive this player.
    ///
    /// Return a revived [`Player`] with full health, and full mana (if level 10 or above),
    /// or `None` if the player is not dead.
    pub fn revive(&self) -> Option<Player> {
        if self.is_alive() {
            None
        } else {
            Some(Self {
                health: FULL_HEALTH,
                mana: self.full_mana_pool(),
                ..*self
            })
        }
    }

    /// Attempt to consume mana, returning the mana successfully consumed.
    fn consume_mana(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            None => {
                // No mana pool: deal mana cost as self damage.
                self.health = self.health.saturating_sub(mana_cost);
                0
            }
            Some(mana) => {
                if mana < mana_cost {
                    // Insufficient mana: do nothing.
                    0
                } else {
                    // Sufficient mana: consume successfully.
                    self.mana = Some(mana.saturating_sub(mana_cost));
                    mana_cost
                }
            }
        }
    }

    /// Attempt to cast a spell, returning the amount of damage performed.
    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        self.consume_mana(mana_cost)
            .checked_mul(SPELL_DAMAGE_FACTOR)
            .expect("cast_spell: overflow")
    }
}
