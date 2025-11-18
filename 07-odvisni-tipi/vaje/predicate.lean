
variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Izjave napišite na list papirja, nato pa jih dokažite v datoteki.

theorem eq1 : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro notExistsPx x px
      apply notExistsPx
      exact ⟨ x, px⟩
    · intro forallNotPx existsPx
      obtain ⟨x, px⟩ := existsPx
      have notPx := forallNotPx
      apply notPx
      exact px


theorem eq2 : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    · intro h1 x hr
      have allPx := h1 hr
      exact allPx x
    · intro h2 hr x
      have rtoallPx := h2 x
      have allPx := rtoallPx hr
      exact allPx

theorem eq3 : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) :=
  by
    apply Iff.intro
    · apply And.intro


theorem eq4 : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  sorry

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical
#check Classical.byContradiction
#check Classical.em

theorem eq5 : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro notForallPx
      apply Classical.byContradiction
      intro notExistsnotPx
      apply notForallPx
      intro x

      apply Classical.byContradiction
      intro notPx
      apply notExistsnotPx
      exact ⟨x, notPx⟩

    · intro h1 h2
      obtain ⟨ x, NotPx ⟩ := h1
      have h3 := h2 x
      apply NotPx
      exact h3


theorem eq6 : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    · intro h1
      sorry
    · intro h1
      have h2 := Classical.em r
      sorry cases h2 with
      | inl r1 => sorry
