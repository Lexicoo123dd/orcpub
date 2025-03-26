(ns orcpub.dnd.e5.classes
  (:require #?(:clj [clojure.spec.alpha :as spec])
            #?(:cljs [cljs.spec.alpha :as spec])
            [orcpub.common :as common]
            [orcpub.template :as t]
            [orcpub.modifiers :as mod]
            [orcpub.dnd.e5.options :as opt5e]
            [orcpub.dnd.e5.modifiers :as mod5e]
            [orcpub.dnd.e5.weapons :as weapon5e]
            [orcpub.dnd.e5.skills :as skill5e]
            [orcpub.dnd.e5.equipment :as equipment5e]
            [orcpub.dnd.e5.character :as char5e]
            [orcpub.dnd.e5.units :as units5e]
            [orcpub.dnd.e5.spells :as spells5e]
            [orcpub.dnd.e5.spell-lists :as sl5e]
            [orcpub.dnd.e5.template-base :as t-base]
            [re-frame.core :refer [subscribe]]
            [clojure.string :as s]))

(spec/def ::name (spec/and string? common/starts-with-letter?))
(spec/def ::key (spec/and keyword? common/keyword-starts-with-letter?))
(spec/def ::option-pack string?)
(spec/def ::homebrew-class (spec/keys :req-un [::name ::key ::option-pack]))

(spec/def ::class (spec/and keyword? common/keyword-starts-with-letter?))
(spec/def ::homebrew-subclass (spec/keys :req-un [::name ::key ::class ::option-pack]))

(spec/def ::homebrew-invocation (spec/keys :req-un [::name ::key ::option-pack]))

(spec/def ::homebrew-boon (spec/keys :req-un [::name ::key ::option-pack]))

(defn class-level [levels class-kw]
  (get-in levels [class-kw :class-level]))

(defn extra-attack-trait [page]
  (mod5e/trait-cfg
   {:name "Extra Attack"
    :page page
    :summary "You can attack twice, instead of once, whenever you take the Attack action on your turn"}))

(defn barbarian-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Barbarian"
    :key :barbarian
    :hit-die 12
    :ability-increase-levels [4 8 12 16 19]
    :profs {:armor {:light true :medium true :shields false}
            :weapon {:simple false :martial false}
            :save {::char5e/str true ::char5e/con true}
            :skill-options {:choose 2 :options {:animal-handling true :athletics true :intimidation true :nature true :perception true :survival true}}}
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/str 13)]
    :weapon-choices [{:name "Martial Weapon"
                      :options {:greataxe 1
                                :martial 1}}
                     {:name "Simple Weapon"
                      :options {:handaxe 2
                                :simple 1}}]
    :weapons {:javelin 4}
    :equipment {:explorers-pack 1}
    :modifiers [(mod/vec-mod ?unarmored-defense :barbarian)
                (mod/cum-sum-mod ?unarmored-ac-bonus (?ability-bonuses ::char5e/con)
                                 nil
                                 nil
                                 [(= :barbarian (first ?unarmored-defense))])
                (mod/cum-sum-mod ?unarmored-with-shield-ac-bonus (?ability-bonuses ::char5e/con)
                                 nil
                                 nil
                                 [(= :barbarian (first ?unarmored-defense))])
                (mod5e/bonus-action
                 {:name "Rage"
                  :page 48
                  :duration units5e/minutes-1
                  :frequency (units5e/long-rests (condp <= (?class-level :barbarian)
                                              17 6
                                              12 5
                                              6 4
                                              3 3
                                              2))
                  :summary (str (if (>= (?class-level :barbarian) 7) "As part of the bonus action, move up to half your speed. ") "Advantage on Strength checks and saves; melee damage using strength bonus +"
                                (common/bonus-str (condp <= (?class-level :barbarian)
                                                    16 4
                                                    9 3
                                                    2))
                                "; resistance to bludgeoning, piercing, and slashing damage; can't cast or concentrate on spells; ends early if knocked unconcious or if you haven't attacked a hostile creature or taken damage since your last turn")})]
    :levels {3 {:selections [(opt5e/skill-selection [:animal-handling :athletics :intimidation :nature :perception :survival] 1)]}
             5 {:modifiers [(extra-attack-trait 49)
                            (mod5e/num-attacks 2)
                            (mod/modifier ?speed-with-armor (fn [armor] (if (not= :heavy (:type armor))
                                                                          (+ 10 ?speed)
                                                                          ?speed)))
                            (mod5e/dependent-trait
                             {:name "Fast Movement"
                              :page 49
                              :summary "Your speed increases by 10 feet while you aren't wearing heavy armor"})]}
             9 {:modifiers [(mod5e/dependent-trait
                             {:name "Brutal Critical"
                              :page 49
                              :summary (let [die-count (condp <= (?class-level :barbarian)
                                                         17 "three"
                                                         13 "two"
                                                         "one")]
                                         (str "You can roll "
                                              die-count
                                              " additional weapon damage "
                                              (if (= "one" die-count)
                                                "die"
                                                "dice")
                                              " when determining the extra damage for a critical hit with a melee attack"))})]}
             10 {:selections [(opt5e/skill-selection [:animal-handling :athletics :intimidation :nature :perception :survival] 1)]}
             18 {:modifiers [(mod5e/dependent-trait
                              {:name "Indomitable Might"
                               :level 18
                               :page 49
                               :summary (let [str-score (::char5e/str ?abilities)]
                                          (str "If your total for a Strength check is less than your Strength score (" str-score "), you can use that score in place of the total"))})]}
             20 {:modifiers [(mod5e/ability ::char5e/str 4)
                             (mod5e/ability ::char5e/con 4)]}}
    :traits [{:name "Reckless Attack"
              :level 2
              :page 48
              :summary "You can throw aside all concern for defense to attack with fierce desperation. When you make your first attack on your turn, you can decide to attack recklessly. Doing so gives you advantage on melee weapon attack rolls using Strength during this turn, but attack rolls against you have advantage until your next turn."}
             {:name "Danger Sense"
              :level 2
              :page 48
              :summary "You gain an uncanny sense of when things nearby aren't as they should be, giving you an edge when you dodge away from danger. You have advantage on Dexterity saving throws against effects that you can see, such as traps and spells. To gain this benefit, you can't be blinded, deafened, or incapacitated."}
             {:name "Feral Instinct"
              :level 7
              :page 49
              :summary (str "Your instincts are so honed that you have advantage on initiative rolls"
                            "\n\nAdditionally, if you are surprised at the beginning of combat and aren't incapacitated, you can act normally on your first turn, but only if you enter your rage before doing anything else on that turn.")}
             {:name "Relentless Rage"
              :level 11
              :page 49
              :summary (str "Your rage can keep you fighting despite grievous wounds. If you drop to 0 hit points while you're raging and don't die outright, you can make a DC 10 Constitution saving throw. If you succeed, you drop to 1 hit point instead."
                            "\n\nEach time you use this feature after the first, the DC increases by 5. When you finish a short or long rest, the DC resets to 10.")}
             {:name "Persistent Rage"
              :level 15
              :page 49
              :summary "your rage is so fierce that it ends early only if you fall unconscious or if you choose to end it"}]
    :subclass-level 3
    :subclass-title "Primal Path"
    :subclass-help "Your primal path shapes the nature of your barbarian rage and gives you additional features."
    :subclasses [{:name "Path of the Ancestral Guardian"
                  :levels {6 {:modifiers [(mod5e/reaction
                                           {:name "Spirit Shield"
                                            :summary (str "The guardian spirits that aid you can provide supernatural protection to those you defend. If you are raging and another creature you can see within 30 feet of you takes damage, you can use your reaction to reduce that damage by " (condp <= (?class-level :barbarian)
                                                         14 "4d6"
                                                         10 "3d6"
                                                         "2d6"))})]}
                           10 {:modifiers [(mod5e/spells-known 2 :augury ::char5e/wis "Barbarian" 1)
                                           (mod5e/spells-known 3 :clairvoyance ::char5e/wis "Barbarian" 1)
                                           (mod5e/trait-cfg
                                             {:name "Consult the Spirits"
                                              :frequency units5e/rests
                                              :summary "You gain the ability to consult with your ancestral spirits. When you do so, you cast the Augury or Clairvoyance spell, without using a spell slot or material components. Rather than creating a spherical sensor, this use of clairvoyance invisibly summons one of your ancestral spirits to the chosen location. Wisdom is your spellcasting ability for these spells"})]}}
                  :traits [{:name "Ancestral Protectors"
                            :level 3
                            :summary "Spectral warriors appear when you enter your rage. While you're raging, the first creature you hit with an attack on your turn becomes the target of the warriors, which hinder its attacks. Until the start of your next turn, that target has disadvantage on any attack roll that isn't against you, and when the target hits a creature other than you with an attack, that creature has resistance to the damage dealt by the attack. The effect on the target ends early if your rage ends"}
                           {:name "Vengeful Ancestors"
                            :level 14
                            :summary "Your ancestral spirits grow powerful enough to retaliate. When you use your Spirit Shield to reduce the damage of an attack, the attacker takes an amount of force damage that your Spirit Shield prevents"}]}
                 {:name "Path of the Berserker"
                  :levels {10 {:modifiers [(mod5e/action
                                            {:name "Intimidating Presence"
                                             :level 10
                                             :page 49
                                             :summary (str "You can use your action to frighten someone with your menacing presence. When you do so, choose one creature that you can see within 30 feet of you. If the creature can see or hear you, it must succeed on a Wisdom saving throw (DC equal to 8 + your proficiency bonus + your Charisma modifier = " (?spell-save-dc ::char5e/cha) ") or be frightened of you until the end of your next turn. On subsequent turns, you can use your action to extend the duration of this effect on the frightened creature until the end of your next turn. This effect ends if the creature ends its turn out of line of sight or more than 60 feet away from you."
                                                           "\n\nIf the creature succeeds on its saving throw, you can't use this feature on that creature again for 24 hours")})]}
                           14 {:modifiers [(mod5e/reaction
                                            {:name "Retaliation"
                                             :page 49
                                             :level 14
                                             :summary "When you take damage from a creature that is within 5 feet of you, you can use your reaction to make a melee weapon attack against that creature"})]}}
                  :traits [{:name "Frenzy"
                            :level 3
                            :page 49
                            :summary "You can go into a frenzy when you rage. If you do so, for the duration of your rage you can make a single melee weapon attack as a bonus action on each of your turns after this one. When your rage ends, you suffer one level of exhaustion"}
                           {:name "Mindless Rage"
                            :level 6
                            :page 49
                            :summary "You can't be charmed or frightened while raging. If you are charmed or frightened when you enter your rage, the effect is suspended for the duration of the rage"}]}
                 {:name "Path of the Totem Warrior"
                    :levels {3 {:modifiers [(mod5e/spells-known 2 :beast-sense nil "Barbarian" 1 "ritual only")
                                            (mod5e/spells-known 1 :speak-with-animals nil "Barbarian" 1 "ritual only")]
                                :selections [(t/selection-cfg
                                              {:name "Totem Spirit"
                                               :tags #{:class}
                                               :order 2
                                               :options [(t/option-cfg
                                                          {:name "Bear"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Totem Spirit: Bear"
                                                                         :page 50
                                                                         :summary "While raging, you have resistance to all damage except psychic damage. The spirit of the bear makes you tough enough to stand up to any punishment"})]})
                                                         (t/option-cfg
                                                          {:name "Eagle"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Totem Spirit: Eagle"
                                                                         :page 50
                                                                         :summary "While you're raging and aren't wearing heavy armor, other creatures have disadvantage on opportunity attack rolls against you, and you can use the Dash action as a bonus action on your turn. The spirit of the eagle makes you into a predator who can weave through the fray with ease"})]})
                                                         (t/option-cfg
                                                          {:name "Elk"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Totem Spirit: Elk"
                                                                         :page 50
                                                                         :summary "While you're raging and aren't wearing heavy armor, your walking speed increases by 15 feet. The spirit of the elk makes you extraordinarily swift"})]})
                                                         (t/option-cfg
                                                          {:name "Tiger"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Totem Spirit: Tiger"
                                                                         :page 50
                                                                         :summary "While raging, you can add 10 feet to your long jump distance and 3 feet to your high jump distance. The spirit of the tiger empowers your leaps"})]})
                                                         (t/option-cfg
                                                          {:name "Wolf"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Totem Spirit: Wolf"
                                                                         :page 50
                                                                         :summary "While you're raging, your friends have advantage on melee attack rolls against any creature within 5 feet of you that is hostile to you. The spirit of the wolf makes you a leader of hunters"})]})]})]}
                             6 {:selections [(t/selection-cfg
                                              {:name "Aspect of the Beast"
                                               :tags #{:class}
                                               :order 3
                                               :options [(t/option-cfg
                                                          {:name "Bear"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Aspect of the Beast: Bear"
                                                                         :page 50
                                                                         :summary "You gain the might of a bear. Your carrying capacity (including maximum load and maximum lift) is doubled, and you have advantage on Strength checks made to push, pull, lift, or break objects"})]})
                                                         (t/option-cfg
                                                          {:name "Eagle"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Aspect of the Beast: Eagle"
                                                                         :page 50
                                                                         :summary "You gain the eyesight of an eagle. You can see up to 1 mile away with no difficulty, able to discern even fine details as though looking at something no more than 100 feet away from you. Additionally, dim light doesn't impose disadvantage on your Wisdom (Perception) checks"})]})
                                                         (t/option-cfg
                                                          {:name "Elk"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Aspect of the Beast: Elk"
                                                                         :page 50
                                                                         :summary "Whether mounted or on foot, your travel pace is doubled, as is the travel pace of up to ten companions while they're within 60 feet of you and you're not incapacitated. The elk spirit helps you roam far and fast"})]})
                                                         (t/option-cfg
                                                          {:name "Tiger"
                                                           :selections [(opt5e/skill-selection [:athletics :acrobatics :stealth :survival] 2)]})
                                                         (t/option-cfg
                                                          {:name "Wolf"
                                                           :modifiers [(mod5e/trait-cfg
                                                                        {:name "Aspect of the Beast: Wolf"
                                                                         :page 50
                                                                         :summary "You gain the hunting sensibilities of a wolf. You can track other creatures while traveling at a fast pace, and you can move stealthily while traveling at a normal pace"})]})]})]}
                             10 {:modifiers [(mod5e/spells-known 5 :commune-with-nature nil "Barbarian" 1 "ritual only")]}
                             14 {:selections [(t/selection-cfg
                                               {:name "Totemic Attunement"
                                                :tags #{:class}
                                                :order 4
                                                :options [(t/option-cfg
                                                           {:name "Bear"
                                                            :modifiers [(mod5e/trait-cfg
                                                                         {:name "Totemic Attunement: Bear"
                                                                          :page 50
                                                                          :summary "While you're raging, any creature within 5 feet of you that's hostile to you has disadvantage on attack rolls against targets other than you or another character with this feature. An enemy is immune to this effect if it can't see or hear you or if it can't be frightened"})]})
                                                          (t/option-cfg
                                                           {:name "Eagle"
                                                            :modifiers [(mod5e/trait-cfg
                                                                         {:name "Totemic Attunement: Eagle"
                                                                          :page 50
                                                                          :summary "While raging, you have a flying speed equal to your current walking speed. This benefit works only in short bursts; you fall if you end your turn in the air and nothing else is holding you aloft"})]})
                                                          (t/option-cfg
                                                           {:name "Elk"
                                                            :modifiers [(mod5e/bonus-action
                                                                         {:name "Totemic Attunement: Elk"
                                                                          :page 50
                                                                          :summary (str "While raging, you can use a bonus action during your move to pass through the space of a Large or smaller creature. That creature must succeed on a Strength saving throw (DC 8 + your Strength bonus + your proficiency bonus = " (?spell-save-dc ::char5e/str) ") or be knocked prone and take bludgeoning damage equal to 1d12 + your Strength modifier (" (?ability-bonuses ::char5e/str) ")")})]})
                                                          (t/option-cfg
                                                           {:name "Tiger"
                                                            :modifiers [(mod5e/bonus-action
                                                                         {:name "Totemic Attunement: Tiger"
                                                                          :page 50
                                                                          :summary "While you're raging, if you move at least 20 feet in a straight line toward a Large or smaller target right before making a melee weapon attack against it, you can use a bonus action to make an additional melee weapon attack against it"})]})
                                                          (t/option-cfg
                                                           {:name "Wolf"
                                                            :modifiers [(mod5e/bonus-action
                                                                         {:name "Totemic Attunement: Wolf"
                                                                          :page 50
                                                                          :summary "While you're raging, you can use a bonus action on your turn to knock a Large or smaller creature prone when you hit it with melee weapon attack"})]})]})]}}}
                 {:name "Path of the Zealot"
                  :levels {3 {:modifiers [(mod5e/dependent-trait
                                           {:name "Divine Fury"
                                            :summary (str "You can channel divine fury into your weapon strikes. While you're raging, the first creature you hit on each of your turns with a weapon attack takes extra damage equal to 1d6 + half your Barbarian level (" (int (/ (?class-level :barbarian) 2)) "). The extra damage is necrotic or radiant; you choose the type of damage when you gain this feature")})
                                          (mod5e/trait-cfg
                                           {:name "Warrior of the Gods"
                                            :summary "Your soul is marked for endless battle. If a spell, such as Raise Dead, has the sole effect of restoring you to life (but not undeath), the caster doesn't need material components to cast the spell on you"})]}
                           6 {:modifiers [(mod5e/trait-cfg
                                           {:name "Fanatical Focus"
                                            :summary "The divine power that fuels your rage can protect you. If you fail a saving throw while raging, you can reroll it, and you must use the new roll. You can use this ability only once per rage"})]}
                           10 {:modifiers [(mod5e/bonus-action
                                            {:name "Zealous Presence"
                                             :frequency units5e/long-rests-1
                                             :summary "You learn to channel divine power to inspire zealotry in others. As a bonus action, you unleash a battle cry infused with divine energy. Up to ten other creatures of your choice within 60 feet of you that can hear you gain advantage on attack rolls and saving throws until the start of your next turn"})]}
                           14 {:modifiers [(mod5e/trait-cfg
                                            {:name "Rage Beyond Death"
                                             :summary "The divine power that fuels your rage allows you to shrug off fatal blows. While you're raging, having 0 hit points doesn’t knock you unconscious. You still must make death saving throws, and you suffer the normal effects of taking damage while at 0 hit points. However, if you would die due to failing death saving throws, you don’t die until your rage ends, and you die then only if you still have 0 hit points"})]}}}]}))

(defn bardic-inspiration-die [levels]
  (condp <= (class-level levels :bard)
    15 12
    10 10
    5 8
    6))

(def musical-instrument-choice-cfg
  {:name "Musical Instrument"
   :options (zipmap (map :key equipment5e/musical-instruments) (repeat 1))})

(defn bard-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Bard"
    :key :bard
    :hit-die 8
    :ability-increase-levels [4 8 12 16 19]
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/cha 13)]
    :profs {:armor {:light false}
            :weapon {:simple true :crossbow-hand true :longsword true :rapier true :shortsword true}
            :save {::char5e/dex true ::char5e/cha true}
            :skill-options {:choose 3 :options {:any true}}
            :multiclass-skill-options {:choose 1 :options {:any true}}
            :tool-options {:musical-instrument 3}
            :multiclass-tool-options {:musical-instrument 1}}
    :weapon-choices [{:name "Weapon"
                      :options {:rapier 1
                                :longsword 1
                                :simple 1}}]
    :weapons {:dagger 1}
    :equipment-choices [{:name "Equipment Pack"
                         :options {:diplomats-pack 1
                                   :entertainers-pack 1}}
                        musical-instrument-choice-cfg]
    :armor {:leather 1}
    :spellcaster true
    :spellcasting {:level-factor 1
                   :cantrips-known {1 2 4 1 10 1}
                   :spells-known {1 4
                                  2 1
                                  3 1
                                  4 1
                                  5 1
                                  6 1
                                  7 1
                                  8 1
                                  9 1
                                  11 1
                                  13 1
                                  15 1
                                  17 1}
                   :known-mode :schedule
                   :ability ::char5e/cha}
    :modifiers [(mod5e/bonus-action
                 {:name "Bardic Inspiration"
                  :page 53
                  :range units5e/ft-60
                  :frequency (units5e/long-rests
                              (max 1 (?ability-bonuses ::char5e/cha)))
                  :summary (str "You can inspire others through stirring words or music. To do so, you use a bonus action on your turn to choose one creature other than yourself within 60 feet of you who can hear you. That creature gains one Bardic Inspiration die, a d"
                                (bardic-inspiration-die ?levels) "."
                                "\n\nOnce within the next 10 minutes, the creature can roll the die and add the number rolled to one ability check, attack roll, or saving throw it makes. The creature can wait until after it rolls the d20 before deciding to use the Bardic Inspiration die, but must decide before the DM says whether the roll succeeds or fails. Once the Bardic Inspiration die is rolled, it is lost. A creature can have only one Bardic Inspiration die at a time")})]
    :levels {2 {:modifiers [(mod/vec-mod ?default-skill-bonus-fns
                                         (fn [_]
                                           (int (/ ?prof-bonus 2))))
                            (mod/cum-sum-mod ?initiative (int (/ ?prof-bonus 2)))
                            (mod5e/dependent-trait
                             {:name "Jack of All Trades"
                              :page 54
                              :summary (str "You can add half your proficiency bonus, rounded down (" (common/bonus-str (int (/ ?prof-bonus 2))) "), to any ability check you make that doesn't already include your proficiency bonus")})
                            (mod5e/dependent-trait
                             {:name "Song of Rest"
                              :page 54
                              :level 2
                              :summary (str "You can use soothing music or oration to help revitalize your wounded allies during a short rest. If you or any friendly creatures who can hear your performance regain hit points at the end of the short rest by spending one or more Hit Dice, each of those creatures regains an extra 1d"
                                            (mod5e/level-val
                                             (?class-level :bard)
                                             {9 8
                                              13 10
                                              17 12
                                              :default 6})
                                            " hit points")})
                            (mod5e/trait-cfg
                             {:name "Magical Inspiration"
                              :level 2
                              :summary "If a creature has a Bardic Inspiration die from you and casts a spell that restores hit points or deals damage, the creature can roll that die and choose a target affected by the spell. Add the number rolled as a bonus to the hit points regained or the damage dealt. The Bardic Inspiration die is then lost"})]}
             3 {:selections [(opt5e/expertise-selection 2)]}
             4 {:modifiers [(mod5e/trait-cfg
                             {:name "Bardic Versatility"
                              :summary "When you reach level 4, 8, 12, 16, and 19 in bard, you can replace one expertise and one cantrip from the class."})]}
             6 {:modifiers [(mod5e/action
                             {:name "Countercharm"
                              :level 6
                              :page 54
                              :summary "You gain the ability to use musical notes or words of power to disrupt mind-influencing effects. As an action, you can start a performance that lasts until the end of your next turn. During that time, you and any friendly creatures within 30 feet of you have advantage on saving throws against being frightened or charmed. A creature must be able to hear you to gain this benefit. The performance ends early if you are incapacitated or silenced or if you voluntarily end it (no action required)"})]}
             10 {:selections (conj [(opt5e/bard-magical-secrets spells-map 10)]
                                   (opt5e/expertise-selection 2))}
             14 {:selections [(opt5e/bard-magical-secrets spells-map 14)]}
             18 {:selections [(opt5e/bard-magical-secrets spells-map 18)]}}
    :traits [{:name "Font of Inspiration"
              :level 5
              :page 54
              :summary "you regain all of your expended uses of Bardic Inspiration when you finish a short or long rest"}
             {:name "Superior Inspiration"
              :level 20
              :page 54
              :summary "when you roll initiative and have no uses of Bardic Inspiration left, you regain one use"}]
    
    :subclass-level 3
    :subclass-title "Bard College"
    :subclass-help "Your bard college is a loose association that preserves bardic traditions and affords additional features"
    :subclasses [{:name "College of Eloquence"
                  :modifiers [(mod5e/trait-cfg
                               {:name "Silver Tongue"
                                :level 3
                                :summary "you are a master at saying the right thing at the right time. When you make a Charisma (Persuasion) or Charisma (Deception) check, you can treat a d20 roll of 9 or lower as a 10"})
                              (mod5e/bonus-action
                               {:name "Unsettling Words"
                                :level 3
                                :summary "you can spin words laced with magic that unsettle a creature and cause it to doubt itself. As a bonus action, you can expend one use of your Bardic Inspiration and choose one creature you can see within 60 feet of you. Roll the Bardic Inspiration die. The creature must subtract the number rolled from the next saving throw it makes before the start of your next turn"})]
                  :levels {6 {:modifiers [(mod5e/trait-cfg
                                           {:name "Unfailing Inspiration"
                                            :level 6
                                            :summary "your inspiring words are so persuasive that others feel driven to succeed. When a creature adds one of your Bardic Inspiration dice to its ability check, attack roll, or saving throw and the roll fails, the creature can keep the Bardic Inspiration die"})
                                          (mod5e/action
                                           {:name "Universal Speech"
                                            :level 6
                                            :duration units5e/hours-1
                                            :frequency units5e/long-rests-1
                                            :summary (str "you have gained the ability to make your speech intelligible to any creature. As an action, choose one or more creatures within 60 feet of you, up to a number equal to your Charisma modifier (minimum of one creature) (" (max 1 (?ability-bonuses ::char5e/cha)) "). The chosen creatures can magically understand you, regardless of the language you speak, for 1 hour."
                                                          "\n\nOnce you use this feature, you can't use it again until you finish a long rest, unless you expend a spell slot to use it again")})]}
                           14 {:modifiers [(mod5e/reaction
                                            {:name "Infectious Inspiration"
                                             :level 14
                                             :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/cha)))
                                             :summary "when you successfully inspire someone, the power of your eloquence can now spread to someone else. When a creature within 60 feet of you adds one of your Bardic Inspiration dice to its ability check, attack roll, or saving throw and the roll succeeds, you can use your reaction to encourage a different creature (other than yourself) that can hear you within 60 feet of you, giving it a Bardic Inspiration die without expending any of your Bardic Inspiration uses"})]}}}
                 {:name "College of Lore"
                  :profs {:skill-options {:choose 3 :options {:any true}}}
                  :modifiers [(mod5e/reaction
                               {:name "Cutting Words"
                                :level 3
                                :page 54
                                :summary "you learn how to use your wit to distract, confuse, and otherwise sap the confidence and competence of others. When a creature that you can see within 60 feet of you makes an attack roll, an ability check, or a damage roll, you can use your reaction to expend one of your uses of Bardic Inspiration, rolling a Bardic Inspiration die and subtracting the number rolled from the creature's roll. You can choose to use this feature after the creature makes its roll, but before the DM determines whether the attack roll or ability check succeeds or fails, or before the creature deals its damage. The creature is immune if it can't hear you or if it's immune to being charmed"})]
                  :levels {6 {:selections [(opt5e/bard-magical-secrets spells-map 6)]}
                           14 {:modifiers [(mod5e/dependent-trait
                                            {:name "Peerless Skill"
                                             :level 14
                                             :page 55
                                             :summary "when you make an ability check, you can expend one use of Bardic Inspiration. Roll a Bardic Inspiration die and add the number rolled to your ability check. You can choose to do so after you roll the die for the ability check, but before the DM tells you whether you succeed or fail"})]}}}
                 {:name "College of Swords"
                    :profs {:armor {:medium true}
                            :weapon {:scimitar true}}
                    :levels {3 {:selections [(opt5e/fighting-style-selection :bard #{:dueling :two-weapon-fighting})]
                                :modifiers [(mod5e/trait-cfg
                                             {:name "Blade Flourish"
                                              :summary (str "Whenever you take the Attack action on your turn, your walking speed increases by 10 feet until the end of the turn, and if a weapon attack that you make as part of this action hits a creature, you can use one of the following Blade Flourish options of your choice. You can use only one Blade Flourish option per turn."
                                                            "\n\nDefensive Flourish. You can expend one use of your Bardic Inspiration to cause the weapon to deal extra damage to the target you hit. The damage equals the number you roll on the Bardic Inspiration die. You also add the number rolled to your AC until the start of your next turn."
                                                            "\n\nSlashing Flourish. You can expend one use of your Bardic Inspiration to cause the weapon to deal extra damage to the target you hit and to any other creature of your choice that you can see within 5 feet of you. The damage equals the number you roll on the Bardic Inspiration die."
                                                            "\n\nMobile Flourish. You can expend one use of your Bardic Inspiration to cause the weapon to deal extra damage to the target you hit. The damage equals the number you roll on the Bardic Inspiration die. You can also push the target up to 5 feet away from you, plus a number of feet equal to the number you roll on that die. You can then immediately use your reaction to move up to your walking speed to an unoccupied space within 5 feet of the target.")})]}
                             6 {:modifiers [(extra-attack-trait)
                                            (mod5e/num-attacks 2)]}
                             14 {:modifiers [(mod5e/bonus-action
                                              {:name "Battle Magic"
                                               :summary "Whenever you use a Blade Flourish option, you can roll a d6 and use it instead of expending a Bardic Inspiration die."})]}}}
                 {:name "College of Valor"
                    :profs {:armor {:medium true
                                    :shields true}
                            :weapon {:martial true}}
                    :levels {3 {:modifiers [(mod5e/trait-cfg
                                             {:name "Combat Inspiration"
                                              :page 55
                                              :summary "you learn to inspire others in battle. A creature that has a Bardic Inspiration die from you can roll that die and add the number rolled to a weapon damage roll it just made. Alternatively, when an attack roll is made against the creature, it can use its reaction to roll the Bardic Inspiration die and add the number rolled to its AC against that attack, after seeing the roll but before knowing whether it hits or misses"})]}
                             6 {:modifiers [(extra-attack-trait 55)
                                            (mod5e/num-attacks 2)]}
                             14 {:modifiers [(mod5e/bonus-action
                                              {:name "Battle Magic"
                                               :page 55
                                               :summary "you have mastered the art of weaving spellcasting and weapon use into a single harmonious act. When you use your action to cast a bard spell, you can make one weapon attack as a bonus action"})]}}}]}))

(defn blessings-of-knowledge-skill [skill-name]
  (let [skill-kw (common/name-to-kw skill-name)]
    (t/option-cfg
     {:name skill-name
      :key skill-kw
      :modifiers [(mod5e/skill-proficiency skill-kw)
                  (mod5e/skill-expertise skill-kw)]})))

(def spell-level-to-cleric-level
  {1 1
   2 3
   3 5
   4 7
   5 9})

(defn cleric-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Cleric",
    :key :cleric
    :spellcasting {:level-factor 1
                   :cantrips-known {1 3 4 1 10 1}
                   :known-mode :all
                   :ability ::char5e/wis
                   :prepares-spells? true}
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/wis 13)]
    :spellcaster true
    :hit-die 8,
    :ability-increase-levels [4 8 12 16 19]
    :profs {:armor {:light false :medium false :shields false}
            :weapon {:simple true}
            :save {::char5e/wis true ::char5e/cha true}
            :skill-options {:choose 2 :options {:history true :insight true :medicine true :persuasion true :religion true}}}
    :equipment-choices [{:name "Equipment Pack"
                         :options {:priests-pack 1
                                   :explorers-pack 1}}]
    :weapon-choices [{:name "Cleric Weapon"
                      :options {:mace 1
                                :warhammer 1}}]
    :armor-choices [{:name "Armor"
                     :options {:scale-mail 1
                               :leather 1
                               :chain-mail 1}}]
    :armor {:shield 1}
    :selections [(opt5e/new-starting-equipment-selection
                  :cleric
                  {:name "Additional Weapon"
                   :options [(t/option-cfg
                              {:name "Light Crossbow and 20 Bolts"
                               :modifiers [(mod5e/weapon :crossbow-light 1)
                                           (mod5e/equipment :crossbow-bolt 20)]})
                             (t/option-cfg
                              {:name "Simple Weapon"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :cleric
                                             {:name "Simple Weapon"
                                              :options (opt5e/simple-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})]})]})
                 (opt5e/new-starting-equipment-selection
                  :cleric
                  {:name "Holy Symbol"
                   :options (map
                             #(opt5e/starting-equipment-option % 1)
                             equipment5e/holy-symbols)})]
    :levels {2 {:modifiers [(mod5e/dependent-trait
                             {:page 59
                              :name "Channel Divinity"
                              :summary "you gain the ability to channel divine energy directly from your deity, using that energy to fuel magical effects. You start with two such effects: Turn Undead and an effect determined by your domain. Some domains grant you additional effects as you advance in levels, as noted in the domain description"
                              :frequency (units5e/rests (mod5e/level-val
                                                         (?class-level :cleric)
                                                         {6 2
                                                          18 3
                                                          :default 1}))})
                            (mod5e/action
                             {:page 59
                              :name "Channel Divinity: Turn Undead"
                              :summary (str "As an action, you present your holy symbol and speak a prayer censuring the undead. Each undead that can see or hear you within 30 feet of you must make a Wisdom saving throw. If the creature fails its saving throw, it is turned for 1 minute or until it takes any damage."
                                            "\n\nA turned creature must spend its turns trying to move as far away from you as it can, and it can't willingly move to a space within 30 feet of you. It also can't take reactions. For its action, it can use only the Dash action or try to escape from an effect that prevents it from moving. If there's nowhere to move, the creature can use the Dodge action")})
                            (mod5e/bonus-action
                             {:name "Harness Divine Power"
                              :frequency (units5e/long-rests (mod5e/level-val
                                             (?class-level :cleric)
                                             {6 2
                                              18 3
                                              :default 1}))
                              :summary (str "you can expend a use of your Channel Divinity to fuel your spells. As a bonus action, you touch your holy symbol, utter a prayer, and regain one expended spell slot, the level of which can be no higher than half your proficiency bonus (rounded up) ("
                                            (common/round-up (/ ?prof-bonus 2))
                                            ")")})]}
             4 {:modifiers [(mod5e/trait-cfg
                             {:name "Cantrip Versatility"
                              :summary "When you reach level 4, 8, 12, 16, and 19 in cleric, you can replace one cantrip from the class."})]}
             5 {:modifiers [(mod5e/dependent-trait
                             {:level 5
                              :name "Destroy Undead"
                              :page 59
                              :summary (str "when an undead fails its saving throw against your Turn Undead feature, the creature is instantly destroyed if its challenge rating is at or below CR "
                                            (let [level (?class-level :cleric)]
                                              (mod5e/level-val
                                               level
                                               {5 "1/2"
                                                8 1
                                                11 2
                                                14 3
                                                17 4})))})]}
             
             10 {:modifiers [(mod5e/action
                              {:name "Divine Intervention"
                               :page 59
                               :summary (str "you can call on your deity to intervene on your behalf when your need is great. Imploring your deity's aid requires you to use your action. Describe the assistance you seek, and roll percentile dice. If you roll a number equal to or lower than your cleric level, your deity intervenes. The DM chooses the nature of the intervention; the effect of any cleric spell or cleric domain spell would be appropriate. If your deity intervenes, you can't use this feature again for 7 days. Otherwise, you can use it again after you finish a long rest."
                                             "\n\nAt 20th level, your call for intervention succeeds automatically, no roll required")})]}}
    :subclass-level 1
    :subclass-title "Divine Domain"
    :subclasses [{:name "Forge Domain"
                    :profs {:armor {:heavy true}
                            :weapon {:martial true}}
                    :modifiers [(opt5e/cleric-spell 1 :identify 1)
                                (opt5e/cleric-spell 1 :searing-smite 1)
                                (opt5e/cleric-spell 2 :heat-metal 3)
                                (opt5e/cleric-spell 2 :magic-weapon 3)
                                (opt5e/cleric-spell 3 :elemental-weapon 5)
                                (opt5e/cleric-spell 3 :protection-from-energy 5)
                                (opt5e/cleric-spell 4 :fabricate 7)
                                (opt5e/cleric-spell 4 :wall-of-fire 7)
                                (opt5e/cleric-spell 5 :animate-objects 9)
                                (opt5e/cleric-spell 5 :creation 9)
                                (mod5e/bonus-action
                                 {:name "Blessing of the Forge"
                                  :level 1
                                  :frequency units5e/long-rests-1
                                  :summary "you gain the ability to imbue magic into a weapon or armor. At the end of a long rest, you can touch one nonmagical object that is a suit of armor or a simple or martial weapon. Until the end of your next long rest or until you die, the object becomes a magic item, granting a +1 bonus to AC if it's armor or a +1 bonus to attack and damage rolls if it's a weapon"})]
                    :levels {6 {:modifiers [(mod5e/damage-resistance :fire)]}
                             8 {:selections [(opt5e/divine-strike-selection "fire" 63)]}}
                    :traits [{:name "Channel Divinity: Artisan's Blessing"
                              :level 2
                              :summary (str "you can use your Channel Divinity to create simple items. You conduct an hour-long ritual that crafts a nonmagical item that must include some metal: a simple or martial weapon, a suit of armor, ten pieces of ammunition, a set of tools, or another metal object. The creation is completed at the end of the hour, coalescing in an unoccupied space of your choice on a surface within 5 feet of you."
                                            "\n\nThe thing you create can be something that is worth no more than 100 gp. As part of this ritual, you must lay out metal, which can include coins, with a value equal to the creation. The metal irretrievably coalesces and transforms into the creation at the ritual's end, magically forming even nonmetal parts of the creation."
                                            "\n\nThe ritual can create a duplicate of a nonmagical item that contains metal, such as a key, if you possess the original during the ritual")}
                             {:name "Soul of the Forge"
                              :level 6
                              :summary (str "your mastery of the forge grants you special abilities:"
                                            "\n\u2022 You gain resistance to fire damage."
                                            "\n\u2022 While wearing heavy armor, you gain a +1 bonus to AC.")}
                             {:name "Saint of Forge and Fire"
                              :level 17
                              :summary (str "your blessed affinity with fire and metal becomes more powerful:"
                                            "\n\u2022 You gain immunity to fire damage."
                                            "\n\u2022 While wearing heavy armor, you have resistance to bludgeoning, piercing, and slashing damage from nonmagical attacks.")}]}

                 {:name "Life Domain"
                  :profs {:armor {:heavy true}}
                  :modifiers [(opt5e/cleric-spell 1 :bless 1)
                              (opt5e/cleric-spell 1 :cure-wounds 1)
                              (opt5e/cleric-spell 2 :lesser-restoration 3)
                              (opt5e/cleric-spell 2 :spiritual-weapon 3)
                              (opt5e/cleric-spell 3 :beacon-of-hope 5)
                              (opt5e/cleric-spell 3 :revivify 5)
                              (opt5e/cleric-spell 4 :death-ward 7)
                              (opt5e/cleric-spell 4 :guardian-of-faith 7)
                              (opt5e/cleric-spell 5 :mass-cure-wounds 9)
                              (opt5e/cleric-spell 5 :raise-dead 9)]
                  :levels {2 {:modifiers [(mod5e/action
                                           {:name "Channel Divinity: Preserve Life"
                                            :summary (str "As an action, you present your holy symbol and evoke healing energy that can restore a number of hit points equal to five times your cleric level ("
                                                          (* 5 (?class-level :cleric))
                                                          "). Choose any creatures within 30 feet of you, and divide those hit points among them. This feature can restore a creature to no more than half of its hit point maximum. You can't use this feature on an undead or a construct.")})]}
                           8 {:selections [(opt5e/divine-strike-selection "radiant" 60)]}}
                  :traits [{:level 1
                            :name "Disciple of Life"
                            :page 60
                            :summary "your healing spells are more effective. Whenever you use a spell of 1st level or higher to restore hit points to a creature, the creature regains additional hit points equal to 2 + the spell's level"}
                           {:level 6
                            :name "Blessed Healer"
                            :page 60
                            :summary "the healing spells you cast on others heal you as well. When you cast a spell of 1st level or higher that restores hit points to a creature other than you, you regain hit points equal to 2 + the spell's level"}
                           {:level 17
                            :name "Supreme Healing"
                            :summary "when you would normally roll one or more dice to restore hit points with a spell, you instead use the highest number possible for each die. For example, instead of restoring 2d6 hit points to a creature, you restore 12"}]}
                 {:name "Peace Domain"
                  :profs {:skill-options {:choose 1 :options {:insight true :performance true :persuasion true}}}
                  :modifiers [(opt5e/cleric-spell 1 :heroism 1)
                              (opt5e/cleric-spell 1 :sanctuary 1)
                              (opt5e/cleric-spell 2 :aid 3)
                              (opt5e/cleric-spell 2 :warding-bond 3)
                              (opt5e/cleric-spell 3 :beacon-of-hope 5)
                              (opt5e/cleric-spell 3 :sending 5)
                              (opt5e/cleric-spell 4 :aura-of-purity 7)
                              (opt5e/cleric-spell 4 :otilukes-resilient-sphere 7)
                              (opt5e/cleric-spell 5 :greater-restoration 9)
                              (opt5e/cleric-spell 5 :rarys-telepathic-bond 9)]
                  :levels {1 {:modifiers [(mod5e/action
                                           {:name "Emboldening Bond"
                                            :frequency (units5e/long-rests ?prof-bonus)
                                            :duration units5e/minutes-10
                                            :summary "you can forge an empowering bond among people who are at peace with one another. As an action, you choose a number of willing creatures within 30 feet of you (this can include yourself) equal to your proficiency bonus. You create a magical bond among them for 10 minutes or until you use this feature again. While any bonded creature is within 30 feet of another, the creature can roll a d4 and add the number rolled to an attack roll, an ability check, or a saving throw it makes. Each creature can add the d4 no more than once per turn"})]}
                           2 {:modifiers [(mod5e/action
                                           {:name "Channel Divinity: Balm of Peace"
                                            :summary "you can use your Channel Divinity to make your very presence a soothing balm. As an action, you can move up to your speed, without provoking opportunity attacks, and when you move within 5 feet of any other creature during this action, you can restore a number of hit points to that creature equal to 2d6 + your Wisdom modifier (minimum of 1 hit point). A creature can receive this healing only once whenever you take this action"})]}
                           6 {:modifiers [(mod5e/reaction
                                           {:name "Protective Bond"
                                            :summary "the bond you forge between people helps them protect each other. When a creature affected by your Emboldening Bond feature is about to take damage, a second bonded creature within 30 feet of the first can use its reaction to teleport to an unoccupied space within 5 feet of the first creature. The second creature then takes all the damage instead"})]}
                           8 {:selections [(opt5e/potent-spellcasting-selection 32)]}
                           17 {:modifiers [(mod5e/trait-cfg
                                             {:name "Expansive Bond"
                                              :summary "the benefits of your Emboldening Bond and Protective Bond features now work when the creatures are within 60 feet of each other. Moreover, when a creature uses Protective Bond to take someone else's damage, the creature has resistance to that damage"})]}}}
                 #_{:name "Knowledge Domain"
                    :modifiers [(opt5e/cleric-spell 1 :command 1)
                                (opt5e/cleric-spell 1 :identify 1)
                                (opt5e/cleric-spell 2 :augury 3)
                                (opt5e/cleric-spell 2 :suggestion 3)
                                (opt5e/cleric-spell 3 :nondetection 5)
                                (opt5e/cleric-spell 3 :speak-with-dead 5)
                                (opt5e/cleric-spell 4 :arcane-eye 7)
                                (opt5e/cleric-spell 4 :confusion 7)
                                (opt5e/cleric-spell 5 :legend-lore 9)
                                (opt5e/cleric-spell 5 :scrying 9)]
                    :selections [(opt5e/language-selection opt5e/languages 2)
                                 (t/selection-cfg
                                  {:name "Blessings of Knowledge Skills"
                                   :tags #{:profs :skill-profs}
                                   :options (map
                                             blessings-of-knowledge-skill
                                             ["Arcana" "History" "Nature" "Religion"])
                                   :min 2
                                   :max 2})]
                    :levels {2 {:modifiers [(mod5e/action
                                             {:page 59
                                              :summary "Become proficient in a tool or skill for 10 mins."
                                              :name "Channel Divinity: Knowledge of the Ages"})]}
                             6 {:modifiers [(mod5e/action
                                             {:page 59
                                              :name "Channel Divinity: Read Thoughts"
                                              :summary (str "a creature within 60 ft. must make a DC "
                                                            (?spell-save-dc ::char5e/wis)
                                                            " Wisdom save or you can read it's thoughts for 1 min, use an action to end the effect and cast 'suggestion' without using a slot and with no save")})]}
                             8 {:selections [(opt5e/potent-spellcasting-selection 60)]}}
                    :traits [
                             {:level 17
                              :page 60
                              :name "Visions of the Past"
                              :summary "Learn the history of an object you hold or area you are in"}]}
                 #_{:name "Light Domain"
                    :modifiers [(opt5e/cleric-spell 0 :light 1)
                                (opt5e/cleric-spell 1 :burning-hands 1)
                                (opt5e/cleric-spell 1 :faerie-fire 1)
                                (opt5e/cleric-spell 2 :flaming-sphere 3)
                                (opt5e/cleric-spell 2 :scorching-ray 3)
                                (opt5e/cleric-spell 3 :daylight 5)
                                (opt5e/cleric-spell 3 :fireball 5)
                                (opt5e/cleric-spell 4 :guardian-of-faith 7)
                                (opt5e/cleric-spell 4 :wall-of-fire 7)
                                (opt5e/cleric-spell 5 :flame-strike 9)
                                (opt5e/cleric-spell 5 :scrying 9)
                                (mod5e/reaction
                                 {:name "Warding Flare"
                                  :page 61
                                  :summary "impose disadvantage on an attack roll against you"
                                  :frequency (units5e/long-rests
                                              (max 1 (?ability-bonuses ::char5e/wis)))})]
                    :levels {2 {:modifiers [(mod5e/action
                                             {:level 2
                                              :class-key :cleric
                                              :name "Channel Divinity: Radiance of the Dawn"
                                              :page 61
                                              :range {:plural :feet
                                                      :amount 30}
                                              :summary (str "Dispel magical darkness and deal 2d10 + "
                                                            (?class-level :cleric)
                                                            " radiant damage (half on successful DC "
                                                            (?spell-save-dc ::char5e/wis)
                                                            " Constitution save) to hostile creatures")})]}
                             6 {:modifiers [(mod5e/reaction
                                             {:level 6
                                              :name "Improved Flare"
                                              :page 61
                                              :summary "use warding flare when another creature within 30 ft. is attacked"})]}
                             8 {:selections [(opt5e/potent-spellcasting-selection 61)]}
                             17 {:modifiers [(mod5e/action
                                              {:level 17
                                               :page 61
                                               :name "Corona of Light"
                                               :summary "emit bright light for 60 ft. and 30 beyond that, enemies in the bright light have disadvantage on saves against spells that deal radiant or fire damage"})]}}}
                 #_{:name "Nature Domain"
                    :profs {:armor {:heavy true}
                            :skill-options {:choose 1 :options {:animal-handling true :nature true :survival true}}}
                    :modifiers [(opt5e/cleric-spell 1 :animal-friendship 1)
                                (opt5e/cleric-spell 1 :speak-with-animals 1)
                                (opt5e/cleric-spell 2 :barkskin 3)
                                (opt5e/cleric-spell 2 :spike-growth 3)
                                (opt5e/cleric-spell 3 :plant-growth 5)
                                (opt5e/cleric-spell 3 :wind-wall 5)
                                (opt5e/cleric-spell 4 :dominate-beast 7)
                                (opt5e/cleric-spell 4 :grasping-vine 7)
                                (opt5e/cleric-spell 5 :insect-plague 9)
                                (opt5e/cleric-spell 5 :tree-stride 9)]
                    :levels {2 {:modifiers [(mod5e/action
                                             {:name "Channel Divinity: Charm Animals and Plants"
                                              :level 2
                                              :page 62
                                              :range {:plural :feet
                                                      :amount 30}
                                              :summary (str "charm beasts and plant creatures unless they succeed on a DC "
                                                            (?spell-save-dc ::char5e/wis)
                                                            " Wisdom save")})]}
                             6 {:modifiers [(mod5e/reaction
                                             {:name "Dampen Elements"
                                              :level 6
                                              :page 62
                                              :range {:plural :feet
                                                      :amount 30}
                                              :summary "to a creature that takes fire, cold, acid, lighting, or thunder damage, grant resistance to that damage"})]}
                             8 {:selections [(opt5e/divine-strike-selection "cold, fire, or lighting" 62)]}
                             17 {:modifiers [(mod5e/bonus-action
                                              {:name "Master of Nature"
                                               :level 17
                                               :page 62
                                               :summary "command creatures charmed with your Charm Animals and Plants"})]}}
                    :selections [(opt5e/druid-cantrip-selection "Cleric")]}
                 {:name "Tempest Domain"
                    :profs {:armor {:heavy true}
                            :weapon {:martial true}}
                    :modifiers [(opt5e/cleric-spell 1 :fog-cloud 1)
                                (opt5e/cleric-spell 1 :thunderwave 1)
                                (opt5e/cleric-spell 2 :gust-of-wind 3)
                                (opt5e/cleric-spell 2 :shatter 3)
                                (opt5e/cleric-spell 3 :call-lightning 5)
                                (opt5e/cleric-spell 3 :sleet-storm 5)
                                (opt5e/cleric-spell 4 :control-water 7)
                                (opt5e/cleric-spell 4 :ice-storm 7)
                                (opt5e/cleric-spell 5 :destructive-wave 9)
                                (opt5e/cleric-spell 5 :insect-plague 9)
                                (mod5e/reaction
                                 {:name "Wrath of the Storm"
                                  :page 62
                                  :frequency (units5e/long-rests
                                              (max 1 (?ability-bonuses ::char5e/wis)))
                                  :summary "you can thunderously rebuke attackers. When a creature within 5 feet of you that you can see hits you with an attack, you can use your reaction to cause the creature to make a Dexterity saving throw. The creature takes 2d8 lightning or thunder damage (your choice) on a failed saving throw, and half as much damage on a successful one"})]
                    :levels {2 {:modifiers [(mod5e/trait-cfg
                                             {:name "Channel Divinity: Destructive Wrath"
                                              :page 62
                                              :level 2
                                              :summary "you can use your Channel Divinity to wield the power of the storm with unchecked ferocity. When you roll lightning or thunder damage, you can use your Channel Divinity to deal maximum damage, instead of rolling"})]}
                             8 {:selections [(opt5e/divine-strike-selection "thunder" 62)]}
                             17 {:modifiers [(mod5e/flying-speed-equal-to-walking)]}}
                    :traits [{:name "Thunderous Strike"
                              :page 62
                              :level 6
                              :summary "when you deal lightning damage to a Large or smaller creature, you can also push it up to 10 feet away from you"}
                             {:name "Stormborn"
                              :page 62
                              :level 17
                              :summary "you have a flying speed equal to your current walking speed whenever you are not underground or indoors"}]}
                 #_{:name "Trickery Domain"
                    :modifiers [(opt5e/cleric-spell 1 :charm-person 1)
                                (opt5e/cleric-spell 1 :disguise-self 1)
                                (opt5e/cleric-spell 2 :mirror-image 3)
                                (opt5e/cleric-spell 2 :pass-without-trace 3)
                                (opt5e/cleric-spell 3 :blink 5)
                                (opt5e/cleric-spell 3 :dispel-magic 5)
                                (opt5e/cleric-spell 4 :dimension-door 7)
                                (opt5e/cleric-spell 4 :polymorph 7)
                                (opt5e/cleric-spell 5 :dominate-person 9)
                                (opt5e/cleric-spell 5 :modify-memory 9)
                                (mod5e/action
                                 {:name "Blessing of the Trickster"
                                  :page 63
                                  :duration units5e/hours-1
                                  :summary "Give another creature advantage on stealth checks"})]
                    :levels {2 {:modifiers [(mod5e/action
                                             {:name "Channel Divinity: Invoke Duplicity"
                                              :level 2
                                              :page 63
                                              :summary "create illusion of yourself for 1 min. or concentration. Move it 30 ft. as a bonus action, cast spells as if in illusion's space, gain advantage on attacks on a creature both you and the illusion are within 5 ft. of"})]}
                             6 {:modifiers [(mod5e/action
                                             {:name "Channel Divinity: Cloak of Shadows"
                                              :level 6
                                              :page 63
                                              :summary "become invisible until end of your next turn"})]}
                             8 {:selections [(opt5e/divine-strike-selection "poison" 63)]}
                             17 {:modifiers [(mod5e/action
                                              {:name "Improved Duplicity"
                                               :level 17
                                               :page 63
                                               :summary "when you use Invoke Duplicity, create up to 4 duplicates"})]}}}
                 {:name "War Domain"
                    :profs {:armor {:heavy true}
                            :weapon {:martial true}}
                    :modifiers [(opt5e/cleric-spell 1 :divine-favor 1)
                                (opt5e/cleric-spell 1 :shield-of-faith 1)
                                (opt5e/cleric-spell 2 :magic-weapon 3)
                                (opt5e/cleric-spell 2 :spiritual-weapon 3)
                                (opt5e/cleric-spell 3 :crusaders-mantle 5)
                                (opt5e/cleric-spell 3 :spirit-guardians 5)
                                (opt5e/cleric-spell 4 :freedom-of-movement 7)
                                (opt5e/cleric-spell 4 :stoneskin 7)
                                (opt5e/cleric-spell 5 :flame-strike 9)
                                (opt5e/cleric-spell 5 :hold-monster 9)
                                (mod5e/bonus-action
                                 {:name "War Priest"
                                  :level 1
                                  :page 63
                                  :frequency (units5e/long-rests
                                              (max 1 (?ability-bonuses ::char5e/wis)))
                                  :summary "your god delivers bolts of inspiration to you while you are engaged in battle. When you use the Attack action, you can make one weapon attack as a bonus action"})]
                    :levels {6 {:modifiers [(mod5e/reaction
                                             {:name "Channel Divinity: War God's Blessing"
                                              :level 6
                                              :page 63
                                              :summary "when a creature within 30 feet of you makes an attack roll, you can use your reaction to grant that creature a +10 bonus to the roll, using your Channel Divinity. You make this choice after you see the roll, but before the DM says whether the attack hits or misses"})]}
                             8 {:selections [(opt5e/divine-strike-selection nil 63)]}}
                    :traits [{:name "Channel Divinity: Guided Strike"
                              :page 63
                              :level 2
                              :summary "you can use your Channel Divinity to strike with supernatural accuracy. When you make an attack roll, you can use your Channel Divinity to gain a +10 bonus to the roll. You make this choice after you see the roll, but before the DM says whether the attack hits or misses"}
                             {:name "Avatar of Battle"
                              :page 63
                              :level 17
                              :summary "you gain resistance to bludgeoning, piercing, and slashing damage from nonmagical attacks"}]}]}))

(defn druid-spell [spell-level spell-key min-level]
  (mod5e/spells-known-cfg spell-level
                          {:key spell-key
                           :ability ::char5e/wis
                           :class "Druid"
                           :class-key :druid
                           :always-prepared? true}
                          min-level
                          nil))

(defn lands-stride [level]
  {:name "Land's Stride"
   :level level
   :page 69
   :summary (str "moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard."
                 "\n\nIn addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such as those created by the Entangle spell.")})

(defn druid-option [spell-lists
                    spells-map
                    plugin-subclasses-map
                    language-map
                    weapon-map]
  (opt5e/class-option
   spell-lists
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Druid"
    :key :druid
    :hit-die 8
    :spellcaster true
    :spellcasting {:level-factor 1
                   :cantrips-known {1 2 4 1 10 1}
                   :known-mode :all
                   :ability ::char5e/wis
                   :prepares-spells? true}
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/wis 13)]
    :ability-increase-levels [4 8 12 16 19]
    :profs {:armor {:light false :medium false :shields false}
            :weapon {:club true :dagger true :dart true :javelin true :mace true :quarterstaff true :scimitar true :sickle true :sling true :spear true}
            :tool {:herbalism-kit true}
            :save {::char5e/int true ::char5e/wis true}
            :skill-options {:choose 2 :options {:arcana true :animal-handling true :insight true :medicine true :nature true :perception true :religion true :survival true}}}
    :armor {:leather 1}
    :equipment-choices [{:name "Equipment Pack"
                         :options {:priests-pack 1
                                   :explorers-pack 1}}
                        {:name "Druidic Focus"
                         :options {:druidic-focus 1}}]
    :equipment {:explorers-pack 1}
    :modifiers [(mod5e/language :druidic)]
    :levels {2 {:modifiers [(mod/modifier
                             ?wild-shape-cr
                             (mod5e/level-val
                              (?class-level :druid)
                              {1 "1/4"
                               4 "1/2"
                               8 "1"}))
                            (mod/modifier
                             ?wild-shape-limitation
                             (mod5e/level-val
                              (?class-level :druid)
                              {1 "no flying or swimming speed"
                               4 "no flying speed"
                               8 nil}))
                            (mod5e/action
                             {:name "Wild Shape"
                              :page 66
                              :frequency (units5e/rests 2)
                              :duration (units5e/hours (int (/ (?class-level :druid) 2)))
                              :summary (str "You can transform into a beast you have seen with CR "
                                            ?wild-shape-cr
                                            (if ?wild-shape-limitation (str " and " ?wild-shape-limitation)))})
                            (mod5e/action
                             {:name "Wild Companion"
                              :duration (units5e/hours (int (/ (?class-level :druid) 2)))
                              :summary "Expend a use of Wild Shape to cast the Find Familiar spell, without material components. The familiar is a spirit in animal form, and is a fey instead of beast"})]}}
    :selections [(opt5e/new-starting-equipment-selection
                  :druid
                  {:name "Druidic Focus"
                   :options (map
                             #(opt5e/starting-equipment-option % 1)
                             equipment5e/druidic-focuses)})
                 (opt5e/new-starting-equipment-selection
                  :druid
                  {:name "Wooden Shield or Simple Weapon"
                   :options [(t/option-cfg
                              {:name "Wooden Shield"
                               :modifiers [(mod5e/armor :shield 1)]})
                             (t/option-cfg
                              {:name "Simple Weapon"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :druid
                                             {:name "Simple Weapon"
                                              :options (opt5e/simple-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})]})]})
                 (opt5e/new-starting-equipment-selection
                  :druid
                  {:name "Melee Weapon"
                   :options [(t/option-cfg
                              {:name "Scimitar"
                               :modifiers [(mod5e/weapon :scimitar 1)]})
                             (t/option-cfg
                              {:name "Simple Melee Weapon"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :druid
                                             {:name "Simple Melee Weapon"
                                              :options (opt5e/simple-melee-weapon-options 1 (vals weapon-map))})]})]})]
    :traits [{:name "Druidic"
              :page 66
              :summary "You know Druidic, the secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic"}
             {:name "Cantrip Versatility"
              :summary "When you reach level 4, 8, 12, 16, and 19 in druid, you can replace one cantrip from the class."}
             {:name "Timeless Body"
              :level 18
              :page 67
              :summary "the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year"}
             {:name "Beast Spells"
              :level 18
              :page 67
              :summary "you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren't able to provide material components"}
             {:name "Archdruid"
              :level 20
              :page 67
              :summary "you can use your Wild Shape an unlimited number of times. Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren't consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape"}]
    :subclass-level 2
    :subclass-title "Druid Circle"
    :subclasses [{:name "Circle of the Land"
                  :selections [(opt5e/spell-selection
                                spell-lists
                                spells-map
                                {:class-key :druid
                                 :level 0
                                 :spellcasting-ability ::char5e/wis
                                 :class-name "Druid"
                                 :num 1})
                               (t/selection-cfg
                                {:name "Land Type"
                                 :tags #{:class}
                                 :options [(t/option-cfg
                                            {:name "Arctic"
                                             :modifiers [(druid-spell 2 :hold-person 3)
                                                         (druid-spell 2 :spike-growth 3)
                                                         (druid-spell 3 :sleet-storm 5)
                                                         (druid-spell 3 :slow 5)
                                                         (druid-spell 4 :freedom-of-movement 7)
                                                         (druid-spell 4 :ice-storm 7)
                                                         (druid-spell 5 :commune-with-nature 9)
                                                         (druid-spell 5 :cone-of-cold 9)]})
                                           (t/option-cfg
                                            {:name "Coast"
                                             :modifiers [(druid-spell 2 :mirror-image 3)
                                                         (druid-spell 2 :misty-step 3)
                                                         (druid-spell 3 :water-breathing 5)
                                                         (druid-spell 3 :water-walk 5)
                                                         (druid-spell 4 :control-water 7)
                                                         (druid-spell 4 :freedom-of-movement 7)
                                                         (druid-spell 5 :conjure-elemental 9)
                                                         (druid-spell 5 :scrying 9)]})
                                           (t/option-cfg
                                            {:name "Desert"
                                             :modifiers [(druid-spell 2 :blur 3)
                                                         (druid-spell 2 :silence 3)
                                                         (druid-spell 3 :create-food-and-water 5)
                                                         (druid-spell 3 :protection-from-energy 5)
                                                         (druid-spell 4 :blight 7)
                                                         (druid-spell 4 :hallucinatory-terrain 7)
                                                         (druid-spell 5 :insect-plague 9)
                                                         (druid-spell 5 :wall-of-stone 9)]})
                                           (t/option-cfg
                                            {:name "Forest"
                                             :modifiers [(druid-spell 2 :barkskin 3)
                                                         (druid-spell 2 :spider-climb 3)
                                                         (druid-spell 3 :call-lightning 5)
                                                         (druid-spell 3 :plant-growth 5)
                                                         (druid-spell 4 :divination 7)
                                                         (druid-spell 4 :freedom-of-movement 7)
                                                         (druid-spell 5 :commune-with-nature 9)
                                                         (druid-spell 5 :tree-stride 9)]})
                                           (t/option-cfg
                                            {:name "Grassland"
                                             :modifiers [(druid-spell 2 :invisibility 3)
                                                         (druid-spell 2 :pass-without-trace 3)
                                                         (druid-spell 3 :daylight 5)
                                                         (druid-spell 3 :haste 5)
                                                         (druid-spell 4 :divination 7)
                                                         (druid-spell 4 :freedom-of-movement 7)
                                                         (druid-spell 5 :dream 9)
                                                         (druid-spell 5 :insect-plague 9)]})
                                           (t/option-cfg
                                            {:name "Mountain"
                                             :modifiers [(druid-spell 2 :spider-climb 3)
                                                         (druid-spell 2 :spike-growth 3)
                                                         (druid-spell 3 :lightning-bolt 5)
                                                         (druid-spell 3 :meld-into-stone 5)
                                                         (druid-spell 4 :stone-shape 7)
                                                         (druid-spell 4 :stoneskin 7)
                                                         (druid-spell 5 :passwall 9)
                                                         (druid-spell 5 :wall-of-stone 9)]})
                                           (t/option-cfg
                                            {:name "Swamp"
                                             :modifiers [(druid-spell 2 :darkness 3)
                                                         (druid-spell 2 :acid-arrow 3)
                                                         (druid-spell 3 :water-walk 5)
                                                         (druid-spell 3 :stinking-cloud 5)
                                                         (druid-spell 4 :freedom-of-movement 7)
                                                         (druid-spell 4 :locate-creature 7)
                                                         (druid-spell 5 :insect-plague 9)
                                                         (druid-spell 5 :scrying 9)]})
                                           (t/option-cfg
                                            {:name "Underdark"
                                             :modifiers [(druid-spell 2 :spider-climb 3)
                                                         (druid-spell 2 :web 3)
                                                         (druid-spell 3 :gaseous-form 5)
                                                         (druid-spell 3 :stinking-cloud 5)
                                                         (druid-spell 4 :greater-invisibility 7)
                                                         (druid-spell 4 :stone-shape 7)
                                                         (druid-spell 5 :cloudkill 9)
                                                         (druid-spell 5 :insect-plague 9)]})]})]
                  :levels {2 {:modifiers [(mod5e/dependent-trait
                                           {:name "Natural Recovery"
                                            :level 2
                                            :page 68
                                            :summary (str "you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up) ("
                                                          (common/round-up (/ (?class-level :druid) 2))
                                                          "), and none of the slots can be 6th level or higher.")})]}
                           6 {:modifiers [(mod5e/saving-throw-advantage ["plants magically created or manipulated to impede movement"])]}
                           10 {:modifiers [(mod5e/damage-immunity :poison)
                                           (mod5e/condition-immunity :poisoned)
                                           (mod5e/condition-immunity :charmed "by elementals or fey")
                                           (mod5e/condition-immunity :frightened "by elementals or fey")
                                           (mod5e/immunity :disease)
                                           (mod5e/trait-cfg
                                            {:name "Nature's Ward"
                                             :page 69
                                             :summary "you can't be charmed or frightened by elementals or fey, and you are immune to poison and disease"})]}
                           14 {:modifiers [(mod5e/dependent-trait
                                            {:name "Nature's Santuary"
                                             :level 14
                                             :page 69
                                             :summary "creatures of the natural world sense your connection to nature and become hesitant to attack you. When a beast or plant creature attacks you, that creature must make a Wisdom saving throw against your druid spell save DC. On a failed save, the creature must choose a different target, or the attack automatically misses. On a successful save, the creature is immune to this effect for 24 hours. The creature is aware of this effect before it makes its attack against you"})]}}
                  :traits [(lands-stride 6)]}
                 {:name "Circle of the Shepherd"
                  :levels {2 {:modifiers [(mod5e/language :sylvan)
                                          (mod5e/bonus-action
                                           {:name "Spirit Totem"
                                            :summary (str "As a bonus action, you can magically summon an incorporeal spirit to a point you can see within 60 feet of you. The spirit creates an aura in a 30-foot radius around that point. It counts as neither a creature nor an object, though it has the spectral appearance of the creature it represents. As a bonus action, you can move the spirit up to 60 feet to a point you can see."
                                                          "\n\nThe spirit persists for 1 minute. Once you use this feature, you can’t use it again until you finish a short or long rest."
                                                          "\n\nThe effect of the spirit’s aura depends on the type of spirit you summon from the options below."
                                                          "\n\nBear Spirit. The bear spirit grants you and your allies its might and endurance. Each creature of your choice in the aura when the spirit appears gains temporary hit points equal to 5 + your druid level (" (+ 5 (?class-level :druid)) "). In addition, you and your allies gain advantage on Strength checks and Strength saving throws while in the aura."
                                                          "\n\nHawk Spirit. The hawk spirit is a consummate hunter, aiding you and your allies with its keen sight. When a creature makes an attack roll against a target in the spirit’s aura, you can use your reaction to grant advantage to that attack roll. In addition, you and your allies have advantage on Wisdom (Perception) checks while in the aura."
                                                          "\n\nUnicorn Spirit. The unicorn spirit lends its protection to those nearby. You and your allies gain advantage on all ability checks made to detect creatures in the spirit’s aura. In addition, if you cast a spell using a spell slot that restores hit points to any creature inside or outside the aura, each creature of your choice in the aura also regains hit points equal to your druid level.")})]}
                           10 {:modifiers [(mod5e/dependent-trait
                                           {:name "Guardian Spirit"
                                            :summary "your Spirit Totem safeguards the beasts and fey that you call forth with your magic. When a beast or fey that you summoned or created with a spell ends its turn in your Spirit Totem aura, that creature regains a number of hit points equal to half your druid level"})]}
                           14 {:modifiers [(mod5e/dependent-trait
                                            {:name "Faithful Summons"
                                             :frequency units5e/long-rests-1
                                             :duration units5e/hours-1
                                             :summary "the nature spirits you commune with protect you when you are the most defenseless. If you are reduced to 0 hit points or are incapacitated against your will, you can immediately gain the benefits of Conjure Animals as if it were cast with a 9th-level spell slot. It summons four beasts of your choice that are challenge rating 2 or lower. The conjured beasts appear within 20 feet of you. If they receive no commands from you, they protect you from harm and attack your foes. The spell lasts for 1 hour, requiring no concentration, or until you dismiss it (no action required)"})]}}
                  :traits [{:name "Speech of the Woods"
                            :level 2
                            :summary "You learn to speak, read, and write Sylvan. In addition, beasts can understand your speech, and you gain the ability to decipher their noises and motions. Most beasts lack the intelligence to convey or understand sophisticated concepts, but a friendly beast could relay what it has seen or heard in the recent past. This ability doesn’t grant you any special friendship with beasts, though you can combine this ability with gifts to curry favor with them as you would with any nonplayer character"}
                           {:name "Mighty Summoner"
                            :level 6
                            :summary (str "beasts and fey that you conjure are more resilient than normal. Any beast or fey summoned or created by a spell that you cast gains two benefits:"
                                          "\n\u2022 The creature appears with more hit points than normal: 2 extra hit points per Hit Die it has."
                                          "\n\u2022 The damage from its natural weapons is considered magical for the purpose of overcoming immunity and resistance to nonmagical attacks and damage.")}]}
                 {:name "Circle of Spores"
                  :levels {2 {:modifiers [(druid-spell 0 :chill-touch 2)
                                          (druid-spell 2 :blindness-deafness 3)
                                          (druid-spell 2 :gentle-repose 3)
                                          (druid-spell 3 :animate-dead 5)
                                          (druid-spell 3 :gaseous-form 5)
                                          (druid-spell 4 :blight 7)
                                          (druid-spell 4 :confusion 7)
                                          (druid-spell 5 :cloudkill 9)
                                          (druid-spell 5 :contagion 9)
                                          (mod5e/reaction
                                           {:name "Halo of Spores"
                                            :summary (str "you are surrounded by invisible, necrotic spores that are harmless until you unleash them on a creature nearby. When a creature you can see moves into a space within 10 feet of you or starts its turn there, you can use your reaction to deal 1d"
                                                          (condp <= (?class-level :druid)
                                                          14 10
                                                          10 8
                                                          6 6
                                                          4)
                                                          " necrotic damage to that creature unless it succeeds on a Constitution saving throw against your spell save DC")})
                                          (mod5e/action
                                           {:name "Symbiotic Entity"
                                            :duration units5e/minutes-10
                                            :summary (str "you gain the ability to channel magic into your spores. As an action, you can expend a use of your Wild Shape feature to awaken those spores, rather than transforming into a beast form, and you gain 4 temporary hit points for each level you have in this class ("
                                                          (* 4 (?class-level :druid)) "). While this feature is active, you gain the following benefits:"
                                                      "\n\u2022 When you deal your Halo of Spores damage, roll the damage die a second time and add it to the total."
                                                      "\n\u2022 Your melee weapon attacks deal an extra 1d6 necrotic damage to any target they hit."
                                                      "\nThese benefits last for 10 minutes, until you lose all these temporary hit points. or until you use your Wild Shape again")})]}
                           6 {:modifiers [(mod5e/reaction
                                           {:name "Fungal Infestation"
                                            :frequency (units5e/long-rests ?prof-bonus)
                                            :duration units5e/hours-1
                                            :summary (str "your spores gain the ability to infest a corpse and animate it. If a beast or a humanoid that is Small or Medium dies within 10 feet of you, you can use your reaction to animate it, causing it to stand up immediately with 1 hit point. The creature uses the Zombie stat block in the Monster Manual. It remains animate for 1 hour, after which time it collapses and dies."
                                                          "\n\nIn combat, the zombie's turn comes immediately after yours. It obeys your mental commands, and the only action it can take is the Attack action, making one melee attack.")})]}
                           10 {:modifiers [(mod5e/bonus-action
                                            {:name "Spreading Spores"
                                             :duration units5e/minutes-1
                                             :summary (str "you gain the ability to seed an area with deadly spores. As a bonus action while your Symbiotic Entity feature is active, you can hurl spores up to 30 feet away, where they swirl in a 10-foot cube for 1 minute. The spores disappear early if you use this feature again, if you dismiss them as a bonus action, or if your Symbiotic Entity feature is no longer active."
                                                           "\n\nWhenever a creature moves into the cube or starts its turn there, that creature takes your Halo of Spores damage, unless the creature succeeds on a Constitution saving throw against your spell save DC. A creature can take this damage no more than once per turn."
                                                           "\n\nWhile the cube of spores persists, you can't use your Halo of Spores reaction")})]}}
                  :traits [{:name "Fungal Body"
                            :level 14
                            :summary "the fungal spores in your body alter you: you can't be blinded, deafened, frightened, or poisoned, and any critical hit against you counts as a normal hit instead, unless you're incapacitated"}]}
                 {:name "Circle of Stars"
                  :levels {2 {:modifiers [(druid-spell 0 :guidance 2)
                                          (druid-spell 1 :guiding-bolt 2)
                                          (mod5e/dependent-trait
                                           {:name "Star Map"
                                            :summary (str "you've created a star chart as part of your heavenly studies. It is a Tiny object and can serve as a spellcasting focus for your druid spells. While holding this map, you have these benefits:"
                                                          "\n\u2022 You know the Guidance cantrip."
                                                          "\n\u2022 You have the Guiding Bolt spell prepared. It counts as a druid spell for you, and it doesn't count against the number of spells you can have prepared."
                                                          "\n\u2022 You can cast Guiding Bolt without expending a spell slot. You can do so a number of times equal to your proficiency bonus, and you regain all expended uses when you finish a long rest."
                                                          "\nIf you lose the map, you can perform a 1-hour ceremony to magically create a replacement. This ceremony can be performed during a short or long rest, and it destroys the previous map.")})
                                          (mod5e/bonus-action
                                           {:name "Starry Form"
                                            :duration units5e/minutes-10
                                            :summary (str "you gain the ability to harness constellations' power to alter your form. As a bonus action, you can expend a use of your Wild Shape feature to take on a starry form, rather than transforming into a beast."
                                                          "\n\nWhile in your starry form, you retain your game statistics, but your body becomes luminous; your joints glimmer like stars, and glowing lines connect them as on a star chart. This form sheds bright light in a 10-foot radius and dim light for an additional 10 feet. The form lasts for 10 minutes. It ends early if you dismiss it (no action required), are incapacitated, die, or use this feature again."
                                                          "\n\nWhenever you assume your starry form, choose which of the following constellations glimmers on your body; your choice gives you certain benefits while in the form:"
                                                          "\nArcher. A constellation of an archer appears on you. When you activate this form, and as a bonus action on your subsequent turns while it lasts, you can make a ranged spell attack, hurling a luminous arrow that targets one creature within 60 feet of you. On a hit, the attack deals radiant damage equal to 1d8 + your Wisdom modifier."
                                                          "\nChalice. A constellation of a life-giving goblet appears on you. Whenever you cast a spell using a spell slot that restores hit points to a creature, you or another creature within 30 feet of you can regain hit points equal to 1d8 + your Wisdom modifier."
                                                          "\nDragon. A constellation of a wise dragon appears on you. When you make an Intelligence or a Wisdom check or a Constitution saving throw to maintain concentration on a spell, you can treat a roll of 9 or lower on the d20 as a 10.")})]}
                           6 {:modifiers [(mod5e/reaction
                                           {:name "Cosmic Omen"
                                            :frequency (units5e/long-rests ?prof-bonus)
                                            :summary (str "you learn to use your star map to divine the will of the cosmos. Whenever you finish a long rest, you can consult your Star Map for omens. When you do so, roll a die. Until you finish your next long rest, you gain access to a special reaction based on whether you rolled an even or an odd number on the die:"
                                                          "\nWeal (even). Whenever a creature you can see within 30 ft. is about to make an attack roll, a save, or an ability check, add a d6 to the total.Whenever a creature you can see within 30 feet of you is about to make an attack roll, a saving throw, or an ability check, you can use your reaction to roll a d6 and add the number rolled to the total."
                                                          "\nWoe (odd). Whenever a creature you can see within 30 feet of you is about to make an attack roll, a saving throw, or an ability check, you can use your reaction to roll a d6 and subtract the number rolled from the total."
                                                          "\nYou can use this reaction a number of times equal to your proficiency bonus, and you regain all expended uses when you finish a long rest")})]}
                           10 {:modifiers [(mod5e/dependent-trait
                                            {:name "Twinkling Constellations"
                                             :summary (str "the constellations of your Starry Form improve. The 1d8 of the Archer and the Chalice becomes 2d8, and while the Dragon is active, you have a flying speed of 20 feet and can hover."
                                                           "\n\nMoreover, at the start of each of your turns while in your Starry Form, you can change which constellation glimmers on your body")})]}}
                  :traits [{:name "Full of Stars"
                            :level 14
                            :summary "while in your Starry Form, you become partially incorporeal, giving you resistance to bludgeoning, piercing, and slashing damage."}]}
                 #_{:name "Circle of the Moon"
                    :levels {2 {:modifiers [(mod5e/bonus-action
                                             {:name "Combat Wild Shape"
                                              :page 69
                                              :summary "can Wild Shape as bonus action instead of action, while transformed expend a spell slot and gain 1d8 HP per slot level"})
                                            (mod/modifier
                                             ?wild-shape-cr
                                             (max 1 (int (/ (?class-level :druid) 3))))]}
                             10 {:modifiers [(mod5e/bonus-action
                                              {:name "Elemental Wild Shape"
                                               :level 10
                                               :page 69
                                               :summary "expend two Wild Shape uses to transform into an air, earth, fire, or water elemental"})]}}
                    :traits [{:name "Primal Strike"
                              :level 6
                              :page 69
                              :summary "Your beast form attacks count as magical"}
                             {:name "Thousand Forms"
                              :page 69
                              :summary "cast alter self at will"
                              :level 14}]}]}))

(defn subclass-wizard-spell-selection [spell-lists spells-map title ref class-key class-name num spell-levels & [filter-fn]]
  (opt5e/spell-selection spell-lists
                         spells-map
                         {:title title
                          :class-key class-key
                          :ref ref
                          :spellcasting-ability ::char5e/int
                          :class-name class-name
                          :num num
                          :prepend-level? true
                          :spell-keys (let [spell-keys
                                            (mapcat
                                             (fn [lvl] (get-in spell-lists [:wizard lvl]))
                                             spell-levels)]
                                        (if filter-fn
                                          (filter
                                           (fn [spell-key]
                                             (filter-fn (spells-map spell-key)))
                                           spell-keys)
                                          spell-keys))}))

(defn eldritch-knight-spell? [s]
    (let [school (:school s)]
      (or (= school "evocation")
          (= school "abjuration"))))

(defn eldritch-knight-ref [subclass-key subpath]
    (concat
     [:class :fighter :levels :level-3 :martial-archetype subclass-key]
     subpath))

(defn eldritch-knight-cantrip [num]
  (opt5e/spell-selection sl5e/spell-lists
                         spells5e/spell-map
                         {:class-key :fighter
                          :level 0
                          :ref (eldritch-knight-ref :eldritch-knight [:cantrips-known])
                          :spellcasting-ability ::char5e/int
                          :class-name "Fighter"
                          :num num
                          :spell-keys (get-in sl5e/spell-lists [:wizard 0])}))

(defn eldritch-knight-spell-selection [num spell-levels]
  (subclass-wizard-spell-selection sl5e/spell-lists
                                   spells5e/spell-map
                                   "Fighter Abjuration or Evocation Spells"
                                   (eldritch-knight-ref :eldritch-knight [:abjuration-or-evocation-spells-known])
                                   :fighter
                                   "Fighter"
                                   num
                                   spell-levels
                                   eldritch-knight-spell?))

(defn eldritch-knight-any-spell-selection [num spell-levels]
  (subclass-wizard-spell-selection sl5e/spell-lists
                                   spells5e/spell-map
                                   "Fighter Spells: Any School"
                                     (eldritch-knight-ref :eldritch-knight [:spells-known-any-school])
                                     :fighter
                                     "Fighter"
                                     num
                                     spell-levels))

(def eldritch-knight-cfg
    {:name "Eldritch Knight"
     :spellcasting {:level-factor 3}
     :modifiers [(mod5e/trait-cfg
                   {:name "Weapon Bond"
                    :page 75
                    :summary (str "you learn a ritual that creates a magical bond between yourself and one weapon. You perform the ritual over the course of 1 hour, which can be done during a short rest. The weapon must be within your reach throughout the ritual, at the conclusion of which you touch the weapon and forge the bond."
                                  "\n\nOnce you have bonded a weapon to yourself, you can't be disarmed of that weapon unless you are incapacitated. If it is on the same plane of existence, you can summon that weapon as a bonus action on your turn, causing it to teleport instantly to your hand."
                                  "\n\nYou can have up to two bonded weapons, but can summon only one at a time with your bonus action. If you attempt to bond with a third weapon, you must break the bond with one of the other two.")})
                 (mod5e/bonus-action
                  {:name "Summon Bonded Weapon"
                   :page 75
                   :summary "If on the same plane of existence, instantly teleport a bonded weapon into your hand"})]
     :levels {3 {:selections [(eldritch-knight-cantrip 2)
                              (eldritch-knight-spell-selection 2 [1])
                              (eldritch-knight-any-spell-selection 1 [1])]}
              4 {:selections [(eldritch-knight-spell-selection 1 [1])]}
              7 {:selections [(eldritch-knight-spell-selection 1 [1 2])]
                 :modifiers [(mod5e/bonus-action
                              {:name "War Magic"
                               :page 75
                               :summary "when you use your action to cast a cantrip, you can make one weapon attack as a bonus action"})]}
              8 {:selections [(eldritch-knight-any-spell-selection 1 [1 2])]}
              10 {:selections [(eldritch-knight-cantrip 1)
                               (eldritch-knight-spell-selection 1 [1 2])]}
              11 {:selections [(eldritch-knight-spell-selection 1 [1 2])]}
              13 {:selections [(eldritch-knight-spell-selection 1 [1 2 3])]}
              14 {:selections [(eldritch-knight-any-spell-selection 1 [1 2 3])]}
              16 {:selections [(eldritch-knight-spell-selection 1 [1 2 3])]}
              18 {:modifiers [(mod5e/bonus-action
                               {:name "Improved War Magic"
                                :page 75
                                :summary "when you use your action to cast a spell, you can make one weapon attack as a bonus action"})]}
              19 {:selections [(eldritch-knight-spell-selection 1 [1 2 3 4])]}
              20 {:selections [(eldritch-knight-any-spell-selection 1 [1 2 3 4])]}}
     :traits [{:name "Eldritch Strike"
               :page 75
               :level 10
               :summary "you learn how to make your weapon strikes undercut a creature's resistance to your spells. When you hit a creature with a weapon attack, that creature has disadvantage on the next saving throw it makes against a spell you cast before the end of your next turn"}
              {:name "Arcane Charge"
               :level 15
               :page 75
               :summary "you gain the ability to teleport up to 30 feet to an unoccupied space you can see when you use your Action Surge. You can teleport before or after the additional action"}]})

(defn martial-maneuvers-selection [num]
    (t/selection-cfg
     {:name "Martial Maneuvers"
      :options opt5e/maneuver-options
      :ref [:class :fighter :levels :level-3 :martial-archetype :battle-master :martial-maneuvers]
      :tags #{:class}
      :min num
      :max num}))


(defn fighter-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Fighter",
    :key :fighter
    :hit-die 10,
    :ability-increase-levels [4 6 8 12 14 16 19]
    :profs {:armor {:light false :medium false :heavy true :shields false}
            :weapon {:simple false :martial false} 
            :save {::char5e/str true ::char5e/con true}
            :skill-options {:choose 2 :options {:acrobatics true :animal-handling true :athletics true :history true :insight true :intimidation true :perception true :survival true}}}
    :multiclass-prereqs [(t/option-prereq "Requires Strength 13 or Dexterity 13"
                                          (fn [c]
                                            (let [abilities @(subscribe [::char5e/abilities nil c])]
                                              (or (>= (::char5e/str abilities) 13)
                                                  (>= (::char5e/dex abilities) 13)))))]
    :equipment-choices [{:name "Equipment Pack"
                         :options {:dungeoneers-pack 1
                                   :explorers-pack 1}}]
    :modifiers [(mod5e/bonus-action
                 {:name "Second Wind"
                  :page 72
                  :frequency units5e/rests-1
                  :summary (str "You have a limited well of stamina that you can draw on to protect yourself from harm. On your turn, you can use a bonus action to regain hit points equal to 1d10 + your fighter level ("
                                (common/mod-str (?class-level :fighter))
                                ")")})]
    :levels {2 {:modifiers [(mod5e/action
                             {:level 2
                              :name "Action Surge"
                              :page 72
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 17)
                                                          2
                                                          1))
                              :summary "you can push yourself beyond your normal limits for a moment. On your turn, you can take one additional action"})]}
             3 {:modifiers [(mod5e/critical 19)]}
             4 {:modifiers [(mod5e/trait-cfg
                             {:name "Martial Versatility"
                              :summary "When you reach level 4, 8, 12, 16, and 19 in fighter, you can replace one fighting style and one maneuver from the class."})]}
             5 {:modifiers [(mod5e/num-attacks 2)]}
             9 {:modifiers [(mod5e/dependent-trait
                             {:level 9
                              :name "Indomitable"
                              :page 72
                              :frequency (units5e/long-rests
                                          (mod5e/level-val
                                           (?class-level :fighter)
                                           {13 2
                                            17 3
                                            :default 1}))
                              :summary "you can reroll a saving throw that you fail. If you do so, you must use the new roll"})]}
             11 {:modifiers [(mod5e/num-attacks 3)]}
             13 {:modifiers [(mod5e/critical 18)]}
             20 {:modifiers [(mod5e/num-attacks 4)]}}
    :subclass-level 3
    :subclass-title "Martial Archetype"
    :selections [(opt5e/fighting-style-selection :fighter #{:archery :blind-fighting :defense :dueling :great-weapon-fighting :interception :protection :superior-technique :thrown-weapon-fighting :two-weapon-fighting :unarmed-fighting})
                 (opt5e/new-starting-equipment-selection
                  :fighter
                  {:name "Armor"
                   :options [(t/option-cfg
                              {:name "Chain Mail"
                               :modifiers [(mod5e/armor :chain-mail 1)]})
                             (t/option-cfg
                              {:name "Leather Armor, Longbow, 20 Arrows"
                               :modifiers [(mod5e/armor :leather 1)
                                           (mod5e/weapon :longbow 1)
                                           (mod5e/equipment :arrow 20)]})]})
                 (opt5e/new-starting-equipment-selection
                  :fighter
                  {:name "Weapons"
                   :options [(t/option-cfg
                              {:name "Martial Weapon and Shield"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :fighter
                                             {:name "Martial Weapon"
                                              :options (opt5e/martial-weapon-options 1 (vals weapon-map))})]
                               :modifiers [(mod5e/armor :shield 1)]})
                             (t/option-cfg
                              {:name "Two Martial Weapons"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :fighter
                                             {:name "Martial Weapon 1"
                                              :options (opt5e/martial-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})
                                            (opt5e/new-starting-equipment-selection
                                             :fighter
                                             {:name "Martial Weapon 2"
                                              :options (opt5e/martial-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})]})]})
                 (opt5e/new-starting-equipment-selection
                  :fighter
                  {:name "Additional Weapons"
                   :options [(t/option-cfg
                              {:name "Light Crossbow and 20 Bolts"
                               :modifiers [(mod5e/weapon :crossbow-light 1)
                                           (mod5e/equipment :crossbow-bolt 20)]})
                             (t/option-cfg
                              {:name "Two Handaxes"
                               :modifiers [(mod5e/weapon :handaxe 2)]})]})]
    :subclasses [#_{:name "Champion"
                  :levels {3 {:modifiers [(mod5e/critical 19)]}
                           7 {:modifiers [(mod/vec-mod ?default-skill-bonus-fns
                                                       (fn [ability-kw]
                                                         (if (#{::char5e/str
                                                                ::char5e/dex
                                                                ::char5e/con}
                                                              ability-kw)
                                                           (common/round-up (/ ?prof-bonus 2))
                                                           0)))
                                          (mod/cum-sum-mod ?initiative (common/round-up (/ ?prof-bonus 2)))
                                          (mod5e/dependent-trait
                                           {:level 7
                                            :name "Remarkable Athlete"
                                            :page 72
                                            :summary (str "+"
                                                          (common/round-up (/ ?prof-bonus 2))
                                                          " to STR, DEX, or CON checks that don't already include prof bonus; running long jump increases by "
                                                          (?ability-bonuses ::char5e/str)
                                                          " ft.")})]}
                           10 {:selections [(opt5e/fighting-style-selection :fighter)]}
                           15 {:modifiers [(mod5e/critical 18)]}
                           18 {:modifiers [(mod5e/dependent-trait
                                            {:page 73
                                             :name "Survivor"
                                             :summary (str "At start of your turns, if you have at most half of your "
                                                           #_(int (/ ?max-hit-points 2))
                                                           " HPs left, regain "
                                                           (+ 5 (?ability-bonuses ::char5e/con)) " HPs")})]}}}
                 {:name "Battle Master"
                    :selections [(martial-maneuvers-selection 3)
                                 (opt5e/tool-selection (map :key equipment5e/artisans-tools) 1)]
                    :modifiers [(mod/modifier ?maneuver-save-dc (max (?spell-save-dc ::char5e/dex)
                                                                     (?spell-save-dc ::char5e/str)))
                                (mod5e/dependent-trait
                                 {:name "Combat Superiority"
                                  :page 73
                                  :level 3
                                  :summary (let [[num-maneuvers num-dice die]
                                                 (mod5e/level-val
                                                  (?class-level :fighter)
                                                  {7 [5 5 8]
                                                   10 [7 5 10]
                                                   15 [9 6 10]
                                                   18 [9 6 12]
                                                   :default [3 4 8]})]
                                             (str "Maneuvers. You learn "
                                                  num-maneuvers
                                                  " maneuvers of your choice. Many maneuvers enhance an attack in some way. You can use only one maneuver per attack."
                                                  "\nSuperiority Dice. You have "
                                                  num-dice
                                                  " superiority dice, which are d"
                                                  die
                                                  "s. A superiority die is expended when you use it. You regain all of your expended superiority dice when you finish a short or long rest."
                                                  "\nSaving Throws. Some of your maneuvers require your target to make a saving throw to resist the maneuver's effects. The saving throw DC is calculated as follows:"
                                                  "\nManeuver save DC = 8 + your proficiency bonus + your Strength or Dexterity modifier (your choice) = "
                                                  ?maneuver-save-dc))})]
                    :levels {7 {:selections [(martial-maneuvers-selection 2)]}
                             10 {:selections [(martial-maneuvers-selection 2)]}
                             15 {:selections [(martial-maneuvers-selection 2)]}}
                    :traits [{:name "Know Your Enemy"
                              :level 7
                              :page 73
                              :class-key :fighter
                              :summary (str "if you spend at least 1 minute observing or interacting with another creature outside combat, you can learn certain information about its capabilities compared to your own. The DM tells you if the creature is your equal, superior, or inferior in regard to two of the following characteristics of your choice:"
                                            "\n\u2022 Strength score"
                                            "\n\u2022 Dexterity score"
                                            "\n\u2022 Constitution score"
                                            "\n\u2022 Armor Class"
                                            "\n\u2022 Current hit points"
                                            "\n\u2022 Total class levels, if any"
                                            "\n\u2022 Fighter class levels, if any")}
                             {:name "Relentless"
                              :level 15
                              :page 74
                              :class-key :fighter
                              :summary "when you roll initiative and have no superiority dice remaining, you regain 1 superiority die"}]}
                 {:name "Cavalier"
                  :selections [(t/selection-cfg
                                {:name "Proficiency"
                                 :tags #{:profs}
                                 :order 0
                                 :options [(t/option-cfg
                                            {:name "Skill Proficiency"
                                             :selections [(opt5e/skill-selection [:animal-handling :history :insight :performance :persuasion] 1)]})
                                           (t/option-cfg
                                            {:name "Language"
                                             :selections [(opt5e/language-selection-aux (vals language-map) 1)]})]})]
                  :modifiers [(mod5e/dependent-trait
                               {:name "Unwavering Mark"
                                :class-key :fighter
                                :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/str)))
                                :summary (str "you can menace your foes, foiling their attacks and punishing them for harming others. When you hit a creature with a melee weapon attack, you can mark the creature until the end of your next turn. This effect ends early if you are incapacitated or you die, or if someone else marks the creature."
                                              "\n\nWhile it is within 5 feet of you, a creature marked by you has disadvantage on any attack roll that doesn't target you."
                                              "\n\nIn addition, if a creature marked by you deals damage to anyone other than you, you can make a special melee weapon attack against the marked creature as a bonus action on your next turn. You have advantage on the attack roll, and if it hits, the attack's weapon deals extra damage to the target equal to half your fighter level (" (int (/ (?class-level :fighter) 2)) ")."
                                              "\n\nRegardless of the number of creatures you mark, you can make this special attack a number of times equal to your Strength modifier (a minimum of once), and you regain all expended uses of it when you finish a long rest")})]
                  :traits [{:name "Born to the Saddle"
                            :class-key :fighter
                            :summary (str "Advantage on saving throws made to avoid falling off your mount. If you fall off and fall 10 ft. or less, you can land on your feet if not incapacitated."
                                          "\n\nFinally, mounting or dismounting a creature costs you only 5 feet of movement, rather than half your speed")}]
                  :levels {7 {:modifiers [(mod5e/reaction
                                           {:name "Warding Maneuver"
                                            :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/con)))
                                            :summary "you learn to fend off strikes directed at you, your mount, or other creatures nearby. If you or a creature you can see within 5 feet of you is hit by an attack, you can roll 1d8 as a reaction if you're wielding a melee weapon or a shield. Roll the die, and add the number rolled to the target's AC against that attack. If the attack still hits, the target has resistance against the attack's damage"})]}
                           10 {:modifiers [(mod5e/trait-cfg
                                            {:name "Hold the Line"
                                            :summary "you become a master of locking down your enemies. Creatures provoke an opportunity attack from you when they move 5 feet or more while within your reach, and if you hit a creature with an opportunity attack, the target's speed is reduced to 0 until the end of the current turn"})]}
                           15 {:modifiers [(mod5e/dependent-trait
                                            {:name "Ferocious Charger"
                                             :frequency units5e/rounds-1
                                             :summary (str "you can run down your foes, whether you're mounted or not. If you move at least 10 feet in a straight line right before attacking a creature and you hit it with the attack, that target must succeed on a Strength saving throw (DC 8 + your proficiency bonus + your Strength modifier = " (?spell-save-dc ::char5e/con) ") or be knocked prone. You can use this feature only once on each of your turns")})]}
                           18 {:modifiers [(mod5e/reaction
                                            {:name "Vigilant Defender"
                                             :summary "you respond to danger with extraordinary vigilance. In combat, you get a special reaction that you can take once on every creature's turn, except your turn. You can use this special reaction only to make an opportunity attack, and you can't use it on the same turn that you take your normal reaction"})]}}}
                 {:name "Echo Knight"
                  :levels {3 {:modifiers [(mod5e/bonus-action
                                           {:name "Manifest Echo"
                                            :summary (str "you can use a bonus action to magically manifest an echo of yourself in an unoccupied space you can see within 15 feet of you. This echo is a magical, translucent, gray image of you that lasts until it is destroyed, until you dismiss it as a bonus action, until you manifest another echo, or until you're incapacitated."
                                                          "\n\nYour echo has AC 14 + your proficiency bonus (" (+ 14 ?prof-bonus) "), 1 hit point, and immunity to all conditions. If it has to make a saving throw, it uses your saving throw bonus for the roll. It is the same size as you, and it occupies its space. On your turn, you can mentally command the echo to move up to 30 feet in any direction (no action required). If your echo is ever more than 30 feet from you at the end of your turn, it is destroyed."
                                                          "\n\u2022 As a bonus action, you can teleport, magically swapping places with your echo at a cost of 15 feet of your movement, regardless of the distance between the two of you."
                                                          "\n\u2022 When you take the Attack action on your turn, any attack you make with that action can originate from your space or the echo's space. You make this choice for each attack."
                                                          "\n\u2022 When a creature that you can see within 5 feet of your echo moves at least 5 feet away from it, you can use your reaction to make an opportunity attack against that creature as if you were in the echo's space.")})
                                          (mod5e/dependent-trait
                                           {:name "Unleash Incarnation"
                                            :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/con)))
                                            :summary "you can heighten your echo's fury. Whenever you take the Attack action, you can make one additional melee attack from the echo's position"})]}
                           7 {:modifiers [(mod5e/action
                                           {:name "Echo Avatar"
                                            :summary "you can temporarily transfer your consciousness to your echo. As an action, you can see through your echo's eyes and hear through its ears. During this time, you are deafened and blinded. You can sustain this effect for up to 10 minutes, and you can end it at any time (requires no action). While your echo is being used in this way, it can be up to 1,000 feet away from you without being destroyed"})]}
                           10 {:modifiers [(mod5e/reaction
                                            {:name "Shadow Martyr"
                                             :frequency units5e/rests-1
                                             :summary "you can make your echo throw itself in front of an attack directed at another creature that you can see. Before the attack roll is made, you can use your reaction to teleport the echo to an unoccupied space within 5 feet of the targeted creature. The attack roll that triggered the reaction is instead made against your echo"})]}
                           15 {:modifiers [(mod5e/dependent-trait
                                            {:name "Reclaim Potential"
                                             :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/con)))
                                             :summary "you've learned to absorb the fleeting magic of your echo. When an echo of yours is destroyed by taking damage, you can gain a number of temporary hit points equal to 2d6 + your Constitution modifier, provided you don't already have temporary hit points"})]}
                           18 {:modifiers [(mod5e/bonus-action
                                            {:name "Legion of One"
                                             :summary (str "you can use a bonus action to create two echos with your Manifest Echo feature, and these echoes can co-exist. If you try to create a third echo, the previous two echoes are destroyed. Anything you can do from one echo's position can be done from the other's instead"
                                                           "\n\nIn addition, when you roll initiative and have no uses of your Unleash Incarnation feature left, you regain one use of that feature")})]}}}
                 eldritch-knight-cfg
                 {:name "Rune Knight"
                  :modifiers [(mod5e/bonus-action
                               {:name "Giant Might"
                                :duration units5e/minutes-1
                                :frequency (units5e/long-rests ?prof-bonus)
                                :summary (str "you have learned how to imbue yourself with the might of giants. As a bonus action, you magically gain the following benefits, which last for 1 minute:"
                                              "\n\u2022 If you are smaller than Large, you become Large, along with anything you are wearing. If you lack the room to become Large, your size doesn't change."
                                              "\n\u2022 You have advantage on Strength checks and Strength saving throws."
                                              "\n\u2022 Once on each of your turns, one of your attacks with a weapon or an unarmed strike can deal an extra 1d6 damage to a target on a hit."
                                              "\n")})
                              (mod5e/trait-cfg
                               {:name "Rune Carver"
                                :summary "Whenever you finish a long rest, you can touch a number of objects equal to the number of runes you know, and you inscribe a different rune onto each of the objects. To be eligible, an object must be a weapon, a suit of armor, a shield, a piece of jewelry, or something else you can wear or hold in a hand. Your rune remains on an object until you finish a long rest, and an object can bear only one of your runes at a time."})]
                  :selections [(opt5e/rune-selection 2 1 (opt5e/total-levels-option-prereq 7 :fighter))]
                  :levels {7 {:selections [(opt5e/rune-selection 1 2 (opt5e/total-levels-option-prereq 7 :fighter))]
                              :modifiers [(mod5e/reaction
                                           {:name "Runic Shield"
                                            :frequency (units5e/long-rests ?prof-bonus)
                                            :summary "you learn to invoke your rune magic to protect your allies. When another creature you can see within 60 feet of you is hit by an attack roll, you can use your reaction to force the attacker to reroll the d20 and use the new roll"})]}
                           10 {:selections [(opt5e/rune-selection 1 3 (opt5e/total-levels-option-prereq 7 :fighter))]
                               :modifiers [(mod5e/trait-cfg
                                            {:name "Great Stature"
                                             :summary "the magic of your runes permanently alters you. When you gain this feature, roll 3d4. You grow a number of inches in height equal to the roll"})]}
                           15 {:selections [(opt5e/rune-selection 1 4 (opt5e/total-levels-option-prereq 7 :fighter))]
                               :modifiers [(mod5e/trait-cfg
                                            {:name "Master of Runes"
                                             :summary "you can invoke each rune you know from your Rune Carver feature twice, rather than once, and you regain all expended uses when you finish a short or long rest"})]}
                           18 {:modifiers [(mod5e/trait-cfg
                                            {:name "Runic Juggernaut"
                                             :summary "you learn how to amplify your rune-powered transformation. As a result, the extra damage you deal with the Giant's Might feature increases to 1d10. Moreover, when you use that feature, your size can increase to Huge, and while you are that size, your reach increases by 5 feet"})]}}}
                 {:name "Samurai"
                  :selections [(t/selection-cfg
                                {:name "Proficiency"
                                 :tags #{:profs}
                                 :order 0
                                 :options [(t/option-cfg
                                            {:name "Skill Proficiency"
                                             :selections [(opt5e/skill-selection [:history :insight :performance :persuasion] 1)]})
                                           (t/option-cfg
                                            {:name "Language"
                                             :selections [(opt5e/language-selection-aux (vals language-map) 1)]})]})]
                  :modifiers [(mod5e/bonus-action
                               {:name "Fighting Spirit"
                                :frequency (units5e/long-rests 3)
                                :summary "your intensity in battle can shield you and help you strike true. As a bonus action on your turn, you can give yourself advantage on all weapon attack rolls until the end of the current turn. When you do so, you also gain 5 temporary hit points. The number of hit points increases when you reach certain levels in this class, increasing to 10 at 10th level and 15 at 15th level"})]
                  :levels {7 {:modifiers [(mod5e/saving-throws nil ::char5e/wis)
                                          (mod5e/dependent-trait
                                           {:name "Elegant Courtier"
                                            :summary (str "your discipline and attention to detail allow you to excel in social situations. Whenever you make a Charisma (Persuasion) check, you gain a bonus to the check equal to your Wisdom modifier."
                                                          "\n\nYour self-control also causes you to gain proficiency in Wisdom saving throws. If you already have this proficiency, you instead gain proficiency in Intelligence or Charisma saving throws (your choice).")})]}
                           10 {:modifiers [(mod5e/trait-cfg
                                            {:name "Tireless Spirit"
                                             :summary "when you roll initiative and have no uses of Fighting Spirit remaining, you regain one use"})]}
                           15 {:modifiers [(mod5e/trait-cfg
                                            {:name "Rapid Strike"
                                             :frequency units5e/turns-1
                                             :summary "you learn to trade accuracy for swift strikes. If you take the Attack action on your turn and have advantage on an attack roll against one of the targets, you can forgo the advantage for that roll to make an additional weapon attack against that target, as part of the same action. You can do so no more than once per turn"})]}
                          18 {:modifiers [(mod5e/reaction
                                           {:name "Strength Before Death"
                                            :frequency units5e/long-rests-1
                                            :summary "your fighting spirit can delay the grasp of death. If you take damage that reduces you to 0 hit points, you can use your reaction to delay falling unconscious, and you can immediately take an extra turn. While you have 0 hit points during that extra turn, taking damage causes death saving throw failures as normal, and three death saving throw failures can still kill you. When the extra turn ends, you fall unconscious if you still have 0 hit points"})]}}}]}))

(defn monk-weapon? [{:keys [key ::weapon5e/type ::weapon5e/melee? ::weapon5e/heavy? ::weapon5e/two-handed?]}]
  (or (= key :shortsword)
      (and (= type :simple)
           melee?
           (not heavy?)
           (not two-handed?))))

(defn weapon-option [dedicated-weapon & [num]]
  (t/option-cfg
   {:name (:name dedicated-weapon)
    :key (:key dedicated-weapon)
    :help (:description dedicated-weapon)
    :modifiers [(mod/vec-mod ?weapon-ability-modifiers
                              (fn [weapon finesse?]
                                (if (= :name dedicated-weapon :name weapon)
                                  (get ?ability-bonuses ::char5e/dex)
                                  0)))]}))

(defn weapon-options [weapons & [num]]
  (map
   #(weapon-option % num)
   weapons))

(defn dedicated-weapon-options [num weapons]
  (weapon-options
   (filter #(and (= :martial (::weapon5e/type %)) (nil? (::weapon5e/heavy? %)) (not= :shortsword (:key %)) (nil? (::weapon5e/special? %))) weapons)
   num))

(defn monk-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   (merge
    opt5e/monk-base-cfg
    {:hit-die 8
     :name "Monk"
     :key :monk
     :ability-increase-levels [4 8 12 16 19]
     :unarmored-abilities [::char5e/wis]
     :profs {:weapon {:simple false :shortsword false}
             :save {::char5e/dex true ::char5e/str true}
             :tool-options {:musical-instrument 1 :artisans-tool 1}
             :skill-options {:choose 2 :options {:acrobatics true :athletics true :history true :insight true :religion true :stealth true}}}
     :multiclass-prereqs [(t/option-prereq "Requires Wisdom 13 and Dexterity 13"
                                           (fn [c]
                                             (let [abilities @(subscribe [::char5e/abilities nil c])]
                                               (and (>= (::char5e/wis abilities) 13)
                                                    (>= (::char5e/dex abilities) 13)))))]
     :equipment-choices [{:name "Equipment Pack"
                          :options {:dungeoneers-pack 1
                                    :explorers-pack 1}}]
     :weapon-choices [{:name "Weapon"
                       :options {:shortsword 1
                                 :simple 1}}]
     :modifiers [(mod/vec-mod ?weapon-ability-modifiers
                              (fn [weapon finesse?]
                                (if (monk-weapon? weapon)
                                  (get ?ability-bonuses ::char5e/dex)
                                  0)))
                 (mod/vec-mod ?unarmored-defense :monk)
                 (mod/cum-sum-mod ?unarmored-ac-bonus
                                  (?ability-bonuses ::char5e/wis)
                                  nil
                                  nil
                                  [(= :monk (first ?unarmored-defense))])
                 (mod/modifier ?martial-arts-die (mod5e/level-val
                                                  (?class-level :monk)
                                                  {5 6
                                                   11 8
                                                   17 10
                                                   :default 4}))
                 (mod5e/attack
                  {:name "Martial Arts"
                   :damage-die ?martial-arts-die
                   :damage-die-count 1
                   :damage-modifier (max (?ability-bonuses ::char5e/str) (?ability-bonuses ::char5e/dex))
                   :summary "Unarmed strike or monk weapon"})
                 (mod5e/bonus-action
                  {:name "Martial Arts"
                   :page 78
                   :summary "When you use the Attack action with an unarmed strike or a monk weapon on your turn, you can make one unarmed strike as a bonus action. For example, if you take the Attack action and attack with a quarterstaff, you can also make an unarmed strike as a bonus action, assuming you haven't already taken a bonus action this turn"})]
     :levels {2 {:modifiers [(mod5e/unarmored-speed-bonus 10)
                             (mod5e/dependent-trait
                              {:name "Ki"
                               :page 78
                               :level 2
                               :summary (str "your training allows you to harness the mystic energy of ki. Your access to this energy is represented by a number of ki points. Your monk level determines the number of points you have, as shown in the Ki Points column of the Monk table."
                                             "\n\nWhen you spend a ki point, it is unavailable until you finish a short or long rest, at the end of which you draw all of your expended ki back into yourself. You must spend at least 30 minutes of the rest meditating to regain your ki points")})
                             (mod5e/bonus-action
                              {:name "Flurry of Blows"
                               :page 78
                               :level 2
                               :summary "Immediately after you take the Attack action on your turn, you can spend 1 ki point to make two unarmed strikes as a bonus action"})
                             (mod5e/bonus-action
                              {:name "Patient Defense"
                               :page 78
                               :summary "You can spend 1 ki point to take the Dodge action as a bonus action on your turn"})
                             (mod5e/bonus-action
                              {:name "Step of the Wind"
                               :page 78
                               :summary "You can spend 1 ki point to take the Disengage or Dash action as a bonus action on your turn, and your jump distance is doubled for the turn"})
                             (mod5e/trait-cfg
                              {:name "Dedicated Weapon"
                               :summary (str "you train yourself to use a variety of weapons as monk weapons, not just simple melee weapons and shortswords. Whenever you finish a short or long rest, you can touch one weapon, focus your ki on it, and then count that weapon as a monk weapon until you use this feature again. The chosen weapon must meet these criteria:"
                                             "\n\u2022 The weapon must be a simple or martial weapon."
                                             "\n\u2022 You must be proficient with it."
                                             "\n\u2022 It must lack the heavy and special properties.")})
                             (mod5e/dependent-trait
                               {:name "Unarmored Movement"
                                :summary (str "your speed increases by 10 feet while you are not wearing armor or wielding a shield. This bonus increases when you reach certain monk levels, as shown in the Monk table"
                                              (if (>= (?class-level :monk) 5)
                                              "\n\nAt 9th level, you gain the ability to move along vertical surfaces and across liquids on your turn without falling during the move"))})]
                 :selections [(t/selection-cfg
                              {:name "Dedicated Weapon"
                                :tags #{:equipment}
                                :options (dedicated-weapon-options 1 (vals weapon-map))})]}
              3 {:modifiers [(mod5e/reaction
                              {:name "Deflect Missiles"
                               :page 78
                               :summary (str "you can use your reaction to deflect or catch the missile when you are hit by a ranged weapon attack. When you do so, the damage you take from the attack is reduced by 1d10 + your Dexterity modifier + your monk level (" (+ (?ability-bonuses ::char5e/dex) (?class-level :monk)) "). If you reduce the damage to 0, you can catch the missile if it is small enough for you to hold in one hand and you have at least one hand free. If you catch a missile in this way, you can spend 1 ki point to make a ranged attack with a range of 20/60 using the weapon or piece of ammunition you just caught, as part of the same reaction. You make this attack with proficiency, regardless of your weapon proficiencies, and the missile counts as a monk weapon for the attack")})
                             (mod5e/bonus-action
                              {:name "Ki-Fueled Attack"
                               :summary "if you spend 1 ki point or more as part of your action on your turn, you can make one attack with an unarmed strike or a monk weapon as a bonus action before the end of the turn"})]}
              4 {:modifiers [(mod5e/reaction
                              {:name "Slow Fall"
                               :page 78
                               :level 4
                               :summary (str "you can use your reaction when you fall to reduce any falling damage you take by an amount equal to five times your monk level (" (* 5  (?class-level :monk)) ")")})
                             (mod5e/action
                              {:name "Quickened Healing"
                               :summary (str "as an action, you can spend 2 ki points and roll a Martial Arts die. You regain a number of hit points equal to the number rolled plus your proficiency bonus (d" ?martial-arts-die "+" ?prof-bonus ")")})]}
              5 {:modifiers [(mod5e/num-attacks 2)
                             (mod5e/dependent-trait
                              {:name "Stunning Strike"
                               :page 79
                               :level 5
                               :summary "you can interfere with the flow of ki in an opponent's body. When you hit another creature with a melee weapon attack, you can spend 1 ki point to attempt a stunning strike. The target must succeed on a Constitution saving throw or be stunned until the end of your next turn"})
                             (mod5e/action
                              {:name "Focused Aim"
                               :summary "when you miss with an attack roll, you can spend 1 to 3 ki points to increase your attack roll by 2 for each of these ki points you spend, potentially turning the miss into a hit"})]}
              6 {:modifiers [(mod5e/unarmored-speed-bonus 5)]}
              7 {:modifiers [(mod5e/action
                              {:name "Stillness of Mind"
                               :page 79
                               :summary "you can use your action to end one effect on yourself that is causing you to be charmed or frightened"})]}
              10 {:modifiers [(mod5e/damage-immunity :poison)
                              (mod5e/immunity :disease)
                              (mod5e/unarmored-speed-bonus 5)]}
              13 {:modifiers (map
                              (fn [{:keys [name key]}]
                                (mod5e/language key))
                              (vals language-map))}
              14 {:modifiers [(apply mod5e/saving-throws nil char5e/ability-keys)
                              (mod5e/unarmored-speed-bonus 5)]}
              18 {:modifiers [(mod5e/unarmored-speed-bonus 5)
                              (mod5e/action
                               {:name "Empty Body: Invisibility"
                                :level 18
                                :page 79
                                :duration units5e/minutes-1
                                :summary "you can use your action to spend 4 ki points to become invisible for 1 minute. During that time, you also have resistance to all damage but force damage"})
                              (mod5e/action
                               {:name "Empty Body: Astral Projection"
                                :page 79
                                :level 18
                                :summary "you can spend 8 ki points to cast the astral projection spell, without needing material components. When you do so, you can't take any other creatures with you"})]}}
     :weapons {:dart 10}
     :traits [{:name "Ki-Empowered Strikes"
               :page 79
               :level 6
               :summary "your unarmed strikes count as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage"}
              (opt5e/evasion 7 79)
              {:name "Tongue of the Sun and Moon"
               :page 79
               :level 13
               :summary "you learn to touch the ki of other minds so that you understand all spoken languages. Moreover, any creature that can understand a language can understand what you say"}
              {:name "Diamond Soul"
               :level 14
               :page 79
               :summary "your mastery of ki grants you proficiency in all saving throws. Additionally, whenever you make a saving throw and fail, you can spend 1 ki point to reroll it and take the second result"}
              {:name "Timeless Body"
               :page 79
               :level 15
               :summary "your ki sustains you so that you suffer none of the frailty of old age, and you can't be aged magically. You can still die of old age, however. In addition, you no longer need food or water"}
              {:name "Perfect Self"
               :page 79
               :level 20
               :summary "when you roll for initiative and have no ki points remaining, you regain 4 ki points"}]
     :subclasses [{:name "Way of the Astral Self"
                   :modifiers [(mod5e/bonus-action
                                {:name "Arms of the Astral Self"
                                 :summary (str "your mastery of your ki allows you to summon a portion of your astral self. As a bonus action, you can spend 1 ki point to summon the arms of your astral self. When you do so, each creature of your choice that you can see within 10 feet of you must succeed on a Dexterity saving throw or take force damage equal to two rolls of your Martial Arts die."
                                              "For 10 minutes, these spectral arms hover near your shoulders or surround your arms (your choice). You determine the arms' appearance, and they vanish early if you are incapacitated or die. While the spectral arms are present, you gain the following benefits:"
                                              "\n\u2022 You can use your Wisdom modifier in place of your Strength modifier when making Strength checks and Strength saving throws."
                                              "\n\u2022 You can use the spectral arms to make unarmed strikes."
                                              "\n\u2022 When you make an unarmed strike with the arms on your turn, your reach for it is 5 feet greater than normal."
                                              "\n\u2022 The unarmed strikes you make with the arms can use your Wisdom modifier in place of your Strength or Dexterity modifier for the attack and damage rolls, and their damage type is force.")})]
                   :levels {6 {:modifiers [(mod5e/bonus-action
                                            {:name "Visage of the Astral Self"
                                             :summary (str "you can summon the visage of your astral self. As a bonus action, or as part of the bonus action you take to activate Arms of the Astral Self, you can spend 1 ki point to summon this visage for 10 minutes. It vanishes early if you are incapacitated or die."
                                                           "\n\nThe spectral visage covers your face like a helmet or mask. You determine its appearance. While the spectral visage is present, you gain the following benefits."
                                                           "\nAstral Sight. You can see normally in darkness, both magical and nonmagical, to a distance of 120 feet."
                                                           "\nWisdom of the Spirit. You have advantage on Wisdom (Insight) and Charisma (Intimidation) checks."
                                                           "\nWord of the Spirit. When you speak, you can direct your words to a creature of your choice that you can see within 60 feet of you, making it so only that creature can hear you. Alternatively, you can amplify your voice so that all creatures within 600 feet can hear you.")})]}
                            11 {:modifiers [(mod5e/dependent-trait
                                             {:name "Body of the Astral Self"
                                              :summary (str "when you have both your astral arms and visage summoned, you can cause the body of your astral self to appear (no action required). This spectral body covers your physical form like a suit of armor, connecting with the arms and visage. You determine its appearance."
                                                            "\n\nWhile the spectral body is present, you gain the following benefits."
                                                            "\nDeflect Energy. When you take acid, cold, fire, force, lightning, or thunder damage, you can use your reaction to deflect it. When you do so, the damage you take is reduced by 1d10 + your Wisdom modifier (minimum reduction of 1)."
                                                            "\nEmpowered Arms. Once on each of your turns when you hit a target with the Arms of the Astral Self, you can deal extra damage to the target equal to your Martial Arts die.")})]}
                            17 {:modifiers [(mod5e/bonus-action
                                             {:name "Awakened Astral Self"
                                              :summary (str "your connection to your astral self is complete, allowing you to unleash its full potential. As a bonus action, you can spend 5 ki points to summon the arms, visage, and body of your astral self and awaken it for 10 minutes. This awakening ends early if you are incapacitated or die."
                                                            "\n\nWhile your astral self is awakened, you gain the following benefits."
                                                            "\nArmor of the Spirit. You gain a +2 bonus to Armor Class."
                                                            "\nAstral Barrage. Whenever you use the Extra Attack feature to attack twice, you can instead attack three times if all the attacks are made with your astral arms.")})]}}}
                  {:name "Way of the Open Hand"
                   :modifiers [(mod5e/dependent-trait
                                {:name "Open Hand Technique"
                                 :page 79
                                 :summary (str "you can manipulate your enemy's ki when you harness your own. Whenever you hit a creature with one of the attacks granted by your Flurry of Blows, you can impose one of the following effects on that target:"
                                               "\n\u2022 It must succeed on a Dexterity saving throw or be knocked prone."
                                               "\n\u2022 It must make a Strength saving throw. If it fails, you can push it up to 15 feet away from you."
                                               "\n\u2022 It can't take reactions until the end of your next turn.")})]
                   :levels {6 {:modifiers [(mod5e/action
                                            {:name "Wholeness of Body"
                                             :page 79
                                             :level 6
                                             :frequency units5e/long-rests-1
                                             :summary (str "you gain the ability to heal yourself. As an action, you can regain hit points equal to three times your monk level ("
                                                           (* 3 (?class-level :monk)) "). You must finish a long rest before you can use this feature again")})]}
                            11 {:modifiers [(mod5e/dependent-trait
                                             {:name "Tranquility"
                                              :page 80
                                              :level 11
                                              :summary "you can enter a special meditation that surrounds you with an aura of peace. At the end of a long rest, you gain the effect of a Sanctuary spell that lasts until the start of your next long rest (the spell can end early as normal). The saving throw DC for the spell equals 8 + your Wisdom modifier + your proficiency bonus"})]}
                            17 {:modifiers [(mod5e/dependent-trait
                                             {:name "Quivering Palm"
                                              :level 17
                                              :page 80
                                              :summary (str "you gain the ability to set up lethal vibrations in someone's body. When you hit a creature with an unarmed strike, you can spend 3 ki points to start these imperceptible vibrations, which last for a number of days equal to your monk level. The vibrations are harmless unless you use your action to end them. To do so, you and the target must be on the same plane of existence. When you use this action, the creature must make a Constitution saving throw. If it fails, it is reduced to 0 hit points. If it succeeds, it takes 10d10 necrotic damage."
                                                            "\n\nYou can have only one creature under the effect of this feature at a time. You can choose to end the vibrations harmlessly without using an action")})]}}}
                  {:name "Way of the Kensei"
                   :modifiers [
                               (mod5e/bonus-action
                                {:name "Kensei's Shot"
                                 :summary "You can use a bonus action on your turn to make your ranged attacks with a kensei weapon more deadly. When you do so, any target you hit with a ranged attack using a kensei weapon takes an extra 1d4 damage of the weapon’s type. You retain this benefit until the end of the current turn"})]
                   :selections [(opt5e/tool-selection [:calligraphers-supplies :painters-supplies] 1)]
                   :levels {6 {:modifiers [(mod5e/trait-cfg
                                            {:name "Magic Kensei Weapons"
                                             :summary "Your attacks with your kensei weapons count as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage"})
                                           (mod5e/dependent-trait
                                            {:name "Deft Strike"
                                            :frequency units5e/turns-1
                                            :summary (str "When you hit a target with a kensei weapon, you can spend 1 ki point to cause the weapon to deal extra damage to the target equal to your Martial Arts die. You can use this feature only once on each of your turns")})]}
                            11 {:modifiers [(mod5e/bonus-action
                                             {:name "Sharpen the Blade"
                                              :duration units5e/minutes-1
                                              :summary "you gain the ability to augment your weapons further with your ki. As a bonus action, you can expend up to 3 ki points to grant one kensei weapon you touch a bonus to attack and damage rolls when you attack with it. The bonus equals the number of ki points you spent. This bonus lasts for 1 minute or until you use this feature again. This feature has no effect on a magic weapon that already has a bonus to attack and damage rolls"})]}
                            17 {:modifiers [(mod5e/dependent-trait
                                             {:name "Unerring Accuracy"
                                              :frequency units5e/turns-1
                                              :summary "your mastery of weapons grants you extraordinary accuracy. If you miss with an attack roll using a monk weapon on your turn, you can reroll it. You can use this feature only once on each of your turns"})]}}
                   :traits [{:name "Agile Parry"
                             :summary "If you make an unarmed strike as part of the Attack action on your turn and are holding a kensei weapon, you can use it to defend yourself if it is a melee weapon. You gain a +2 bonus to AC until the start of your next turn, while the weapon is in your hand and you aren’t incapacitated"}]}
                  {:name "Way of the Long Death"
                   :modifiers [(mod5e/dependent-trait
                                {:name "Touch of Death"
                                 :summary (str "your study of death allows you to extract vitality from another creature as it nears its demise. When you reduce a creature within 5 feet of you to 0 hit points, you gain temporary hit points equal to your Wisdom modifier + your monk level (minimum of 1 temporary hit point) (" (max 1 (+ (?ability-bonuses ::char5e/wis) (?class-level :monk))) ")")})]
                   :levels {6 {:modifiers [(mod5e/action
                                            {:name "Hour of Reaping"
                                             :summary "you gain the ability to unsettle or terrify those around you as an action, for your soul has been touched by the shadow of death. When you take this action, each creature within 30 feet of you that can see you must succeed on a Wisdom saving throw or be frightened of you until the end of your next turn"})]}
                            11 {:modifiers [(mod5e/trait-cfg
                                             {:name "Mastery of Death"
                                              :summary "you use your familiarity with death to escape its grasp. When you are reduced to 0 hit points, you can expend 1 ki point (no action required) to have 1 hit point instead"})]}
                            17 {:modifiers [(mod5e/action
                                             {:name "Touch of the Long Death"
                                              :summary "your touch can channel the energy of death into a creature. As an action, you touch one creature within 5 feet of you, and you expend 1 to 10 ki points. The target must make a Constitution saving throw, and it takes 2d10 necrotic damage per ki point spent on a failed save, or half as much damage on a successful one"})]}}}
                  {:name "Way of the Ascendant Dragon"
                   :modifiers [(mod5e/reaction
                                {:name "Draconic Disciple: Draconic Presence"
                                 :summary "If you fail a Charisma (Intimidation) or Charisma (Persuasion) check, you can use your reaction to reroll the check, as you tap into the mighty presence of dragons. Once this feature turns a failure into a success, you can’t use it again until you finish a long rest"})
                               (mod5e/trait-cfg
                                {:name "Draconic Disciple: Draconic Strike"
                                 :summary "When you damage a target with an unarmed strike, you can change the damage type to acid, cold, fire, lightning, or poison"})
                               (mod5e/dependent-trait
                                {:name "Breath of the Dragon"
                                 :frequency (units5e/long-rests ?prof-bonus)
                                 :summary (str "you can channel destructive waves of energy, like those created by the dragons you emulate. When you take the Attack action on your turn, you can replace one of the attacks with an exhalation of draconic energy in either a 20-foot cone or a 30-foot line that is 5 feet wide (your choice). Choose a damage type: acid, cold, fire, lightning, or poison. Each creature in that area must make a Dexterity saving throw against your ki save DC, taking damage of the chosen type equal to "
                                               (if (>= (?class-level :monk) 5) 
                                                 "three"
                                                 "two")
                                               " rolls of your Martial Arts die on a failed save, or half as much damage on a successful one."
                                               "\n\nYou can use this feature a number of times equal to your proficiency bonus, and you regain all expended uses when you finish a long rest. While you have no uses available, you can spend 2 ki points to use this feature again")})]
                   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
                   :levels {6 {:modifiers [(mod5e/dependent-trait
                                            {:name "Wings Unfurled"
                                             :frequency (units5e/long-rests ?prof-bonus)
                                             :summary "when you use your Step of the Wind, you can unfurl spectral draconic wings from your back that vanish at the end of your turn. While the wings exist, you have a flying speed equal to your walking speed"})]}
                            11 {:modifiers [(mod5e/bonus-action
                                             {:name "Aspect of the Wyrm"
                                              :summary (str "the power of your draconic spirit now radiates from you, warding your allies or inspiring fear in your enemies. As a bonus action, you can create an aura of draconic power that radiates 10 feet from you for 1 minute. For the duration, you gain one of the following effects of your choice:"
                                                        "\n\u2022 Frightful Presence. When you create this aura, and as a bonus action on subsequent turns, you can choose a creature within the aura. The target must succeed on a Wisdom saving throw against your ki save DC or become frightened of you for 1 minute. The target can repeat the saving throw at the end of each of its turns, ending the effect on itself on a successful save."
                                                        "\n\u2022 Resistance. Choose a damage type when you activate this aura: acid, cold, fire, lightning, or poison. You and your allies within the aura have resistance to that damage."
                                                        "\nOnce you create this aura, you can’t create it again until you finish a long rest, unless you expend 3 ki points to create it again")})]}
                            17 {:modifiers [(mod5e/dependent-trait
                                             {:name "Ascendant Aspect: Augment Breath"
                                              :summary "When you use your Breath of the Dragon, you can spend 1 ki point to augment its shape and power. The exhalation of draconic energy becomes either a 60-foot cone or a 90-foot line that is 5 feet wide (your choice), and each creature in that area takes damage equal to four rolls of your Martial Arts die on a failed save, or half as much damage on a successful one"})
                                             (mod5e/trait-cfg
                                              {:name "Ascendant Aspect: Blindsight"
                                               :summary "You gain blindsight out to 10 feet. Within that range, you can effectively see anything that isn’t behind total cover, even if you’re blinded or in darkness. Moreover, you can see an invisible creature within that range, unless the creature successfully hides from you"})
                                             (mod5e/dependent-trait
                                              {:name "Ascendant Aspect: Explosive Fury"
                                               :summary (str "When you activate Aspect of the Wyrm, any creature you choose in the aura must make a DC " (?spell-save-dc ::char5e/wis) "When you activate your Aspect of the Wyrm, draconic fury explodes from you. Choose any number of creatures you can see in your aura. Each of those creatures must succeed on a Dexterity saving throw against your ki save DC or take 3d10 acid, cold, fire, lightning, or poison damage (your choice)")})]}}}
                  #_{:name "Way of the Four Elements"
                   :modifiers [(mod5e/dependent-trait
                                  {:name "Disciple of the Elements"
                                   :page 80
                                   :summary (str "You learn elemental disciplines with spell save DC " (?spell-save-dc ::char5e/wis) "."
                                                 (if (>= (?class-level :monk) 5)
                                                   (str " You can increase the level of elemental discipline spells you cast by 1 for each additional ki point you spend, up to " (mod5e/level-val (?class-level :monk)
                                                                                                                                                                                                   {9 4 13 5 17 6 :default 3}))))})]
                   :levels {3 {:selections [(opt5e/monk-elemental-disciplines)]}
                              6 {:selections [(opt5e/monk-elemental-disciplines)]}
                              11 {:selections [(opt5e/monk-elemental-disciplines)]}
                              17 {:selections [(opt5e/monk-elemental-disciplines)]}}
                   :traits [{:name "Elemental Attunement"
                               :page 81
                               :summary "create minor elemental effect"}]}]})))


(defn paladin-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   (merge
    opt5e/paladin-base-cfg
    {:name "Paladin"
     :key :paladin
     :spellcaster true
     :spellcasting {:level-factor 2
                    :known-mode :all
                    :ability ::char5e/cha
                    :prepares-spells? true}
     :hit-die 10
     :ability-increase-levels [4 8 12 16 19]
     :profs {:armor {:light false :medium false :heavy true :shields false}
             :weapon {:simple false :martial false}
             :save {::char5e/wis true ::char5e/cha true}
             :skill-options {:choose 2 :options {:athletics true :insight true :intimidation true :medicine true :persuasion true :religion true}}}
     :multiclass-prereqs [(t/option-prereq "Requires Strength 13 and Charisma 13"
                                           (fn [c]
                                             (let [abilities @(subscribe [::char5e/abilities nil c])]
                                               (and (>= (::char5e/str abilities) 13)
                                                    (>= (::char5e/cha abilities) 13)))))]
     :equipment-choices [{:name "Equipment Pack"
                          :options {:priests-pack 1
                                    :explorers-pack 1}}]
     :armor {:chain-mail 1}
     :levels {2 {:selections [(opt5e/fighting-style-selection :paladin #{:blind-fighting :defense :dueling :great-weapon-fighting :interception :protection :two-weapon-fighting})]}
              3 {:modifiers [(mod5e/immunity :disease)
                             (mod5e/trait-cfg
                              {:name "Divine Health"
                               :page 85
                               :summary "the divine magic flowing through you makes you immune to disease"})
                             (mod5e/bonus-action
                             {:name "Harness Divine Power"
                              :frequency (units5e/long-rests (mod5e/level-val
                                             (?class-level :cleric)
                                             {7 2
                                              15 3
                                              :default 1}))
                              :summary "you can expend a use of your Channel Divinity to fuel your spells. As a bonus action, you touch your holy symbol, utter a prayer, and regain one expended spell slot, the level of which can be no higher than half your proficiency bonus (rounded up)"})]}
              4 {:modifiers [(mod5e/trait-cfg
                             {:name "Martial Versatility"
                              :summary "When you reach level 4, 8, 12, 16, and 19 in paladin, you can replace one fighting style from the class."})]}
              5 {:modifiers [(mod5e/num-attacks 2)]}
              6 {:modifiers (conj
                             (map
                              #(mod/modifier ?saving-throw-bonuses
                                             (merge-with +
                                                         ?saving-throw-bonuses
                                                         {% (get ?ability-bonuses ::char5e/cha 0)}))
                              char5e/ability-keys)
                             (mod5e/dependent-trait
                              {:name "Aura of Protection"
                               :page 85
                               :summary (str "whenever you or a friendly creature within " ?paladin-aura "feet of you must make a saving throw, the creature gains a bonus to the saving throw equal to your Charisma modifier (with a minimum bonus of +1). You must be conscious to grant this bonus")}))}
              10 {:modifiers [(mod5e/dependent-trait
                               {:name "Aura of Courage"
                                :page 85
                                :summary (str (str "you and friendly creatures within " ?paladin-aura " feet of you can't be frightened while you are conscious"))})]}
              14 {:modifiers [(mod5e/action
                               {:name "Cleansing Touch"
                                :page 85
                                :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/cha)))
                                :summary "you can use your action to end one spell on yourself or on one willing creature that you touch"})]}}
     :modifiers [(mod/modifier ?paladin-aura (if (< (?class-level :paladin) 18) 10 30))
                 (mod5e/action
                  {:name "Divine Sense"
                   :page 84
                   :frequency (units5e/long-rests
                               (inc (?ability-bonuses ::char5e/cha)))
                   :summary "The presence of strong evil registers on your senses like a noxious odor, and powerful good rings like heavenly music in your ears. As an action, you can open your awareness to detect such forces. Until the end of your next turn, you know the location of any celestial, fiend, or undead within 60 feet of you that is not behind total cover. You know the type (celestial, fiend, or undead) of any being whose presence you sense, but not its identity (the vampire Count Strahd von Zarovich, for instance). Within the same radius, you also detect the presence of any place or object that has been consecrated or desecrated, as with the Hallow spell"})
                 (mod5e/action
                  {:name "Lay on Hands"
                   :page 84
                   :frequency (units5e/long-rests
                                (* 5 (?class-level :paladin)))
                   :summary (str "Your blessed touch can heal wounds. You have a pool of healing power that replenishes when you take a long rest. With that pool, you can restore a total number of hit points equal to your paladin level x 5 (" (* 5 (?class-level :paladin)) ")."
                                 "\n\nAs an action, you can touch a creature and draw power from the pool to restore a number of hit points to that creature, up to the maximum amount remaining in your pool."
                                 "\n\nAlternatively, you can expend 5 hit points from your pool of healing to cure the target of one disease or neutralize one poison affecting it. You can cure multiple diseases and neutralize multiple poisons with a single use of Lay on Hands, expending hit points separately for each one."
                                 "\n\nThis feature has no effect on undead and constructs.")})
                 (mod5e/dependent-trait
                  {:name "Channel Divinity"
                   :page 85
                   :level 3
                   :frequency units5e/rests-1
                   :summary "Your oath allows you to channel divine energy to fuel magical effects. Each Channel Divinity option provided by your oath explains how to use it"})]
     :selections [(opt5e/new-starting-equipment-selection
                   :paladin
                   {:name "Weapons"
                    :options [(t/option-cfg
                               {:name "Martial Weapon and Shield"
                                :selections [(opt5e/new-starting-equipment-selection
                                              :paladin
                                              {:name "Martial Weapon"
                                               :options (opt5e/martial-weapon-options 1 (vals weapon-map))})]
                                :modifiers [(mod5e/armor :shield 1)]})
                              (t/option-cfg
                               {:name "Two Martial Weapons"
                                :selections [(opt5e/new-starting-equipment-selection
                                              :paladin
                                              {:name "Martial Weapon 1"
                                               :options (opt5e/martial-weapon-options 1 (vals weapon-map))
                                               :min 1
                                               :max 1})
                                             (opt5e/new-starting-equipment-selection
                                              :paladin
                                              {:name "Martial Weapon 2"
                                               :options (opt5e/martial-weapon-options 1 (vals weapon-map))
                                               :min 1
                                               :max 1})]})]})
                  (opt5e/new-starting-equipment-selection
                   :paladin
                   {:name "Holy Symbol"
                    :options (map
                              #(opt5e/starting-equipment-option % 1)
                              equipment5e/holy-symbols)})
                  (opt5e/new-starting-equipment-selection
                   :paladin
                   {:name "Melee Weapon"
                    :options [(t/option-cfg
                               {:name "Five Javelins"
                                :modifiers [(mod5e/weapon :javelin 5)]})
                              (t/option-cfg
                               {:name "Simple Melee Weapon"
                                :selections [(opt5e/new-starting-equipment-selection
                                              :paladin
                                              {:name "Simple Melee Weapon"
                                               :options (opt5e/simple-melee-weapon-options 1 (vals weapon-map))})]})]})]
     :traits [{:name "Divine Smite"
               :level 2
               :page 85
               :summary "when you hit a creature with a melee weapon attack, you can expend one spell slot to deal radiant damage to the target, in addition to the weapon's damage. The extra damage is 2d8 for a 1st-level spell slot, plus 1d8 for each spell level higher than 1st, to a maximum of 5d8. The damage increases by 1d8 if the target is an undead or a fiend, to a maximum of 6d8"}
              {:name "Improved Divine Smite"
               :level 11
               :page 85
               :summary "you are so suffused with righteous might that all your melee weapon strikes carry divine power with them. Whenever you hit a creature with a melee weapon, the creature takes an extra 1d8 radiant damage"}]
     :subclass-level 3
     :subclass-title "Sacred Oath"
     :subclasses [{:name "Oath of Devotion"
                   :modifiers [(opt5e/paladin-spell 1 :protection-from-evil-and-good)
                               (opt5e/paladin-spell 1 :sanctuary)
                               (opt5e/paladin-spell 2 :lesser-restoration)
                               (opt5e/paladin-spell 2 :zone-of-truth)
                               (opt5e/paladin-spell 3 :beacon-of-hope)
                               (opt5e/paladin-spell 3 :dispel-magic)
                               (opt5e/paladin-spell 4 :freedom-of-movement)
                               (opt5e/paladin-spell 4 :guardian-of-faith)
                               (opt5e/paladin-spell 5 :commune)
                               (opt5e/paladin-spell 5 :flame-strike)
                               (mod5e/action
                                {:name "Channel Divinity: Sacred Weapon"
                                 :page 86
                                 :duration units5e/minutes-1
                                 :summary (str "As an action, you can imbue one weapon that you are holding with positive energy, using your Channel Divinity. For 1 minute, you add your Charisma modifier to attack rolls made with that weapon (with a minimum bonus of +1). The weapon also emits bright light in a 20-foot radius and dim light 20 feet beyond that. If the weapon is not already magical, it becomes magical for the duration."
                                               "\n\nYou can end this effect on your turn as part of any other action. If you are no longer holding or carrying this weapon, or if you fall unconscious, this effect ends")})
                               (mod5e/action
                                {:name "Channel Divinity: Turn the Unholy"
                                 :page 86
                                 :duration units5e/minutes-1
                                 :summary (str "As an action, you present your holy symbol and speak a prayer censuring fiends and undead, using your Channel Divinity. Each fiend or undead that can see or hear you within 30 feet of you must make a Wisdom saving throw. If the creature fails its saving throw, it is turned for 1 minute or until it takes damage."
                                               "\n\nA turned creature must spend its turns trying to move as far away from you as it can, and it can't willingly move to a space within 30 feet of you. It also can't take reactions. For its action, it can use only the Dash action or try to escape from an effect that prevents it from moving. If there's nowhere to move, the creature can use the Dodge action")})]
                   :levels {7 {:modifiers [(mod5e/dependent-trait
                                            {:name "Aura of Devotion"
                                             :page 86
                                             :summary (str "you and friendly creatures within " ?paladin-aura " feet of you can't be charmed while you are conscious")})]}
                            20 {:modifiers [(mod5e/action
                                             {:name "Holy Nimbus"
                                              :page 86
                                              :frequency units5e/long-rests-1
                                              :duration units5e/minutes-1
                                              :summary (str "as an action, you can emanate an aura of sunlight. For 1 minute, bright light shines from you in a 30-foot radius, and dim light shines 30 feet beyond that."
                                                            "\n\nWhenever an enemy creature starts its turn in the bright light, the creature takes 10 radiant damage."
                                                            "\n\nIn addition, for the duration, you have advantage on saving throws against spells cast by fiends or undead")})]}}
                   :traits [{:name "Purity of Spirit"
                             :level 15
                             :page 86
                             :summary "you are always under the effects of a Protection from Evil and Good spell"}]}
                  {:name "Oathbreaker"
                   :modifiers [(opt5e/paladin-spell 1 :hellish-rebuke)
                               (opt5e/paladin-spell 1 :inflict-wounds)
                               (opt5e/paladin-spell 2 :crown-of-madness)
                               (opt5e/paladin-spell 2 :darkness)
                               (opt5e/paladin-spell 3 :animate-dead)
                               (opt5e/paladin-spell 3 :bestow-curse)
                               (opt5e/paladin-spell 4 :blight)
                               (opt5e/paladin-spell 4 :confusion)
                               (opt5e/paladin-spell 5 :contagion)
                               (opt5e/paladin-spell 5 :dominate-person)
                               (mod5e/action
                                {:name "Channel Divinity: Control Undead"
                                 :duration (units5e/hours 24)
                                 :summary "As an action, you target one undead creature you can see within 30 feet of you. The target must make a Wisdom saving throw. On a failed save, the target must obey your commands for the next 24 hours, or until you use this Channel Divinity option again. An undead whose challenge rating is equal to or greater than your paladin level is immune to this effect"})
                               (mod5e/action
                                {:name "Channel Divinity: Dreadful Aspect"
                                 :duration units5e/minutes-1
                                 :summary "As an action, you channel the darkest emotions and focus them into a burst of magical menace. Each creature of your choice within 30 feet of you must make a Wisdom saving throw if it can see you. On a failed save, the target is frightened of you for 1 minute. If a creature frightened by this effect ends its turn more than 30 feet away from you, it can attempt another Wisdom saving throw to end the effect on it"})]
                   :levels {7 {:modifiers [(mod5e/dependent-trait
                                            {:name "Aura of Hate"
                                             :summary (str "you, as well any fiends and undead within " ?paladin-aura " feet of you, gain a bonus to melee weapon damage rolls equal to your Charisma modifier (minimum of +1). A creature can benefit from this feature from only one paladin at a time")})]}
                            20 {:modifiers [(mod5e/action
                                             {:name "Dread Lord"
                                              :frequency units5e/long-rests-1
                                              :duration units5e/minutes-1
                                              :summary (str "you can, as an action, surround yourself with an aura of gloom that lasts for 1 minute. The aura reduces any bright light in a 30-foot radius around you to dim light. Whenever an enemy that is frightened by you starts its turn in the aura, it takes 4d10 psychic damage. Additionally, you and any creatures of your choosing in the aura are draped in deeper shadow. Creatures that rely on sight have disadvantage on attack rolls against creatures draped in this shadow."
                                                            "\n\nWhile the aura lasts, you can use a bonus action on your turn to cause the shadows in the aura to attack one creature. Make a melee spell attack against the target. If the attack hits, the target takes necrotic damage equal to 3d10 + your Charisma modifier")})]}}
                   :traits [{:name "Supernatural Resistance"
                             :level 15
                             :summary "you gain resistance to bludgeoning, piercing, and slashing damage from nonmagical weapons"}]}
                  {:name "Oath of the Crown"
                   :modifiers [(opt5e/paladin-spell 1 :command)
                               (opt5e/paladin-spell 1 :compelled-duel)
                               (opt5e/paladin-spell 2 :warding-bond)
                               (opt5e/paladin-spell 2 :zone-of-truth)
                               (opt5e/paladin-spell 3 :aura-of-vitality)
                               (opt5e/paladin-spell 3 :spirit-guardians)
                               (opt5e/paladin-spell 4 :banishment)
                               (opt5e/paladin-spell 4 :guardian-of-faith)
                               (opt5e/paladin-spell 5 :circle-of-power)
                               (opt5e/paladin-spell 5 :geas)
                               (mod5e/bonus-action
                                {:name "Channel Divinity: Champion Challenge"
                                 :summary "As a bonus action, you issue a challenge that compels other creatures to do battle with you. Each creature of your choice that you can see within 30 feet of you must make a Wisdom saving throw. On a failed save, a creature can't willingly move more than 30 feet away from you. This effect ends on the creature if you are incapacitated or die or if the creature is more than 30 feet away from you"})
                               (mod5e/bonus-action
                                {:name "Channel Divinity: Turn the Tide"
                                 :summary "As a bonus action, you can bolster injured creatures with your Channel Divinity. Each creature of your choice that can hear you within 30 feet of you regains hit points equal to 1d6 + your Charisma modifier (minimum of 1) if it has no more than half of its hit points"})]
                   :levels {7 {:modifiers [(mod5e/reaction
                                            {:name "Divine Allegiance"
                                             :summary "when a creature within 5 feet of you takes damage, you can use your reaction to magically substitute your own health for that of the target creature, causing that creature not to take the damage. Instead, you take the damage. This damage to you can't be reduced or prevented in any way"})]}
                            15 {:modifiers [(mod5e/trait-cfg
                                             {:name "Unyielding Saint"
                                              :summary "you have advantage on saving throws to avoid becoming paralyzed or stunned"})
                                            (mod5e/saving-throw-advantage [:paralyzed])
                                            (mod5e/saving-throw-advantage [:stunned])]}
                            20 {:modifiers [(mod5e/action
                                             {:name "Exalted Champion"
                                              :duration units5e/hours-1
                                              :frequency units5e/long-rests-1
                                              :summary (str "your presence on the field of battle is an inspiration to those dedicated to your cause. You can use your action to gain the following benefits for 1 hour:"
                                                            "\n\u2022 You have resistance to bludgeoning, piercing, and slashing damage from nonmagical weapons."
                                                            "\n\u2022 Your allies have advantage on death saving throws while within 30 feet of you."
                                                            "\n\u2022 You have advantage on Wisdom saving throws, as do your allies within 30 feet of you."
                                                            "\nThis effect ends early if you are incapacitated or die")})]}}}
                  #_{:name "Oath of the Ancients"
                     :modifiers [(opt5e/paladin-spell 1 :ensnaring-strike 3)
                                 (opt5e/paladin-spell 1 :speak-with-animals 3)
                                 (opt5e/paladin-spell 2 :misty-step 5)
                                 (opt5e/paladin-spell 2 :moonbeam 5)
                                 (opt5e/paladin-spell 3 :plant-growth 9)
                                 (opt5e/paladin-spell 3 :protection-from-energy 9)
                                 (opt5e/paladin-spell 4 :ice-storm 13)
                                 (opt5e/paladin-spell 4 :stoneskin 13)
                                 (opt5e/paladin-spell 5 :commune-with-nature 17)
                                 (opt5e/paladin-spell 5 :tree-stride 17)
                                 (mod5e/action
                                  {:name "Channel Divinity: Nature's Wrath"
                                   :level 3
                                   :page 87
                                   :summary (str "restrain a creature with vines on a failed DC " (?spell-save-dc ::char5e/cha) " STR or DEX save. It makes the save every turn until freed.")})
                                 (mod5e/action
                                  {:name "Channel Divinity: Turn the Faithless"
                                   :level 3
                                   :page 87
                                   :duration units5e/minutes-1
                                   :summary "turn and reveal the true form of fey and fiends within 30 ft."})]
                     :levels {7 {:modifiers [(mod5e/dependent-trait
                                              {:name "Aura of Warding"
                                               :level 7
                                               :page 87
                                               :summary (str "you and friendly creatures within " ?paladin-aura " have resistance to spell damage")})]}}
                     :traits [{:name "Undying Sentinal"
                               :level 15
                               :page 87
                               :frequency units5e/long-rests-1
                               :summary "when you are reduced to 0 HP without being killed, you drop to 1 instead"}
                              {:name "Elder Champion"
                               :level 20
                               :page 87
                               :frequency units5e/long-rests-1
                               :duration units5e/minutes-1
                               :summary "undergo a tranformation where you 1) regain 10 HPs at start of your turns 2) can cast spells with casting time action as bonus action 3) enemies within 10 ft. have disadvantage on saves against your Channel Divinity and spells"}]}
                  #_{:name "Oath of Vengeance"
                     :modifiers [(opt5e/paladin-spell 1 :bane 3)
                                 (opt5e/paladin-spell 1 :hunters-mark 3)
                                 (opt5e/paladin-spell 2 :hold-person 5)
                                 (opt5e/paladin-spell 2 :misty-step 5)
                                 (opt5e/paladin-spell 3 :haste 9)
                                 (opt5e/paladin-spell 3 :protection-from-energy 9)
                                 (opt5e/paladin-spell 4 :banishment 13)
                                 (opt5e/paladin-spell 4 :dimension-door 13)
                                 (opt5e/paladin-spell 5 :hold-monster 17)
                                 (opt5e/paladin-spell 5 :scrying 17)
                                 (mod5e/action
                                  {:name "Channel Divinity: Abjure Enemy"
                                   :level 3
                                   :page 88
                                   :duration units5e/minutes-1
                                   :summary (str "a creature of your choosing within 60 ft. must succeed on a DC " (?spell-save-dc ::char5e/cha) " WIS save or be frightened and have a speed of 0, speed is halved on successful save")})
                                 (mod5e/bonus-action
                                  {:name "Channel Divinity: Vow of Eternity"
                                   :level 3
                                   :page 88
                                   :duration units5e/minutes-1
                                   :summary "gain advantage on attacks against a creature"})]
                     :levels {15 {:modifiers [(mod5e/reaction
                                               {:name "Soul of Vengeance"
                                                :level 15
                                                :page 88
                                                :summary "when a creature under you Vow of Enmity attacks, make a melee weapon attack against it"})]}
                              20 {:modifiers [(mod5e/action
                                               {:name "Avenging Angel"
                                                :level 20
                                                :page 88
                                                :duration units5e/hours-1
                                                :frequency units5e/long-rests-1
                                                :summary (str "transform, gain flying speed of 60 ft., emanate a 30 ft. aura and creatures within it must succeed on a DC " (?spell-save-dc ::char5e/cha) " WIS or be frightened for 1 min and attacks against them have advantage")})]}}
                     :traits [{:name "Relentless Avenger"
                               :level 7
                               :page 88
                               :summary "when you hit with opportunity attack, you can also move up to half your speed after the attack without provoking opportunity attacks"}]}
                  {:name "Oath of the Watchers"
                     :modifiers [(opt5e/paladin-spell 1 :alarm)
                                 (opt5e/paladin-spell 1 :detect-magic)
                                 (opt5e/paladin-spell 2 :moonbeam)
                                 (opt5e/paladin-spell 2 :see-invisibility)
                                 (opt5e/paladin-spell 3 :counterspell)
                                 (opt5e/paladin-spell 3 :nondetection)
                                 (opt5e/paladin-spell 4 :aura-of-purity)
                                 (opt5e/paladin-spell 4 :banishment)
                                 (opt5e/paladin-spell 5 :hold-monster)
                                 (opt5e/paladin-spell 5 :scrying)
                                 (mod5e/action
                                  {:name "Channel Divinity: Watcher's Will"
                                   :level 3
                                   :duration units5e/minutes-1
                                   :summary "You can use your Channel Divinity to invest your presence with the warding power of your faith. As an action, you can choose a number of creatures you can see within 30 feet of you, up to a number equal to your Charisma modifier (minimum of one creature). For 1 minute, you and the chosen creatures have advantage on Intelligence, Wisdom, and Charisma saving throws"})
                                 (mod5e/action
                                  {:name "Channel Divinity: Abjure the Extraplanar"
                                   :level 3
                                   :duration units5e/minutes-1
                                   :summary "You can use your Channel Divinity to castigate unworldly beings. As an action, you present your holy symbol and each aberration, celestial, elemental, fey, or fiend within 30 feet of you that can hear you must make a Wisdom saving throw. On a failed save, the creature is turned for 1 minute or until it takes damage"})]
                     :levels {7 {:modifiers [(mod5e/dependent-trait
                                              {:name "Aura of the Sentinel"
                                               :level 7
                                               :summary (str "you emit an aura of alertness while you aren't incapacitated. When you and any creatures of your choice within " ?paladin-aura " feet of you roll initiative, you all gain a bonus to initiative equal to your proficiency bonus")})]}
                              15 {:modifiers [(mod5e/reaction
                                               {:name "Vigilant Rebuke"
                                                :level 15
                                                :summary "you've learned how to chastise anyone who dares wield beguilements against you and your wards. Whenever you or a creature you can see within 30 feet of you succeeds on an Intelligence, a Wisdom, or a Charisma saving throw, you can use your reaction to deal 2d8 + your Charisma modifier force damage to the creature that forced the saving throw"})]}
                              20 {:modifiers [(mod5e/bonus-action
                                               {:name "Mortal Bulwark"
                                                :level 20
                                                :summary (str "you manifest a spark of divine power in defense of the mortal realms. As a bonus action, you gain the following benefits for 1 minute:"
                                                              "\n\u2022 You gain truesight with a range of 120 feet."
                                                              "\n\u2022 You have advantage on attack rolls against aberrations, celestials, elementals, fey, and fiends."
                                                              "\n\u2022 When you hit a creature with an attack roll and deal damage to it, you can also force it to make a Charisma saving throw against your spell save DC. On a failed save, the creature is magically banished to its native plane of existence if it's currently not there. On a successful save, the creature can't be banished by this feature for 24 hours."
                                                              "\nOnce you use this bonus action, you can't use it again until you finish a long rest, unless you expend a 5th-level spell slot to use it again")})]}}}]})))

(defn favored-enemy-option [language-map [enemy-type info]]
  (let [vec-info? (sequential? info)
        languages (if vec-info? info (:languages info))
        name (if vec-info? (common/kw-to-name enemy-type) (:name info))
        language-options (zipmap languages (repeat true))]
      (t/option-cfg
       {:name name
        :selections (if (> (count languages) 1)
                      [(opt5e/language-selection
                        language-map
                        {:choose 1
                         :options language-options})])
        :modifiers (remove
                    nil?
                    [(if (= 1 (count languages))
                       (mod5e/language (first languages)))
                     (mod/set-mod ?ranger-favored-enemies enemy-type)])})))

(defn favored-enemy-selection [language-map order & [prereq-level]]
  (t/selection-cfg
  {:name (str "Favored Enemy " order)
    :tags #{:class}
    :order 3
    :prereq-fn (fn [c] (let [class-level @(subscribe [::char5e/class-level-fn nil c])]
                               (>= (class-level :ranger) prereq-level)))
    :options [(t/option-cfg
               {:name "Type"
                :selections [(t/selection-cfg
                              {:name "Favored Enemy Type"
                               :tags #{:class}
                               :order 4
                               :multiselect? true
                               :ref [:class :ranger :favored-enemy-type]
                               :options (map
                                         (partial favored-enemy-option language-map)
                                         (opt5e/favored-enemy-types language-map))})]})
              (t/option-cfg
               {:name "Two Humanoid Races"
                :selections [(t/selection-cfg
                              {:name "Favored Enemy Humanoid Race"
                               :tags #{:class}
                               :order 4
                               :min 2
                               :max 2
                               :multiselect? true
                               :ref [:class :ranger :favored-enemy-race]
                               :options (map
                                         (partial favored-enemy-option language-map)
                                         opt5e/humanoid-enemies)})]})]}))

(defn favored-terrain-selection [order & [prereq-level]]
  (t/selection-cfg
   {:name "Favored Terrain"
    :tags #{:class}
    :order 5
    :ref [:class :ranger :favored-terrain]
    :multiselect? true
    :prereq-fn (fn [c] (let [class-level @(subscribe [::char5e/class-level-fn nil c])]
                               (>= (class-level :ranger) prereq-level)))
    :options (map
              (fn [terrain]
                (t/option-cfg
                 {:name (common/kw-to-name terrain)
                  :modifiers [(mod/set-mod ?ranger-favored-terrain terrain)]}))
              [:arctic :coast :desert :forest :grassland :mountain :swamp :underdark])}))


(def third-caster-spells-known-schedule
  {3 3
   4 1
   7 1
   8 1
   10 1
   11 1
   13 1
   14 1
   16 1
   19 1
   20 1})

(def half-caster-spells-known-schedule
  {2 2
   3 1
   5 1
   7 1
   9 1
   11 1
   13 1
   15 1
   17 1
   19 1})

(def full-caster-spells-known-schedule
  {1 2
   2 1
   3 1
   4 1
   5 1
   6 1
   7 1
   8 1
   9 1
   10 1
   11 1
   13 1
   15 1
   17 1})

(defn ranger-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   (merge
    opt5e/ranger-base-cfg
    {:hit-die 10
     :name "Ranger"
     :key :ranger
     :profs {:armor {:light false :medium false :shields false}
             :weapon {:simple false :martial false}
             :save {::char5e/str true ::char5e/dex true}
             :skill-options {:choose 3 :options opt5e/ranger-skills}
             :multiclass-skill-options {:choose 1 :options opt5e/ranger-skills}}
     :multiclass-prereqs [(t/option-prereq "Requires Wisdom 13 and Dexterity 13"
                                           (fn [c]
                                             (let [abilities @(subscribe [::char5e/abilities nil c])]
                                               (and (>= (::char5e/wis abilities) 13)
                                                    (>= (::char5e/dex abilities) 13)))))]
     :ability-increase-levels [4 8 12 16 19]
     :spellcaster true
     :spellcasting {:level-factor 2
                    :known-mode :all
                    ;;:spells-known half-caster-spells-known-schedule
                    :ability ::char5e/wis
                    :prepares-spells? true}
     :armor-choices [{:name "Armor"
                      :options {:scale-mail 1
                                :leather 1}}]
     :equipment-choices [{:name "Equipment Pack"
                          :options {:dungeoneers-pack 1
                                    :explorers-pack 1}}]
     :weapons {:longbow 1}
     :equipment {:quiver 1
                 :arrow 20}
     :selections [(opt5e/new-starting-equipment-selection
                   :ranger
                   {:name "Melee Weapon"
                    :options [(t/option-cfg
                               {:name "Two Shortswords"
                                :modifiers [(mod5e/weapon :shortsword 2)]})
                              (t/option-cfg
                               {:name "Simple Melee Weapon"
                                :selections [(opt5e/new-starting-equipment-selection
                                              :ranger
                                              {:name "Simple Melee Weapon"
                                               :options (opt5e/simple-melee-weapon-options 1 (vals weapon-map))
                                               :min 2
                                               :max 2})]})]})
                  (t/selection-cfg
                   {:name "Favored Selection"
                    :tags #{:class}
                    :order 3
                    :options [(t/option-cfg
                               {:name "Favored Enemy"
                                :order 1
                                :selections [(favored-enemy-selection language-map 1)
                                             (favored-enemy-selection language-map 2 6)
                                             (favored-enemy-selection language-map 3 14)]
                                :modifiers [(mod5e/dependent-trait
                                             {:name "Favored Enemy"
                                              :page 91
                                              :summary (str "your favored enemies are " (common/list-print (map #(common/kw-to-name % false) ?ranger-favored-enemies))"."
                                                            "\n\nYou have advantage on Wisdom (Survival) checks to track your favored enemies, as well as on Intelligence checks to recall information about them."
                                                            "\n\nWhen you gain this feature, you also learn one language of your choice that is spoken by your favored enemies, if they speak one at all.")})]})
                              (t/option-cfg
                               {:name "Favored Foe"
                                :order 2
                                :modifiers [(mod5e/action
                                             {:name "Favored Foe"
                                              :frequency (units5e/long-rests ?prof-bonus)
                                              :summary (str "When you hit a creature with an attack roll, you can call on your mystical bond with nature to mark the target as your favored enemy for 1 minute or until you lose your concentration (as if you were concentrating on a spell)."
                                                            "\n\nThe first time on each of your turns that you hit the favored enemy and deal damage to it, including when you mark it, you increase that damage by 1d"
                                                            (condp <= (?class-level :ranger) 14 8  6 6  4) ".")})]})]})
                  (t/selection-cfg
                   {:name "Explorer Selection"
                    :tags #{:class}
                    :order 3
                    :options [(t/option-cfg
                               {:name "Natural Explorer"
                                :order 1
                                :selections [(favored-terrain-selection 1)
                                             (favored-terrain-selection 2 6)
                                             (favored-terrain-selection 3 10)]
                                :modifiers [(mod5e/dependent-trait
                                             {:name "Natural Explorer"
                                              :page 91
                                              :summary (let [favored-terrain ?ranger-favored-terrain
                                                             one-terrain? (= 1 (count favored-terrain))]
                                                        (str "your favored terrain " (if one-terrain? "type is" "types are") " " (if (seq favored-terrain) (common/list-print (map #(common/kw-to-name % false) ?ranger-favored-terrain)) "not selected") ". When you make an Intelligence or Wisdom check related to your favored terrain, your proficiency bonus is doubled if you are using a skill that you’re proficient in."
                                                             "\n\nWhile traveling for an hour or more in your favored terrain, you gain the following benefits:"
                                                             "\n\u2022 Difficult terrain doesn’t slow your group’s travel."
                                                             "\n\u2022 Your group can’t become lost except by magical means."
                                                             "\n\u2022 Even when you are engaged in another activity while traveling (such as foraging, navigating, or tracking), you remain alert to danger."
                                                             "\n\u2022 If you are traveling alone, you can move stealthily at a normal pace."
                                                             "\n\u2022 When you forage, you find twice as much food as you normally would."
                                                             "\n\u2022 While tracking other creatures, you also learn their exact number, their sizes, and how long ago they passed through the area."))})]})
                              (t/option-cfg
                               {:name "Deft Explorer"
                                :order 2
                                :selections [(opt5e/language-selection-aux (vals language-map) 2)
                                             (opt5e/expertise-selection 1)]
                                :modifiers [(mod/cum-sum-mod ?speed (if (>= (?class-level :ranger) 6) 5 0))
                                            (mod/vec-mod ?swimming-speed-overrides (if (>= (?class-level :ranger) 6) ?speed 0))
                                            (mod/vec-mod ?climbing-speed-overrides (if (>= (?class-level :ranger) 6) ?speed 0))
                                            (mod5e/trait-cfg
                                             {:name "Roving"
                                              :level 6
                                              :summary "Your walking speed increases by 5, and you gain a climbing speed and a swimming speed equal to your walking speed"})
                                            (mod5e/trait-cfg
                                             {:name "Tireless"
                                              :level 10
                                              :summary "whenever you finish a short rest, your exhaustion level, if any, is decreased by 1"})
                                            (mod5e/action
                                             {:name "Tireless"
                                              :level 10
                                              :frequency (units5e/long-rests ?prof-bonus)
                                              :summary "As an action, you can give yourself a number of temporary hit points equal to 1d8 + your Wisdom modifier (minimum of 1 temporary hit point)"})]})]})]
     :levels {2 {:selections [(opt5e/fighting-style-selection :ranger #{:archery :blind-fighting :defense :druidic-warrior :dueling :thrown-weapon-fighting :two-weapon-fighting})]}
              3 {:selections [(t/selection-cfg
                               {:name "Awareness"
                                :tags #{:class}
                                :options [(t/option-cfg
                                           {:name "Primeval Awareness"
                                            :order 1
                                            :modifiers [(mod5e/action
                                                          {:name "Primeval Awareness"
                                                          :level 3
                                                          :page 92
                                                          :summary "you can use your action and expend one ranger spell slot to focus your awareness on the region around you. For 1 minute per level of the spell slot you expend, you can sense whether the following types of creatures are present within 1 mile of you (or within up to 6 miles if you are in your favored terrain): aberrations, celestials, dragons, elementals, fey, fiends, and undead. This feature doesn’t reveal the creatures’ location or number"})]})
                                            (t/option-cfg
                                             {:name "Primal Awareness"
                                              :order 2
                                              :modifiers [(mod5e/spells-known 1 :speak-with-animals ::char5e/wis "Ranger (Primal Awareness)" 3)
                                                          (mod5e/spells-known 2 :beast-sense ::char5e/wis "Ranger (Primal Awareness)" 5)
                                                          (mod5e/spells-known 3 :speak-with-plants ::char5e/wis "Ranger (Primal Awareness)" 9)
                                                          (mod5e/spells-known 4 :locate-creature ::char5e/wis "Ranger (Primal Awareness)" 13)
                                                          (mod5e/spells-known 5 :commune-with-nature ::char5e/wis "Ranger (Primal Awareness)" 17)
                                                          (mod5e/dependent-trait
                                                           {:name "Primal Awareness"
                                                            :summary (str "You learn the spell" (if (>= (?class-level :ranger) 5) "s")
                                                                      (common/list-print
                                                                        (let [lvl (?class-level :ranger)]
                                                                          (cond-> []
                                                                            (>= lvl 3) (conj " Speak with Animals")
                                                                            (>= lvl 5) (conj "Beast Sense")
                                                                            (>= lvl 9) (conj "Speak with Plants")
                                                                            (>= lvl 13) (conj "Locate Creature")
                                                                            (>= lvl 17) (conj "Commune with Nature"))))
                                                                      " as " (if (< (?class-level :ranger) 5) "a ranger spell and can cast it" "ranger spells and can cast each one") " once per long rest without expending a spell slot.")})]})]})]}
              5 {:modifiers [(mod5e/num-attacks 2)]}
              10 {:selections [(t/selection-cfg
                                {:name "Hide in Plain Sight/Nature's Veil"
                                 :tags #{:class}
                                 :options [(t/option-cfg
                                            {:name "Hide in Plain Sight"
                                             :order 1
                                             :modifiers [(mod5e/trait-cfg
                                                          {:name "Hide in Plain Sight"
                                                           :page 92
                                                           :summary (str "you can spend 1 minute creating camouflage for yourself. You must have access to fresh mud, dirt, plants, soot, and other naturally occurring materials with which to create your camouflage"
                                                                         "\n\nOnce you are camouflaged in this way, you can try to hide by pressing yourself up against a solid surface, such as a tree or wall, that is at least as tall and wide as you are. You gain a +10 bonus to Dexterity (Stealth) checks as long as you remain there without moving or taking actions. Once you move or take an action or a reaction, you must camouflage yourself again to gain this benefit")})]})
                                           (t/option-cfg
                                            {:name "Nature's Veil"
                                             :order 2
                                             :modifiers [(mod5e/bonus-action
                                                          {:name "Nature's Veil"
                                                           :frequency (units5e/long-rests ?prof-bonus)
                                                           :summary "You draw on the powers of nature to hide yourself from view briefly. As a bonus action, you can magically become invisible, along with any equipment you are wearing or carrying, until the start of your next turn"})]})]})]}
              14 {:modifiers [(mod5e/bonus-action
                               {:name "Vanish"
                                :page 92
                                :summary "you can use the Hide action as a bonus action on your turn. Also, you can't be tracked by nonmagical means, unless you choose to leave a trail"})]}
              20 {:modifiers [(mod5e/dependent-trait
                               {:name "Foe Slayer"
                                :frequency units5e/turns-1
                                :level 20
                                :page 92
                                :summary "you become an unparalleled hunter of your enemies. Once on each of your turns, you can add your Wisdom modifier to the attack roll or the damage roll of an attack you make against one of your favored enemies. You can choose to use this feature before or after the roll, but before any effects of the roll are applied"})]}}
     :traits [{:name "Spellcasting Focus"
               :level 2
               :summary "You can use a druidic focus as a spellcasting focus for your ranger spells. A druidic focus might be a sprig of mistletoe or holly, a wand or rod made of yew or another special wood, a staff drawn whole from a living tree, or an object incorporating feathers, fur, bones, and teeth from sacred animals."}
              {:name "Martial Versatility"
               :level 4
               :summary "When you reach level 4, 8, 12, 16, and 19 in ranger, you can replace one fighting style from the class"}
              (lands-stride 8)
              {:name "Feral Senses"
               :level 18
               :page 92
               :summary (str "you gain preternatural senses that help you fight creatures you can't see. When you attack a creature you can't see, your inability to see it doesn't impose disadvantage on your attack rolls against it."
                             "\n\nYou are also aware of the location of any invisible creature within 30 feet of you, provided that the creature isn't hidden from you and you aren't blinded or deafened")}]
     :subclasses [{:name "Hunter"
                   :levels {3 {:selections [(t/selection-cfg
                                             {:name "Hunter's Prey"
                                              :tags #{:class}
                                              :options [(t/option-cfg
                                                         {:name "Colossus Slayer"
                                                          :modifiers [(mod5e/trait-cfg
                                                                       {:name "Colossus Slayer"
                                                                        :page 93
                                                                        :frequency units5e/turns-1
                                                                        :summary "Your tenacity can wear down the most potent foes. When you hit a creature with a weapon attack, the creature takes an extra 1d8 damage if it’s below its hit point maximum. You can deal this extra damage only once per turn"})]})
                                                        (t/option-cfg
                                                         {:name "Giant Killer"
                                                          :modifiers [(mod5e/reaction
                                                                       {:name "Giant Killer"
                                                                        :page 93
                                                                        :frequency units5e/turns-1
                                                                        :summary "When a Large or larger creature within 5 feet of you hits or misses you with an attack, you can use your reaction to attack that creature immediately after its attack, provided that you can see the creature"})]})
                                                        (t/option-cfg
                                                         {:name "Horde Breaker"
                                                          :modifiers [(mod5e/trait-cfg
                                                                       {:name "Horde Breaker"
                                                                        :page 93
                                                                        :frequency units5e/turns-1
                                                                        :summary "Once on each of your turns when you make a weapon attack, you can make another attack with the same weapon against a different creature that is within 5 feet of the original target and within range of your weapon"})]})]})]}
                            7 {:selections [(t/selection-cfg
                                             {:name "Defensive Tactics"
                                              :tags #{:class}
                                              :options [(t/option-cfg
                                                         {:name "Escape the Horde"
                                                          :modifiers [(mod5e/trait-cfg
                                                                       {:name "Escape the Horde"
                                                                        :frequency units5e/turns-1
                                                                        :page 93
                                                                        :summary "Opportunity attacks against you are made with disadvantage"})]})
                                                        (t/option-cfg
                                                         {:name "Multiattack Defense"
                                                          :modifiers [(mod5e/trait-cfg
                                                                       {:name "Multiattack Defense"
                                                                        :frequency units5e/turns-1
                                                                        :page 93
                                                                        :summary "When a creature hits you with an attack, you gain a +4 bonus to AC against all subsequent attacks made by that creature for the rest of the turn"})]})
                                                        (t/option-cfg
                                                         {:name "Steel Will"
                                                          :modifiers [(mod5e/saving-throw-advantage [:frightened])
                                                                      (mod5e/trait-cfg
                                                                       {:name "Steel Will"
                                                                        :page 93
                                                                        :summary "You have advantage on saving throws against being frightened"})]})]})]}
                            11 {:selections [(t/selection-cfg
                                              {:name "Multiattack"
                                               :tags #{:class}
                                               :options [(t/option-cfg
                                                          {:name "Volley"
                                                           :modifiers [(mod5e/action
                                                                        {:name "Volley"
                                                                         :page 93
                                                                         :summary "You can use your action to make a ranged attack against any number of creatures within 10 feet of a point you can see within your weapon’s range. You must have ammunition for each target, as normal, and you make a separate attack roll for each target"})]})
                                                         (t/option-cfg
                                                          {:name "Whirlwind Attack"
                                                           :modifiers [(mod5e/action
                                                                        {:name "Whirlwind Attack"
                                                                         :page 93
                                                                         :summary "You can use your action to make melee attacks against any number of creatures within 5 feet of you, with a separate attack roll for each target"})]})]})]}
                            15 {:selections [(t/selection-cfg
                                              {:name "Superior Hunter's Defense"
                                               :tags #{:class}
                                               :options [(t/option-cfg
                                                          {:name "Evasion"
                                                           :modifiers [(opt5e/evasion 15 93)
                                                                       (mod5e/trait-cfg
                                                                        {:page 93
                                                                         :summary "When you are subjected to an effect, such as a red dragon’s fiery breath or a lightning bolt spell, that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on a saving throw, and only half damage if you fail"})]})
                                                         (t/option-cfg
                                                          {:name "Stand Against the Tide"
                                                           :modifiers  [(mod5e/reaction
                                                                         {:name "Stand Against the Tide"
                                                                          :page 93
                                                                          :summary "When a hostile creature misses you with a melee attack, you can use your reaction to force that creature to repeat the same attack against another creature (other than itself) of your choice"})]})
                                                         (t/option-cfg
                                                          {:name "Uncanny Dodge"
                                                           :modifiers [(opt5e/uncanny-dodge-modifier 93)
                                                                       (mod5e/trait-cfg {:name "Uncanny Dodge"
                                                                                         :page 93
                                                                                         :summary "When an attacker that you can see hits you with an attack, you can use your reaction to halve the attack’s damage against you"})]})]})]}}}
                  {:name "Drakewarden"
                   :modifiers [(mod5e/spells-known 0 :thaumaturgy ::char5e/wis "Drakewarden")
                               (mod5e/language :draconic)
                               (mod5e/dependent-trait
                                 {:name "Drake Companion"
                                  :summary (str "as an action, you can magically summon the drake that is bound to you. It appears in an unoccupied space of your choice within 30 feet of you."
                                                "\n\nThe drake is friendly to you and your companions, and it obeys your commands. Whenever you summon the drake, choose a damage type listed in its Draconic Essence trait."
                                                "\n\nIn combat, the drake shares your initiative count, but it takes its turn immediately after yours. It can move and use its reaction on its own, but the only action it takes on its turn is the Dodge action, unless you take a bonus action on your turn to command it to take another action. That action can be one in its stat block or some other action. If you are incapacitated, the drake can take any action of its choice, not just Dodge."
                                                "\n\nThe drake remains until it is reduced to 0 hit points, until you use this feature to summon the drake again, or until you die. Anything the drake was wearing or carrying is left behind when the drake vanishes."
                                                "\n\nOnce you summon the drake, you can’t do so again until you finish a long rest, unless you expend a spell slot of 1st level or higher to summon it.")})
                               #_(mod5e/action
                                {:name "Summon Drake"
                                 :frequency units5e/long-rests-1
                                 :summary "Magically summon the bounded drake in an unoccupied space within 30 ft., and choose a Draconic Essence damage type. Can expend a spell splot to summon it again"})
                               #_(mod5e/bonus-action
                                {:name "Command Drake"
                                 :summary "Command the drake to take an action other than dodge"})
                               #_(mod5e/dependent-trait
                                {:name "Drake Companion"
                                 :summary "The drake is friendly to you and your companions, and it obeys your commands.\n\nIn combat, the drake shares your initiative count, but it takes its turn immediately after yours. It can move and use its reaction on its own, but the only action it takes on its turn is the Dodge action. If you are incapacitated, the drake can take any action of its choice, not just Dodge.\n\nThe drake remains until it is reduced to 0 hit points, until you use this feature to summon the drake again, or until you die. Anything the drake was wearing or carrying is left behind when the drake vanishes."})
                               (mod5e/dependent-trait
                                {:name "Drake Statblock"
                                 :summary (str (condp <= (?class-level :ranger) 15 "Large"  7 "Medium"  "Small") " dragon\n"
                                           "Armor Class: " (+ 14 ?prof-bonus)
                                           "\nHit Points: " (+ 5 (* 5 (?class-level :ranger))) ", " (?class-level :ranger) "d10 hit dice"
                                           "\nSpeed: 40 ft.\n"
                                           "\nStr: 16 (+3)  Dex: 12 (+1)  Con: 15 (+2)  Int: 8 (-1)   Wis: 14 (+2)  Cha: 8 (-1)"
                                           "\nSaving Throws: Dex: +" (+ 1 ?prof-bonus) "  Wis: +" (+ 2 ?prof-bonus)
                                           "\nDamage Immunities: Draconic Essence trait"
                                           "\nSenses: darkvision 60ft., passive Perception 12"
                                           "\nLanguages: Draconic"
                                           "\nDraconic Essence: When you summon the drake, choose a damage type: acid, cold, fire, lightning, or poison.\n"
                                           "\nActions:\nBite. Melee Weapon Attack: +" (+ 3 ?prof-bonus) " to hit, reach 5 ft., one target. Hit: 1d6+" ?prof-bonus "piercing damage.\n"
                                           "\nReactions:\nInfused Strikes. When another creature within 30 ft. of the drake that it can see hits a target with a weapon attack, the drake causes the target to take an extra 1d6 damage of the type determined by its Draconic Essence.")})]
                   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
                   :levels {7 {:modifiers [(mod5e/trait-cfg
                                            {:name "Bond of Fang and Scale"
                                             :summary (str "the bond you share with your drake intensifies, protecting you and stoking the drake’s fury. When you summon your drake, it grows wings on its back and gains a flying speed equal to its walking speed."
                                                           "\n\nIn addition, while your drake is summoned, you and the drake gain the following benefits:"
                                                           "\nDrake Mount. The drake grows to Medium size. Reflecting your special bond, you can use the drake as a mount if your size is Medium or smaller. While you are riding your drake, it can’t use the flying speed of this feature."
                                                           "\nMagic Fang. The drake’s Bite attack deals an extra 1d6 damage of the type chosen for the drake’s Draconic Essence."
                                                           "\nResistance. You gain resistance to the damage type chosen for the drake’s Draconic Essence.")})]}
                            11 {:modifiers [(mod5e/action
                                             {:name "Drake's Breath"
                                              :summary (str "as an action, you can exhale a 30-foot cone of damaging breath or cause your drake to exhale it. Choose acid, cold, fire, lightning, or poison damage (your choice doesn’t have to match your drake’s Draconic Essence). Each creature in the cone must make a Dexterity saving throw against your spell save DC, taking " (condp <= (?class-level :ranger) 15 "10d6"  "8d6") " damage on a failed save, or half as much damage on a successful one."
                                                            "\n\nOnce you use this feature, you can’t do so again until you finish a long rest, unless you expend a spell slot of 3rd level or higher to use it again")})]}
                            15 {:modifiers [(mod5e/trait-cfg
                                             {:name "Perfected Bond"
                                              :summary (str "Empowered Bite. The drake’s Bite attack deals an extra 1d6 damage of the type chosen for its Draconic Essence (for a total of 2d6 extra damage)."
                                                            "\nLarge Drake. The drake grows to Large size. When you ride your drake, it is no longer prohibited from using the flying speed of Bond of Fang and Scale.")})
                                            (mod5e/reaction
                                             {:name "Reflexive Resistance"
                                              :frequency (units5e/long-rests ?prof-bonus)
                                              :summary "When either you or the drake takes damage while you’re within 30 feet of each other, you can use your reaction to give yourself or the drake resistance to that instance of damage"})]}}}
                  ;; (condp <= (?class-level :ranger) 15 "Large"  7 "Medium"  "Small") " dragon\n"
                  ;;                          "Armor Class: " (+ 14 ?prof-bonus)
                                          ;;  "\nHit Points: " (+ 5 (* 5 (?class-level :ranger))) ", " (?class-level :ranger) "d10 hit dice"
                                          ;;  "\nSpeed: 40 ft.\n"
                                          ;;  "\nStr: 16 (+3)  Dex: 12 (+1)  Con: 15 (+2)  Int: 8 (-1)   Wis: 14 (+2)  Cha: 8 (-1)\n"
                                          ;;  "Saving Throws: Dex: +" (+ 1 ?prof-bonus) "  Wis: +" (+ 2 ?prof-bonus)
                                          ;;  "\nDamage Immunities: Draconic Essence trait"
                                          ;;  "\nSenses: darkvision 60ft., passive Perception 12"
                                          ;;  "\nLanguages: Draconic"
                                          ;;  "\nDraconic Essence: When you summon the drake, choose a damage type: acid, cold, fire, lightning, or poison.\n"
                                          ;;  "\nActions:\nBite. Melee Weapon Attack: +" (+ 3 ?prof-bonus) " to hit, reach 5 ft., one target. Hit: 1d6 + " (?prof-bonus) "piercing damage.\n"
                                          ;;  "\nReactions:\nInfused Strikes. When another creature within 30 ft. of the drake that it can see hits a target with a weapon attack, the drake causes the target to take an extra 1d6 damage of the type determined by its Draconic Essence."
                  
                  {:name "Beast Master"
                     :selections [(t/selection-cfg
                                   {:name "Companion Selection"
                                    :tags #{:class}
                                    :options [(t/option-cfg
                                               {:name "Ranger's Companion"
                                                :order 1
                                                :selections [(t/selection-cfg
                                                              {:name "Ranger's Companion"
                                                                :tags #{:class}
                                                                :options (map
                                                                          (fn [monster-name]
                                                                            (t/option-cfg
                                                                            {:name monster-name
                                                                              :modifiers [(mod5e/action
                                                                                          {:name "Ranger's Companion"
                                                                                            :page 93
                                                                                            :summary (str "You have a " monster-name " as your companion, you can command it to Attack, Dash, Disengage, Dodge, or Help")})]}))
                                                                          ["Stirge" "Baboon" "Bat" "Badger" "Blood Hawk" "Boar" "Cat" "Crab" "Deer" "Eagle" "Flying Snake" "Frog" "Giant Badger" "Giant Centipede" "Giant Crab" "Giant Fire Beetle" "Giant Frog" "Giant Poisonous Snake" "Giant Rat" "Giant Wolf Spider" "Goat" "Hawk" "Hyena" "Jackal" "Lizard" "Mastiff" "Mule" "Octopus" "Panther" "Owl" "Poisonous Snake" "Pony" "Quipper" "Rat" "Raven" "Scorpion" "Sea Horse" "Spider" "Vulture" "Weasel" "Wolf"])})]})
                                              (t/option-cfg
                                               {:name "Primal Companion"
                                                :order 2
                                                :modifiers [(mod5e/trait-cfg
                                                             {:name "Primal Companion"
                                                              :summary (str "You magically summon a primal beast, which draws strength from your bond with nature. The beast is friendly to you and your companions and obeys your commands. Choose its stat block-Beast of the Land, Beast of the Sea, or Beast of the Sky-which uses your proficiency bonus (PB) in several places. You also determine the kind of animal the beast is, choosing a kind appropriate for the stat block. Whatever kind you choose, the beast bears primal markings, indicating its mystical origin."
                                                                            "\n\nIn combat, the beast acts during your turn. It can move and use its reaction on its own, but the only action it takes is the Dodge action, unless you take a bonus action on your turn to command it to take another action. That action can be one in its stat block or some other action. You can also sacrifice one of your attacks when you take the Attack action to command the beast to take the Attack action. If you are incapacitated, the beast can take any action of its choice, not just Dodge."
                                                                            "\n\nIf the beast has died within the last hour, you can use your action to touch it and expend a spell slot of 1st level or higher. The beast returns to life after 1 minute with all its hit points restored. When you finish a long rest, you can summon a different primal beast. The new beast appears in an unoccupied space within 5 feet of you, and you choose its stat block and appearance. If you already have a beast from this feature, it vanishes when the new beast appears. The beast also vanishes if you die.")})]})]})]
                     :levels {7 {:modifiers [(mod5e/bonus-action
                                              {:name "Exceptional Training"
                                               :page 93
                                               :level 7
                                               :summary "on any of your turns when your beast companion doesn’t attack, you can use a bonus action to command the beast to take the Dash, Disengage, or Help action on its turn. In addition, the beast’s attacks now count as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage"})]}}
                     :traits [{:name "Bestial Fury"
                               :level 11
                               :page 93
                               :summary "when you command your beast companion to take the Attack action, the beast can make two attacks, or it can take the Multiattack action if it has that action"}
                              {:name "Share Spells"
                               :level 15
                               :page 93
                               :summary "when you cast a spell targeting yourself, you can also affect your beast companion with the spell if the beast is within 30 feet of you"}]}]})))

(defn arcane-trickster-spell? [s]
    (let [school (:school s)]
      (or (= school "enchantment")
          (= school "illusion"))))

(defn arcane-trickster-ref [subclass-key subpath]
    (concat
     [:class :rogue :levels :level-3 :roguish-archetype subclass-key]
     subpath))

(defn arcane-trickster-spell-selection [num spell-levels]
  (subclass-wizard-spell-selection sl5e/spell-lists
                                   spells5e/spell-map
                                   "Rogue Enchantment or Illusion Spells"
                                     (arcane-trickster-ref :arcane-trickster [:enchantment-or-illusion-spells-known])
                                     :rogue
                                     "Rogue"
                                     num
                                     spell-levels
                                     arcane-trickster-spell?))

(defn arcane-trickster-any-spell-selection [num spell-levels]
  (subclass-wizard-spell-selection sl5e/spell-lists
                                   spells5e/spell-map
                                   "Rogue Spells: Any School"
                                     (arcane-trickster-ref :arcane-trickster [:spells-known-any-school])
                                     :rogue
                                     "Rogue"
                                     num
                                     spell-levels))

(defn arcane-trickster-cantrip [num]
  (opt5e/spell-selection sl5e/spell-lists
                         spells5e/spell-map
                         {:class-key :rogue
                          :level 0
                          :ref (arcane-trickster-ref :arcane-trickster [:cantrips-known])
                          :spellcasting-ability ::char5e/int
                          :class-name "Rogue"
                          :num num
                          :spell-keys (get-in sl5e/spell-lists [:wizard 0])}))

(def rogue-skills {:acrobatics true :athletics true :deception true :insight true :intimidation true :investigation true :perception true :performance true :persuasion true :sleight-of-hand true :stealth true})

(defn rogue-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Rogue",
    :key :rogue
    :hit-die 8
    :ability-increase-levels [4 8 10 12 16 19]
    :expertise true
    :profs {:armor {:light false}
            :weapon {:simple true :crossbow-hand true :longsword true :rapier true :shortsword true}
            :save {::char5e/dex true ::char5e/int true}
            :tool {:thieves-tools false}
            :skill-options {:order 0 :choose 4 :options rogue-skills}
            :multiclass-skill-options {:order 0 :choose 1 :options rogue-skills}}
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/dex 13)]
    :weapon-choices [{:name "Melee Weapon"
                      :options {:rapier 1
                                :shortsword 1}}]
    :armor {:leather 1}
    :weapons {:dagger 2}
    :equipment {:thieves-tools 1}
    :equipment-choices [{:name "Equipment Pack"
                         :options {:burglars-pack 1
                                   :dungeoneers-pack 1
                                   :explorers-pack 1}}]
    :modifiers [(mod5e/dependent-trait
                 {:name "Sneak Attack"
                  :page 96
                  :frequency units5e/turns-1
                  :summary (str (common/round-up (/ (?class-level :rogue) 2)) "d6 extra damage on attack where you have advantage or another enemy of creature is within 5 ft.")
                  })
                (mod5e/bonus-action
                 {:name "Steady Aim"
                  :summary "Give yourself advantage on an attack if you don't move during the turn."
                  })]
    :levels {2 {:modifiers [(mod5e/bonus-action
                             {:level 2
                              :name "Cunning Action"
                              :page 96
                              :frequency units5e/turns-1
                              :summary "as a bonus action you can Dash, Disengage or Hide"
                              })]}
             5 {:modifiers [(opt5e/uncanny-dodge-modifier 96)]}
             6 {:selections [(assoc
                              opt5e/rogue-expertise-selection
                              ::t/order
                              1)]}
             15 {:modifiers [(mod5e/saving-throws nil ::char5e/wis)]}}
    :selections [(opt5e/new-starting-equipment-selection
                  :rogue
                  {:name "Additional Weapon"
                   :options [(t/option-cfg
                              {:name "Shortbow, Quiver, 20 Arrows"
                               :modifiers [(mod5e/weapon :shortbow 5)
                                           (mod5e/equipment :quiver 1)
                                           (mod5e/equipment :arrow 20)]})
                             (t/option-cfg
                              {:name "Shortsword"
                               :modifiers [(mod5e/weapon :shortsword 1)]})]})
                 (assoc
                  opt5e/rogue-expertise-selection
                  ::t/order
                  1)]
    :traits [{:name "Thieves' Cant"
              :page 96
              :summary "convey secret messages hidden in normal conversation"
              }
             (opt5e/evasion 7 96)
             {:level 11
              :name "Reliable Talent"
              :page 96
              :summary "when you make an ability check with proficiency, treat a roll less than 10 as a 10"
              }
             {:level 14
              :name "Blindsense"
              :page 96
              :summary "know location of hidden or invisible creatures within 10 ft."
              }
             {:level 18
              :name "Elusive"
              :page 96
              :summary "no attack roll has advantage against you while you aren’t incapacitated."
              }
             {:level 20
              :name "Stroke of Luck"
              :page 97
              :frequency units5e/rests-1
              :summary "turn missed attack into a hit or a failed ability check roll as 20"
              }]
    :subclass-level 3
    :subclass-title "Roguish Archetype"
    :subclasses [{:name "Thief"
                  :modifiers [(mod5e/bonus-action
                               {:level 3
                                :name "Fast Hands"
                                :page 96
                                :summary "use your Cunning Action to make Sleight of Hand checks, use thieves' tools, or take Use and Object action"
                                })
                              (mod5e/dependent-trait
                               {:level 3
                                :name "Second-Story Work"
                                :page 97
                                :summary (str "climbing costs no extra movement, your running jump distance increases by " (?ability-bonuses ::char5e/dex) " ft.")
                                })]
                  :traits [{:level 9
                            :name "Supreme Sneak"
                            :page 97
                            :summary "advantage on Stealth checks if you move no more than half your speed"}
                           {:level 13
                            :name "Use Magic Device"
                            :page 97
                            :summary "ignore race, class, level requirements to use magic items"}
                           {:level 17
                            :name "Thief's Reflexes"
                            :page 97
                            :summary "when not surprised, take 2 turns in first round of combat, one at your normal initiative and the next at your initiative minus 10"}]}
                 {:name "Assassin"
                    :profs {:tool {:disguise-kit true :poisoners-kit true}}
                    :levels {9 {:modifiers [(mod5e/tool-expertise :poisoners-kit)]} ;;homebrew
                             17 {:modifiers [(mod5e/dependent-trait
                                              {:name "Death Strike"
                                               :level 17
                                               :page 97
                                               :summary (str "double damage against a surpised creature if it fails a DC " (?spell-save-dc ::char5e/dex) " CON save")})]}}
                    :traits [{:name "Assassinate"
                              :level 3
                              :page 97
                              :summary "advantage on attack against creatures that haven't taken a turn yet. Hits against surprised creatures are critical"}
                             {:name "Infiltration Expertise"
                              :level 9
                              :page 97
                              :summary "spend 25 gp and 7 days to establish a false identity, which can't be someone else"}
                             {:name "Impostor"
                              :level 13
                              :page 97
                              :summary "spend 3 hours studying to accurately mimic the behavior, speech, and writing of another person. Advantage on Deception checks to avoid detection of this"}
                             {:name "Poison Expert"
                              :level 13
                              :summary "When extracting or making poison, treat any roll lower than 10 as a 10"}]}
                 {:name "Arcane Trickster"
                    :spellcasting {:level-factor 3}
                    :modifiers [(mod5e/spells-known 0 :mage-hand ::char5e/int "Arcane Trickster")]
                    :levels {3 {:selections [(arcane-trickster-cantrip 2)
                                             (arcane-trickster-spell-selection 2 [1])
                                             (arcane-trickster-any-spell-selection 1 [1])]}
                             4 {:selections [(arcane-trickster-spell-selection 1 [1])]}
                             7 {:selections [(arcane-trickster-spell-selection 1 [1 2])]}
                             8 {:selections [(arcane-trickster-any-spell-selection 1 [1 2])]}
                             10 {:selections [(arcane-trickster-cantrip 1)
                                              (arcane-trickster-spell-selection 1 [1 2])]}
                             11 {:selections [(arcane-trickster-spell-selection 1 [1 2])]}
                             13 {:selections [(arcane-trickster-spell-selection 1 [1 2 3])]
                                 :modifiers [(mod5e/bonus-action
                                              {:name "Versatile Trickster"
                                               :level 13
                                               :page 98
                                               :summary "use mage hand to gain advantage on attack rolls against a creature within 5 ft. of the hand"})]}
                             14 {:selections [(arcane-trickster-any-spell-selection 1 [1 2 3])]}
                             16 {:selections [(arcane-trickster-spell-selection 1 [1 2 3])]}
                             17 {:modifiers [(mod5e/reaction
                                              {:name "Spell Thief"
                                               :level 17
                                               :page 98
                                               :frequency units5e/long-rests-1
                                               :summary (str "Negate a spells effect against you if the spellcaster fails a DC " (?spell-save-dc ::char5e/int) " save with its spellcasting ability. Steal the spell if it's a 1st level or higher, letting you cast it using your spell slots for 8 hours. The creature can't cast that spell again until the 8 hours have passed")})]}
                             19 {:selections [(arcane-trickster-spell-selection 1 [1 2 3 4])]}
                             20 {:selections [(arcane-trickster-any-spell-selection 1 [1 2 3 4])]}}
                    :traits [{:name "Mage Hand Legerdemain"
                              :level 3
                              :page 98
                              :summary "when you cast mage hand, you can make it invisible and perform Sleight of Hand tasks, and can control it using your Cunning Action bonus action"}
                             {:name "Magical Ambush"
                              :level 9
                              :page 98
                              :summary "creatures have disadvantage on saves against your spells (only on the turn you cast them) if you are hidden from them"}]}
                 {:name "Inquisitive"
                  :modifiers [(mod5e/bonus-action
                               {:name "Eye for Detail"
                                :summary "Make a Perception check to stop a hidden creature or object or an Investigation check to uncover or decipher clues"})
                              (mod5e/bonus-action
                               {:name "Insightful Fighting"
                                :duration units5e/minutes-1
                                :summary "Make an Insight check against a creature you can see that isn't incapacitated, contested by the target's Deception check. If you succeed, you can use your Sneak Attack against that target even if you don't have advantage on the attack roll, but not if you have disadvantage on it. Ends if you successfully target another creature"})]
                  :levels {13 {:modifiers [(mod5e/action
                                            {:name "Unerring Eye"
                                             :frequency (units5e/long-rests (max 1 (?ability-bonuses ::char5e/wis)))
                                             :summary "You sense the presence of illusions, shapechangers not in their original form, and other magic designed to deceive the senses within 30 ft., provided you aren't blinded or deafened. You sense that an effect is attempting to trick you, but you gain no insight into what is hidden or into its true nature"})]}
                           17 {:modifiers [(mod5e/trait-cfg
                                            {:name "Eye for Weakness"
                                             :summary "Sneak Attack damage increases by 3d6 while your Insightful Fighting feature is applied"})]}}
                  :traits [{:name "Ear for Deceit"
                            :level 3
                            :summary "When making Insight checks to determine whether a creature is lying, treat a roll of 7 or lower on the d20 as an 8"}
                           {:name "Steady Eye"
                            :level 9
                            :summary "Advantage on Perception and Investigation checks if moving no more than half your speed on the turn"}]}
                 {:name "Mastermind"
                  :modifiers [(mod5e/tool-proficiency :disguise-kit)
                              (mod5e/tool-proficiency :forgery-kit)]
                  :selections [(opt5e/tool-selection (map :key equipment5e/gaming-sets) 1)
                               (opt5e/language-selection-aux (vals language-map) 2)]
                  :levels {3 {:modifiers [(mod5e/bonus-action
                                           {:name "Master of Tactics"
                                            :summary "You can use the help action as a bonus action. When you use the Help action to aid an ally in attacking a creature, the target of that attack can be within 30 ft. of you, rather than 5 ft., if the target can see or hear you"})]}
                           13 {:modifiers [(mod5e/reaction
                                            {:name "Misdirection"
                                             :summary "When targeted by an attack while a creature within 5 ft. is ganting cover against it, have the attack target that creature instead"})]}}
                  :traits [{:name "Master of Intrigue"
                            :level 3
                            :summary "You can unerringly mimic the speech patterns and accent of a creature that you hear speak for at least 1 minute, enabling you to pass yourself off as a native speaker of a particular land, provided that you know the language"}
                           {:name "Insightful Manipulator"
                            :level 9
                            :summary "If you spend at least 1 minute observing or interacting with a creature outside combat, you can learn if it is equal, superior, or inferior in regard to two characteristics: int, wis, cha, class levels. DM might also tell you a piece of their history or one pf its personality traits"}
                           {:name "Soul of Deceit"
                            :level 17
                            :summary (str "Your thoughts can't be read by telepathy or other means, unless you allow it. You can present false thoughts by making a Deception check contested by the mind reader's Insight check.\n"
                                      "\nNo matter what you say, magic that would determine if you are telling the truth indicates you are being truthful if you so choose, and you can't be compelled to tell the truth by magic")}]}
                 {:name "Scout"
                  :modifiers [(mod5e/skill-proficiency :nature)
                              (mod5e/skill-proficiency :survival)
                              (mod5e/skill-expertise :nature)
                              (mod5e/skill-expertise :survival)
                              (mod5e/reaction
                               {:name "Skirmisher"
                                :summary "Move up to half your speed when an enemy ends its turn within 5 ft. of you, without provoking opportunity attacks"})]
                  :levels {9 {:modifiers [(mod5e/speed 10)
                                          (mod5e/climbing-speed 10) ;;should be only if has climbing/swimming speed
                                          (mod5e/swimming-speed 10)]}
                           17 {:modifiers [(mod5e/bonus-action
                                            {:name "Sudden Strike"
                                             :summary "Make one additional attack if you take the Attack action on your turn, which can benefit from sneak attack even if you've already used it, but can't be used against the same target"})]}}
                  :traits [{:name "Ambush Master"
                            :level 13
                            :summary "You have advantage on initiative. Attack rolls against the first creature you hit on the first round of combat have advantage until the start of your next turn"}]
                  }
                 {:name "Swashbuckler"
                  :modifiers [(mod5e/dependent-trait
                               {:name "Rakish Audacity"
                                :summary (str "You can give yourself a " (common/bonus-str (?ability-bonuses ::char5e/cha)) " bonus to your initiative rolls.\n"
                                          "\nYou don't need advantage on the attack roll to use your Sneak Attack against a creature if you are within 5 ft. of it, no other creatures are within 5 feet of you, and you don't have disadvantage on the attack roll")})]
                  :levels {9 {:modifiers [(mod5e/action
                                           {:name "Panache"
                                            :summary (str "Make a Persuasion check contested by a creature's Insight check that can hear you and share's a language.\n"
                                                      "\nIf the creature is hostile to you, it has disadvantage on attack rolls against targets other than you and can't make opportunity attacks against targets other than you. Lasts for 1 minute, until one of your companions attacks it or affects it with a spell, or until you are more than 60 ft. apart.\n"
                                                      "\nIf the creature is isn't hostile to you, it is charmed for 1 minute. While charmed, it regards you as a friendly acquaintance. Ends immediately if you or your companions do anything harmful to it.")})]}
                           13 {:modifiers [(mod5e/bonus-action
                                            {:name "Elegant Maneuver"
                                             :summary "Gain advantage on the next Acrobatics or Athletics check you make during the same turn"})]}
                           17 {:modifiers [(mod5e/trait-cfg
                                            {:name "Master Duelist"
                                             :frequency units5e/rests-1
                                             :summary "If you miss with an attack roll, you can roll it again with advantage"})]}}
                  :traits [{:name "Fancy Footwork"
                            :level 3
                            :summary "During your turn, if you make a melee attack against a creature, that creature can't make opportunity attacks against you for the rest of your turn."}]}
                 ]}))

(defn metamagic-selection [num]
  (t/selection-cfg
   {:name "Metamagic"
    :tags #{:class}
    :min num
    :max num
    :ref [:class :sorcerer :metamagic]
    :options [(t/option-cfg
               {:name "Careful Spell"
                :modifiers [(mod5e/dependent-trait
                             {:name "Careful Spell"
                              :page 102
                              :class-key :sorcerer
                              :summary (str "When you cast a spell that requires a save, spend 1 sorcery pt. to allow up to " (?ability-bonuses ::char5e/cha) " creatures to automatically succeed")})]})
              (t/option-cfg
               {:name "Distant Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Distant Spell"
                              :page 102
                              :summary "spend 1 sorcery pt. double the range of a spell with range 5 ft. or greater or make the range of a touch spell 30 ft."})]})
              (t/option-cfg
               {:name "Empowered Spell"
                :modifiers [(mod5e/dependent-trait
                             {:name "Empowered Spell"
                              :page 102
                              :summary (str "spend 1 sorcery pt. to reroll up to " (?ability-bonuses ::char5e/cha) " spell damage dice")})]})
              (t/option-cfg
               {:name "Extended Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Extended Spell"
                              :page 102
                              :summary "spend 1 sorcery pt. to double the duration of a spell to a max 24 hrs."})]})
              (t/option-cfg
               {:name "Heightened Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Heightened Spell"
                              :page 102
                              :summary "when you cast a spell with a save to resist it's effects, spend 3 sorcery pts. to give one target disadvantage on its first save against it"})]})
              (t/option-cfg
               {:name "Quickened Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Quickened Spell"
                              :page 102
                              :summary "spend 2 sorcery pts. to convert a casting of a spell with 1 action casting time to 1 bonus-action"})]})
              (t/option-cfg
               {:name "Seeking Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Seeking Spell"
                              :page 102
                              :summary "spend 2 sorcery pts. to reroll a spell you miss. You must use the new roll, and you can use Seeking Spell even if you already used a different Metamagic on the spell"})]})
              (t/option-cfg
               {:name "Subtle Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Subtle Spell"
                              :page 102
                              :summary "spend 1 sorcery pt. to cast a spell without somatic or verbal components"})]})
              (t/option-cfg
               {:name "Transmuted Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Transmuted Spell"
                              :page 102
                              :summary "spend 1 sorcery pt. to change the damage type of a spell from one of the following options to another: acid, cold, fire, lightning, poison, thunder"})]})
              (t/option-cfg
               {:name "Twinned Spell"
                :modifiers [(mod5e/trait-cfg
                             {:name "Twinned Spell"
                              :page 102
                              :summary "spend X sorcery pts. (min 1) to target two creatures with a single target spell, where X is the spell level"})]})]}))

(defn sorcerer-option [spells spells-map plugin-subclasses-map language-map weapon-map]
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Sorcerer"
    :key :sorcerer
    :spellcasting {:level-factor 1
                   :cantrips-known {1 4 4 1 10 1}
                   :known-mode :schedule
                   :spells-known full-caster-spells-known-schedule
                   :ability ::char5e/cha}
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/cha 13)]
    :spellcaster true
    :hit-die 6
    :ability-increase-levels [4 8 12 16 19]
    :profs {:weapon {:dagger true :dart true :sling true :quarterstaff true :crossbow-light true}
            :save {::char5e/con true ::char5e/cha true}
            :skill-options {:choose 2 :options {:arcana true :deception true :insight true :intimidation true :persuasion true :religion true}}}
    :selections [(opt5e/new-starting-equipment-selection
                  :sorcerer
                  {:name "Weapon"
                   :options [(t/option-cfg
                              {:name "Light Crossbow"
                               :modifiers [(mod5e/weapon :crossbow-light 1)
                                           (mod5e/equipment :crossbow-bolt 20)]})
                             (t/option-cfg
                              {:name "Simple Weapon"
                               :selections [(opt5e/new-starting-equipment-selection
                                             :sorcerer
                                             {:name "Simple Weapon"
                                              :options (opt5e/simple-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})]})]})]
    :levels {2 {:modifiers [(mod5e/dependent-trait
                             {:name "Sorcery Points"
                              :level 2
                              :page 101
                              :frequency (units5e/long-rests
                                           (?class-level :sorcerer))
                              :summary (str "You have " (?class-level :sorcerer) " sorcery points")})
                            (mod5e/bonus-action
                             {:name "Flexible Casting"
                              :level 2
                              :page 101
                              :summary "you can convert sorcery points into spell slots (level - point cost: 1st - 2, 2nd - 3, 3rd - 5, 4th - 6, 5th - 7). You can also convert spell slots into sorcery points equal to the slot's level"})]}
             3 {:selections [(metamagic-selection 2)]}
             4 {:selections [(mod5e/trait-cfg
                             {:name "Sorcerous Versatility"
                              :summary "When you reach an ASI in sorcerer, you can replace one Metamagic option and one cantrip from the class."})]}
             5 {:selections [(mod5e/trait-cfg
                              {:name "Magical Guidance"
                               :summary "When you make an ability check that fails, you can spend 1 sorcery point to reroll the d20, and you must use the new roll."})]}
             10 {:selections [(metamagic-selection 1)]}
             17 {:selections [(metamagic-selection 1)]}}
    :equipment-choices [{:name "Equipment Pack"
                         :options {:dungeoneers-pack 1
                                   :explorers-pack 1}}
                        {:name "Spellcasting Equipment"
                         :options {:component-pouch 1
                                   :arcane-focus 1}}]
    :weapons {:dagger 2}
    :subclass-title "Sorcerous Origin"
    :subclass-level 1
    :subclasses [{:name "Draconic Bloodline"
                  :modifiers [(mod/map-mod ?class-hit-point-level-bonus
                                           :sorcerer
                                           1)
                              (mod/modifier ?natural-ac-bonus 3)
                              (mod5e/language :draconic)]
                  :selections [(t/selection-cfg
                                {:name "Draconic Ancestry Type"
                                 :tags #{:class}
                                 :options (map
                                           (fn [{:keys [name] :as ancestry}]
                                             (t/option-cfg
                                              {:name name
                                               :modifiers [(mod/modifier ?sorcerer-draconic-ancestry ancestry)]}))
                                           opt5e/draconic-ancestries)})]
                  :traits [{:name "Draconic Resilience"
                            :page 102
                            :summary "+1 HP/level, unarmored AC 13 + DEX modifier"}
                           {:name "Dragon Ancestor"
                            :summary "Your proficiency bonus is doubled for Charisma checks when interacting with dragons."}]
                  :levels {6 {:modifiers [(mod5e/dependent-trait
                                           {:name "Elemental Affinity"
                                            :page 102
                                            :summary (str "Add CHA mod to one damage roll of a spell that deals "
                                                          (if ?sorcerer-draconic-ancestry
                                                            (str (common/safe-name
                                                                  (get-in
                                                                   ?sorcerer-draconic-ancestry
                                                                   [:breath-weapon :damage-type]))
                                                                 " damage")
                                                            "damage of type associated with your draconic ancestry")
                                                          ", you may also spend 1 sorcery pt. to gain resistance to that damage type for an hr.")})]}
                           14 {:modifiers [(mod5e/bonus-action
                                            {:name "Dragon Wings"
                                             :page 103
                                             :summary "Sprout wings and gain flying speed equal to current speed"})]}
                           18 {:modifiers [(mod5e/action
                                            {:name "Draconic Presence"
                                             :page 103
                                             :summary (str "Spend 5 sorcery pts. and create an aura that causes hostile creatures that start their turn within it to be charmed or frightened if they fail a DC " (?spell-save-dc ::char5e/cha) " Wisdom save.")})]}}}
                 {:name "Shadow Magic"
                  :modifiers [(mod5e/darkvision 120 1)
                              (mod5e/dependent-trait
                               {:name "Strength of the Grave"
                                :frequency units5e/long-rests-1
                                :summary (str "When damage reduces you to 0 HP, make a CHA Save (DC 5 + damage taken). On a success, you instead drop to 1 HP. You can't use this feature if you are reduced to 0 HP by radiant damage or by a critical hit."
                                              "\nAfter the saving throw succeeds, you can't use this feature again until you finish a long rest.")})]
                  :levels {3 {:modifiers [(mod5e/trait-cfg
                                           {:name "Eyes of the Dark"
                                            :summary "You know the darkness spell. Can also cast it using 2 sorcery points, in which case you can see through it."})]}
                           6 {:modifiers [(mod5e/bonus-action
                                            {:name "Hound of Ill Omen"
                                             :summary (str "Spend 3 sorcery points to summon a hound of ill omen to target one creature you can see within 120 ft. Uses the dire wolf's stats with the following changes:"
                                                           "\n- The hound is medium and a monstrosity."
                                                           "\n- Appears with " (/ (?class-level :sorcerer) 2) " temp HP."
                                                           "\n- Can move through other creatures and objects as if difficult terrain. Takes 5 force damage if it ends its turn inside an object."
                                                           "\n- At the start of its turn, the hound automatically knows its target's location. If the target was hidden, it is no longer hidden from the hound."
                                                           "\nThe hound appears within 30 ft. of the target. Roll initiative for the hound. On its turn, it can move only toward its target by the most direct route, and it can use its action only to attack its target. The hound can make opportunity attacks, but only against its target. Additionally, while the hound is within 5 ft. of the target, the target has disadvantage on saves against any spell you cast. Disappears if it is reduced to 0 HP, if its target is reduced to 0 HP, or after 5 min.")})]}
                           14 {:modifiers [(mod5e/bonus-action
                                            {:name "Shadow Walk"
                                             :summary "If in dim light or darkness, teleport up to 120 ft. to a space you can see within dim light or darkness."})]}
                           18 {:modifiers [(mod5e/bonus-action
                                            {:name "Umbral Form"
                                             :duration units5e/minutes-1
                                             :summary "Spend 6 sorcery points to transform into a shadowy form. Gain resistance to all damage except force and radiant, and move through other creatures and objects as if difficult terrain. Take 5 force damage if turn ends inside an object. Ends if incapacitated, you die, or dismissed as bonus action."})]}}}                     
                 {:name "Wild Magic"
                    :levels {6 {:modifiers [(mod5e/reaction
                                             {:name "Bend Luck"
                                              :level 6
                                              :page 103
                                              :summary "spend 2 sorcery pts. to add or subtract 1d4 from a creature you can see's attack roll, ability check, or saving throw"})]}}
                    :traits [{:name "Wild Magic Surge"
                              :level 1
                              :frequency units5e/turns-1
                              :summary "DM can have you roll a d20 check immediately after casting a leveled sorcerer spell. On a 1, roll on the Wild Magic Surge table"
                              :page 103}
                             {:name "Tides of Chaos"
                              :level 1
                              :summary "Gain advantage on an attack roll, ability check, or saving throw. Immediately after casting a leveled sorcerer spell, the DM can have you roll the the Wild Magic Surge table, regaining this feature"
                              :page 103
                              :frequency units5e/long-rests-1}
                             {:name "Controlled Chaos"
                              :level 14
                              :frequency units5e/turns-1
                              :page 103
                              :summary "When rolling on the Wild Magic Surge table, roll twice and use either roll"}
                             {:name "Spell Bombardment"
                              :level 18
                              :page 103
                              :summary "When you roll max on a die when rolling for spell damage, roll that die one additional time"
                              :frequency units5e/turns-1}]}]}))

(defn spell-school-savant [school page]
  {:level 2
   :name (str (s/capitalize school) " Savant")
   :page page
   :description (str "the gold and time you must spend to copy an " school " spell into your spellbook is halved")})

(defn spell-in-spells-known? [known level spell-key]
  (and known (some #(= spell-key (:key %)) (known level))))

(defn spell-mastery-selection [level]
  (t/selection-cfg
   {:name (str "Spell Mastery Level " level " Spell")
    :tags #{:spells}
    :options (map
              (fn [spell-kw]
                (let [{:keys [name] :as spell} (spells5e/spell-map spell-kw)]
                  (t/option-cfg
                   {:name name
                    :help (opt5e/spell-help spell)
                    :modifiers [(mod/set-mod ?spell-mastery name)]
                    :prereqs [(t/option-prereq
                               nil
                               (fn [c]
                                 (let [spells-known @(subscribe [::char5e/spells-known nil c])]
                                   (get-in spells-known [level ["Wizard" spell-kw]])))
                               true)]})))
              (get-in sl5e/spell-lists [:wizard level]))}))

(defn signature-spells-selection []
  (t/selection-cfg
   {:name "Signature Spells"
    :tags #{:spells}
    :min 2
    :max 2
    :options (map
              (fn [spell-kw]
                (let [{:keys [name] :as spell} (spells5e/spell-map spell-kw)]
                  (t/option-cfg
                   {:name name
                    :help (opt5e/spell-help spell)
                    :modifiers [(mod/set-mod ?signature-spells name)]
                    :prereqs [(t/option-prereq
                               nil
                               (fn [c]
                                 (let [spells-known @(subscribe [::char5e/spells-known nil c])]
                                   (get-in spells-known [3 ["Wizard" spell-kw]])))
                               true)]})))
              (get-in sl5e/spell-lists [:wizard 3]))}))

(defn bladesinging-weapon-options [weapons]
  (opt5e/weapon-proficiency-options
   (filter
    #(and (= nil (::weapon5e/two-handed? %)) (::weapon5e/melee? %))
    weapons)))

(defn bladesinging-weapon-prof-selection [weapon-map]
  (t/selection-cfg
   {:name "Weapon Proficiency"
    :tags #{:profs}
    :options (bladesinging-weapon-options (vals weapon-map))}))

(defn wizard-option [spells spells-map plugin-subclasses-map language-map weapon-map] 
  (opt5e/class-option
   spells
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   {:name "Wizard",
    :key :wizard
    :spellcasting {:level-factor 1
                   :cantrips-known {1 3 4 1 10 1}
                   :known-mode :acquire
                   :spells-known (zipmap (range 1 21) (cons 6 (repeat 2)))
                   :ability ::char5e/int
                   :prepares-spells? true}
    :spellcaster true
    :multiclass-prereqs [(opt5e/ability-prereq ::char5e/int 13)]
    :hit-die 6
    :ability-increase-levels [4 8 12 16 19]
    :equipment-choices [{:name "Equipment Pack"
                         :options {:scholars-pack 1
                                   :explorers-pack 1}}
                        {:name "Spellcasting Equipment"
                         :options {:component-pouch 1
                                   :arcane-focus 1}}]
    :weapon-choices [{:name "Melee Weapon"
                      :options {:quarterstaff 1
                                :dagger 1}}]
    :equipment {:spellbook 1}
    :profs {:weapon {:dagger true :dart true :sling true :quarterstaff true :crossbow-light true}
            :save {::char5e/int true ::char5e/wis true}
            :skill-options {:choose 2 :options {:arcana true :history true :insight true :investigation true :medicine true :religion true}}}
    :modifiers [(mod5e/dependent-trait
                 {:name "Arcane Recovery"
                  :page 115
                  :frequency units5e/days-1
                  :summary (str "Once per day when you finish a short rest, you can choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your wizard level (rounded up) (" (common/round-up (/ ?wizard-level 2)) "), and none of the slots can be 6th level or higher.")})]
    :levels {3 {:modifiers [(mod5e/trait-cfg
                             {:name "Cantrip Formulas"
                              :summary "You have scribed a set of arcane formulas in your spellbook that you can use to formulate a cantrip in your mind. Whenever you finish a long rest and consult those formulas in your spellbook, you can replace one wizard cantrip you know with another cantrip from the wizard spell list."})]}
             18 {:selections [(spell-mastery-selection 1)
                              (spell-mastery-selection 2)]
                 :modifiers [(mod5e/dependent-trait
                              {:name "Spell Mastery"
                               :page 115
                               :summary (str (if (seq ?spell-mastery)
                                               (str "Cast " (common/list-print ?spell-mastery))
                                               "Choose a 1st and 2nd level spell, cast those")
                                             " at lowest level without expending a slot if you have them prepared")})]}
             20 {:selections [(signature-spells-selection)]
                 :modifiers [(mod5e/dependent-trait
                              {:name "Signature Spells"
                               :page 115
                               :summary (str (if (seq ?signature-spells)
                                               (str "Your signature spells are " (common/list-print ?signature-spells))
                                               "Choose two 3rd level spells")
                                             ", you always have them prepared and can cast them once without expending a slot")})]}}
    :subclass-level 2
    :subclass-title "Arcane Tradition"
    :subclasses [{:name "School of Bladesinging"
                  :profs {:armor {:light true}}
                  :modifiers [(mod5e/skill-proficiency :performance)
                              (mod5e/bonus-action
                               {:name "Bladesong"
                                :frequency (units5e/long-rests ?prof-bonus)
                                :duration units5e/minutes-1
                                :summary (str "Invoke an elven magic called the Bladesong, provided that you aren't wearing medium or heavy armor or using a shield.
                                          \nThe bladesong lasts for 1 minute, and ends early if you are incapacitated, if you don medium or heavy armor or a shield, or if you use two hands to make an attack with a weapon. You can also dismiss the Bladesong at any time (no action required).
                                          \nWhile your Bladesong is active, you gain the following benefits:
                                          \n• You gain a +" (max 1 (?ability-bonuses ::char5e/int)) " bonus to your AC.
                                          \n• Your walking speed increases by 10 ft.
                                          \n• You have advantage on Acrobatics checks.
                                          \n• You gain a +" (max 1 (?ability-bonuses ::char5e/int)) " bonus to any CON save you make to maintain your concentration on a spell.")})]
                  :selections [(bladesinging-weapon-prof-selection weapon-map)]
                  :levels {6 {:modifiers [(mod5e/num-attacks 2)
                                          (mod5e/trait-cfg
                                           {:name "Extra Attack"
                                             :summary "Attack twice when taking the Attack action. Can cast a cantrip in place of one attack"})]}
                           10 {:modifiers [(mod5e/reaction
                                            {:name "Song of Defense"
                                             :summary "When you take damage, expend one spell slot to reduce that damage to you by an amount equal to five times the spell slot's level"})]}
                           14 {:modifiers [(mod5e/dependent-trait
                                            {:name "Song of Victory"
                                             :summary (str "Add +" (max 1 (?ability-bonuses ::char5e/int) " to the damage of your melee weapon attacks while bladesong is active"))})]}}}
                 {:name "School of Evocation"
                  :levels {10 {:modifiers [(mod5e/dependent-trait
                                            {:level 10
                                             :name "Empowered Evocation"
                                             :page 117
                                             :summary (str "add your INT mod (" (?ability-bonuses ::char5e/int) ") to one damage roll of evocation spell you cast")})]}}
                  :traits [(spell-school-savant "evocation" 117)
                           {:level 2
                            :name "Sculpt Spells"
                            :page 117
                            :summary "When you cast an evocation spell that affects other creatures that you can see, you can choose a number of them equal to 1 + the spell’s level. The chosen creatures automatically succeed on their saving throws against the spell, and they take no damage if they would normally take half damage on a successful save."}
                           {:level 6
                            :name "Potent Cantrip"
                            :page 117
                            :summary "creature take half damage on sucessful saves against your cantrips"}
                           {:level 14
                            :name "Overchannel"
                            :page 118
                            :summary "deal max damage with evocation spells 1st-5th level. You take necrotic damage if you use this feature more than once per long rest"}]}
                 {:name "Order of Scribes"
                  :modifiers [(mod5e/bonus-action
                               {:name "Wizardly Quill"
                                :summary (str "Create a Tiny Quill in your hand.\n"
                                          "• Doesn't require ink, and produces ink of a color of your choice on the writing surface.\n"
                                          "• Time to copy a spell into your spellbook equals 2 minutes per spell level.\n"
                                          "• You can erase anything your write with it by using a bonus action if within 5 ft.\n"
                                          "The quill disappears if you create another one or if you die.")})]
                  :traits [{:level 2
                            :name "Awakened Spellbook"
                            :summary (str "While holding your spellbook, you gain the following:\n"
                                      "• You can use the book as a spellcasting focus for your wizard spells.\n"
                                      "• When you cast a wizard spell with a spell slot, you can replace its damage type with a type that appears in another spell in your spellbook, which alters the spell for this casting only. The latter spell must be of the same level as the spell slot you expend.\n"
                                      "• When you cast a wizard spell as a ritual, you can use the spell's normal casting time (use once/long rest).\n"
                                      "You can replace the book over a short rest to a magic spellbook to which you're attuned, copying over the spells to it. The spells in the previous book vanishes.")}]}
                 {:name "School of Abjuration"
                    :modifiers [(mod5e/dependent-trait
                                 {:name "Arcane Ward"
                                  :page 115
                                  :summary (str "you can weave magic around yourself for protection. When you cast an abjuration spell of 1st level or higher, you can simultaneously use a strand of the spell's magic to create a magical ward on yourself that lasts until you finish a long rest. The ward has hit points equal to twice your wizard level + your Intelligence modifier (" (+ (* 2 (?class-level :wizard)) (?ability-bonuses ::char5e/int)) "). Whenever you take damage, the ward takes the damage instead. If this damage reduces the ward to 0 hit points, you take any remaining damage."
                                                "\nWhile the ward has 0 hit points, it can't absorb damage, but its magic remains. Whenever you cast an abjuration spell of 1st level or higher, the ward regains a number of hit points equal to twice the level of the spell."
                                                "\nOnce you create the ward, you can't create it again until you finish a long rest.")})]
                    :levels {6 {:modifiers [(mod5e/reaction
                                             {:name "Projected Ward"
                                              :page 115
                                              :range units5e/ft-30
                                              :summary "when a creature that you can see within 30 feet of you takes damage, you can use your reaction to cause your Arcane Ward to absorb that damage. If this damage reduces the ward to 0 hit points, the warded creature takes any remaining damage."})]}
                             10 {:modifiers [(mod5e/dependent-trait
                                              {:name "Improved Abjuration"
                                               :page 115
                                               :summary (str "when you cast an abjuration spell that requires you to make an ability check as a part of casting that spell (as in Counterspell and Dispel Magic), you add your proficiency bonus (" ?prof-bonus ") to that ability check.")})]}
                             14 {:modifiers [(mod5e/saving-throw-advantage [:spells])
                                             (mod5e/damage-resistance :spells)]}}
                    :traits [(spell-school-savant "abjuration" 115)
                             {:name "Spell Resistance"
                              :level 14
                              :page 116
                              :summary "you have advantage on saving throws against spells. Furthermore, you have resistance against the damage of spells."}]}
                 #_{:name "School of Conjuration"
                    :levels {6 {:modifiers [(mod5e/action
                                             {:name "Benign Transposition"
                                              :page 116
                                              :range units5e/ft-30
                                              :summary "Teleport to unoccupied space or swap spaces with willing Small or Medium creature"})]}}
                    :traits [(spell-school-savant "conjuration" 116)
                             {:name "Minor Conjuration"
                              :level 2
                              :page 116
                              :duration units5e/hours-1
                              :summary "conjure an inanimate object 3 ft per side or less and 15 lbs or less, it radiates dim light to 5 ft."}
                             {:name "Focused Conjuration"
                              :level 10
                              :page 116
                              :summary "concentration on conjuration spells cannot be broken by taking damage"}
                             {:name "Durable Summons"
                              :level 14
                              :summary "creatures you conjure have 30 temp hit points"}]}
                 #_{:name "School of Divination"
                    :levels {10 {:modifiers [(mod5e/action
                                              {:name "The Third Eye"
                                               :level 10
                                               :page 117
                                               :frequency units5e/long-rests-1
                                               :summary "gain one: 1) darkvision 60 ft., 2) see etherial plane 60 ft. 3) read any language 4) see invisible within 10 ft."})]}}
                    :traits [(spell-school-savant "divination" 116)
                             {:name "Portent"
                              :level 2
                              :frequency units5e/long-rests-1
                              :summary "roll 2 d20s after long rest, can replace rolls you or a creature you can see make with these"}
                             {:name "Expert Divination"
                              :level 6
                              :page 117
                              :summary "when you cast divination spell 2nd level or higher, regain a spell slot of lower level (max 5th level)"}
                             {:name "Greater Portent"
                              :level 14
                              :page 117
                              :summary "roll 3 d20s for your Portent feature"}]}
                 #_{:name "School of Enchantment"
                    :levels {2 {:modifiers [(mod5e/action
                                             {:name "Hypnotic Gaze"
                                              :level 2
                                              :page 117
                                              :range units5e/ft-5
                                              :summary (str "charm a creature until end of your next turn unless it succeeds on a DC " (?spell-save-dc ::char5e/int) " WIS save, it is incapacitated and dazed")})]}
                             6 {:modifiers [(mod5e/reaction
                                             {:name "Instinctive Charm"
                                              :page 117
                                              :range units5e/ft-30
                                              :frequency units5e/long-rests-1
                                              :summary (str "redirect a creature's attack against you to the creature closest to it, not including you, if it fails a DC " (?spell-save-dc ::char5e/int) " WIS save")})]}
                             14 {:modifiers [(mod5e/dependent-trait
                                              {:name "Alter Memories"
                                               :level 14
                                               :page 117
                                               :summary (str "make a creature unaware of your charm on it, can also use your action to erase up to " (inc (?ability-bonuses ::char5e/cha)) " hours from it's memory if it fails a DC " (?spell-save-dc ::char5e/int) " INT check")})]}}
                    :traits [(spell-school-savant "enchantment" 117)
                             {:name "Split Enchantment"
                              :level 10
                              :page 117
                              :summary "target 2 creatures with an enchantment spell that normally targets 1"}]}
                 #_{:name "School of Illusion"
                    :modifiers [(mod5e/spells-known-cfg 0
                                                        {:key :minor-illusion
                                                         :ability ::char5e/int
                                                         :class "Wizard"
                                                         :illusionist-cantrip? true}
                                                        0
                                                        [(not (spell-in-spells-known? ?spells-known 0 :minor-illusion))])]
                    :selections [(t/selection-cfg
                                  {:name "Illusionist Cantrip"
                                   :order 0
                                   :tags (opt5e/spell-tags :wizard 0)
                                   :options (opt5e/spell-options (get-in sl/spell-lists [:wizard 0]) ::char5e/int "Wizard")
                                   :prereq-fn (fn [c]
                                                (let [spells-known @(subscribe [::char5e/spells-known nil c])
                                                      passes? (or (nil? spells-known)
                                                                  (some
                                                                   (fn [s]
                                                                     (and (= :minor-illusion (:key s))
                                                                          (not (:illusionist-cantrip? s))))
                                                                   (vals (spells-known 0))))]
                                                  passes?))})]
                    :levels {6 {:modifiers [(mod5e/action
                                             {:name "Malleable Illusions"
                                              :page 118
                                              :summary "change the nature of an illusion you cast"})]}
                             10 {:modifiers [(mod5e/reaction
                                              {:name "Illusory Self"
                                               :page 118
                                               :frequency units5e/rests-1
                                               :summary "attacker hits an illusion of you instead of you"})]}
                             14 {:modifiers [(mod5e/bonus-action
                                              {:name "Illusory Reality"
                                               :page 119
                                               :summary "make an illusory object real for 1 minute"})]}}
                    :traits [(spell-school-savant "illusion" 118)
                             {:name "Improved Minor Illusion"
                              :page 118
                              :summary "can create a sound and an image with the same casting of minor illusion"}]}
                 #_{:name "School of Necromancy"
                    :levels {10 {:modifiers [(mod5e/damage-resistance :necrotic)]}
                             14 {:modifiers [(mod5e/action
                                              {:name "Command Undead"
                                               :page 119
                                               :summary (str "bring undead under your control unless it succeeds on a DC " (?spell-save-dc ::char5e/int) " CHA save")})]}}
                    :traits [(spell-school-savant "necromancy" 118)
                             {:name "Grim Harvest"
                              :level 2
                              :page 118
                              :frequency units5e/turns-1
                              :summary "when you kill a creature with a spell, you regain HPs equal to 2X the spell level or 3X the spell level for necromancy spells"}
                             {:name "Undead Thralls"
                              :page 119
                              :level 6
                              :summary (str "target one additional corpse or pile of bones for animate dead; whenever you create an undead it's HP max is increased by your wizard level amount and it adds your prof bonus to weapon damage rolls")}
                             {:name "Inured to Undeath"
                              :level 10
                              :summary "resistant to necrotic damage; your HP max cannot be reduced"}]}
                 #_{:name "School of Transmutation"
                    :modifiers [(mod5e/spells-known 4 :polymorph ::char5e/int "Wizard")]
                    :levels {14 {:modifiers [(mod5e/action
                                              {:name "Master Transmuter"
                                               :page 119
                                               :summary "expend your transmuter's stone for 1 effect: 1) convert a non-magical object into another non-magical object, 2) remove curses, poisons, diseases and damage from a creature 3) raise dead 4) reduce a willing creature's apparent age by 3d10"})]}}
                    :traits [(spell-school-savant "transmutation" 119)
                             {:name "Minor Alchemy"
                              :level 2
                              :page 119
                              :duration units5e/hours-1
                              :summary "transform an object of one substance to another substance"}
                             {:name "Transmuter's Stone"
                              :level 6
                              :page 119
                              :summary "create a stone with 1 benefit: 1) darkvision 60 ft. 2) +10 speed 3) prof in CON saves 4) resistance to acid, fire, cold, lightning, or thunder damage"}
                             {:name "Shapechanger"
                              :level 10
                              :page 119
                              :frequency units5e/long-rests-1
                              :summary "cast polymorph without a spell slot to turn into a CR 1 or less beast"}]}]}))

(def melee-weapons-xform
  (comp
   (filter
    ::weapon5e/melee?)
   (map
    (fn [weapon]
      (t/option-cfg
       {:name (or (:name weapon) (::weapon5e/name weapon))})))))

(defn pact-weapon-option [title weapons]
  (t/option-cfg
   {:name title
    :selections [(t/selection-cfg
                  {:name "Pact Weapon"
                   :tags #{:class}
                   :options (sequence
                             melee-weapons-xform
                             weapons)})]}))

(defn pact-boon-options [plugin-boons spell-lists spells-map ?ability]
 (concat
   (map
    (fn [{:keys [name description edit-event]}]
      (t/option-cfg
       {:name name
        :modifiers [(mod5e/trait-cfg
                     {:name (str "Pact Boon: " name)
                      :description description})]
        :edit-event edit-event}))
    plugin-boons)
  [(t/option-cfg
    {:name "Pact of the Chain"
     :modifiers [(mod5e/spells-known 1 :find-familiar ?ability "Warlock")
                 (mod5e/trait-cfg
                  {:name opt5e/pact-of-the-chain-name
                   :page 107
                   :summary "Can cast find familiar as a ritual, use your attack action to give your familiar an attack as a reaction"})]})
   (t/option-cfg
    {:name "Pact of the Blade"
     :modifiers [(mod5e/trait-cfg
                  {:name opt5e/pact-of-the-blade-name
                   :page 107
                   :summary "summon a magical weapon"})]})
   (t/option-cfg
    {:name "Pact of the Tome"
     :selections [(t/selection-cfg
                   {:name "Book of Shadows Cantrips"
                    :tags #{:spells}
                    :min 3
                    :max 3
                    :options (opt5e/spell-options spells-map
                                                  (into
                                                   #{}
                                                   (mapcat
                                                    (fn [[cls-kw spells-by-level]]
                                                      (spells-by-level 0))
                                                    spell-lists))
                                                  ?ability
                                                  "Warlock"
                                                  false
                                                  "uses Book of Shadows")})]
     :modifiers [(mod5e/trait-cfg
                  {:name opt5e/pact-of-the-tome-name
                   :page 108
                   :summary "you have a spellbook with 3 extra cantrips"})]})]))


(defn eldritch-invocation-options [plugin-invocations spell-lists spells-map ?ability]
  (concat
   (map
    (fn [{:keys [name description]}]
      (t/option-cfg
       {:name name
        :modifiers [(mod5e/trait-cfg
                     {:name (str "Eldritch Invocation: " name)
                      :description description})]}))
    plugin-invocations)
   [(t/option-cfg 
     {:name "Agonizing Blast"
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Agonizing Blast"
                    :page 110
                    :summary (str "add " (?ability-bonuses ?warlock-ability) " to eldritch blast spell damage")})]
      :help "Add your Charisma modifier to eldritch blast spell damage."
      :prereqs [opt5e/has-eldritch-blast-prereq]})
    (t/option-cfg
     {:name "Armor of Shadows"
      :help "Cast mage armor on yourself at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Armor of Shadows"
                    :page 110
                    :summary "cast mage armor on yourself at will"})
                  (mod5e/spells-known 1 :mage-armor ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "Ascendant Step"
      :help "Cast levitate on yourself at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Ascendant Step"
                    :page 110
                    :summary "cast levitate on yourself at will"})
                  (mod5e/spells-known 2 :levitate ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 9 :warlock)]})
    (t/option-cfg
     {:name "Aspect of the Moon"
      :help "Don't need to sleep and can't be forced to sleep."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Aspect of the Moon"
                    :summary "don't need to sleep and can't be forced to sleep"})]
      :prereqs [opt5e/pact-of-the-tome-prereq]})
    (t/option-cfg
     {:name "Beast Speech"
      :help "Can cast speak with animals at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Beast Speech"
                    :page 110
                    :summary "can cast speak with animals at will"})
                  (mod5e/spells-known 1 :speak-with-animals ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "Beguiling Influence"
      :help "Proficiency in deception and persuasion."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Beguiling Influence"
                    :page 110
                    :summary "proficiency in deception and persuasion"})
                  (mod5e/skill-proficiency :deception)
                  (mod5e/skill-proficiency :persuasion)]})
    (t/option-cfg
     {:name "Bewitching Whispers"
      :help "Cast compulsion using warlock spell slot once per long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Bewitching Whispers"
                    :page 110
                    :frequency units5e/long-rests-1
                    :summary "cast compulsion once using warlock spell slot"})
                  (mod5e/spells-known 4 :compulsion ?ability "Warlock" 0 "once per long rest")]})
    (t/option-cfg
     {:name "Bond of the Talisman"
      :help "Teleport to the unoccupied space closest to the talisman wearer if on the same plane, or they can teleport to you."
      :modifiers [(mod5e/action
                   {:name "Eldritch Invocation: Bond of the Talisman"
                    :frequency (units5e/long-rests ?prof-bonus)
                    :summary "teleport to the unoccupied space closest to the talisman wearer if on the same plane, or they can teleport to you"})]
      :prereqs [opt5e/pact-of-the-talisman-prereq
                (opt5e/total-levels-option-prereq 12 :warlock)]})
    (t/option-cfg
     {:name "Book of Ancient Secrets"
      :help "Learn 2 ritual spells; inscribe and cast rituals."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Book of Ancient Secrets"
                    :page 110
                    :summary "inscribe and cast rituals"})]
      :selections [(t/selection-cfg
                    {:name "Book of Ancient Secrets Rituals"
                     :tags #{:spells}
                     :multiselect? true
                     :options (opt5e/spell-options
                               spells-map
                               (map
                                (fn [s] (or (:key s)
                                            (common/name-to-kw (:name s))))
                                (filter
                                 (fn [s] (and (= 1 (:level s)) (opt5e/ritual-spell? s)))
                                 spells5e/spells))
                               ?ability
                               "Warlock"
                               false
                               "Book of Ancient Secrets Ritual")
                     :min 2
                     :max 2})]
      :prereqs [opt5e/pact-of-the-tome-prereq]})
    (t/option-cfg
     {:name "Chains of Carceri"
      :help "Cast hold monster at will on celestials, fiends, or elementals."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Chains of Carceri"
                    :page 110
                    :frequency units5e/long-rests-1
                    :summary "cast hold monster at will on celestials, fiends, or elementals. Must finish a long rest before using it on the same creature again"})
                  (mod5e/spells-known 5 :hold-monster ?ability "Warlock" 0 "at will")]
      :prereqs [opt5e/pact-of-the-chain-prereq
                (opt5e/total-levels-option-prereq 15 :warlock)]})
    (t/option-cfg
     {:name "Cloak of Flies"
      :help "Once per short rest, active to get advantage on Intimidation checks and disadvantage on all other CHA checks. Any other creature starting their turn within 5 ft. takes poison damage equal to your CHA modifier. Ends if incapacitated or dismissed as bonus action."
      :modifiers [(mod5e/bonus-action
                   {:name "Eldritch Invocation: Cloak of Flies"
                    :frequency units5e/rests-1
                    :summary (str "advantage on Intimidation checks and disadvantage on all other CHA checks. Any other creature starting their turn within 5 ft. takes " (max 0 (?ability-bonuses ?warlock-ability)) " poison damage. Ends if incapacitated or dismissed as bonus action")})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Devil's Sight"
      :help "See normally in magical and nonmagical darkness up to 120 ft."
      :modifiers [(mod5e/darkvision 120 1)
                  (mod5e/trait-cfg
                   {:name "Eldritch Invocation: Devil's Sight"
                    :page 110
                    :range units5e/ft-120
                    :summary "see normally in magical and nonmagical darkness"})]})
    (t/option-cfg
     {:name "Dreadful Word"
      :help "Use warlock spell slot to cast confusion once per long rest"
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Dreadful Word"
                    :page 110
                    :summary "use warlock spell slot to cast confusion"
                    :frequency units5e/long-rests-1})
                  (mod5e/spells-known 4 :confusion ?ability "Warlock" 0 "once per long rest")]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)]})
    (t/option-cfg
     {:name "Eldritch Mind"
      :help "Advantage on CON saves to mantain concentration on a spell."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Eldritch Mind"
                    :summary "advantage on CON saves to mantain concentration on a spell"})]})
    (t/option-cfg
     {:name "Eldritch Sight"
      :help "Cast detect magic at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Eldritch Sight"
                    :page 110
                    :summary "cast detect magic at will"})
                  (mod5e/spells-known 1 :detect-magic ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "Eldritch Smite"
      :help "When you hit a creature with your pact weapon, expend a warlock spell slot to deal an extra 1d8 force damage plus 1d8 per level of the slot, and knock prone if it is Huge or smaller."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Eldritch Smite"
                    :frequency units5e/turns-1
                    :summary "when you hit a creature with your pact weapon, expend a warlock spell slot to deal an extra 1d8 force damage plus 1d8 per level of the slot, and knock prone if it is Huge or smaller"})]
      :prereqs [opt5e/pact-of-the-blade-prereq
                (opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Eldritch Spear"
      :help "Eldrich blast with range 300 ft."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Eldritch Spear"
                    :page 111
                    :summary "eldrich blast with range 300 ft."})]
      :prereqs [opt5e/has-eldritch-blast-prereq]})
    (t/option-cfg
     {:name "Eyes of the Rune Keeper"
      :help "Read any writing."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Eyes of the Rune Keeper"
                    :page 111
                    :summary "read any writing"})]})
    (t/option-cfg
     {:name "Far Scribe"
      :help "Up to your PB creatures can write their names in your book. Can cast sending at will to those creatures by writing the message on the page. Replies appear on the page, and disappears after 1 minute."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Far Scribe"
                    :summary (str "a creature can use its action to write its name of a page in the book (max " ?prof-bonus " names). Cast sending without using spell slot or material components to the creature by writing the message on the page. Replies appear on the page, and disappears after 1 minute. Use action to erase name")})
                  (mod5e/spells-known 3 :false-life ?ability "Warlock")]
      :prereqs [opt5e/pact-of-the-tome-prereq
                (opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Fiendish Vigor"
      :help "Cast false life at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Fiendish Vigor"
                    :page 111
                    :summary "cast false life at will"})
                  (mod5e/spells-known 1 :false-life ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "Gaze of Two Minds"
      :help "Touch a willing humanoid to perceive through its senses while it's on the same plane as you."
      :modifiers [(mod5e/trait "Eldritch Invocation: Gaze of Two Minds"
                               "You can use your action to touch a willing humanoid and perceive through its senses until the end of your next turn. As long as the creature is on the same plane of existence as you, you can use your action on subsequent turns to maintain this connection, extending the duration until the end of your next turn. While perceiving through the other creature’s senses, you benefit from any special senses possessed by that creature, and you are blinded and deafened to your own surroundings.")]})
    (t/option-cfg
     {:name "Ghostly Gaze"
      :help "See through solid objects to a 30 ft. range. Darkvision within the range, and uses your concentration. Objects are percieved as ghostly, transparent images."
      :modifiers [(mod5e/action
                   {:name "Eldritch Invocation: Ghostly Gaze"
                    :duration units5e/minutes-1
                    :summary "see through solid objects to a 30 ft. range. Darkvision within the range, and uses your concentration. Objects are percieved as ghostly, transparent images"})]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)]})
    (t/option-cfg
     {:name "Gift of the Depths"
      :help "Breathe underwater and gain swimming speed equal to your walking speed. Cast water breathing once per long rest for free"
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Gift of the Depths"
                    :frequency units5e/long-rests-1
                    :summary "breathe underwater and gain swimming speed equal to your walking speed. Cast water breathing once for free"})
                  (mod5e/swimming-speed-equal-to-walking)
                  (mod5e/spells-known 3 :water-breathing ?ability "Warlock" 0 "once/long rest")]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Gift of the Ever-Living Ones"
      :help "Always roll max when rolling to regain hp while your familiar is within 100 ft."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Gift of the Ever-Living Ones"
                    :summary "always roll max when rolling to regain hp while your familiar is within 100 ft"})]
      :prereqs [opt5e/pact-of-the-chain-prereq]})
    (t/option-cfg
     {:name "Gift of the Protectors"
      :help "Up to your PB creatures can write their names in your book. Once per long rest, a creature drops to 1 hp instead of 0."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Gift of the Protectors"
                    :frequency units5e/long-rests-1
                    :summary (str "a creature can use its action to write its name of a page in the book (max " ?prof-bonus " names). Use action to erase name. Any creature on the page drops to 1 hp instead of 0")})]
      :prereqs [(opt5e/total-levels-option-prereq 9 :warlock)
                opt5e/pact-of-the-tome-prereq]})
    (t/option-cfg
     {:name "Grasp of Hadar"
      :help "Once per turn, move a creature that you hit with eldritch blast 10 ft. straight closer to you."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Grasp of Hadar"
                    :frequency units5e/turns-1
                    :summary "move a creature that you hit with eldritch blast 10 ft. straight closer to you"})]
      :prereqs [opt5e/has-eldritch-blast-prereq]})
    (t/option-cfg
     {:name "Improved Pact Weapon"
      :help "Use any Pact of the Blade weapon as a warlock spellcasting focus. The weapon gains a +1 bonus to attack and damage rolls unless if it already has a bonus. You can conjure a shortbow, longbow, light crossbow, or heavy crossbow."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Improved Pact Weapon"
                    :summary "use any Pact of the Blade weapon as a warlock spellcasting focus. The weapon gains a +1 bonus to attack and damage rolls unless if it already has a bonus. You can conjure a shortbow, longbow, light crossbow, or heavy crossbow"})]
      :prereqs [opt5e/pact-of-the-blade-prereq]})
    (t/option-cfg
     {:name "Investment of the Chain Master"
      :help "Familiars from find familiar gain a flying or swimming speed of 40 ft., as a bonus action, command the familiar to take the Attack action, familiar's attacks are magical. familiar uses your spell save DC, you can use your reaction to grant the familiar resistance against damage it takes."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Investment of the Chain Master"
                    :summary "familiars from find familiar gets these benefits:\n- gain flying or swimming speed of 40 ft.\n- as a bonus action, command the familiar to take the Attack action.\n- attacks are magical.\n- familiar uses your spell save DC.\n- you can use your reaction to grant the familiar resistance against damage it takes"})]
      :prereqs [opt5e/pact-of-the-chain-prereq]})
    (t/option-cfg
     {:name "Lance of Lethargy"
      :help "Once per turn, reduce the speed of a creature you hit with eldritch blast by 10 ft. until the end of your next turn."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Lance of Lethargy"
                    :frequency units5e/turns-1
                    :summary "reduce the speed of a creature you hit with eldritch blast by 10 ft. until the end of your next turn"})]
      :prereqs [opt5e/has-eldritch-blast-prereq]})
    (t/option-cfg
     {:name "Lifedrinker"
      :help "Deal your CHA modifier extra damage with your pact weapon"
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Lifedrinker"
                    :page 111
                    :summary (str "extra " (max 1 (?ability-bonuses ?warlock-ability)) " necrotic damage with your pact weapon")})]
      :prereqs [(opt5e/total-levels-option-prereq 12 :warlock)
                opt5e/pact-of-the-blade-prereq]})
    (t/option-cfg
     {:name "Maddening Hex"
      :help "Use bonus action to deal your CHA modifier psychic damage to a creature cursed by you and each creature you can see of your choice within 5 ft. of it."
      :modifiers [(mod5e/bonus-action
                   {:name "Eldritch Invocation: Maddening Hex"
                    :summary (str "deal " (max 1 (?ability-bonuses ?warlock-ability)) "psychic damage to a creature within 30 ft. cursed by your hex spell or by a warlock feature of yours that you can see, and each creature you can see within 5 ft. of it of your choice")})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]}) ;; curse prereq
    (t/option-cfg
     {:name "Mask of Many Faces"
      :help "Cast disguise self at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Mask of Many Faces"
                    :page 111
                    :summary "cast disguise self at will"})
                  (mod5e/spells-known 1 :disguise-self ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "Master of Myriad Forms"
      :help "Cast alter self at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Master of Myriad Forms"
                    :page 111
                    :summary "cast alter self at will"})
                  (mod5e/spells-known 2 :alter-self ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 15 :warlock)]})
    (t/option-cfg
     {:name "Minions of Chaos"
      :help "Cast conjure elemental using warlock spell slot once long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Minions of Chaos"
                    :page 111
                    :frequency units5e/long-rests-1
                    :summary "cast conjure elemental using a warlock spell slot"})
                  (mod5e/spells-known 5 :conjure-elemental ?ability "Warlock" 0 "once per rest")]
      :prereqs [(opt5e/total-levels-option-prereq 9 :warlock)]})
    (t/option-cfg
     {:name "Mire the Mind"
      :help "Cast slow using warlock spell slot once per long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Mire the Mind"
                    :page 111
                    :frequency units5e/long-rests-1
                    :summary "cast slow using warlock spell slot"})
                  (mod5e/spells-known 3 :slow ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Misty Visions"
      :help "Cast silent image at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Misty Visions"
                    :page 111
                    :summary "cast silent image at will"})
                  (mod5e/spells-known 1 :silent-image ?ability "Warlock" 0 "at will")]})
    (t/option-cfg
     {:name "One with Shadows"
      :help "In dim light or darkness, become invisible until you move or take an action or a reaction."
      :modifiers [(mod5e/action
                   {:name "Eldritch Invocation: One with Shadows"
                    :page 111
                    :summary "in dim light or darkness, become invisible until you move or take an action or a reaction"})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Otherworldly Leap"
      :help "Cast jump on yourself at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Otherworldly Leap"
                    :page 111
                    :summary "cast jump on yourself at will"})
                  (mod5e/spells-known 1 :jump ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 9 :warlock)]})
    (t/option-cfg
     {:name "Protection of the Talisman"
      :help "The wearer of your talisman can add a d4 to a failed saving throw a number of times equal to your PB."
      :modifiers [(mod5e/dependent-trait
                   {:name "Eldritch Invocation: Protection of the Talisman"
                    :frequency (units5e/long-rests ?prof-bonus)
                    :summary "the wearer of your talisman can add a d4 to a failed saving throw"})]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)
                opt5e/pact-of-the-talisman-prereq]})
    (t/option-cfg
     {:name "Rebuke of the Talisman"
      :help "When an attacker you can see hits the wearer of your talisman, use your reaction to Deal your PB psychic damage to to it and push it up to 10 ft. away from the wearer."
      :modifiers [(mod5e/reaction
                   {:name "Eldritch Invocation: Rebuke of the Talisman"
                    :summary (str "deal " ?prof-bonus " psychic damage to an attacker that you can see within 30 ft. that hit the wearer of your talisman and push it up to 10 ft. away from the wearer")})]
      :prereqs [opt5e/pact-of-the-talisman-prereq]})
    (t/option-cfg
     {:name "Relentless Hex"
      :help "Teleport up to 30 ft. to an unoccupied space you can see within 5 ft. of a target you can see cursed by your hex spell of by a warlock feature of yours"
      :modifiers [(mod5e/bonus-action
                   {:name "Eldritch Invocation: Relentless Hex"
                    :summary "teleport up to 30 ft. to an unoccupied space you can see within 5 ft. of a target you can see cursed by your hex spell of by a warlock feature of yours"})]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)]}) ;; curse prereq
    (t/option-cfg
     {:name "Repelling Blast"
      :help "Push a creature hit by your eldritch blast up to 10 ft. straight away from you."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Repelling Blast"
                    :page 111
                    :summary "push a creature hit by your eldritch blast up to 10 ft. straight away from you"})]
      :prereqs [opt5e/has-eldritch-blast-prereq]})
    (t/option-cfg
     {:name "Sculptor of Flesh"
      :help "Cast polymorph using a warlock spell slot once per long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Sculptor of Flesh"
                    :page 111
                    :frequency units5e/long-rests-1
                    :summary "cast polymorph using a warlock spell slot"})
                  (mod5e/spells-known 4 :polymorph ?ability "Warlock" 0 "once per long rest")]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)]})
    (t/option-cfg
     {:name "Shroud of Shadow"
      :help "Cast invisibility at will without expending a spell slot."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Shroud of Shadow"
                    :summary "cast invisibility at will without expending a spell slot"})
                  (mod5e/spells-known 2 :invisibility ?ability "Warlock")]
      :prereqs [(opt5e/total-levels-option-prereq 15 :warlock)]})
    (t/option-cfg
     {:name "Sign of Ill Omen"
      :help "Cast bestow curse using a warlock spell slot once per long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Sign of Ill Omen"
                    :page 111
                    :frequency units5e/long-rests-1
                    :summary "cast bestow curse using warlock spell slot"})
                  (mod5e/spells-known 3 :bestow-curse ?ability "Warlock" 0 "once per long rest")]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Thief of Five Fates"
      :help "Cast bane using a warlock spell slot once per long rest."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Thief of Five Fates"
                    :page 111
                    :frequency units5e/long-rests-1
                    :summary "cast bane warlock spell slot"})
                  (mod5e/spells-known 1 :bane ?ability "Warlock" 0 "once per long rest")]})
    (t/option-cfg
     {:name "Thirsting Blade"
      :help "When taking the Attack action, attack with pact blade twice."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Thirsting Blade"
                    :page 111
                    :summary "when taking the Attack action, attack with pact blade twice"})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)
                opt5e/pact-of-the-blade-prereq]})
    (t/option-cfg
     {:name "Tomb of Levistus"
      :help "When you take damage, use your reaction to entomb yourself in ice until the end of your next turn, gaining temp HP which take from the triggering damage. Immediately afterwards, you gain vulnerability to fire damage, speed is reduced to 0, and you are incapacitated. These effects end when the ice melts, including the temp HP"
      :modifiers [(mod5e/reaction
                   {:name "Eldritch Invocation: Tomb of Levistus"
                    :frequency units5e/rests-1
                    :summary (str "when you take damage, entomb yourself in ice until the end of your next turn. You gain " (* 10 (?class-level :warlock)) " temp HP, which take from the triggering damage. Immediately afterwards, you gain vulnerability to fire damage, speed is reduced to 0, and you are incapacitated. These effects end when the ice melts, including the temp HP")})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Trickster's Escape"
      :help "Cast freedom of movement on yourself without expending a spell slot once per long rest."
      :modifiers [(mod5e/reaction
                   {:name "Eldritch Invocation: Trickster's Escape"
                    :frequency units5e/long-rests-1
                    :summary "cast freedom of movement on yourself without expending a spell slot"})]
      :prereqs [(opt5e/total-levels-option-prereq 7 :warlock)]})
    (t/option-cfg
     {:name "Undying Servitude"
      :help "Cast animate dead without expending a spell slot once per long rest."
      :modifiers [(mod5e/reaction
                   {:name "Eldritch Invocation: Undying Servitude"
                    :frequency units5e/long-rests-1
                    :summary "cast animate dead without expending a spell slot"})]
      :prereqs [(opt5e/total-levels-option-prereq 5 :warlock)]})
    (t/option-cfg
     {:name "Visions of Distant Realms"
      :help "Cast arcane eye at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Visions of Distant Realms"
                    :page 111
                    :summary "cast arcane eye at will"})
                  (mod5e/spells-known 4 :arcane-eye ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 15 :warlock)]})
    (t/option-cfg
     {:name "Voice of the Chain Master"
      :help "Communicate telepathically with, perceive through, and speak through your familiar."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Voice of the Chain Master"
                    :page 111
                    :summary "communicate telepathically with and perceive through your familiar's senses while on the same plane. While perceiving through your familiar's senses, you can speak through it in your own voice"})]
      :prereqs [opt5e/pact-of-the-chain-prereq]})
    (t/option-cfg
     {:name "Whispers of the Grave"
      :help "Cast speak with dead at will."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Whispers of the Grave"
                    :page 111
                    :summary "cast speak with dead at will"})
                  (mod5e/spells-known 3 :speak-with-dead ?ability "Warlock" 0 "at will")]
      :prereqs [(opt5e/total-levels-option-prereq 9 :warlock)]})
    (t/option-cfg
     {:name "Witch Sight"
      :help "See the true form of any shapechanger or creature concealed by illusion or transmutation magic while the creature is within 30 feet of you and within line of sight."
      :modifiers [(mod5e/trait-cfg
                   {:name "Eldritch Invocation: Witch Sight"
                    :range units5e/ft-30
                    :page 111
                    :summary "you can see the true form of any shapechanger or creature concealed by illusion or transmutation magic while the creature is within 30 feet of you and within line of sight"})]
      :prereqs [(opt5e/total-levels-option-prereq 15 :warlock)]})]))


(def warlock-spells-known
  {1 2
   2 1
   3 1
   4 1
   5 1
   6 1
   7 1
   8 1
   9 1
   11 1
   13 1
   15 1
   17 1
   19 1})

(defn eldritch-invocation-selection [plugin-invocations spell-lists spells-map ?ability class-kw & [num]]
  (opt5e/eldritch-invocation-selection
   {:options (eldritch-invocation-options plugin-invocations spell-lists spells-map ?ability)
    :min (or num 1)
    :max (or num 1)}
   class-kw))

(defn mystic-arcanum-selection [spells-map spell-level ?ability class-kw]
  (t/selection-cfg
   {:name (str "Mystic Arcanum: Spell Level " spell-level)
    :tags #{:spells}
    :options (opt5e/spell-options
              spells-map
              (get-in sl5e/spell-lists [class-kw spell-level])
              ?ability
              "Warlock"
              false
              "uses Mystic Arcanum")}))

(defn warlock-option [spell-lists spells-map plugin-subclasses-map language-map weapon-map invocations boons ?ability ?ability-name class-kw]
   {
    :spellcasting {:cantrips-known {1 2 4 1 10 1}
                   :spells-known warlock-spells-known
                   :slot-schedule t-base/warlock-spell-slot-schedule
                   :known-mode :schedule
                   :pact-magic? true
                   :ability ?ability}
    :multiclass-prereqs [(opt5e/ability-prereq ?ability 13)]
    :spellcaster true
    :hit-die 8
    :ability-increase-levels [4 8 12 16 19]
    :profs {:armor {:light false}
            :weapon {:simple false}
            :save {::char5e/wis true ?ability true}
            :skill-options {:choose 2 :options {:arcana true :deception true :history true :intimidation true :investigation true :nature true :religion true}}}
    ;; :modifiers [(mod/modifier ?pact-magic? true)]
    :selections [(opt5e/new-starting-equipment-selection
                  class-kw
                  {:name "Weapon"
                   :options [(t/option-cfg
                              {:name "Light Crossbow & 20 Bolts"
                               :modifiers [(mod5e/weapon :crossbow-light 1)
                                           (mod5e/equipment :crossbow-bolt 20)]})
                             (t/option-cfg
                              {:name "Simple Weapon"
                               :selections [(opt5e/new-starting-equipment-selection
                                             class-kw
                                             {:name "Simple Weapon"
                                              :options (opt5e/simple-weapon-options 1 (vals weapon-map))
                                              :min 1
                                              :max 1})]})]})
                 (opt5e/simple-weapon-selection 1 class-kw weapon-map)]
    :equipment-choices [{:name "Equipment Pack"
                         :options {:scholars-pack 1
                                   :dungeoneers-pack 1}}
                        {:name "Spellcasting Equipment"
                         :options {:component-pouch 1
                                   :arcane-focus 1}}]
    :weapons {:dagger 2}
    :armor {:leather 1}
    :levels {2 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw 2)]}
             3 {:selections [(t/selection-cfg
                              {:name "Pact Boon"
                               :tags #{:class}
                               :options (pact-boon-options boons spell-lists spells-map ?ability)})]}
             5 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)]}
             7 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)]}
             9 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)]}
             11 {:selections [(mystic-arcanum-selection spells-map 6 ?ability class-kw)]
                 :modifiers [(mod5e/dependent-trait
                              {:name "Mystic Arcanum"
                               :level 11
                               :page 108
                               :summary "You gain a 6th level spell you can cast without expending a slot, more at higher levels"
                               :frequency (units5e/long-rests
                                           (mod5e/level-val
                                            (?class-level :class-kw)
                                            {13 2
                                             15 3
                                             17 4
                                             :default 1}))})]}
             12 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)]}
             13 {:selections [(mystic-arcanum-selection spells-map 7 ?ability class-kw)]}
             15 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)
                              (mystic-arcanum-selection spells-map 8 ?ability class-kw)]}
             17 {:selections [(mystic-arcanum-selection spells-map 9 ?ability class-kw)]}
             18 {:selections [(eldritch-invocation-selection invocations spell-lists spells-map ?ability class-kw)]}}
    :traits [{:name "Eldrich Master"
              :level 20
              :page 108
              :summary "Regain all Pact Magic spell slots"
              :frequency units5e/long-rests-1}
             {:name "Eldritch  Versatility"
              :level 4
              :summary "When you reach an ASI in warlock, you can replace one cantrip, one Pact Boon, and one spell from Mystic Arcanum from the class. You must replace any Eldritch Invocations you become ineligible for."}]
    :subclass-level 1
    :subclass-title "Otherworldly Patron"
    :subclasses [{:name "The Fiend"
                  :traits [{:name "Dark One's Own Luck"
                            :level 6
                            :page 109
                            :summary "add d10 to an ability check or save roll"
                            :frequency units5e/rests-1}
                           {:name "Fiendish Resilience"
                            :level 10
                            :page 109
                            :summary "resistance to a chosen damage type"}
                           {:name "Hurl Through Hell"
                            :level 14
                            :page 109
                            :summary "deal 10d10 psychic damage when you hit with an attack"
                            :frequency units5e/rests-1}]
                  :levels {1 {:modifiers [(mod5e/dependent-trait
                                           {:name "Dark One's Blessing"
                                            :page 109
                                            :summary (str "gain " (+ (?class-level :class-kw)
                                                                     (?ability-bonuses ?warlock-ability)) " temp HPs when you reduce a hostile creature to 0 HPs")})]
                              :selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:burning-hands :command])]}
                           3 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:blindness-deafness :scorching-ray])]}
                           5 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:fireball :stinking-cloud])]}
                           7 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:fire-shield :wall-of-fire])]}
                           9 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:flame-strike :hallow])]}}}
                 {:name "The Archfey"
                    :modifiers [(mod5e/action
                                 {:name "Fey Presence"
                                  :page 109
                                  :summary (str "As an action, you can cause each creature in a 10-foot cube originating from you to make a Wisdom saving throw against your warlock spell save DC. The creatures that fail their saving throws are all charmed or frightened by you (your choice) until the end of your next turn.")
                                  :duration units5e/turns-1
                                  :frequency units5e/rests-1})]
                    :levels {1 {:selections [(opt5e/warlock-subclass-spell-selection class-kw ?ability [:faerie-fire :sleep])]}
                             3 {:selections [(opt5e/warlock-subclass-spell-selection class-kw ?ability [:calm-emotions :phantasmal-force])]}
                             5 {:selections [(opt5e/warlock-subclass-spell-selection class-kw ?ability [:blink :plant-growth])]}
                             6 {:modifiers [(mod5e/reaction
                                             {:name "Misty Escape"
                                              :page 109
                                              :frequency units5e/rests-1
                                              :duration units5e/rounds-1
                                              :summary "When you take damage, you can use your reaction to turn invisible and teleport up to 60 feet to an unoccupied space you can see. You remain invisible until the start of your next turn or until you attack or cast a spell."})]}
                             7 {:selections [(opt5e/warlock-subclass-spell-selection class-kw ?ability [:dominate-beast :greater-invisibility])]}
                             9 {:selections [(opt5e/warlock-subclass-spell-selection class-kw ?ability [:dominate-person :seeming])]}
                             10 {:modifiers [(mod5e/condition-immunity :charmed)
                                             (mod5e/reaction
                                              {:name "Beguiling Defenses"
                                               :page 109
                                               :duration units5e/minutes-1
                                               :summary (str "You are immune to being charmed, and when another creature attempts to charm you, you can use your reaction to attempt to turn the charm back on that creature. The creature must succeed on a Wisdom saving throw against your warlock spell save DC or be charmed by you for 1 minute or until the creature takes any damage.")})]}
                             14 {:modifiers [(mod5e/action
                                              {:name "Dark Delerium"
                                               :page 109
                                               :summary (str "As an action, choose a creature that you can see within 60 feet of you. It must make a Wisdom saving throw against your warlock spell save DC. On a failed save, it is charmed or frightened by you (your choice) for 1 minute or until your concentration is broken (as if you are concentrating on a spell). This effect ends early if the creature takes any damage."
                                                             "\n\nUntil this illusion ends, the creature thinks it is lost in a misty realm, the appearance of which you choose. The creature can see and hear only itself, you, and the illusion.")
                                               :frequency units5e/rests-1})]}}}
                 {:name "The Celestial"
                  :modifiers [(mod5e/spells-known 0 :light ?ability "Warlock")
                              (mod5e/spells-known 0 :sacred-flame ?ability "Warlock")]
                  :levels {1 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:cure-wounds :guiding-bolt])]
                              :modifiers [(mod5e/bonus-action
                                           {:name "Healing Light"
                                            :frequency (units5e/long-rests (+ 1 (?class-level :class-kw)))
                                            :summary (str "Heal a creature within 60 ft. from a " (+ 1 (?class-level class-kw)) "d6 dice pool, spending at max " (max 1 (?ability-bonuses ?warlock-ability)) " dice at once")})]}
                           3 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:flaming-sphere :lesser-restoration])]}
                           5 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:daylight :revivify])]}
                           6 {:modifiers [(mod5e/damage-resistance :radiant)
                                          (mod5e/dependent-trait
                                           {:name "Radiant Soul"
                                            :level 6
                                            :summary (str "When you cast a spell that deals radiant or fire damage, add " (common/bonus-str (?ability-bonuses ?warlock-ability)) " to the damage against one target")})]}
                           7 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:guardian-of-faith :wall-of-fire])]}
                           9 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:flame-strike :greater-restoration])]}
                           10 {:modifiers [(mod5e/dependent-trait
                                            {:name "Celestial Resistance"
                                             :summary (str "Gain " (+ (?class-level :class-kw) (?ability-bonuses ?warlock-ability)) " temp HP at the end of a rest. Choose up to 5 creatures that each gain " (+ (int (/ (?class-level :class-kw) 2)) (?ability-bonuses ?warlock-ability)) " temp HP")})]}
                           14 {:modifiers [(mod5e/dependent-trait
                                            {:name "Searing Vengeance"
                                             :frequency units5e/long-rests-1
                                             :summary (str "When you have to make a death saving throw, instead regain " (int (/ ?max-hit-points 2)) " HP, and stand up if you so choose. Each creature of choice within 30 ft. takes 2d8+" (?ability-bonuses ?warlock-ability) " radiant damage, and is blinded until the end the turn")})]}}}
                 {:name "The Great Old One"
                    :levels {1 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:dissonant-whispers :hideous-laughter])]}
                             3 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:detect-thoughts :phantasmal-force])]}
                             5 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:clairvoyance :sending])]}
                             6 {:modifiers [(mod5e/reaction
                                             {:name "Entropic Ward"
                                              :page 110
                                              :frequency units5e/rests-1
                                              :summary "impose disadvantage on an attack roll against you, if it misses, gain advantage on your next attack roll against the attacker before the end of your next turn"})]}
                             7 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:dominate-beast :black-tentacles])]}
                             9 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:dominate-person :telekinesis])]}
                             10 {:modifiers [(mod5e/damage-resistance :psychic)]}
                             14 {:modifiers [(mod5e/action
                                              {:name "Create Thrall"
                                               :page 110
                                               :summary "Touch an incapacitated humanoid to charm it until Remove Curse is cast on it, the charmed condition is removed from it, or you use this feature again.\n\nYou can communicate telepathically with it while on the same plane"})]}}
                    :traits [{:name "Awakened Mind"
                              :level 1
                              :page 110
                              :summary "speak telepathically to a creature. You don't need to share a language, but the target must be able to understand at least one language"
                              :range units5e/ft-30}
                             {:name "Thought Shield"
                              :level 10
                              :page 110
                              :summary "your thoughts can't be read unless allowed; resistance to psychic damage; when a creature deals psychic damage to you it takes the same amount"}]}
                 {:name "The Hexblade"
                  :profs {:armor {:medium true :shields true}
                          :weapon {:martial true}}
                  :modifiers [(mod5e/bonus-action
                                {:name "Hexblade's Curse"
                                :frequency units5e/rests-1
                                :duration units5e/minutes-1
                                :summary (str "Curse a creature you can see within 30 ft. for 1 minute. Ends early if the target dies, you die, or you are incapacitated."
                                          "\n• Gain a +" ?prof-bonus " bonus to damage rolls against the cursed target."
                                          "\n• Attacks rolls against the cursed target are critical on 19s and 20s."
                                          "\n• If the cursed target dies, regain " (max 1 (+ (?class-level class-kw) (?ability-bonuses ?warlock-ability))) " HP.")})
                              (mod5e/dependent-trait
                                {:name "Hex Warrior"
                                 :summary (str "Touch one proficient, one-handed weapon when you finish a long rest. You can use your " (str ?ability-name) " modifier for attack and damage rolls with it. Lasts until you finish a long rest. Applies to every pact weapon conjured with the pact of the blade feature")})]
                  :levels {1 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:shield :wrathful-smite])]}
                            3 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:blur :branding-smite])]}
                            5 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:blink :elemental-weapon])]}
                            6 {:modifiers [(mod5e/dependent-trait
                                            {:name "Accursed Specter"
                                            :frequency units5e/long-rests-1
                                            :summary (str "Cause a specter to appear when you slay a humanoid. It has " (int (/ (?class-level class-kw) 2)) " temp HP, its own initiative, obeys verbal commands, and a +" (max 0 (?ability-bonuses ?warlock-ability)) " bonus to its attack rolls."
                                                      "\nRemains until the end of your next long rest")})]}
                            7 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:phantasmal-killer :staggering-smite])]}
                            9 {:selections [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class-kw ?ability [:banishing-smite :cone-of-cold])]}
                            10 {:modifiers [(mod5e/reaction
                                            {:name "Armor of Hexes"
                                              :summary "If the target cursed by your Hexblade’s Curse hits you with an attack roll, roll a d6. On a 4 or higher, the attack instead misses"})]}}
                  :traits [{:name "Master of Hexes"
                            :level 14
                            :summary "When the creature cursed by your Hexblade's Curse dies, you can apply the curse to a different creature you can see within 30 ft., provided you aren't incapacitated. You don't regain hit points from the death of the previously cursed creature"}]}]})

(defn warlock-cha-option [spell-lists spells-map plugin-subclasses-map language-map weapon-map invocations boons]
  (opt5e/class-option
   spell-lists
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   (merge 
   {:name "Warlock"
    :key :warlock-cha
    :modifiers [(mod/modifier ?pact-magic? true)
                (mod/modifier ?warlock-ability ::char5e/cha)]}
   (warlock-option spell-lists spells-map plugin-subclasses-map language-map weapon-map invocations boons ::char5e/cha "Charisma" :warlock-cha))))

(defn warlock-int-option [spell-lists spells-map plugin-subclasses-map language-map weapon-map invocations boons]
  (opt5e/class-option
   spell-lists
   spells-map
   plugin-subclasses-map
   language-map
   weapon-map
   (merge 
   {:name "Warlock (Int)"
    :key :warlock-int
    :modifiers [(mod/modifier ?pact-magic? true)
                (mod/modifier ?warlock-ability ::char5e/int)]}
   (warlock-option spell-lists spells-map plugin-subclasses-map language-map weapon-map invocations boons ::char5e/int "Intelligence" :warlock-int))))
