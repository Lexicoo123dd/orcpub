(ns orcpub.dnd.e5.spells2
  (:require [orcpub.common :as common]
            #?(:cljs [cljs.spec.alpha :as spec])
            #?(:clj [clojure.spec.alpha :as spec])
            [clojure.string :as s]))

(spec/def ::name (spec/and string? common/starts-with-letter?))
(spec/def ::key (spec/and keyword? common/keyword-starts-with-letter?))
(spec/def ::school string?)
(spec/def ::level (spec/int-in 0 10))
(spec/def ::casting-time string?)
(spec/def ::duration string?)
(spec/def ::range string?)
(spec/def ::source keyword?)
(spec/def ::page nat-int?)
(spec/def ::summary string?)
(spec/def ::description string?)

(spec/def ::verbal boolean?)
(spec/def ::somatic boolean?)
(spec/def ::material boolean?)
(spec/def ::material-component string?)

(spec/def ::components (spec/keys :opt-un [::verbal ::somatic ::material ::material-component]))

(spec/def ::spell (spec/keys :req-un [::name ::key ::school ::level]
                             :opt-un [::casting-time
                                      ::duration
                                      ::range
                                      ::source
                                      ::page
                                      ::summary
                                      ::components
                                      ::description]))

(spec/def ::option-pack string?)
(spec/def ::homebrew (spec/keys :req-un [::option-pack]))
(spec/def ::spell-lists (fn [lists]
                          (let [s (into #{} (vals lists))]
                            (and (s true)
                                 (every? keyword? (keys lists))))))

(spec/def ::has-spell-lists (spec/keys :req-un [::spell-lists]))

(spec/def ::homebrew-spell (spec/and ::spell
                                     ::homebrew
                                     ::has-spell-lists))

(def necromancy "necromancy")
(def abjuration "abjuration")
(def evocation "evocation")
(def divination "divination")
(def transmutation "transmutation")
(def illusion "illusion")
(def conjuration "conjuration")
(def enchantment "enchantment")

(def schools [necromancy
              abjuration
              evocation
              divination
              transmutation
              illusion
              conjuration
              enchantment])

(def conc-1-min "Concentration, up to 1 minute")
(def conc-10-min "Concentration, up to 10 minutes")

(def instantaneous "Instantaneous")

(def actions-1 "1 action")
(def bonus-actions-1 "1 bonus action")

(def self "Self")
(def touch "Touch")

(def s-spells
  [{
    :name "Sacred Flame"
    :school evocation
    :level 0
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "Flame-like radiance descends on a creature that you can see within range. The target must succeed on a Dexterity saving throw or take 1d8 radiant damage. The target gains no benefit from cover for this saving throw.
The spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
   {
    :name "Sanctuary"
    :school abjuration
    :level 1
    :casting-time bonus-actions-1
    :range "30 feet"
    :components {:verbal true :somatic true :material true :material-component "a small silver mirror"}
    :duration "1 minute"
    :description "You ward a creature within range against attack. Until the spell ends, any creature who targets the warded creature with an attack or a harmful spell must first make a Wisdom saving throw. On a failed save, the creature must choose a new target or lose the attack or spell. This spell doesn't protect the warded creature from area effects, such as the explosion of a fireball.
If the warded creature makes an attack or casts a spell that affects an enemy creature, this spell ends."
    }
   {
    :name "Sapping Sting"
    :school necromancy
    :level 0
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "You sap the vitality of one creature you can see in range. The target must succeed on a Constitution saving throw or take 1d4 necrotic damage and fall prone.
This spell's damage increases by 1d4 when you reach 5th level (2d4), 11th level (3d4), and 17th level (4d4)."
    }
   {
    :name "Scatter"
    :school conjuration
    :level 6
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true}
    :duration instantaneous
    :description "The air quivers around up to five creatures of your choice that you can see within range. An unwilling creature must succeed on a Wisdom saving throw to resist this spell. You teleport each affected target to an unoccupied space that you can see within 120 feet of you. That space must be on the ground or on a floor."
    }
   {
    :name "Scorching Ray"
    :school evocation
    :level 2
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "You create three rays of fire and hurl them at targets within range. You can hurl them at one target or several.
Make a ranged spell attack for each ray. On a hit, the target takes 2d6 fire damage.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, you create one additional ray for each slot level above 2nd."
    }
   {
    :name "Scrying"
    :school divination
    :level 5
    :casting-time "10 minutes"
    :range self
    :components {:verbal true :somatic true :material true :material-component "a focus worth at least 1,000 gp, such as a crystal ball, a silver mirror, or a font filled with holy water"}
    :duration conc-10-min
    :description "You can see and hear a particular creature you choose that is on the same plane of existence as you. The target must make a Wisdom saving throw, which is modified by how well you know the target and the sort of physical connection you have to it. If a target knows you're casting this spell, it can fail the saving throw voluntarily if it wants to be observed.
Knowledge Save Modifier
Secondhand (you have heard of the target) +5
Firsthand (you have met the target) +0
Familiar (you know the target well) -5
Connection Save Modifier
Likeness or picture -2
Possession or garment -4
Body part, lock of hair, bit of nail, or the like -10
On a successful save, the target isn't affected, and you can't use this spell against it again for 24 hours.
On a failed save, the spell creates an invisible sensor within 10 feet of the target. You can see and hear through the sensor as if you were there. The sensor moves with the target, remaining within 10 feet of it for the duration. A creature that can see invisible objects sees the sensor as a luminous orb about the size of your fist.
Instead of targeting a creature, you can choose a location you have seen before as the target of this spell. When you do, the sensor appears at that location and doesn't move."
    }
   {
    :name "Searing Smite"
    :school evocation
    :level 1
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true}
    :duration conc-1-min
    :description "The next time you hit a creature with a melee weapon attack during the spell's duration, your weapon flares with white-hot intensity, and the attack deals an extra 1d6 fire damage to the target and causes the target to ignite in flames. At the start of each of its turns until the spell ends, the target must make a Constitution saving throw. On a failed save, it takes 1d6 fire damage. On a successful save, the spell ends. If the target or a creature within 5 feet of it uses an action to put out the flames, or if some other effect douses the flames (such as the target being submerged in water), the spell ends.
At Higher Levels. When you cast this spell using a spell slot of 2nd level or higher, the initial extra damage dealt by the attack increases by 1d6 for each slot level above 1st."
    }
   {
    :name "See Invisibility"
    :school divination
    :level 2
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true :material true :material-component "a pinch of talc and a small sprinkling of powdered silver"}
    :duration "1 hour"
    :description "For the duration, you see invisible creatures and objects as if they were visible, and you can see into the Ethereal Plane. Ethereal creatures and objects appear ghostly and translucent."
    }
   {
    :name "Seeming"
    :school illusion
    :level 5
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true}
    :duration "8 hours"
    :description "This spell allows you to change the appearance of any number of creatures that you can see within range. You give each target you choose a new, illusory appearance. An unwilling target can make a Charisma saving throw, and if it succeeds, it is unaffected by this spell.
The spell disguises physical appearance as well as clothing, armor, weapons, and equipment. You can make each creature seem 1 foot shorter or taller and appear thin, fat, or in between. You can't change a target's body type, so you must choose a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you. The spell lasts for the duration, unless you use your action to dismiss it sooner.
The changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to a creature's outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel the creature's head and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.
A creature can use its action to inspect a target and make an Intelligence (Investigation) check against your spell save DC. If it succeeds, it becomes aware that the target is disguised."
    }
   {
    :name "Sending"
    :school evocation
    :level 3
    :casting-time actions-1
    :range "Unlimited"
    :components {:verbal true :somatic true :material true :material-component "a short piece of fine copper wire"}
    :duration "1 round"
    :description "You send a short message of twenty-five words or less to a creature with which you are familiar. The creature hears the message in its mind, recognizes you as the sender if it knows you, and can answer in a like manner immediately. The spell enables creatures with Intelligence scores of at least 1 to understand the meaning of your message.
You can send the message across any distance and even to other planes of existence, but if the target is on a different plane than you, there is a 5 percent chance that the message doesn't arrive."
    }
   {
    :name "Sequester"
    :school transmutation
    :level 7
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "a powder composed of diamond, emerald, ruby, and sapphire dust worth at least 5,000 gp, which the spell consumes"}
    :duration "Until dispelled"
    :description "By means of this spell, a willing creature or an object can be hidden away, safe from detection for the duration. When you cast the spell and touch the target, it becomes invisible and can't be targeted by divination spells or perceived through scrying sensors created by divination spells.
If the target is a creature, it falls into a state of suspended animation. Time ceases to flow for it, and it doesn't grow older.
You can set a condition for the spell to end early. The condition can be anything you choose, but it must occur or be visible within 1 mile of the target. Examples include “after 1,000 years” or “when the tarrasque awakens.” This spell also ends if the target takes any damage."
    }
   {
    :name "Shadow Blade"
    :school illusion
    :level 2
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true :somatic true}
    :duration conc-1-min
    :description "You weave together threads of shadow to create a sword of solidified gloom in your hand. This magic sword lasts until the spell ends. It counts as a simple melee weapon with which you are proficient. It deals 2d8 psychic damage on a hit and has the finesse, light, and thrown properties (range 20/60). In addition, when you use the sword to attack a target that is in dim light or darkness, you make the attack roll with advantage.
If you drop the weapon or throw it, it dissipates at the end of the turn. Thereafter, while the spell persists, you can use a bonus action to cause the sword to reappear in your hand.
At Higher Levels. When you cast this spell using a 3rd- or 4th-level spell slot, the damage increases to 3d8. When you cast it using a 5th- or 6th-level spell slot, the damage increases to 4d8. When you cast it using a spell slot of 7th level or higher, the damage increases to 5d8."
    }
   {
    :name "Shadow of Moil"
    :school necromancy
    :level 4
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true :material true :material-component "an undead eyeball encased in a gem worth at least 150 gp"}
    :duration conc-1-min
    :description "Flame-like shadows wreathe your body until the spell ends, causing you to become heavily obscured to others. The shadows turn dim light within 10 feet of you into darkness, and bright light in the same area to dim light.
Until the spell ends, you have resistance to radiant damage. In addition, whenever a creature within 10 feet of you hits you with an attack, the shadows lash out at that creature, dealing it 2d8 necrotic damage."
    }
   {
    :name "Shape Water"
    :school transmutation
    :level 0
    :casting-time actions-1
    :range "30 feet"
    :components {:somatic true}
    :duration "Instantaneous or 1 hour (see below)"
    :description "You choose an area of water that you can see within range and that fits within a 5-foot cube. You manipulate it in one of the following ways:
• You instantaneously move or otherwise change the flow of the water as you direct, up to 5 feet in any direction. This movement doesn't have enough force to cause damage.
• You cause the water to form into simple shapes and animate at your direction. This change lasts for 1 hour.
• You change the water's color or opacity. The water must be changed in the same way throughout. This change lasts for 1 hour.
• You freeze the water, provided that there are no creatures in it. The water unfreezes in 1 hour.
If you cast this spell multiple times, you can have no more than two of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
   {
    :name "Shapechange"
    :school transmutation
    :level 9
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true :material true :material-component "a jade circlet worth at least 1,500 gp, which you must place on your head before you cast the spell"}
    :duration "Concentration, up to 1 hour"
    :description "You assume the form of a different creature for the duration. The new form can be of any creature with a challenge rating equal to your level or lower. The creature can't be a construct or an undead, and you must have seen the sort of creature at least once. You transform into an average example of that creature, one without any class levels or the Spellcasting trait.
Your game statistics are replaced by the statistics of the chosen creature, though you retain your alignment and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus listed in its statistics is higher than yours, use the creature's bonus in place of yours. You can't use any legendary actions or lair actions of the new form.
You assume the hit points and Hit Dice of the new form. When you revert to your normal form, you return to the number of hit points you had before you transformed. If you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. As long as the excess damage doesn't reduce your normal form to 0 hit points, you aren't knocked unconscious.
You retain the benefit of any features from your class, race, or other source and can use them, provided that your new form is physically capable of doing so. You can't use any special senses you have (for example, darkvision) unless your new form also has that sense. You can only speak if the creature can normally speak.
When you transform, you choose whether your equipment falls to the ground, merges into the new form, or is worn by it. Worn equipment functions as normal. The GM determines whether it is practical for the new form to wear a piece of equipment, based on the creature's shape and size. Your equipment doesn't change shape or size to match the new form, and any equipment that the new form can't wear must either fall to the ground or merge into your new form. Equipment that merges has no effect in that state.
During this spell's duration, you can use your action to assume a different form following the same restrictions and rules for the original form, with one exception: if your new form has more hit points than your current one, your hit points remain at their current value."
    }
   {
    :name "Shatter"
    :school evocation
    :level 2
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true :material true :material-component "a chip of mica"}
    :duration instantaneous
    :description "A sudden loud ringing noise, painfully intense, erupts from a point of your choice within range. Each creature in a 10-foot-radius sphere centered on that point must make a Constitution saving throw. A creature takes 3d8 thunder damage on a failed save, or half as much damage on a successful one. A creature made of inorganic material such as stone, crystal, or metal has disadvantage on this saving throw.
A nonmagical object that isn't being worn or carried also takes the damage if it's in the spell's area.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
   {
    :name "Shield"
    :school abjuration
    :level 1
    :casting-time "1 reaction, which you take when you are hit by an attack or targeted by the magic missile spell"
    :range self
    :components {:verbal true :somatic true}
    :duration "1 round"
    :description "An invisible barrier of magical force appears and protects you. Until the start of your next turn, you have a +5 bonus to AC, including against the triggering attack, and you take no damage from magic missile."
    }
   {
    :name "Shield of Faith"
    :school abjuration
    :level 1
    :casting-time bonus-actions-1
    :range "60 feet"
    :components {:verbal true :somatic true :material true :material-component "a small parchment with a bit of holy text written on it"}
    :duration conc-10-min
    :description "A shimmering field appears and surrounds a creature of your choice within range, granting it a +2 bonus to AC for the duration."
    }
   {
    :name "Shillelagh"
    :school transmutation
    :level 0
    :casting-time bonus-actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "mistletoe, a shamrock leaf, and a club or quarterstaff"}
    :duration "1 minute"
    :description "The wood of a club or quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon."
    }
   {
    :name "Shocking Grasp"
    :school evocation
    :level 0
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true}
    :duration instantaneous
    :attack-roll? true
    :description "Lightning springs from your hand to deliver a shock to a creature you try to touch. Make a melee spell attack against the target. You have advantage on the attack roll if the target is wearing armor made of metal. On a hit, the target takes 1d8 lightning damage, and it can't take reactions until the start of its next turn.
The spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
   {
    :name "Sickening Radiance"
    :school evocation
    :level 4
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true}
    :duration conc-10-min
    :description "Dim, greenish light spreads within a 30-foot-radius sphere centered on a point you choose within range. The light spreads around corners, and it lasts until the spell ends.
When a creature moves into the spell's area for the first time on a turn or starts its turn there, that creature must succeed on a Constitution saving throw or take 4d10 radiant damage, and it suffers one level of exhaustion and emits a dim, greenish light in a 5-foot radius. This light makes it impossible for the creature to benefit from being invisible. The light and any levels of exhaustion caused by this spell go away when the spell ends."
    }
   {
    :name "Silence"
    :ritual true
    :school illusion
    :level 2
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true}
    :duration conc-10-min
    :description "For the duration, no sound can be created within or pass through a 20-foot-radius sphere centered on a point you choose within range. Any creature or object entirely inside the sphere is immune to thunder damage, and creatures are deafened while entirely inside it. Casting a spell that includes a verbal component is impossible there."
    }
   {
    :name "Silent Image"
    :school illusion
    :level 1
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true :material true :material-component "a bit of fleece"}
    :duration conc-10-min
    :description "You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 15-foot cube. The image appears at a spot within range and lasts for the duration. The image is purely visual; it isn't accompanied by sound, smell, or other sensory effects.
You can use your action to cause the image to move to any spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking.
Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image."
    }
   {
    :name "Silvery Barbs"
    :school enchantment
    :level 1
    :casting-time "1 reaction, which you take when a creature you can see within 60 feet of yourself succeeds on an attack roll, an ability check, or a saving throw"
    :range "60 feet"
    :components {:verbal true}
    :duration instantaneous
    :description "You magically distract the triggering creature and turn its momentary uncertainty into encouragement for another creature. The triggering creature must reroll the d20 and use the lower roll.
You can then choose a different creature you can see within range (you can choose yourself). The chosen creature has advantage on the next attack roll, ability check, or saving throw it makes within 1 minute. A creature can be empowered by only one use of this spell at a time."
    }
   {
    :name "Simulacrum"
    :school illusion
    :level 7
    :casting-time "12 hours"
    :range touch
    :components {:verbal true :somatic true :material true :material-component "snow or ice in quantities sufficient to made a life-size copy of the duplicated creature; some hair, fingernail clippings, or other piece of that creature's body placed inside the snow or ice; and powdered ruby worth 1,500 gp, sprinkled over the duplicate and consumed by the spell"}
    :duration "Until dispelled"
    :description "You shape an illusory duplicate of one beast or humanoid that is within range for the entire casting time of the spell. The duplicate is a creature, partially real and formed from ice or snow, and it can take actions and otherwise be affected as a normal creature. It appears to be the same as the original, but it has half the creature's hit point maximum and is formed without any equipment. Otherwise, the illusion uses all the statistics of the creature it duplicates.
The simulacrum is friendly to you and creatures you designate. It obeys your spoken commands, moving and acting in accordance with your wishes and acting on your turn in combat. The simulacrum lacks the ability to learn or become more powerful, so it never increases its level or other abilities, nor can it regain expended spell slots.
If the simulacrum is damaged, you can repair it in an alchemical laboratory, using rare herbs and minerals worth 100 gp per hit point it regains. The simulacrum lasts until it drops to 0 hit points, at which point it reverts to snow and melts instantly.
If you cast this spell again, any currently active duplicates you created with this spell are instantly destroyed."
    }
   {
    :name "Skill Empowerment"
    :school transmutation
    :level 5
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true}
    :duration "Concentration, up to 1 hour"
    :description "Your magic deepens a creature's understanding of its own talent. You touch one willing creature and give it expertise in one skill of your choice; until the spell ends, the creature doubles its proficiency bonus for ability checks it makes that use the chosen skill.
You must choose a skill in which the target is proficient and that isn't already benefiting from an effect, such as Expertise, that doubles its proficiency bonus."
    }
   {
    :name "Skywrite"
    :ritual true
    :school transmutation
    :level 2
    :casting-time actions-1
    :range "Sight"
    :components {:verbal true :somatic true}
    :duration "Concentration, up to 1 hour"
    :description "You cause up to ten words to form in a part of the sky you can see. The words appear to be made of cloud and remain in place for the spell's duration. The words dissipate when the spell ends. A strong wind can disperse the clouds and end the spell early."
    }
   {
    :name "Sleep"
    :school enchantment
    :level 1
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :somatic true :material true :material-component "a pinch of fine sand, rose petals, or a cricket"}
    :duration "1 minute"
    :description "This spell sends creatures into a magical slumber. Roll 5d8; the total is how many hit points of creatures this spell can affect. Creatures within 20 feet of a point you choose within range are affected in ascending order of their current hit points (ignoring unconscious creatures). Starting with the creature that has the lowest current hit points, each creature affected by this spell falls unconscious until the spell ends, the sleeper takes damage, or someone uses an action to shake or slap the sleeper awake. Subtract each creature's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected.
Undead and creatures immune to being charmed aren't affected by this spell.
At Higher Levels. When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d8 for each slot level above 1st."
    }
   {
    :name "Sleet Storm"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "150 feet"
    :components {:verbal true :somatic true :material true :material-component "a pinch of dust and a few drops of water"}
    :duration conc-1-min
    :description "Until the spell ends, freezing rain and sleet fall in a 20-foot-tall cylinder with a 40-foot radius centered on a point you choose within range. The area is heavily obscured, and exposed flames in the area are doused.
The ground in the area is covered with slick ice, making it difficult terrain. When a creature enters the spell's area for the first time on a turn or starts its turn there, it must make a Dexterity saving throw. On a failed save, it falls prone.
If a creature is concentrating in the spell's area, the creature must make a successful Constitution saving throw against your spell save DC or lose concentration."
    }
   {
    :name "Slow"
    :school transmutation
    :level 3
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true :material true :material-component "a drop of molasses"}
    :duration conc-1-min
    :description "You alter time around up to six creatures of your choice in a 40-foot cube within range. Each target must succeed on a Wisdom saving throw or be affected by this spell for the duration.
An affected target's speed is halved, it takes a -2 penalty to AC and Dexterity saving throws, and it can't use reactions. On its turn, it can use either an action or a bonus action, not both. Regardless of the creature's abilities or magic items, it can't make more than one melee or ranged attack during its turn.
If the creature attempts to cast a spell with a casting time of 1 action, roll a d20. On an 11 or higher, the spell doesn't take effect until the creature's next turn, and the creature must use its action on that turn to complete the spell. If it can't, the spell is wasted.
A creature affected by this spell makes another Wisdom saving throw at the end of its turn. On a successful save, the effect ends for it."
    }
   {
    :name "Snare"
    :school abjuration
    :level 1
    :casting-time "1 minute"
    :range touch
    :components {:somatic true :material true :material-component "25 feet of rope, which the spell consumes"}
    :duration "8 hours"
    :description "As you cast this spell, you use the rope to create a circle with a 5-foot radius on the ground or the floor. When you finish casting, the rope disappears and the circle becomes a magic trap.
This trap is nearly invisible, requiring a successful Intelligence (Investigation) check against your spell save DC to be discerned.
The trap triggers when a Small, Medium, or Large creature moves onto the ground or the floor in the spell's radius. That creature must succeed on a Dexterity saving throw or be magically hoisted into the air, leaving it hanging upside down 3 feet above the ground or the floor. The creature is restrained there until the spell ends.
A restrained creature can make a Dexterity saving throw at the end of each of its turns, ending the effect on itself on a success. Alternatively, the creature or someone else who can reach it can use an action to make an Intelligence (Arcana) check against your spell save DC. On a success, the restrained effect ends.
After the trap is triggered, the spell ends when no creature is restrained by it."
    }
   {
    :name "Snilloc's Snowball Swarm"
    :key :snillocs-snowball-swarm
    :school evocation
    :level 2
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :somatic true :material true :material-component "a piece of ice or a small white rock chip"}
    :duration instantaneous
    :description "A flurry of magic snowballs erupts from a point you choose within range. Each creature in a 5-foot-radius sphere centered on that point must make a Dexterity saving throw. A creature takes 3d6 cold damage on a failed save, or half as much damage on a successful one.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
   {
    :name "Soul Cage"
    :school necromancy
    :level 6
    :casting-time "1 reaction, which you take when a humanoid you can see within 60 feet of you dies"
    :range "60 feet"
    :components {:verbal true :somatic true :material true :material-component "a tiny silver cage worth 100 gp"}
    :duration "8 hours"
    :description "This spell snatches the soul of a humanoid as it dies and traps it inside the tiny cage you use for the material component. A stolen soul remains inside the cage until the spell ends or until you destroy the cage, which ends the spell. While you have a soul inside the cage, you can exploit it in any of the ways described below. You can use a trapped soul up to six times. Once you exploit a soul for the sixth time, it is released, and the spell ends. While a soul is trapped, the dead humanoid it came from can't be revived.
Steal Life. You can use a bonus action to drain vigor from the soul and regain 2d8 hit points.
Query Soul. You ask the soul a question (no action required) and receive a brief telepathic answer, which you can understand regardless of the language used. The soul knows only what it knew in life, but it must answer you truthfully and to the best of its ability. The answer is no more than a sentence or two and might be cryptic.
Borrow Experience. You can use a bonus action to bolster yourself with the soul's life experience, making your next attack roll, ability check, or saving throw with advantage. If you don't use this benefit before the start of your next turn, it is lost.
Eyes of the Dead. You can use an action to name a place the humanoid saw in life, which creates an invisible sensor somewhere in that place if it is on the plane of existence you're currently on. The sensor remains for as long as you concentrate, up to 10 minutes (as if you were concentrating on a spell). You receive visual and auditory information from the sensor as if you were in its space using your senses.
A creature that can see the sensor (such as one using see invisibility or truesight) sees a translucent image of the tormented humanoid whose soul you caged."
    }
   {
    :name "Spare the Dying"
    :school necromancy
    :level 0
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "You touch a living creature that has 0 hit points. The creature becomes stable. This spell has no effect on undead or constructs."}
   {
    :name "Speak with Animals"
    :ritual true
    :school divination
    :level 1
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true}
    :duration "10 minutes"
    :description "You gain the ability to comprehend and verbally communicate with beasts for the duration. The knowledge and awareness of many beasts is limited by their intelligence, but at minimum, beasts can give you information about nearby locations and monsters, including whatever they can perceive or have perceived within the past day. You might be able to persuade a beast to perform a small favor for you, at the GM's discretion."}
   {
    :name "Speak with Dead"
    :school necromancy
    :level 3
    :casting-time actions-1
    :range "10 feet"
    :components {:verbal true :somatic true :material true :material-component "burning incense"}
    :duration "10 minutes"
    :description "You grant the semblance of life and intelligence to a corpse of your choice within range, allowing it to answer the questions you pose. The corpse must still have a mouth and can't be undead. The spell fails if the corpse was the target of this spell within the last 10 days.
Until the spell ends, you can ask the corpse up to five questions. The corpse knows only what it knew in life, including the languages it knew. Answers are usually brief, cryptic, or repetitive, and the corpse is under no compulsion to offer a truthful answer if you are hostile to it or it recognizes you as an enemy. This spell doesn't return the creature's soul to its body, only its animating spirit. Thus, the corpse can't learn new information, doesn't comprehend anything that has happened since it died, and can't speculate about future events."
    }
   {
    :name "Speak with Plants"
    :school transmutation
    :level 3
    :casting-time actions-1
    :range "Self (30-foot radius)"
    :components {:verbal true :somatic true}
    :duration "10 minutes"
    :description "You imbue plants within 30 feet of you with limited sentience and animation, giving them the ability to communicate with you and follow your simple commands. You can question plants about events in the spell's area within the past day, gaining information about creatures that have passed, weather, and other circumstances.
You can also turn difficult terrain caused by plant growth (such as thickets and undergrowth) into ordinary terrain that lasts for the duration. Or you can turn ordinary terrain where plants are present into difficult terrain that lasts for the duration, causing vines and branches to hinder pursuers, for example.
Plants might be able to perform other tasks on your behalf, at the GM's discretion. The spell doesn't enable plants to uproot themselves and move about, but they can freely move branches, tendrils, and stalks.
If a plant creature is in the area, you can communicate with it as if you shared a common language, but you gain no magical ability to influence it.
This spell can cause the plants created by the entangle spell to release a restrained creature."
    }
   {
    :name "Spider Climb"
    :school transmutation
    :level 2
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "a drop of bitumen and a spider"}
    :duration "Concentration, up to 1 hour"
    :description "Until the spell ends, one willing creature you touch gains the ability to move up, down, and across vertical surfaces and upside down along ceilings, while leaving its hands free. The target also gains a climbing speed equal to its walking speed."
    }
   {
    :name "Spike Growth"
    :school transmutation
    :level 2
    :casting-time actions-1
    :range "150 feet"
    :components {:verbal true :somatic true :material true :material-component "seven sharp thorns or seven small twigs, each sharpened to a point"}
    :duration conc-10-min
    :description "The ground in a 20-foot radius centered on a point within range twists and sprouts hard spikes and thorns. The area becomes difficult terrain for the duration. When a creature moves into or within the area, it takes 2d4 piercing damage for every 5 feet it travels.
The transformation of the ground is camouflaged to look natural. Any creature that can't see the area at the time the spell is cast must make a Wisdom (Perception) check against your spell save DC to recognize the terrain as hazardous before entering it."
    }
   {
    :name "Spirit Guardians"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "Self (15-foot radius)"
    :components {:verbal true :somatic true :material true :material-component "a holy symbol"}
    :duration conc-10-min
    :description "You call forth spirits to protect you. They flit around you to a distance of 15 feet for the duration. If you are good or neutral, their spectral form appears angelic or fey (your choice). If you are evil, they appear fiendish.
When you cast this spell, you can designate any number of creatures you can see to be unaffected by it. An affected creature's speed is halved in the area, and when the creature enters the area for the first time on a turn or starts its turn there, it must make a Wisdom saving throw. On a failed save, the creature takes 3d8 radiant damage (if you are good or neutral) or 3d8 necrotic damage (if you are evil). On a successful save, the creature takes half as much damage.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1d8 for each slot level above 3rd."
    }
   {
    :name "Spirit Shroud"
    :school necromancy
    :level 3
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true :somatic true}
    :duration conc-1-min
    :description "You call forth spirits of the dead, which flit around you for the spell's duration. The spirits are intangible and invulnerable.
Until the spell ends, any attack you make deals 1d8 extra damage when you hit a creature within 10 feet of you. This damage is radiant, necrotic, or cold (your choice when you cast the spell). Any creature that takes this damage can't regain hit points until the start of your next turn.
In addition, any creature of your choice that you can see that starts its turn within 10 feet of you has its speed reduced by 10 feet until the start of your next turn.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1d8 for every two slot levels above 3rd."
    }
   {
    :name "Spiritual Weapon"
    :school evocation
    :level 2
    :casting-time bonus-actions-1
    :range "60 feet"
    :components {:verbal true :somatic true}
    :duration "1 minute"
    :description "You create a floating, spectral weapon within range that lasts for the duration or until you cast this spell again. When you cast the spell, you can make a melee spell attack against a creature within 5 feet of the weapon. On a hit, the target takes force damage equal to 1d8 + your spellcasting ability modifier.
As a bonus action on your turn, you can move the weapon up to 20 feet and repeat the attack against a creature within 5 feet of it.
The weapon can take whatever form you choose. Clerics of deities who are associated with a particular weapon (as St. Cuthbert is known for his mace and Thor for his hammer) make this spell's effect resemble that weapon.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for every two slot levels above 2nd."
    }
   {
    :name "Staggering Smite"
    :school evocation
    :level 4
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true}
    :duration conc-1-min
    :description "The next time you hit a creature with a melee weapon attack during this spell's duration, your weapon pierces both body and mind, and the attack deals an extra 4d6 psychic damage to the target. The target must make a Wisdom saving throw. On a failed save, it has disadvantage on attack rolls and ability checks, and can't take reactions, until the end of its next turn."
    }
   {
    :name "Steel Wind Strike"
    :school conjuration
    :level 5
    :casting-time actions-1
    :range "30 feet"
    :components {:somatic true :material true :material-component "Steel Wind Strike"}
    :duration instantaneous
    :description "You flourish the weapon used in the casting and then vanish to strike like the wind. Choose up to five creatures you can see within range. Make a melee spell attack against each target. On a hit, a target takes 6d10 force damage.
You can then teleport to an unoccupied space you can see within 5 feet of one of the targets you hit or missed."
    }
   {
    :name "Stinking Cloud"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :somatic true :material true :material-component "a rotten egg or several skunk cabbage leaves"}
    :duration conc-1-min
    :description "You create a 20-foot-radius sphere of yellow, nauseating gas centered on a point within range. The cloud spreads around corners, and its area is heavily obscured. The cloud lingers in the air for the duration.
Each creature that is completely within the cloud at the start of its turn must make a Constitution saving throw against poison. On a failed save, the creature spends its action that turn retching and reeling. Creatures that don't need to breathe or are immune to poison automatically succeed on this saving throw.
A moderate wind (at least 10 miles per hour) disperses the cloud after 4 rounds. A strong wind (at least 20 miles per hour) disperses it after 1 round."
    }
   {
    :name "Stone Shape"
    :school transmutation
    :level 4
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "soft clay, which must be worked into roughly the desired shape of the stone object"}
    :duration instantaneous
    :description "You touch a stone object of Medium size or smaller or a section of stone no more than 5 feet in any dimension and form it into any shape that suits your purpose. So, for example, you could shape a large rock into a weapon, idol, or coffer, or make a small passage through a wall, as long as the wall is less than 5 feet thick. You could also shape a stone door or its frame to seal the door shut. The object you create can have up to two hinges and a latch, but finer mechanical detail isn't possible."
    }
   {
    :name "Stoneskin"
    :school abjuration
    :level 4
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "diamond dust worth 100 gp, which the spell consumes"}
    :duration "Concentration, up to 1 hour"
    :description "This spell turns the flesh of a willing creature you touch as hard as stone. Until the spell ends, the target has resistance to nonmagical bludgeoning, piercing, and slashing damage."
    }
   {
    :name "Storm of Vengeance"
    :school conjuration
    :level 9
    :casting-time actions-1
    :range "Sight"
    :components {:verbal true :somatic true}
    :duration conc-1-min
    :description "A churning storm cloud forms, centered on a point you can see and spreading to a radius of 360 feet. Lightning flashes in the area, thunder booms, and strong winds roar. Each creature under the cloud (no more than 5,000 feet beneath the cloud) when it appears must make a Constitution saving throw. On a failed save, a creature takes 2d6 thunder damage and becomes deafened for 5 minutes.
Each round you maintain concentration on this spell, the storm produces additional effects on your turn.
Round 2. Acidic rain falls from the cloud. Each creature and object under the cloud takes 1d6 acid damage.
Round 3. You call six bolts of lightning from the cloud to strike six creatures or objects of your choice beneath the cloud. A given creature or object can't be struck by more than one bolt. A struck creature must make a Dexterity saving throw. The creature takes 10d6 lightning damage on a failed save, or half as much damage on a successful one.
Round 4. Hailstones rain down from the cloud. Each creature under the cloud takes 2d6 bludgeoning damage.
Round 5–10. Gusts and freezing rain assail the area under the cloud. The area becomes difficult terrain and is heavily obscured. Each creature there takes 1d6 cold damage. Ranged weapon attacks in the area are impossible. The wind and rain count as a severe distraction for the purposes of maintaining concentration on spells. Finally, gusts of strong wind (ranging from 20 to 50 miles per hour) automatically disperse fog, mists, and similar phenomena in the area, whether mundane or magical."
    }
   {
    :name "Storm Sphere"
    :school evocation
    :level 4
    :casting-time actions-1
    :range "150 feet"
    :components {:verbal true :somatic true}
    :duration conc-1-min
    :description "A 20-foot-radius sphere of whirling air springs into existence, centered on a point you choose within range. The sphere remains for the spell's duration. Each creature in the sphere when it appears or that ends its turn there must succeed on a Strength saving throw or take 2d6 bludgeoning damage. The sphere's space is difficult terrain.
Until the spell ends, you can use a bonus action on each of your turns to cause a bolt of lightning to leap from the center of the sphere toward one creature you choose within 60 feet of the center. Make a ranged spell attack. You have advantage on the attack roll if the target is in the sphere. On a hit, the target takes 4d6 lightning damage.
Creatures within 30 feet of the sphere have disadvantage on Wisdom (Perception) checks made to listen.
At Higher Levels. When you cast this spell using a spell slot of 5th level or higher, the damage for each of its effects increases by 1d6 for each slot level above 4th."
    }
   {
    :name "Suggestion"
    :school enchantment
    :level 2
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :material true :material-component "a snake's tongue and either a bit of honeycomb or a drop of sweet oil"}
    :duration "Concentration, up to 8 hours"
    :description "You suggest a course of activity (limited to a sentence or two) and magically influence a creature you can see within range that can hear and understand you. Creatures that can't be charmed are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act ends the spell.
The target must make a Wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.
You can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a knight give her warhorse to the first beggar she meets. If the condition isn't met before the spell expires, the activity isn't performed.
If you or any of your companions damage the target, the spell ends."
    }
   {
    :name "Summon Aberration"
    :school conjuration
    :level 4
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "a pickled tentacle and an eyeball in a platinum-inlaid vial worth at least 400 gp)"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth an aberrant spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Aberrant Spirit stat block. When you cast the spell, choose Beholderkin, Slaad, or Star Spawn. The creature resembles an aberration of that kind, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 5th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Beast"
    :school conjuration
    :level 2
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "a feather, tuft of fur, and fish tail inside a gilded acorn worth at least 200 gp)"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a bestial spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Bestial Spirit stat block. When you cast the spell, choose an environment: Air, Land, or Water. The creature resembles an animal of your choice that is native to the chosen environment, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Celestial"
    :school conjuration
    :level 5
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "a golden reliquary worth at least 500 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a celestial spirit. It manifests in an angelic form in an unoccupied space that you can see within range. This corporeal form uses the Celestial Spirit stat block. When you cast the spell, choose Avenger or Defender. Your choice determines the creature's attack in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 6th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Construct"
    :school conjuration
    :level 4
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "an ornate stone and metal lockbox worth at least 400 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth the spirit of a construct. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Construct Spirit stat block. When you cast the spell, choose a material: Clay, Metal, or Stone. The creature resembles a golem or a modron (your choice) made of the chosen material, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Draconic Spirit"
    :school conjuration
    :level 5
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :material true :material-component "an object with the image of a dragon engraved on it, worth at least 500 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a draconic spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Draconic Spirit stat block. When you cast this spell, choose a family of dragon: chromatic, gem, or metallic. The creature resembles a dragon of the chosen family, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 6th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Elemental"
    :school conjuration
    :level 4
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "air, a pebble, ash, and water inside a gold-inlaid vial worth at least 400 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth an elemental spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Elemental Spirit stat block. When you cast the spell, choose an element: Air, Earth, Fire, or Water. The creature resembles a bipedal form wreathed in the chosen element, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 5th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Fey"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "a gilded flower worth at least 300 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a fey spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Fey Spirit stat block. When you cast the spell, choose a mood: Fuming, Mirthful, or Tricksy. The creature resembles a fey creature of your choice marked by the chosen mood, which determines one of the traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Fiend"
    :school conjuration
    :level 6
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "humanoid blood inside a ruby vial worth at least 600 gp)"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a fiendish spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Fiendish Spirit stat block. When you cast the spell, choose Demon, Devil, or Yugoloth. The creature resembles a fiend of the chosen type, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 7th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Greater Demon"
    :school conjuration
    :level 4
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :material true :material-component "a vial of blood from a humanoid killed within the past 24 hours"}
    :duration "Concentration, up to 1 hour"
    :description "You utter foul words, summoning one demon from the chaos of the Abyss. You choose the demon's type, which must be one of challenge rating 5 or lower, such as a shadow demon or a barlgura. The demon appears in an unoccupied space you can see within range, and the demon disappears when it drops to 0 hit points or when the spell ends.
Roll initiative for the demon, which has its own turns. When you summon it and on each of your turns thereafter, you can issue a verbal command to it (requiring no action on your part), telling it what it must do on its next turn. If you issue no command, it spends its turn attacking any creature within reach that has attacked it.
At the end of each of the demon's turns, it makes a Charisma saving throw. The demon has disadvantage on this saving throw if you say its true name. On a failed save, the demon continues to obey you. On a successful save, your control of the demon ends for the rest of the duration, and the demon spends its turns pursuing and attacking the nearest non-demons to the best of its ability. If you stop concentrating on the spell before it reaches its full duration, an uncontrolled demon doesn't disappear for 1d6 rounds if it still has hit points.
As part of casting the spell, you can form a circle on the ground with the blood used as a material component. The circle is large enough to encompass your space. While the spell lasts, the summoned demon can't cross the circle or harm it, and it can't target anyone within it. Using the material component in this manner consumes it when the spell ends.
At Higher Levels. When you cast this spell using a spell slot of 5th level or higher, the challenge rating increases by 1 for each slot level above 4th."
    }
   {
    :name "Summon Lesser Demons"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :material true :material-component "a vial of blood from a humanoid killed within the past 24 hours"}
    :duration "Concentration, up to 1 hour"
    :description "You utter foul words, summoning demons from the chaos of the Abyss. Roll on the following table to determine what appears.
d6	Demons Summoned
1-2	Two demons of challenge rating 1 or lower
3-4	Four demons of challenge rating 1/2 or lower
5-6	Eight demons of challenge rating 1/4 or lower
The DM chooses the demons, such as manes or dretches, and you choose the unoccupied spaces you can see within range where they appear. A summoned demon disappears when it drops to 0 hit points or when the spell ends.
The demons are hostile to all creatures, including you. Roll initiative for the summoned demons as a group, which has its own turns. The demons pursue and attack the nearest non-demons to the best of their ability.
As part of casting the spell, you can form a circle on the ground with the blood used as a material component. The circle is large enough to encompass your space. While the spell lasts, the summoned demons can't cross the circle or harm it, and they can't target anyone within it. Using the material component in this manner consumes it when the spell ends.
At Higher Levels. When you cast this spell using a spell slot of 6th or 7th level, you summon twice as many demons. If you cast it using a spell slot of 8th or 9th level, you summon three times as many demons."
    }
   {
    :name "Summon Shadowspawn"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "tears inside a gem worth at least 300 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth a shadowy spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Shadow Spirit stat block. When you cast the spell, choose an emotion: Fury, Despair, or Fear. The creature resembles a misshapen biped marked by the chosen emotion, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Summon Undead"
    :school necromancy
    :level 3
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :material true :material-component "a gilded skull worth at least 300 gp"}
    :duration "Concentration, up to 1 hour"
    :description "You call forth an undead spirit. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Undead Spirit stat block. When you cast the spell, choose the creature's form: Ghostly, Putrid, or Skeletal. The spirit resembles an undead creature with the chosen form, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.
The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
   {
    :name "Sunbeam"
    :school evocation
    :level 6
    :casting-time actions-1
    :range "Self (60-foot line)"
    :components {:verbal true :somatic true :material true :material-component "a magnifying glass"}
    :duration conc-1-min
    :description "A beam of brilliant light flashes out from your hand in a 5-foot-wide, 60-foot-long line. Each creature in the line must make a Constitution saving throw. On a failed save, a creature takes 6d8 radiant damage and is blinded until your next turn. On a successful save, it takes half as much damage and isn't blinded by this spell. Undead and oozes have disadvantage on this saving throw.
You can create a new line of radiance as your action on any turn until the spell ends.
For the duration, a mote of brilliant radiance shines in your hand. It sheds bright light in a 30-foot radius and dim light for an additional 30 feet. This light is sunlight."
    }
   {
    :name "Sunburst"
    :school evocation
    :level 8
    :casting-time actions-1
    :range "150 feet"
    :components {:verbal true :somatic true :material true :material-component "fire and a piece of sunstone"}
    :duration instantaneous
    :description "Brilliant sunlight flashes in a 60-foot radius centered on a point you choose within range. Each creature in that light must make a Constitution saving throw. On a failed save, a creature takes 12d6 radiant damage and is blinded for 1 minute. On a successful save, it takes half as much damage and isn't blinded by this spell. Undead and oozes have disadvantage on this saving throw.
A creature blinded by this spell makes another Constitution saving throw at the end of each of its turns. On a successful save, it is no longer blinded.
This spell dispels any darkness in its area that was created by a spell."
    }
   {
    :name "Swift Quiver"
    :school transmutation
    :level 5
    :casting-time bonus-actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "a quiver containing at least one piece of ammunition"}
    :duration conc-1-min
    :description "You transmute your quiver so it produces an endless supply of nonmagical ammunition, which seems to leap into your hand when you reach for it.
On each of your turns until the spell ends, you can use a bonus action to make two attacks with a weapon that uses ammunition from the quiver. Each time you make such a ranged attack, your quiver magically replaces the piece of ammunition you used with a similar piece of nonmagical ammunition. Any pieces of ammunition created by this spell disintegrate when the spell ends. If the quiver leaves your possession, the spell ends."
    }
   {
    :name "Sword Burst"
    :school conjuration
    :level 0
    :casting-time actions-1
    :range "Self (5-foot radius)"
    :components {:verbal true}
    :duration instantaneous
    :description "You create a momentary circle of spectral blades that sweep around you. All other creatures within 5 feet of you must succeed on a Dexterity saving throw or take 1d6 force damage.
This spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
   {
    :name "Symbol"
    :school abjuration
    :level 7
    :casting-time "1 minute"
    :range touch
    :components {:verbal true :somatic true :material true :material-component "mercury, phosphorus, and powdered diamond and opal with a total value of at least 1,000 gp, which the spell consumes"}
    :duration "Until dispelled or triggered"
    :description "When you cast this spell, you inscribe a harmful glyph either on a surface (such as a section of floor, a wall, or a table) or within an object that can be closed to conceal the glyph (such as a book, a scroll, or a treasure chest). If you choose a surface, the glyph can cover an area of the surface no larger than 10 feet in diameter. If you choose an object, that object must remain in its place; if the object is moved more than 10 feet from where you cast this spell, the glyph is broken, and the spell ends without being triggered.
The glyph is nearly invisible, requiring an Intelligence (Investigation) check against your spell save DC to find it. You decide what triggers the glyph when you cast the spell. For glyphs inscribed on a surface, the most typical triggers include touching or stepping on the glyph, removing another object covering it, approaching within a certain distance of it, or manipulating the object that holds it. For glyphs inscribed within an object, the most common triggers are opening the object, approaching within a certain distance of it, or seeing or reading the glyph.
You can further refine the trigger so the spell is activated only under certain circumstances or according to a creature's physical characteristics (such as height or weight), or physical kind (for example, the ward could be set to affect hags or shapechangers). You can also specify creatures that don't trigger the glyph, such as those who say a certain password.
When you inscribe the glyph, choose one of the options below for its effect. Once triggered, the glyph glows, filling a 60-foot-radius sphere with dim light for 10 minutes, after which time the spell ends. Each creature in the sphere when the glyph activates is targeted by its effect, as is a creature that enters the sphere for the first time on a turn or ends its turn there.
Death. Each target must make a Constitution saving throw, taking 10d10 necrotic damage on a failed save, or half as much damage on a successful save.
Discord. Each target must make a Constitution saving throw. On a failed save, a target bickers and argues with other creatures for 1 minute. During this time, it is incapable of meaningful communication and has disadvantage on attack rolls and ability checks.
Fear. Each target must make a Wisdom saving throw and becomes frightened for 1 minute on a failed save. While frightened, the target drops whatever it is holding and must move at least 30 feet away from the glyph on each of its turns, if able.
Hopelessness. Each target must make a Charisma saving throw. On a failed save, the target is overwhelmed with despair for 1 minute. During this time, it can't attack or target any creature with harmful abilities, spells, or other magical effects.
Insanity. Each target must make an Intelligence saving throw. On a failed save, the target is driven insane for 1 minute. An insane creature can't take actions, can't understand what other creatures say, can't read, and speaks only in gibberish. The GM controls its movement, which is erratic.
Pain. Each target must make a Constitution saving throw and becomes incapacitated with excruciating pain for 1 minute on a failed save.
Sleep. Each target must make a Wisdom saving throw and falls unconscious for 10 minutes on a failed save. A creature awakens if it takes damage or if someone uses an action to shake or slap it awake.
Stunning. Each target must make a Wisdom saving throw and becomes stunned for 1 minute on a failed save."
    }
   {
    :name "Synaptic Static"
    :school enchantment
    :level 5
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "You choose a point within range and cause psychic energy to explode there. Each creature in a 20-foot-radius sphere centered on that point must make an Intelligence saving throw. A creature with an Intelligence score of 2 or lower can't be affected by this spell. A target takes 8d6 psychic damage on a failed save, or half as much damage on a successful one.
After a failed save, a target has muddled thoughts for 1 minute. During that time, it rolls a d6 and subtracts the number rolled from all its attack rolls and ability checks, as well as its Constitution saving throws to maintain concentration. The target can make an Intelligence saving throw at the end of each of its turns, ending the effect on itself on a success."
    }])

(def t-spells
  [
   {
    :name "Tasha's Caustic Brew"
    :key :tashas-caustic-brew
    :school evocation
    :level 1
    :casting-time actions-1
    :range "Self (30-foot line)"
    :components {:verbal true :somatic true :material true :material-component "a bit of rotten food"}
    :duration conc-1-min
    :description "A stream of acid emanates from you in a line 30 feet long and 5 feet wide in a direction you choose. Each creature in the line must succeed on a Dexterity saving throw or be covered in acid for the spell's duration or until a creature uses its action to scrape or wash the acid off itself or another creature. A creature covered in the acid takes 2d4 acid damage at start of each of its turns.
At Higher Levels. When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 2d4 for each slot level above 1st."
    }
   {
    :name "Tasha's Hideous Laughter"
    :key :tashas-hideous-laughter
    :school enchantment
    :level 1
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true :material true :material-component "tiny tarts and a feather that is waved in the air"}
    :duration conc-1-min
    :description "A creature of your choice that you can see within range perceives everything as hilariously funny and falls into fits of laughter if this spell affects it. The target must succeed on a Wisdom saving throw or fall prone, becoming incapacitated and unable to stand up for the duration. A creature with an Intelligence score of 4 or less isn't affected.
At the end of each of its turns, and each time it takes damage, the target can make another Wisdom saving throw. The target has advantage on the saving throw if it's triggered by damage. On a success, the spell ends."
    }
   {
    :name "Tasha's Mind Whip"
    :key :tashas-mind-whip
    :school enchantment
    :level 2
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true}
    :duration "1 round"
    :description "You psychically lash out at one creature you can see within range. The target must make an Intelligence saving throw. On a failed save, the target takes 3d6 psychic damage, and it can't take a reaction until the end of its next turn. Moreover, on its next turn, it must choose whether it gets a move, an action, or a bonus action; it gets only one of the three. On a successful save, the target takes half as much damage and suffers none of the spell's other effects.
At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd. The creatures must be within 30 feet of each other when you target them."
    }
   {
    :name "Tasha's Otherworldly Guise"
    :key :tashas-otherworldly-guise
    :school transmutation
    :level 6
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true :somatic true :material true :material-component "an object engraved with a symbol of the Outer Planes, worth at least 500 gp"}
    :duration conc-1-min
    :description "Uttering an incantation, you draw on the magic of the Lower Planes or Upper Planes (your choice) to transform yourself. You gain the following benefits until the spell ends:
• You are immune to fire and poison damage (Lower Planes) or radiant and necrotic damage (Upper Planes).
• You are immune to the poisoned condition (Lower Planes) or the charmed condition (Upper Planes).
• Spectral wings appear on your back, giving you a flying speed of 40 feet.
• You have a +2 bonus to AC.
• All your weapon attacks are magical, and when you make a weapon attack, you can use your spellcasting ability modifier, instead of Strength or Dexterity, for the attack and damage rolls.
• You can attack twice, instead of once, when you take the Attack action on your turn. You ignore this benefit if you already have a feature, like Extra Attack, that lets you attack more than once when you take the Attack action on your turn."
    }
   {
    :name "Telekinesis"
    :school transmutation
    :level 5
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true}
    :duration conc-10-min
    :description "You gain the ability to move or manipulate creatures or objects by thought. When you cast the spell, and as your action each round for the duration, you can exert your will on one creature or object that you can see within range, causing the appropriate effect below. You can affect the same target round after round, or choose a new one at any time. If you switch targets, the prior target is no longer affected by the spell.
Creature. You can try to move a Huge or smaller creature. Make an ability check with your spellcasting ability contested by the creature's Strength check. If you win the contest, you move the creature up to 30 feet in any direction, including upward but not beyond the range of this spell. Until the end of your next turn, the creature is restrained in your telekinetic grip. A creature lifted upward is suspended in mid-air.
On subsequent rounds, you can use your action to attempt to maintain your telekinetic grip on the creature by repeating the contest.
Object. You can try to move an object that weighs up to 1,000 pounds. If the object isn't being worn or carried, you automatically move it up to 30 feet in any direction, but not beyond the range of this spell.
If the object is worn or carried by a creature, you must make an ability check with your spellcasting ability contested by that creature's Strength check. If you succeed, you pull the object away from that creature and can move it up to 30 feet in any direction but not beyond the range of this spell.
You can exert fine control on objects with your telekinetic grip, such as manipulating a simple tool, opening a door or a container, stowing or retrieving an item from an open container, or pouring the contents from a vial."
    }
   {
    :name "Telepathy"
    :school evocation
    :level 8
    :casting-time actions-1
    :range "Unlimited"
    :components {:verbal true :somatic true :material true :material-component "a pair of linked silver rings"}
    :duration "24 hours"
    :description "You create a telepathic link between yourself and a willing creature with which you are familiar. The creature can be anywhere on the same plane of existence as you. The spell ends if you or the target are no longer on the same plane.
Until the spell ends, you and the target can instantaneously share words, images, sounds, and other sensory messages with one another through the link, and the target recognizes you as the creature it is communicating with. The spell enables a creature with an Intelligence score of at least 1 to understand the meaning of your words and take in the scope of any sensory messages you send to it."
    }
   {
    :name "Teleport"
    :school conjuration
    :level 7
    :casting-time actions-1
    :range "10 feet"
    :components {:verbal true}
    :duration instantaneous
    :description "This spell instantly transports you and up to eight willing creatures of your choice that you can see within range, or a single object that you can see within range, to a destination you select. If you target an object, it must be able to fit entirely inside a 10-foot cube, and it can't be held or carried by an unwilling creature.
The destination you choose must be known to you, and it must be on the same plane of existence as you. Your familiarity with the destination determines whether you arrive there successfully. The GM rolls d100 and consults the table.
Familiarity Mishap
Similar Area
Off Target
On Target
Permanent circle
— — — 01–100
Associated object
— — — 01–100
Very familiar 01–05 06–13 14–24 25–100
Seen casually 01–33 34–43 44–53 54–100
Viewed once 01–43 44–53 54–73 74–100
Description 01–43 44–53 54–73 74–100
False destination
01–50 51–100 — —
Familiarity. “Permanent circle” means a permanent teleportation circle whose sigil sequence you know. “Associated object” means that you possess an object taken from the desired destination within the last six months, such as a book from a wizard's library, bed linen from a royal suite, or a chunk of marble from a lich's secret tomb.
“Very familiar” is a place you have been very often, a place you have carefully studied, or a place you can see when you cast the spell. “Seen casually” is someplace you have seen more than once but with which you aren't very familiar. “Viewed once” is a place you have seen once, possibly using magic. “Description” is a place whose location and appearance you know through someone else's description, perhaps from a map.
“False destination” is a place that doesn't exist. Perhaps you tried to scry an enemy's sanctum but instead viewed an illusion, or you are attempting to teleport to a familiar location that no longer exists.
On Target. You and your group (or the target object) appear where you want to.
Off Target. You and your group (or the target object) appear a random distance away from the destination in a random direction. Distance off target is 1d10 × 1d10 percent of the distance that was to be traveled. For example, if you tried to travel 120 miles, landed off target, and rolled a 5 and 3 on the two d10s, then you would be off target by 15 percent, or 18 miles. The GM determines the direction off target randomly by rolling a d8 and designating 1 as north, 2 as northeast, 3 as east, and so on around the points of the compass. If you were teleporting to a coastal city and wound up 18 miles out at sea, you could be in trouble.
Similar Area. You and your group (or the target object) wind up in a different area that's visually or thematically similar to the target area. If you are heading for your home laboratory, for example, you might wind up in another wizard's laboratory or in an alchemical supply shop that has many of the same tools and implements as your laboratory. Generally, you appear in the closest similar place, but since the spell has no range limit, you could conceivably wind up anywhere on the plane.
Mishap. The spell's unpredictable magic results in a difficult journey. Each teleporting creature (or the target object) takes 3d10 force damage, and the GM rerolls on the table to see where you wind up (multiple mishaps can occur, dealing damage each time)."
    }
   {
    :name "Teleportation Circle"
    :school conjuration
    :level 5
    :casting-time "1 minute"
    :range "10 feet"
    :components {:verbal true :material true :material-component "rare chalks and inks infused with precious gems with 50 gp, which the spell consumes"}
    :duration "1 round"
    :description "As you cast the spell, you draw a 10-foot-diameter circle on the ground inscribed with sigils that link your location to a permanent teleportation circle of your choice whose sigil sequence you know and that is on the same plane of existence as you. A shimmering portal opens within the circle you drew and remains open until the end of your next turn. Any creature that enters the portal instantly appears within 5 feet of the destination circle or in the nearest unoccupied space if that space is occupied.
Many major temples, guilds, and other important places have permanent teleportation circles inscribed somewhere within their confines. Each such circle includes a unique sigil sequence—a string of magical runes arranged in a particular pattern. When you first gain the ability to cast this spell, you learn the sigil sequences for two destinations on the Material Plane, determined by the GM. You can learn additional sigil sequences during your adventures. You can commit a new sigil sequence to memory after studying it for 1 minute.
You can create a permanent teleportation circle by casting this spell in the same location every day for one year. You need not use the circle to teleport when you cast the spell in this way."
    }
   {
    :name "Temple of the Gods"
    :school conjuration
    :level 7
    :casting-time "1 hour"
    :range "120 feet"
    :components {:verbal true :somatic true :material true :material-component "a holy symbol worth at least 5 gp"}
    :duration "24 hours"
    :description "You cause a temple to shimmer into existence on ground you can see within range. The temple must fit within an unoccupied cube of space, up to 120 feet on each side. The temple remains until the spell ends. It is dedicated to whatever god, pantheon, or philosophy is represented by the holy symbol used in the casting.
You make all decisions about the temple's appearance. The interior is enclosed by a floor, walls, and a roof, with one door granting access to the interior and as many windows as you wish. Only you and any creatures you designate when you cast the spell can open or close the door.
The temple's interior is an open space with an idol or altar at one end. You decide whether the temple is illuminated and whether that illumination is bright light or dim light. The smell of burning incense fills the air within, and the temperature is mild.
The temple opposes types of creatures you choose when you cast this spell. Choose one or more of the following: celestials, elementals, fey, fiends, or undead. If a creature of the chosen type attempts to enter the temple, that creature must make a Charisma saving throw. On a failed save, it can't enter the temple for 24 hours. Even if the creature can enter the temple, the magic there hinders it; whenever it makes an attack roll, an ability check, or a saving throw inside the temple, it must roll a d4 and subtract the number rolled from the d20 roll.
In addition, the sensors created by divination spells can't appear inside the temple, and creatures within can't be targeted by divination spells.
Finally, whenever any creature in the temple regains hit points from a spell of 1st level or higher, the creature regains additional hit points equal to your Wisdom modifier (minimum 1 hit point).
The temple is made from opaque magical force that extends into the Ethereal Plane, thus blocking ethereal travel into the temple's interior. Nothing can physically pass through the temple's exterior. It can't be dispelled by dispel magic, and antimagic field has no effect on it. A disintegrate spell destroys the temple instantly.
Casting this spell on the same spot every day for a year makes this effect permanent."
    }
   {
    :name "Temporal Shunt"
    :school transmutation
    :level 5
    :casting-time "1 reaction, taken when a creature you can see makes an attack roll or starts to cast a spell"
    :range "120 feet"
    :components {:verbal true :somatic true}
    :duration "1 round"
    :description "You target the triggering creature, which must succeed on a Wisdom saving throw or vanish, being thrown to another point in time and causing the attack to miss or the spell to be wasted. At the start of its next turn, the target reappears where it was or in the closest unoccupied space. The target doesn't remember you casting the spell or being affected by it.
At Higher Levels. When you cast this spell using a spell slot of 6th level or higher, you can target one additional creature for each slot level above 5th. All targets must be within 30 feet of each other."
    }
   {
    :name "Tenser's Floating Disk"
    :key :tensers-floating-disk
    :ritual true
    :school conjuration
    :level 1
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true :material true :material-component "a drop of mercury"}
    :duration "1 hour"
    :description "This spell creates a circular, horizontal plane of force, 3 feet in diameter and 1 inch thick, that floats 3 feet above the ground in an unoccupied space of your choice that you can see within range. The disk remains for the duration, and can hold up to 500 pounds. If more weight is placed on it, the spell ends, and everything on the disk falls to the ground.
The disk is immobile while you are within 20 feet of it. If you move more than 20 feet away from it, the disk follows you so that it remains within 20 feet of you. It can move across uneven terrain, up or down stairs, slopes and the like, but it can't cross an elevation change of 10 feet or more. For example, the disk can't move across a 10-foot-deep pit, nor could it leave such a pit if it was created at the bottom.
If you move more than 100 feet from the disk (typically because it can't move around an obstacle to follow you), the spell ends."
    }
   {
    :name "Tenser's Transformation"
    :key :tensers-transformation
    :school transmutation
    :level 6
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true :material true :material-component "a few hairs from a bull"}
    :duration conc-10-min
    :description "You endow yourself with endurance and martial prowess fueled by magic. Until the spell ends, you can't cast spells, and you gain the following benefits:
• You gain 50 temporary hit points. If any of these remain when the spell ends, they are lost.
• You have advantage on attack rolls that you make with simple and martial weapons.
• When you hit a target with a weapon attack, that target takes an extra 2d12 force damage.
• You have proficiency with all armor, shields, simple weapons, and martial weapons.
• You have proficiency in Strength and Constitution saving throws.
• You can attack twice, instead of once, when you take the Attack action on your turn. You ignore this benefit if you already have a feature, like Extra Attack, that gives you extra attacks.
Immediately after the spell ends, you must succeed on a DC 15 Constitution saving throw or suffer one level of exhaustion."
    }
   {
    :name "Tether Essence"
    :school conjuration
    :level 7
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true :material true :material-component "a spool of platinum cord worth at least 250 gp, which the spell consumes"}
    :duration "Concentration, up to 1 hour"
    :description "Two creatures you can see within range must make a Constitution saving throw, with disadvantage if they are within 30 feet of each other. Either creature can willingly fail the save. If either save succeeds, the spell has no effect. If both saves fail, the creatures are magically linked for the duration, regardless of the distance between them. When damage is dealt to one of them, the same damage is dealt to the other one. If hit points are restored to one of them, the same number of hit points are restored to the other one. If either of the tethered creatures is reduced to 0 hit points, the spell ends on both. If the spell ends on one creature, it ends on both."
    }
   {
    :name "Thaumaturgy"
    :school transmutation
    :level 0
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true}
    :duration "Up to 1 minute"
    :description "You manifest a minor wonder, a sign of supernatural power, within range. You create one of the following magical effects within range:
• Your voice booms up to three times as loud as normal for 1 minute.
• You cause flames to flicker, brighten, dim, or change color for 1 minute.
• You cause harmless tremors in the ground for 1 minute.
• You create an instantaneous sound that originates from a point of your choice within range, such as a rumble of thunder, the cry of a raven, or ominous whispers.
• You instantaneously cause an unlocked door or window to fly open or slam shut.
• You alter the appearance of your eyes for 1 minute.
If you cast this spell multiple times, you can have up to three of its 1-minute effects active at a time, and you can dismiss such an effect as an action."
    }
   {
    :name "Thorn Whip"
    :school transmutation
    :level 0
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true :material true :material-component "the stem of a plant with thorns"}
    :duration instantaneous
    :attack-roll? true
    :description "You create a long, vine-like whip covered in thorns that lashes out at your command toward a creature in range. Make a melee spell attack against the target. If the attack hits, the creature takes 1d6 piercing damage, and if the creature is Large or smaller, you pull the creature up to 10 feet closer to you.
This spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
   {
    :name "Thunder Step"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true}
    :duration instantaneous
    :description "You teleport yourself to an unoccupied space you can see within range. Immediately after you disappear, a thunderous boom sounds, and each creature within 10 feet of the space you left must make a Constitution saving throw, taking 3d10 thunder damage on a failed save, or half as much damage on a successful one. The thunder can be heard from up to 300 feet away.
You can bring along objects as long as their weight doesn't exceed what you can carry. You can also teleport one willing creature of your size or smaller who is carrying gear up to its carrying capacity. The creature must be within 5 feet of you when you cast this spell, and there must be an unoccupied space within 5 feet of your destination space for the creature to appear in; otherwise, the creature is left behind.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1d10 for each slot level above 3rd."
    }
   {
    :name "Thunderclap"
    :school evocation
    :level 0
    :casting-time actions-1
    :range "5 feet"
    :components {:somatic true}
    :duration instantaneous
    :description "You create a burst of thunderous sound that can be heard up to 100 feet away. Each creature within range, other than you, must make a Constitution saving throw or take 1d6 thunder damage.
The spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
   {
    :name "Thunderous Smite"
    :school evocation
    :level 1
    :casting-time bonus-actions-1
    :range self
    :components {:verbal true}
    :duration conc-1-min
    :description "The first time you hit with a melee weapon attack during this spell's duration, your weapon rings with thunder that is audible within 300 feet of you, and the attack deals an extra 2d6 thunder damage to the target. Additionally, if the target is a creature, it must succeed on a Strength saving throw or be pushed 10 feet away from you and knocked prone."
    }
   {
    :name "Thunderwave"
    :school evocation
    :level 1
    :casting-time actions-1
    :range "Self (15-foot cube)"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "A wave of thunderous force sweeps out from you. Each creature in a 15-foot cube originating from you must make a Constitution saving throw. On a failed save, a creature takes 2d8 thunder damage and is pushed 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed.
In addition, unsecured objects that are completely within the area of effect are automatically pushed 10 feet away from you by the spell's effect, and the spell emits a thunderous boom audible out to 300 feet.
At Higher Levels. When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d8 for each slot level above 1st."
    }
   {
    :name "Tidal Wave"
    :school conjuration
    :level 3
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :somatic true :material true :material-component "a drop of water"}
    :duration instantaneous
    :description "You conjure up a wave of water that crashes down on an area within range. The area can be up to 30 feet long, up to 10 feet wide, and up to 10 feet tall. Each creature in that area must make a Dexterity saving throw. On a failed save, a creature takes 4d8 bludgeoning damage and is knocked prone. On a successful save, a creature takes half as much damage and isn't knocked prone. The water then spreads out across the ground in all directions, extinguishing unprotected flames in its area and within 30 feet of it, and then it vanishes."
    }
   {
    :name "Time Ravage"
    :school necromancy
    :level 9
    :casting-time actions-1
    :range "90 feet"
    :components {:verbal true :somatic true :material true :material-component "an hourglass filled with diamond dust worth at least 5,000 gp, which the spell consumes"}
    :duration instantaneous
    :description "You target a creature you can see within range, putting its physical form through the devastation of rapid aging. The target must make a Constitution saving throw, taking 10d12 necrotic damage on a failed save, or half as much damage on a successful one. If the save fails, the target also ages to the point where it has only 30 days left before it dies of old age. In this aged state, the target has disadvantage on attack rolls, ability checks, and saving throws, and its walking speed is halved. Only the wish spell or the greater restoration cast with a 9th-level spell slot can end these effects and restore the target to its previous age."
    }
   {
    :name "Time Stop"
    :school transmutation
    :level 9
    :casting-time actions-1
    :range self
    :components {:verbal true}
    :duration instantaneous
    :description "You briefly stop the flow of time for everyone but yourself. No time passes for other creatures, while you take 1d4 + 1 turns in a row, during which you can use actions and move as normal.
This spell ends if one of the actions you use during this period, or any effects that you create during this period, affects a creature other than you or an object being worn or carried by someone other than you. In addition, the spell ends if you move to a place more than 1,000 feet from the location where you cast it."
    }
   {
    :name "Tiny Servant"
    :school transmutation
    :level 3
    :casting-time "1 minute"
    :range touch
    :components {:verbal true :somatic true}
    :duration "8 hours"
    :description "You touch one Tiny, nonmagical object that isn't attached to another object or a surface and isn't being carried by another creature. The target animates and sprouts little arms and legs, becoming a creature under your control until the spell ends or the creature drops to 0 hit points. See the stat block for its statistics.
As a bonus action, you can mentally command the creature if it is within 120 feet of you. (If you control multiple creatures with this spell, you can command any or all of them at the same time, issuing the same command to each one.) You decide what action the creature will take and where it will move during its next turn, or you can issue a simple, general command, such as to fetch a key, stand watch, or stack some books. If you issue no commands, the servant does nothing other than defend itself against hostile creatures. Once given an order, the servant continues to follow that order until its task is complete.
When the creature drops to 0 hit points, it reverts to its original form, and any remaining damage carries over to that form.
At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, you can animate two additional objects for each slot level above 3rd."
    }
   {
    :name "Toll the Dead"
    :school necromancy
    :level 0
    :casting-time actions-1
    :range "60 feet"
    :components {:verbal true :somatic true}
    :duration instantaneous
    :description "You point at one creature you can see within range, and the sound of a dolorous bell fills the air around it for a moment. The target must succeed on a Wisdom saving throw or take 1d8 necrotic damage. If the target is missing any of its hit points, it instead takes 1d12 necrotic damage.
The spell's damage increases by one die when you reach 5th level (2d8 or 2d12), 11th level (3d8 or 3d12), and 17th level (4d8 or 4d12)."
    }
   {
    :name "Tongues"
    :school divination
    :level 3
    :casting-time actions-1
    :range touch
    :components {:verbal true :material true :material-component "a small clay model of a ziggurat"}
    :duration "1 hour"
    :description "This spell grants the creature you touch the ability to understand any spoken language it hears. Moreover, when the target speaks, any creature that knows at least one language and can hear the target understands what it says."
    }
   {
    :name "Transmute Rock"
    :school transmutation
    :level 5
    :casting-time actions-1
    :range "120 feet"
    :components {:verbal true :material true :material-component "clay and water"}
    :duration "Until dispelled"
    :description "You choose an area of stone or mud that you can see that fits within a 40-foot cube and is within range, and choose one of the following effects.
Transmute Rock to Mud. Nonmagical rock of any sort in the area becomes an equal volume of thick, flowing mud that remains for the spell's duration.
The ground in the spell's area becomes muddy enough that creatures can sink into it. Each foot that a creature moves through the mud costs 4 feet of movement, and any creature on the ground when you cast the spell must make a Strength saving throw. A creature must also make the saving throw when it moves into the area for the first time on a turn or ends its turn there. On a failed save, a creature sinks into the mud and is restrained, though it can use an action to end the restrained condition on itself by pulling itself free of the mud.
If you cast the spell on a ceiling, the mud falls. Any creature under the mud when it falls must make a Dexterity saving throw. A creature takes 4d8 bludgeoning damage on a failed save, or half as much damage on a successful one.
Transmute Mud to Rock. Nonmagical mud or quicksand in the area no more than 10 feet deep transforms into soft stone for the spell's duration. Any creature in the mud when it transforms must make a Dexterity saving throw. On a successful save, a creature is shunted safely to the surface in an unoccupied space. On a failed save, a creature becomes restrained by the rock. A restrained creature, or another creature within reach, can use an action to try to break the rock by succeeding on a DC 20 Strength check or by dealing damage to it. The rock has AC 15 and 25 hit points, and it is immune to poison and psychic damage."
    }
   {
    :name "Transport via Plants"
    :school conjuration
    :level 6
    :casting-time actions-1
    :range "10 feet"
    :components {:verbal true :somatic true}
    :duration "1 round"
    :description "This spell creates a magical link between a Large or larger inanimate plant within range and another plant, at any distance, on the same plane of existence. You must have seen or touched the destination plant at least once before. For the duration, any creature can step into the target plant and exit from the destination plant by using 5 feet of movement."
    }
   {
    :name "Tree Stride"
    :school conjuration
    :level 5
    :casting-time actions-1
    :range self
    :components {:verbal true :somatic true}
    :duration conc-1-min
    :description "You gain the ability to enter a tree and move from inside it to inside another tree of the same kind within 500 feet. Both trees must be living and at least the same size as you. You must use 5 feet of movement to enter a tree. You instantly know the location of all other trees of the same kind within 500 feet and, as part of the move used to enter the tree, can either pass into one of those trees or step out of the tree you're in. You appear in a spot of your choice within 5 feet of the destination tree, using another 5 feet of movement. If you have no movement left, you appear within 5 feet of the tree you entered.
You can use this transportation ability once per round for the duration. You must end each turn outside a tree."
    }
   {
    :name "True Polymorph"
    :school transmutation
    :level 9
    :casting-time actions-1
    :range "30 feet"
    :components {:verbal true :somatic true :material true :material-component "a drop of mercury, a dollop of gum arabic, and a wisp of smoke"}
    :duration "Concentration, up to 1 hour"
    :description "Choose one creature or nonmagical object that you can see within range. You transform the creature into a different creature, the creature into an object, or the object into a creature (the object must be neither worn nor carried by another creature). The transformation lasts for the duration, or until the target drops to 0 hit points or dies. If you concentrate on this spell for the full duration, the transformation lasts until it is dispelled.
This spell has no effect on a shapechanger or a creature with 0 hit points. An unwilling creature can make a Wisdom saving throw, and if it succeeds, it isn't affected by this spell.
Creature into Creature. If you turn a creature into another kind of creature, the new form can be any kind you choose whose challenge rating is equal to or less than the target's (or its level, if the target doesn't have a challenge rating). The target's game statistics, including mental ability scores, are replaced by the statistics of the new form. It retains its alignment and personality. The target assumes the hit points of its new form, and when it reverts to its normal form, the creature returns to the number of hit points it had before it transformed. If it reverts as a result of dropping to 0 hit points, any excess damage carries over to its normal form. As long as the excess damage doesn't reduce the creature's normal form to 0 hit points, it isn't knocked unconscious.
The creature is limited in the actions it can perform by the nature of its new form, and it can't speak, cast spells, or take any other action that requires hands or speech, unless its new form is capable of such actions.
The target's gear melds into the new form. The creature can't activate, use, wield, or otherwise benefit from any of its equipment.
Object into Creature. You can turn an object into any kind of creature, as long as the creature's size is no larger than the object's size and the creature's challenge rating is 9 or lower. The creature is friendly to you and your companions. It acts on each of your turns. You decide what action it takes and how it moves. The GM has the creature's statistics and resolves all of its actions and movement.
If the spell becomes permanent, you no longer control the creature. It might remain friendly to you, depending on how you have treated it.
Creature into Object. If you turn a creature into an object, it transforms along with whatever it is wearing and carrying into that form. The creature's statistics become those of the object, and the creature has no memory of time spent in this form, after the spell ends and it returns to its normal form."
    }
   {
    :name "True Resurrection"
    :school necromancy
    :level 9
    :casting-time "1 hour"
    :range touch
    :components {:verbal true :somatic true :material true :material-component "a sprinkle of holy water and diamonds worth at least 25,000 gp, which the spell consumes"}
    :duration instantaneous
    :description "You touch a creature that has been dead for no longer than 200 years and that died for any reason except old age. If the creature's soul is free and willing, the creature is restored to life with all its hit points.
This spell closes all wounds, neutralizes any poison, cures all diseases, and lifts any curses affecting the creature when it died. The spell replaces damaged or missing organs and limbs.
The spell can even provide a new body if the original no longer exists, in which case you must speak the creature's name. The creature then appears in an unoccupied space you choose within 10 feet of you."
    }
   {
    :name "True Seeing"
    :school divination
    :level 6
    :casting-time actions-1
    :range touch
    :components {:verbal true :somatic true :material true :material-component "an ointment for the eyes that costs 25 gp; is made from mushroom powder, saffron, and fat; and is consumed by the spell"}
    :duration "1 hour"
    :description "This spell gives the willing creature you touch the ability to see things as they actually are. For the duration, the creature has truesight, notices secret doors hidden by magic, and can see into the Ethereal Plane, all out to a range of 120 feet."
    }
   {
    :name "True Strike"
    :school divination
    :level 0
    :casting-time actions-1
    :range "30 feet"
    :components {:somatic true}
    :duration "Concentration, up to 1 round"
    :description "You extend your hand and point a finger at a target in range. Your magic grants you a brief insight into the target's defenses. On your next turn, you gain advantage on your first attack roll against the target, provided that this spell hasn't ended."
    }
   {
    :name "Tsunami"
    :school conjuration
    :level 8
    :casting-time "1 minute"
    :range "Sight"
    :components {:verbal true :somatic true}
    :duration "Concentration, up to 6 rounds"
    :description "A wall of water springs into existence at a point you choose within range. You can make the wall up to 300 feet long, 300 feet high, and 50 feet thick. The wall lasts for the duration.
When the wall appears, each creature within its area must make a Strength saving throw. On a failed save, a creature takes 6d10 bludgeoning damage, or half as much damage on a successful save.
At the start of each of your turns after the wall appears, the wall, along with any creatures in it, moves 50 feet away from you. Any Huge or smaller creature inside the wall or whose space the wall enters when it moves must succeed on a Strength saving throw or take 5d10 bludgeoning damage. A creature can take this damage only once per round. At the end of the turn, the wall's height is reduced by 50 feet, and the damage creatures take from the spell on subsequent rounds is reduced by 1d10. When the wall reaches 0 feet in height, the spell ends.
A creature caught in the wall can move by swimming. Because of the force of the wave, though, the creature must make a successful Strength (Athletics) check against your spell save DC in order to move at all. If it fails the check, it can't move. A creature that moves out of the area falls to the ground."
    }])


(spec/def ::level (spec/int-in 0 21))
(spec/def ::attunement? boolean?)
(spec/def ::item string?)

(spec/def ::infusion (spec/keys :req-un [::name ::key ::level]
                                :opt-un [
                                         ::attunement?
                                         ::item
                                         ::summary
                                         ::description]))

(def all-infusions
  [{:name "Arcane Propulsion Armor"
    :level 14
    :attunement? true
    :item "A suit of armor"
    :description (str "The wearer of this armor gains these benefits:"
                      "\n\u2022 The wearer's walking speed increases by 5 feet."
                      "\n\u2022 The armor includes gauntlets, each of which is a magic melee weapon that can be wielded only when the hand is holding nothing. The wearer is proficient with the gauntlets, and each one deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the gauntlet detaches and flies at the attack's target, then immediately returns to the wearer and reattaches."
                      "\n\u2022 The armor can't be removed against the wearer's will."
                      "\n\u2022 If the wearer is missing any limbs, the armor replaces those limbs - hands, arms, feet, legs, or similar appendages. The replacements function identically to the body parts they replace.")}
   {:name "Armor of Magical Strength"
    :level 2
    :attunement? true
    :item "A suit of armor"
    :description (str "This armor has 6 charges. The wearer can expend the armor's charges in the following ways:"
                      "\n\u2022 When the wearer makes a Strength check or a Strength saving throw, it can expend 1 charge to add a bonus to the roll equal to its Intelligence modifier."
                      "\n\u2022 If the creature would be knocked prone, it can use its reaction to expend 1 charge to avoid being knocked prone."
                      "\nThe armor regains 1d6 expended charges daily at dawn.")}
   {
    :name "Boots of the Winding Path"
    :level 6
    :attunement? true
    :item "A pair of boots"
    :description "While wearing these boots, a creature can teleport up to 15 feet as a bonus action to an unoccupied space the creature can see. The creature must have occupied that space at some point during the current turn."
    }
   {:name "Enhanced Arcane Focus"
    :level 2
    :attunement? true
    :item "A rod, staff or wand"
    :description (str "While holding this item, a creature gains +1 bonus to spell attack rolls. In addition, the creature ignores half cover when making a spell attack."
                      "\nThe bonus increases to +2 when you reach 10th level in this class.")}
   {:name "Enhanced Defense"
    :level 2
    :attunement? false
    :item "A suit of armor or a shield"
    :description (str "A creature gains a +1 bonus to Armor Class while wearing (armor) or wielding (shield) the infused item."
                      "\nThe bonus increases to +2 when you reach 10th level in this class.")}
   {:name "Enhanced Weapon"
    :level 2
    :attunement? false
    :item "A simple or martial weapon"
    :description (str "This magic weapon grants a +1 bonus to attack and damage rolls made with it."
                      "\nThe bonus increases to +2 when you reach 10th level in this class.")}
   {:name "Helm of Awareness"
    :level 10
    :attunement? true
    :item "A helmet"
    :description "While wearing this helmet, a creature has advantage on initiative rolls. In addition, the wearer can’t be surprised, provided it isn’t incapacitated."}
   {:name "Mind Sharpener"
    :level 2
    :attunement? false
    :item "A suit of armor or robes"
    :description "The infused item can send a jolt to the wearer to refocus their mind. The item has 4 charges. When the wearer fails a Constitution saving throw to maintain concentration on a spell, the wearer can use its reaction to expend 1 of the item's charges to succeed instead. The item regains 1d4 expended charges daily at dawn."}
   {:name "Radiant Weapon"
    :level 6
    :attunement? true
    :item "A simple or martial weapon"
    :description (str "This magic weapon grants a +1 bonus to attack and damage rolls made with it. While holding it, the wielder can take a bonus action to cause it to shed bright light in a 30-foot radius and dim light for an additional 30 feet. The wielder can extinguish the light as a bonus action."
                      "\nThe weapon has 4 charges. As a reaction immediately after being hit by an attack, the wielder can expend 1 charge and cause the attacker to be blinded until the end of the attacker's next turn, unless the attacker succeeds on a Constitution saving throw against your spell save DC. The weapon regains 1d4 expended charges daily at dawn.")}
   {:name "Repeating Shot"
    :level 2
    :attunement? true
    :item "A simple or martial weapon with the ammunition property"
    :description (str "This magic weapon grants a +1 bonus to attack and damage rolls made with it when it's used to make a ranged attack, and it ignores the loading property if it has it."
                      "\nIf the weapon lacks ammunition, it produces its own, automatically creating one piece of magic ammunition when the wielder makes a ranged attack with it. The ammunition created by the weapon vanishes the instant after it hits or misses a target.")}
   {:name "Repulsion Shield"
    :level 6
    :attunement? true
    :item "A shield"
    :description (str "A creature gains a +1 bonus to Armor Class while wielding this shield."
                      "\nThe shield has 4 charges. While holding it, the wielder can use a reaction immediately after being hit by a melee attack to expend 1 of the shield's charges and push the attacker up to 15 feet away. The shield regains 1d4 expended charges daily at dawn.")}
   {:name "Resistance Armor"
    :level 6
    :attunement? true
    :item "A suit of armor"
    :description "While wearing this armor, a creature has resistance to one of the following damage types, which you choose when you infuse the item: acid, cold, fire, force, lightning, necrotic, poison, psychic, radiant, or thunder."}
   {:name "Returning Weapon"
    :level 2
    :attunement? false
    :item "A simple or martial weapon with the thrown property"
    :description "This magic weapon grants a +1 bonus to attack and damage rolls made with it, and it returns to the wielder’s hand immediately after it is used to make a ranged attack."}
   {:name "Spell-Refueling Ring"
    :level 6
    :attunement? true
    :item "A ring"
    :description "While wearing this ring, the creature can recover one expended spell slot as an action. The recovered slot can be of 3rd level or lower. Once used, the ring can't be used again until the next dawn."}
   {:name "Bag of Holding"
    :level 2
    :attunement? false
    :description (str "This bag has an interior space considerably larger than its outside dimensions, roughly 2 feet in diameter at the mouth and 4 feet deep. The bag can hold up to 500 pounds, not exceeding a volume of 64 cubic feet. The bag weighs 15 pounds, regardless of its contents. Retrieving an item from the bag requires an action."
                      "\nIf the bag is overloaded, pierced, or torn, it ruptures and is destroyed, and its contents are scattered in the Astral Plane. If the bag is turned inside out, its contents spill forth, unharmed, but the bag must be put right before it can be used again. Breathing creatures inside the bag can survive up to a number of minutes equal to 10 divided by the number of creatures (minimum 1 minute), after which time they begin to suffocate."
                      "\nPlacing a bag of holding inside an extradimensional space created by a Heward's handy haversack, portable hole, or similar item instantly destroys both items and opens a gate to the Astral Plane. The gate originates where the one item was placed inside the other. Any creature within 10 feet of the gate is sucked through it to a random location on the Astral Plane. The gate then closes. The gate is one-way only and can't be reopened.")}
   {:name "Cap of Water Breathing"
    :level 2
    :attunement? false
    :description "While wearing this cap underwater, you can speak its command word as an action to create a bubble of air around your head. It allows you to breathe normally underwater. This bubble stays with you until you speak the command word again, the cap is removed, or you are no longer underwater."}
   {:name "Goggles of Night"
    :level 2
    :attunement? false
    :description "While wearing these dark lenses, you have darkvision out to a range of 60 feet. If you already have darkvision, wearing the goggles increases its range by 60 feet."}
   {:name "Rope of Climbing"
    :level 2
    :attunement? false
    :description (str "This 60-foot length of silk rope weighs 3 pounds and can hold up to 3,000 pounds. If you hold one end of the rope and use an action to speak the command word, the rope animates. As a bonus action, you can command the other end to move toward a destination you choose. That end moves 10 feet on your turn when you first command it and 10 feet on each of your turns until reaching its destination, up to its maximum length away, or until you tell it to stop. You can also tell the rope to fasten itself securely to an object or to unfasten itself, to knot or unknot itself, or to coil itself for carrying."
                      "\nIf you tell the rope to knot, large knots appear at 1-foot intervals along the rope. While knotted, the rope shortens to a 50-foot length and grants advantage on checks made to climb it."
                      "\nThe rope has AC 20 and 20 hit points. It regains 1 hit point every 5 minutes as long as it has at least 1 hit point. If the rope drops to 0 hit points, it is destroyed.")}
   {:name "Sending Stones"
    :level 2
    :attunement? true
    :description (str "Sending stones come in pairs, with each smooth stone carved to match the other so the pairing is easily recognized. While you touch one stone, you can use an action to cast the sending spell from it. The target is the bearer of the other stone. If no creature bears the other stone, you know that fact as soon as you use the stone and don't cast the spell."
                      "\nOnce sending is cast through the stones, they can't be used again until the next dawn. If one of the stones in a pair is destroyed, the other one becomes nonmagical.")}
   {:name "Wand of Magic Detection"
    :level 2
    :attunement? false
    :description "This wand has 3 charges. While holding it, you can expend 1 charge as an action to cast the detect magic spell from it. The wand regains 1d3 expended charges daily at dawn."}
   {:name "Wand of Secrets"
    :level 2
    :attunement? false
    :description "The wand has 3 charges. While holding it, you can use an action to expend 1 of its charges, and if a secret door or trap is within 30 feet of you, the wand pulses and points at the one nearest to you. The wand regains 1d3 expended charges daily at dawn."}
   {:name "Boots of Elvenkind"
    :level 6
    :attunement? false
    :description "While you wear these boots, your steps make no sound, regardless of the surface you are moving across. You also have advantage on Dexterity (Stealth) checks that rely on moving silently."}
   {:name "Cloak of Elvenkind"
    :level 6
    :attunement? true
    :description "While you wear this cloak with its hood up, Wisdom (Perception) checks made to see you have disadvantage, and you have advantage on Dexterity (Stealth) checks made to hide, as the cloak's color shifts to camouflage you. Pulling the hood up or down requires an action."}
   {:name "Cloak of the Manta Ray"
    :level 6
    :attunement? false
    :description "While wearing this cloak with its hood up, you can breathe underwater, and you have a swimming speed of 60 feet. Pulling the hood up or down requires an action."}
   {:name "Eyes of Charming"
    :level 6
    :attunement? true
    :description "These crystal lenses fit over the eyes. They have 3 charges. While wearing them, you can expend 1 charge as an action to cast the charm person spell (save DC 13) on a humanoid within 30 feet of you, provided that you and the target can see each other. The lenses regain all expended charges daily at dawn."}
   {:name "Gloves of Thievery"
    :level 6
    :attunement? false
    :description "These gloves are invisible while worn. While wearing them, you gain a +5 bonus to Dexterity (Sleight of Hand) checks and Dexterity checks made to pick locks."}
   {:name "Lantern of Revealing"
    :level 6
    :attunement? false
    :description "While lit, this hooded lantern burns for 6 hours on 1 pint of oil, shedding bright light in a 30-foot radius and dim light for an additional 30 feet. Invisible creatures and objects are visible as long as they are in the lantern's bright light. You can use an action to lower the hood, reducing the light to dim light in a 5-foot radius."}
   {:name "Pipes of Haunting"
    :level 6
    :attunement? false
    :description "You must be proficient with wind instruments to use these pipes. They have 3 charges. You can use an action to play them and expend 1 charge to create an eerie, spellbinding tune. Each creature within 30 feet of you that hears you play must succeed on a DC 15 Wisdom saving throw or become frightened of you for 1 minute. If you wish, all creatures in the area that aren't hostile toward you automatically succeed on the saving throw. A creature that fails the saving throw can repeat it at the end of each of its turns, ending the effect on itself on a success. A creature that succeeds on its saving throw is immune to the effect of these pipes for 24 hours. The pipes regain 1d3 expended charges daily at dawn."}
   {:name "Ring of Water Walking"
    :level 6
    :attunement? false
    :description "While wearing this ring, you can stand on and move across any liquid surface as if it were solid ground."}
   {:name "Amulet of Health"
    :level 14
    :attunement? true
    :description "Your Constitution score is 19 while you wear this amulet. It has no effect on you if your Constitution score is already 19 or higher without it."}
   {:name "Arcane Propulsion Arm"
    :level 14
    :attunement? true
    :description (str "This prosthetic appendage was developed by artificers of House Cannith. To attune to this item, you must attach it to your arm at the wrist, elbow, or shoulder, at which point the prosthetic magically forms a copy of the appendage it's replacing."
                      "\nWhile attached, the prosthetic provides these benefits:"
                      "\n\u2022 The prosthetic is a fully capable part of your body."
                      "\n\u2022 You can take an action to remove the prosthetic, and it removes itself if your attunement to it ends. It can't be removed against your will."
                      "\n\u2022 The prosthetic is a magic melee weapon with which you're proficient. It deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the prosthetic detaches and flies at the target of the attack, then immediately returns to you and reattaches.")}
   {:name "Belt of Hill Giant Strength"
    :level 14
    :attunement? true
    :description "While wearing this belt, your Strength score changes to 21. The item has no effect on you if your Strength without the belt is equal to or greater than the belt's score."}
   {:name "Boots of Levitation"
    :level 14
    :attunement? true
    :description "While you wear these boots, you can use an action to cast the levitate spell on yourself at will."}
   {:name "Boots of Speed"
    :level 14
    :attunement? true
    :description (str "While you wear these boots, you can use a bonus action and click the boots' heels together. If you do, the boots double your walking speed, and any creature that makes an opportunity attack against you has disadvantage on the attack roll. If you click your heels together again, you end the effect."
                      "\nWhen the boots' property has been used for a total of 10 minutes, the magic ceases to function until you finish a long rest.")}
   {:name "Bracers of Defense"
    :level 14
    :attunement? true
    :description "While wearing these bracers, you gain a +2 bonus to AC if you are wearing no armor and using no shield."}
   {:name "Cloak of the Bat"
    :level 14
    :attunement? true
    :description (str "While wearing this cloak, you have advantage on Dexterity (Stealth) checks. In an area of dim light or darkness, you can grip the edges of the cloak with both hands and use it to fly at a speed of 40 feet. If you ever fail to grip the cloak's edges while flying in this way, or if you are no longer in dim light or darkness, you lose this flying speed."
                      "\nWhile wearing the cloak in an area of dim light or darkness, you can use your action to cast polymorph on yourself, transforming into a bat. While you are in the form of the bat, you retain your Intelligence, Wisdom, and Charisma scores. The cloak can't be used this way again until the next dawn.")}
   {:name "Dimensional Shackles"
    :level 14
    :attunement? false
    :description (str "You can use an action to place these shackles on an incapacitated creature. The shackles adjust to fit a creature of Small to Large size. In addition to serving as mundane manacles, the shackles prevent a creature bound by them from using any method of extradimensional movement, including teleportation or travel to a different plane of existence. They don't prevent the creature from passing-through an interdimensional portal."
                      "\nYou and any creature you designate when you use the shackles can use an action to remove them. Once every 30 days, the bound creature can make a DC 30 Strength (Athletics) check. On a success, the creature breaks free and destroys the shackles.")}
   {:name "Gem of Seeing"
    :level 14
    :attunement? true
    :description (str "This gem has 3 charges. As an action, you can speak the gem's command word and expend 1 charge. For the next 10 minutes, you have truesight out to 120 feet when you peer through the gem."
                      "\nThe gem regains 1d3 expended charges daily at dawn.")}
   {:name "Horn of Blasting"
    :level 14
    :attunement? false
    :description (str "You can use an action to speak the horn's command word and then blow the horn, which emits a thunderous blast in a 30-foot cone that is audible 600 feet away. Each creature in the cone must make a DC 15 Constitution saving throw. On a failed save, a creature takes 5d6 thunder damage and is deafened for 1 minute. On a successful save, a creature takes half as much damage and isn't deafened. Creatures and objects made of glass or crystal have disadvantage on the saving throw and take 10d6 thunder damage instead of 5d6."
                      "\nEach use of the horn's magic has a 20 percent chance of causing the horn to explode. The explosion deals 10d6 fire damage to the blower and destroys the horn.")}
   {:name "Ring of Free Action"
    :level 14
    :attunement? true
    :description "While you wear this ring, difficult terrain doesn't cost you extra movement. In addition, magic can neither reduce your speed nor cause you to be paralyzed or restrained."}
   {:name "Ring of Protection"
    :level 14
    :attunement? true
    :description "You gain a +1 bonus to AC and saving throws while wearing this ring."}
   {:name "Ring of the Ram"
    :level 14
    :attunement? true
    :description (str "This ring has 3 charges, and it regains 1d3 expended charges daily at dawn. While wearing the ring, you can use an action to expend 1 to 3 of its charges to make a ranged spell attack against one creature you can see within 60 feet of you. The ring produces a spectral ram's head and makes its attack roll with a +7 bonus. On a hit, for each charge you spend, the target takes 2d10 force damage and is pushed 5 feet away from you."
                      "\nAlternatively, you can expend 1 to 3 of the ring's charges as an action to try to break an object you can see within 60 feet of you that isn't being worn or carried. The ring makes a Strength check with a +5 bonus for each charge you spend.")}
   ; {:name ""
   ;  :level 
   ;  :attunement? true
   ;  :description ""}
   ])

