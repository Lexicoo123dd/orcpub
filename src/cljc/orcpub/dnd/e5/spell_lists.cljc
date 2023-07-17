(ns orcpub.dnd.e5.spell-lists
  (:require [orcpub.dnd.e5.character :as char5e]
            [re-frame.core :refer [dispatch subscribe]]))

(def phb-spell-lists
  {
   :bard
   {0
    [:blade-ward :dancing-lights :friends :light :mage-hand :mending :message :minor-illusion
     :prestidigitation :thunderclap :true-strike :vicious-mockery]
    1
    [:animal-friendship :bane :charm-person :color-spray :command :comprehend-languages :cure-wounds
     :detect-magic :disguise-self :dissonant-whispers :distort-value :earth-tremor :faerie-fire :feather-fall
     :healing-word :heroism :identify :illusory-script
     :longstrider :silent-image :silvery-barbs :sleep :speak-with-animals :tashas-hideous-laughter :thunderwave
     :unseen-servant],
    2
    [:aid :animal-messenger :blindness-deafness :borrowed-knowledge :calm-emotions
     :cloud-of-daggers :crown-of-madness :detect-thoughts :enhance-ability :enlarge-reduce :enthrall :gift-of-gab :heat-metal
     :hold-person :invisibility :kinetic-jaunt :knock :lesser-restoration
     :locate-animals-or-plants :locate-object :magic-mouth :mirror-image :nathairs-mischief
     :phantasmal-force :pyrotechnics
     :see-invisibility :shatter :silence :skywrite :suggestion :warding-wind :zone-of-truth],
    3
    [:bestow-curse :catnap :clairvoyance :dispel-magic :enemies-abound :fast-friends :fear :feign-death :glyph-of-warding
     :hypnotic-pattern :intellect-fortress :leomunds-tiny-hut :major-image :mass-healing-word :motivational-speech :nondetection :plant-growth :sending
     :slow :speak-with-dead :speak-with-plants :stinking-cloud
     :tongues],
    4
    [:charm-monster :compulsion :confusion :dimension-door :freedom-of-movement
     :greater-invisibility :hallucinatory-terrain :locate-creature :phantasmal-force
     :polymorph :raulothims-psychic-lance],
    5
    [:animate-objects :awaken :dominate-person :dream :geas
     :greater-restoration :hold-monster :legend-lore :mass-cure-wounds
     :mislead :modify-memory :planar-binding :raise-dead :rarys-telepathic-bond :scrying
     :seeming :skill-empowerment :synaptic-static :teleportation-circle],
    6
    [:eyebite :find-the-path :guards-and-wards :heroes-feast
     :mass-suggestion :ottos-irresistible-dance :programmed-illusion :true-seeing],
    7
    [:dream-of-the-blue-veil :etherealness :forcecage
     :mirage-arcane :mordenkainens-sword :mordenkainens-magnificent-mansion :prismatic-spray :project-image
     :regenerate :resurrection :symbol
     :teleport],
    8
    [:antipathy-sympathy :dominate-monster :feeblemind :glibness :mind-blank
     :power-word-stun],
    9
    [:foresight :mass-polymorph :power-word-heal :power-word-kill :prismatic-wall :psychic-scream :true-polymorph]},
   :cleric
   {0
    [:guidance :light :mending :resistance :sacred-flame :spare-the-dying :thaumaturgy :toll-the-dead :word-of-radiance],
    1
    [:bane :bless :ceremony :command :create-or-destroy-water :cure-wounds
     :detect-evil-and-good :detect-magic :detect-poison-and-disease
     :guiding-bolt :healing-word :inflict-wounds
     :protection-from-evil-and-good :purify-food-and-drink :sanctuary
     :shield-of-faith],
    2
    [:aid :augury :blindness-deafness :borrowed-knowledge :calm-emotions :continual-flame
     :enhance-ability :find-traps :gentle-repose :hold-person
     :lesser-restoration :locate-object :prayer-of-healing
     :protection-from-poison :silence :spiritual-weapon :warding-bond
     :zone-of-truth],
    3
    [:animate-dead :aura-of-vitality :beacon-of-hope :bestow-curse :clairvoyance
     :create-food-and-water :daylight :dispel-magic :fast-friends :feign-death :glyph-of-warding
     :incite-greed :life-transference :magic-circle :mass-healing-word :meld-into-stone
     :motivational-speech :protection-from-energy :remove-curse :revivify :sending
     :speak-with-dead :spirit-guardians :spirit-shroud :tongues :water-walk],
    4
    [:aura-of-life :aura-of-purity :banishment :control-water :death-ward :divination
     :freedom-of-movement :guardian-of-faith :locate-creature :stone-shape],
    5
    [:commune :contagion :dawn :dispel-evil-and-good :flame-strike :geas
     :greater-restoration :hallow :holy-weapon :insect-plague :legend-lore
     :mass-cure-wounds :planar-binding :raise-dead :scrying :summon-celestial],
    6
    [:blade-barrier :create-undead :find-the-path :forbiddance :harm
     :heal :heroes-feast :planar-ally :sunbeam :true-seeing :word-of-recall],
    7
    [:conjure-celestial :divine-word :etherealness :fire-storm
     :plane-shift :regenerate :resurrection :symbol :temple-of-the-gods],
    8
    [:antimagic-field :control-weather :earthquake :holy-aura :sunburst],
    9
    [:astral-projection :gate :mass-heal :power-word-heal :true-resurrection]},
   :druid
   {0
    [:control-flames :create-bonfire :druidcraft :frostbite :guidance :gust :infestation :magic-stone :mending
     :mold-earth :poison-spray :primal-savagery :produce-flame :resistance :shape-water :shillelagh :thorn-whip :thunderclap],
    1
    [:absorb-elements :animal-friendship :beast-bond :charm-person :create-or-destroy-water :cure-wounds :detect-magic
     :detect-poison-and-disease :earth-tremor :entangle :faerie-fire :fog-cloud :goodberry
     :healing-word :ice-knife :jump :longstrider :protection-from-evil-and-good :purify-food-and-drink
     :snare :speak-with-animals :thunderwave]
    ,
    2
    [:air-bubble :animal-messenger :augury :barkskin :beast-sense :continual-flame :darkvision :dust-devil :earthbind :enhance-ability
     :enlarge-reduce :find-traps :flame-blade :flaming-sphere :gust-of-wind :healing-spirit :heat-metal
     :hold-person :lesser-restoration :locate-animals-or-plants
     :locate-object :moonbeam :pass-without-trace
     :protection-from-poison :skywrite :spike-growth :summon-beast :warding-wind :wither-and-bloom],
    3
    [:aura-of-vitality :call-lightning :conjure-animals :daylight :dispel-magic :elemental-weapon :erupting-earth
     :feign-death :flame-arrows :meld-into-stone :plant-growth :protection-from-energy :revivify :sleet-storm
     :speak-with-plants :summon-fey :tidal-wave :wall-of-water :water-breathing :water-walk :wind-wall],
    4
    [:blight :charm-monster :confusion :conjure-minor-elementals
     :conjure-woodland-beings :control-water :divination :dominate-beast :elemental-bane
     :fire-shield :freedom-of-movement :giant-insect :grasping-wine :guardian-of-nature :hallucinatory-terrain
     :ice-storm :locate-creature :polymorph :stone-shape :stoneskin :summon-elemental
     :wall-of-fire :watery-sphere],
    5
    [:antilife-shell :awaken :commune-with-nature :cone-of-cold :conjure-elemental
     :contagion :control-winds :geas :greater-restoration :insect-plague :maelstrom
     :mass-cure-wounds :planar-binding :reincarnate :scrying :summon-draconic-spirit
     :transmute-rock :tree-stride :wall-of-stone :wrath-of-nature],
    6
    [:bones-of-the-earth :conjure-fey :druid-grove :find-the-path :flesh-to-stone :heal :heroes-feast
     :investiture-of-flame :investiture-of-ice :investiture-of-stone :investiture-of-wind :move-earth
     :primordial-ward :sunbeam :transport-via-plants :wall-of-thorns :wind-walk],
    7
    [:draconic-transformation :fire-storm :mirage-arcane :plane-shift :regenerate
     :reverse-gravity :symbol :whirlwind],
    8
    [:animal-shapes :antipathy-sympathy :control-weather :earthquake
     :feeblemind :incendiary-cloud :sunburst :tsunami],
    9 
    [:foresight :shapechange :storm-of-vengeance :true-resurrection]},
   :paladin
   {1
    [:bless :ceremony :command :compelled-duel :cure-wounds :detect-evil-and-good :detect-magic
     :detect-poison-and-disease :divine-favor :heroism
     :protection-from-evil-and-good :purify-food-and-drink
     :searing-smite :shield-of-faith :thunderous-smite :wrathful-smite]
    2
    [:aid :branding-smite :find-steed :gentle-repose :lesser-restoration :locate-object :magic-weapon
     :prayer-of-healing :protection-from-poison :warding-bond :zone-of-truth]
    3
    [:aura-of-vitality :blinding-smite :create-food-and-water :crusaders-mantle :daylight
     :dispel-magic :elemental-weapon :magic-circle :remove-curse :revivify :spirit-shroud]
    4 
    [:aura-of-life :aura-of-purity :banishment :death-ward :find-greater-steed :locate-creature :staggering-smite]
    5 
    [:banishing-smite :circle-of-power :destructive-wave :dispel-evil-and-good :geas :holy-weapon :raise-dead :summon-celestial]}
   :ranger
   {1
    [:absorb-elements :alarm :animal-friendship :beast-bond :cure-wounds :detect-magic :detect-poison-and-disease
     :ensnaring-strike :entangle :fog-cloud :goodberry :hail-of-thorns :hunters-mark
     :jump :longstrider :searing-smite :snare :speak-with-animals :zephyr-strike],
    2
    [:aid :air-bubble :animal-messenger :barkskin :beast-sense :cordon-of-arrows :darkvision :enhance-ability :find-traps
     :gust-of-wind :healing-spirit :lesser-restoration :locate-animals-or-plants :locate-object :magic-weapon
     :pass-without-trace :protection-from-poison :silence
     :spike-growth :summon-beast],
    3
    [:ashardalons-stride :conjure-animals :conjure-barrage :daylight :elemental-weapon :flame-arrows :lightning-arrow :meld-into-stone :nondetection :plant-growth
     :protection-from-energy :revivify :speak-with-plants :summon-fey :water-breathing
     :water-walk :wind-wall ],
    4
    [:conjure-woodland-beings :dominate-beast :freedom-of-movement :grasping-vine :guardian-of-nature :locate-creature
     :stoneskin :summon-elemental],
    5 
    [:commune-with-nature :conjure-volley :greater-restoration :steel-wind-strike
     :swift-quiver :tree-stride :wrath-of-nature]},
   :sorcerer
   {0
    [:acid-splash :blade-ward :booming-blade :chill-touch :control-flames :create-bonfire :dancing-lights :fire-bolt
     :friends :frostbite :green-flame-blade :gust :infestation :light :lightning-lure :mage-hand :mending :message :mind-sliver :minor-illusion :mold-earth :poison-spray
     :prestidigitation :ray-of-frost :shape-water :shocking-grasp :sword-burst :thunderclap :true-strike],
    1
    [:absorb-elements :burning-hands :catapult :chaos-bolt :charm-person :chromatic-orb :color-spray :comprehend-languages
     :detect-magic :disguise-self :distort-value :earth-tremor :expeditious-retreat :false-life
     :feather-fall :fog-cloud :grease :ice-knife :jump :mage-armor :magic-missile :ray-of-sickness :shield
     :silent-image :silvery-barbs :sleep :tashas-caustic-brew :thunderwave :witch-bolt],
    2
    [:aganazzars-scorcher :air-bubble :alter-self :blindness-deafness :blur :cloud-of-daggers :crown-of-madness :darkness :darkvision
     :detect-thoughts :dragons-breath :dust-devil :earthbind :enhance-ability :enlarge-reduce :flame-blade :flaming-sphere :gust-of-wind
     :hold-person :invisibility :kinetic-jaunt :knock :levitate :magic-weapon :maximilians-earthen-grasp :mind-spike :mirror-image
     :misty-step :nathairs-mischief :phantasmal-force :pyrotechnics :rimes-binding-ice :scorching-ray :see-invisibility :shadow-blade :shatter :snillocs-snowball-swarm :spider-climb
     :suggestion :tashas-mind-whip :vortex-warp :warding-wind :web :wither-and-bloom],
    3
    [:ashardalons-stride :blink :catnap :clairvoyance :counterspell :daylight :dispel-magic :enemies-abound :erupting-earth :fear :fireball :flame-arrows :fly
     :gaseous-form :haste :hypnotic-pattern :intellect-fortress :lightning-bolt :major-image :melfs-minute-meteors
     :protection-from-energy :sleet-storm :slow :stinking-cloud :thunder-step :tidal-wave :tongues
     :vampiric-touch :wall-of-water :water-breathing :water-walk],
    4
    [:banishment :blight :charm-monster :confusion :dimension-door :dominate-beast
     :fire-shield :greater-invisibility :ice-storm :polymorph :raulothims-psychic-lance :sickening-radiance :stoneskin
     :storm-sphere :vitriolic-sphere :wall-of-fire :watery-sphere],
    5
    [:animate-objects :bigbys-hand :cloudkill :cone-of-cold :control-winds :creation
     :dominate-person :enervation :far-step :hold-monster :immolation :insect-plague :seeming :skill-empowerment :summon-draconic-spirit :synaptic-static :telekinesis
     :teleportation-circle :wall-of-light :wall-of-stone],
    6
    [:arcane-gate :chain-lightning :circle-of-death :disintegrate :eyebite :fizbans-platinum-shield
     :flesh-to-stone :globe-of-invulnerability :investiture-of-flame :investiture-of-ice :investiture-of-stone
     :investiture-of-wind :mass-suggestion :mental-prison :move-earth :otilukes-freezing-sphere :scatter :sunbeam
     :tashas-otherworldly-guise :true-seeing],
    7
    [:crown-of-stars :delayed-blast-fireball :draconic-transformation :dream-of-the-blue-veil :etherealness :finger-of-death :fire-storm
     :plane-shift :power-word-pain :prismatic-spray :reverse-gravity :teleport :whirlwind ],
    8
    [:abi-dalzims-horrid-wilting :demiplane :dominate-monster :earthquake :incendiary-cloud :power-word-stun
     :sunburst],
    9 
    [:blade-of-disaster :gate :mass-polymorph :meteor-swarm :power-word-kill :psychic-scream :time-stop :wish]},
   :warlock
   {0
    [:blade-ward :booming-blade :chill-touch :create-bonfire :eldritch-blast :friends
     :frostbite :green-flame-blade :infestation :lightning-lure :mage-hand :magic-stone
     :mind-sliver :minor-illusion :poison-spray :prestidigitation
     :sword-burst :thunderclap :toll-the-dead :true-strike],
    1
    [:armor-of-agathys :arms-of-hadar :cause-fear :charm-person :comprehend-languages :distort-value :expeditious-retreat
     :hellish-rebuke :hex :illusory-script :protection-from-evil-and-good :unseen-servant :witch-bolt],
    2
    [:borrowed-knowledge :cloud-of-daggers :crown-of-madness :darkness :earthbind :enthrall :hold-person :invisibility :mind-spike :mirror-image
     :misty-step :ray-of-enfeeblement :shadow-blade :shatter :spider-climb
     :suggestion],
    3
    [:counterspell :dispel-magic :enemies-abound :fear :fly :gaseous-form :hunger-of-hadar :hypnotic-pattern
     :incite-greed :intellect-fortress :magic-circle :major-image :remove-curse :spirit-shroud :summon-fey
     :summon-lesser-demons :summon-shadowspawn :summon-undead :thunder-step :tongues :vampiric-touch],
    4 
    [:banishment :blight :charm-monster :dimension-door :elemental-bane :hallucinatory-terrain
     :raulothims-psychic-lance :shadow-of-moil :sickening-radiance :summon-aberration :summon-greater-demon],
    5 
    [:contact-other-plane :danse-macabre :dream :enervation :far-step :hold-monster :infernal-calling :mislead
     :negative-energy-flood :planar-binding :scrying :synaptic-static :teleportation-circle :wall-of-light],
    6
    [:arcane-gate :circle-of-death :conjure-fey :create-undead :eyebite
     :flesh-to-stone :investiture-of-flame :investiture-of-ice :investiture-of-stone
     :investiture-of-wind :mass-suggestion :mental-prison :scatter :soul-cage :summon-fiend :tashas-otherworldly-guise :true-seeing],
    7 
    [:crown-of-stars :dream-of-the-blue-veil :etherealness :finger-of-death :forcecage :plane-shift :power-word-pain],
    8
    [:demiplane :dominate-monster :feeblemind :glibness
     :maddening-darkness :power-word-stun],
    9 
    [:astral-projection :blade-of-disaster :foresight :gate :imprisonment :power-word-kill :psychic-scream :true-polymorph :weird]},
   :wizard
   {0
    [:acid-splash :blade-ward :booming-blade :chill-touch :control-flames :create-bonfire
     :dancing-lights :fire-bolt :friends :frostbite :green-flame-blade :gust :infestation
     :light :lightning-lure :mage-hand :mending :message :mind-sliver :minor-illusion :mold-earth
     :poison-spray :prestidigitation :ray-of-frost :shape-water
     :shocking-grasp :sword-burst :thunderclap :toll-the-dead :true-strike],
    1
    [:absorb-elements :alarm :burning-hands :catapult :cause-fear
     :charm-person :chromatic-orb :color-spray
     :comprehend-languages :detect-magic :disguise-self :distort-value :earth-tremor
     :expeditious-retreat :false-life :feather-fall :find-familiar
     :fog-cloud :frost-fingers :grease :ice-knife :identify :illusory-script
     :jims-magic-missile :jump :longstrider :mage-armor :magic-missile
     :protection-from-evil-and-good :ray-of-sickness :shield :silent-image :silvery-barbs :sleep
     :snare :tashas-caustic-brew :tashas-hideous-laughter :tensers-floating-disk :thunderwave :unseen-servant :witch-bolt],
    2
    [:aganazzars-scorcher :air-bubble :alter-self :arcane-lock :augury
     :blindness-deafness :blur :borrowed-knowledge :cloud-of-daggers :continual-flame :crown-of-madness :darkness :darkvision
     :detect-thoughts :dragons-breath :dust-devil :earthbind :enhance-ability :enlarge-reduce :flaming-sphere :gentle-repose
     :gift-of-gab :gust-of-wind :hold-person :invisibility :jims-glowing-coin :kinetic-jaunt :knock :levitate
     :locate-object :magic-mouth :magic-weapon :maximilians-earthen-grasp :melfs-acid-arrow :mind-spike :mirror-image :misty-step
     :nathairs-mischief :nystuls-magic-aura :phantasmal-force :pyrotechnics :ray-of-enfeeblement :rimes-binding-ice :rope-trick :scorching-ray :see-invisibility
     :shadow-blade :shatter :skywrite :snillocs-snowball-swarm :spider-climb :suggestion :tashas-mind-whip :vortex-warp :warding-wing :web :wither-and-bloom],
    3
    [:animate-dead :ashardalons-stride :bestow-curse :blink :catnap :clairvoyance :counterspell :dispel-magic :enemies-abound
     :erupting-earth :fast-friends :fear :feign-death :fireball :flame-arrows :fly :gaseous-form :glyph-of-warding :haste
     :hypnotic-pattern :incite-greed :intellect-fortress :leomunds-tiny-hut :life-transference :lightning-bolt :magic-circle :major-image
     :melfs-minute-meteors :nondetection :phantom-steed :protection-from-energy :remove-curse
     :sending :sleet-storm :slow :speak-with-dead :spirit-shroud :stinking-cloud :summon-fey :summon-lesser-demons :summon-shadowspawn :summon-undead
     :thunder-step :tidal-wave :tiny-servant :tongues :vampiric-touch :wall-of-sand :wall-of-water :water-breathing],
    4
    [:arcane-eye :banishment :blight :charm-monster :confusion
     :conjure-minor-elementals :control-water :dimension-door :divination :elemental-bane :evards-black-tentacles :fabricate
     :fire-shield :greater-invisibility
     :hallucinatory-terrain :ice-storm :leomunds-secret-chest :locate-creature
     :mordenkainens-faithful-hound :mordenkainens-private-sanctum :otilukes-resilient-sphere
     :phantasmal-killer :polymorph :raulothims-psychic-lance :sickening-radiance
     :stone-shape :stoneskin :storm-sphere :summon-aberration :summon-construct
     :summon-elemental :summon-greater-demon :vitriolic-sphere :wall-of-fire :watery-sphere],
    5
    [:animate-objects :bigbys-hand :cloudkill :cone-of-cold
     :conjure-elemental :contact-other-plane :control-winds :create-spelljamming-helm :creation :danse-macabre :dawn :dominate-person
     :dream :enervation :far-step :geas :hold-monster :immolation :infernal-calling :legend-lore :mislead :modify-memory
     :negative-energy-flood :passwall :planar-binding :rarys-telepathic-bond :scrying :seeming :skill-empowerment
     :steel-wind-strike :summon-draconic-spirit :synaptic-static :telekinesis
     :teleportation-circle :transmute-rock :wall-of-force :wall-of-light :wall-of-stone],
    6
    [:arcane-gate :chain-lightning :circle-of-death :contingency :create-homunculus :create-undead
     :disintegrate :drawmijs-instant-summons :eyebite :fizbans-platinum-shield :flesh-to-stone
     :globe-of-invulnerability :guards-and-wards :investiture-of-flame :investiture-of-ice :investiture-of-stone :investiture-of-wind
     :magic-jar :mass-suggestion :mental-prison :move-earth :otilukes-freezing-sphere :ottos-irresistible-dance
     :programmed-illusion :scatter :soul-cage :summon-fiend :sunbeam :tashas-otherworldly-guise :tensers-transformation :true-seeing :wall-of-ice],
    7
    [:create-magen :crown-of-stars :delayed-blast-fireball :draconic-transformation :dream-of-the-blue-veil :etherealness
     :finger-of-death :forcecage :mirage-arcane :mordenkainens-magnificent-mansion
     :mordenkainens-sword :plane-shift :power-word-pain :prismatic-spray :project-image :reverse-gravity
     :sequester :simulacrum :symbol :teleport :whirlwind],
    8
    [:abi-dalzims-horrid-wilting :antimagic-field :antipathy-sympathy :clone :control-weather
     :demiplane :dominate-monster :feeblemind :illusory-dragon :incendiary-cloud :maddening-darkness :maze
     :mighty-fortress :mind-blank :power-word-stun :sunburst :telepathy],
    9
    [:astral-projection :blade-of-disaster :foresight :gate :imprisonment :invulnerability :mass-polymorph :meteor-swarm
     :power-word-kill :prismatic-wall :psychic-scream :shapechange :time-stop
     :true-polymorph :weird :wish]}})

(def graviturgy-spell-list
  {0 [:mage-hand :mold-earth :sapping-sting]
   1 [:catapult :earth-tremor :expeditious-retreat :feather-fall :tensers-floating-disk :jump :magnify-gravity]})

#_(def graviturgy-spell-list
  {
   :bard
    {0
     [:dancing-lights :mold-earth]
     1
     [:tensers-floating-disk]
     2
     [:earthbind]}
  })

(defn merge-class-lists [class-list-1 class-list-2]
  (merge-with
   concat
   class-list-1
   class-list-2))

(def spell-lists
  phb-spell-lists
  #_(merge-with
     merge-class-lists
     graviturgy-spell-list
     phb-spell-lists
     ee-spell-lists
     ua-starter-spells-lists
     ))
