(ns orcpub.dnd.e5.options
  (:require [clojure.string :as s]
            [clojure.set :as sets]
            [orcpub.common :as common]
            [orcpub.template :as t]
            [orcpub.entity :as entity]
            [orcpub.dice :as dice]
            [orcpub.entity-spec :as es]
            [orcpub.modifiers :as mods]
            [orcpub.dnd.e5.character :as character]
            [orcpub.dnd.e5.character.equipment :as char-equip]
            [orcpub.dnd.e5.modifiers :as modifiers]
            [orcpub.dnd.e5.weapons :as weapons]
            [orcpub.dnd.e5.languages :as languages]
            [orcpub.dnd.e5.units :as units5e]
            [orcpub.dnd.e5.races :as races]
            [orcpub.dnd.e5.armor :as armor]
            [orcpub.dnd.e5.spells :as spells]
            [orcpub.dnd.e5.equipment :as equipment]
            [orcpub.dnd.e5.spell-lists :as sl]
            [orcpub.dnd.e5.display :as disp]
            [orcpub.dnd.e5.skills :as skills]
            [orcpub.dnd.e5.magic-items :as mi]
            [orcpub.dnd.e5.event-handlers :as eh]
            [orcpub.components :as comps]
            [re-frame.core :refer [dispatch subscribe]])
  #?(:cljs (:require-macros [orcpub.dnd.e5.modifiers :as modifiers])))

#?(:cljs (enable-console-print!))

(def alignment-titles
  ["Lawful Good" "Lawful Neutral" "Lawful Evil" "Neutral Good" "Neutral" "Neutral Evil" "Chaotic Good" "Chaotic Neutral" "Chaotic Evil"])

(def xps [0 300 900 2700 6500 14000 23000 34000 48000 64000 85000 100000 120000 140000 165000 195000 225000 265000 305000 355000])

(def levels
  (map-indexed
   (fn [i xp] {:level (inc i) :min-xp xp})
   xps))

(def level-xps
  (zipmap
   (map inc (range))
   xps))

(def alignments
  (map
   (fn [alignment]
     {:name alignment
      :key (common/name-to-kw alignment)})
   alignment-titles))

(def abilities
  [{:key ::character/str
    :name "Strength"
    :abbr "STR"}
   {:key ::character/dex
    :name "Dexterity"
    :abbr "DEX"}
   {:key ::character/con
    :name "Constitution"
    :abbr "CON"}
   {:key ::character/int
    :name "Intelligence"
    :abbr "INT"}
   {:key ::character/wis
    :name "Wisdom"
    :abbr "WIS"}
   {:key ::character/cha
    :name "Charisma"
    :abbr "CHA"}])

(def abilities-map
  (common/map-by-key abilities))

(def conditions
  [{:name "Blinded"
    :key :blinded
    :icon "sight-disabled"}
   {:name "Charmed"
    :key :charmed
    :icon "smitten"}
   {:name "Deafened"
    :key :deafened
    :icon "hearing-disabled"}
   {:name "Exhausted"
    :key :exhausted
    :icon "knockout"}
   {:name "Frightened"
    :key :frightened
    :icon "terror"}
   {:name "Grappled"
    :key :grappled
    :icon "grab"}
   {:name "Incapacitated"
    :key :incapacitated
    :icon "cement-shoes"}
   {:name "Invisible"
    :key :invisible
    :icon "invisible"}
   {:name "Paralyzed"
    :key :paralyzed
    :icon "oppression"}
   {:name "Petrified"
    :key :petrified
    :icon "stone-block"}
   {:name "Poisoned"
    :key :poisoned
    :icon "vomiting"}
   {:name "Prone"
    :key :prone
    :icon "despair"}
   {:name "Restrained"
    :key :restrained
    :icon "imprisoned"}
   {:name "Stunned"
    :key :stunned
    :icon "knockout"}
   {:name "Unconscious"
    :key :unconscious
    :icon "coma"}])

(def damage-types
  [:acid
   :bludgeoning
   :cold
   :fire
   :force
   :lightning
   :necrotic
   :piercing
   :poison
   :psychic
   :radiant
   :slashing
   :thunder])

(def conditions-map
  (common/map-by-key (common/add-keys conditions)))

(defn skill-option [skill]
  (t/option-cfg
   {:name (:name skill)
    :icon (:icon skill)
    :key (:key skill)
    :help (:description skill)
    :prereqs [(t/option-prereq
               "You already have this skill"
               (fn [c]
                 (let [skill-profs @(subscribe [::character/skill-profs nil c])]
                   (not (get skill-profs (:key skill))))))]
    :modifiers [(modifiers/skill-proficiency (:key skill))]}))

(defn weapon-proficiency-option [{:keys [name key]}]
  (t/option-cfg
   {:name name
    :prereqs [(t/option-prereq
               "You already have this weapon proficiency"
               (fn [c]
                 (let [weapon-profs @(subscribe [::character/weapon-profs nil c])]
                   (not (get weapon-profs key)))))]
    :modifiers [(modifiers/weapon-proficiency key)]}))

(defn tool-option [tool]
  (t/option-cfg
   {:name (:name tool)
    :key (:key tool)
    :icon (:icon tool)
    :modifiers [(modifiers/tool-proficiency (:key tool))]}))

(defn weapon-option [weapon & [num]]
  (t/option-cfg
   {:name (:name weapon)
    :key (:key weapon)
    :help (:description weapon)
    :modifiers [(modifiers/weapon (:key weapon) {::char-equip/equipped? true
                                                 ::char-equip/quantity (or num 1)})]}))

(defn weapon-options [weapons & [num]]
  (map
   #(weapon-option % num)
   weapons))

(defn simple-melee-weapon-options [num weapons]
  (weapon-options
   (filter
    #(and (= :simple (::weapons/type %)) (::weapons/melee? %))
    weapons)
   num))

(defn martial-weapon-options [num weapons]
  (weapon-options
   (filter
    #(= :martial (::weapons/type %))
    weapons)
   num))

(defn simple-weapon-options [num weapons]
  (weapon-options
   (filter
    #(= :simple (::weapons/type %))
    weapons)
   num))

(defn skill-options [skills]
  (map
   skill-option
   skills))

(defn weapon-proficiency-options [weapons]
  (map
   weapon-proficiency-option
   weapons))

(defn tool-options [tools]
  (map
   tool-option
   tools))

(defn ability-bonus [ability-value]
  (- (int (/ ability-value 2)) 5))

(defn ability-bonus-str [ability-value]
  (common/bonus-str (ability-bonus ability-value)))

(defn get-raw-abilities [character]
  (get-in character [::entity/options :ability-scores ::entity/value]))

(defn fey-touched-spell? [s]
    (let [school (:school s)]
      (or (= school "divination")
          (= school "enchantment"))))

(defn shadow-touched-spell? [s]
    (let [school (:school s)]
      (or (= school "illusion")
          (= school "necromancy"))))

(defn ability-increase-selection-2 [{:keys [ability-keys num-increases min max max-ability different? modifier-fn modifier-fns selection-fn]}]
  (t/selection-cfg
   {:name "Ability Score Improvement"
    :key :asi
    :min (or num-increases min)
    :max (or num-increases max)
    :max-ability max-ability
    :tags #{:ability-scores}
    :different? different?
    :multiselect? true
    :options (map
              (fn [k]
                (t/option-cfg
                 {:name (:name (abilities-map k))
                  :key k
                  :selections [(if selection-fn (selection-fn k))]
                  :modifiers (concat
                              [(if modifier-fn
                                 (modifier-fn k)
                                 (modifiers/level-ability-increase k 1))]
                              (map
                               #(% k)
                               modifier-fns))}))
              (or ability-keys
                  character/ability-keys))}))

(defn ability-increase-selection [ability-keys num-increases & [different? modifier-fns]]
  (ability-increase-selection-2 {:ability-keys ability-keys
                                 :num-increases num-increases
                                 :different? different?
                                 :modifier-fns modifier-fns}))

(defn ability-increase-option [num-increases different? ability-keys]
  (t/option-cfg
   {:name "Ability Score Improvement"
    :key :ability-score-improvement
    :selections [(ability-increase-selection ability-keys num-increases different?)]
    :modifiers [(modifiers/deferred-ability-increases)]}))

(defn min-ability [ability-kw min-value]
  (fn [c] (>= (ability-kw @(subscribe [::character/abilities nil c])) min-value)))

(defn ability-prereq [ability-kw min-value]
  (t/option-prereq (str "Requires " (s/upper-case (name ability-kw)) " " min-value " or higher")
                   (min-ability ability-kw min-value)))

(defn armor-prereq [armor-kw]
  (t/option-prereq (str "Requires proficiency with " (name armor-kw) " armor")
                   (fn [c] (let [prof-keys @(subscribe [::character/armor-profs nil c])]
                             (boolean (and prof-keys (prof-keys armor-kw)))))))

(defn race-prereq [race-nms & [hide-if-fail?]]
  (let [name-set (if (string? race-nms)
                   #{race-nms}
                   (into #{} race-nms))]
    (t/option-prereq
     (str (common/list-print name-set "or") " Only")
     (fn [c] (name-set @(subscribe [::character/race nil c])))
     hide-if-fail?)))

#_(defn race-prereq [race-kw]
  (t/option-prereq (str "Requires being a " (s/upper-case (name race-kw)))
                   (fn [c] (let [race-key @(subscribe [::character/race nil c])]
                             (boolean (and race-key (= race-key race-kw)))))))

(defn subrace-prereq [race-nm subrace-nm & [hide-if-fail?]]
  (t/option-prereq
   (str subrace-nm " Only")
   (fn [c] (and (= race-nm @(subscribe [::character/race nil c]))
                (= subrace-nm @(subscribe [::character/subrace nil c]))))
   hide-if-fail?))

(def elemental-disciplines
  [(t/option-cfg
    {:name "Breath of Winter"
     :modifiers [(modifiers/action
                  {:name "Breath of Winter"
                   :level 17
                   :page 81
                   :summary "spend 6 ki to cast cone of cold"})]})
   (t/option-cfg
    {:name "Clench of the North Wind"
     :modifiers [(modifiers/action
                  {:name "Clench of the North Wind"
                   :page 81
                   :level 6
                   :summary "spend 3 ki to cast hold person"})]})
   (t/option-cfg
    {:name "Eternal Mountain Defense"
     :modifiers [(modifiers/action
                  {:name "Eternal Mountain Defense"
                   :level 17
                   :page 81
                   :summary "spend 5 ki to cast stoneskin on yourself"})]})
   (t/option-cfg
    {:name "Fangs of the Fire Snake"
     :modifiers [(modifiers/trait-cfg
                  {:name "Fangs of the Fire Snake"
                   :page 81
                   :summary "spend 1 ki point when you use Attack action to increase your unarmed strike reach by 10 ft. You unarmed strike deals fire damage and if you spend 1 more ki it deals an extra 2d10 damage"})]})
   (t/option-cfg
    {:name "Fist of Four Thunders"
     :modifiers [(modifiers/action
                  {:name "Fist of Four Thunders"
                   :page 81
                   :summary "spend 2 ki to cast thunderwave"})]})
   (t/option-cfg
    {:name "Fist of Unbroken Air"
     :modifiers [(modifiers/action
                  {:name "Fist of Unbroken Air"
                   :page 81
                   :summary (str "spend 2 + X ki, a creature within 30 ft. takes 3d10 + Xd10 damage on failed DC " (?spell-save-dc ::character/wis) " STR save, is pushed up to 20 ft., and is knocked prone. On successful save it just takes half damage.")})]})
   (t/option-cfg
    {:name "Flames of the Phoenix"
     :modifiers [(modifiers/action
                  {:name "Flames of the Phoenix"
                   :level 11
                   :page 81
                   :summary "spend 4 ki to cast fireball"})]})
   (t/option-cfg
    {:name "Gong of the Summit"
     :modifiers [(modifiers/action
                  {:name "Gong of the Summit"
                   :page 81
                   :level 6
                   :summary "spend 3 ki to cast shatter"})]})
   (t/option-cfg
    {:name "Mist Stance"
     :modifiers [(modifiers/action
                  {:name "Mist Stance"
                   :page 81
                   :level 11
                   :summary "spend 4 ki to cast gaseous form on yourself"})]})
   (t/option-cfg
    {:name "Ride the Wind"
     :modifiers [(modifiers/action
                  {:name "Ride the Wind"
                   :page 81
                   :level 11
                   :summary "spend 4 ki to cast fly on yourself"})]})
   (t/option-cfg
    {:name "River of Hungry Flame"
     :modifiers [(modifiers/action
                  {:name "River of Hungry Flame"
                   :page 81
                   :level 17
                   :summary "spend 5 ki to cast wall of fire"})]})
   (t/option-cfg
    {:name "Rush of the Gale Spirits"
     :modifiers [(modifiers/action
                  {:name "Rush of the Gale Spirits"
                   :page 81
                   :summary "spend 2 ki to cast gust of wind"})]})
   (t/option-cfg
    {:name "Shape of the Flowing River"
     :modfiers [(modifiers/action
                 {:name "Shape of the Flowing River"
                  :page 81
                  :summary "spend 1 ki to transform ice to water, and vice versa, reshape ice"})]})
   (t/option-cfg
    {:name "Sweeping Cinder Strike"
     :modifiers [(modifiers/action
                  {:name "Sweeping Cinder Strike"
                   :page 81
                   :summary "spend 2 ki to cast burning hands"})]})
   (t/option-cfg
    {:name "Water Whip"
     :modifiers [(modifiers/bonus-action
                  {:name "Water Whip"
                   :page 81
                   :summary (str "spend 2 + X ki, a creature within 30 ft. takes 3d10 + Xd10 damage on failed DC " (?spell-save-dc ::character/wis) " DEX save, is pulled up to 25 ft. or knocked prone. On successful save it just takes half damage.")})]})
   (t/option-cfg
    {:name "Wave of Rolling Earth"
     :modifiers [(modifiers/action
                  {:name "Wave of Rolling Earth"
                   :level 17
                   :page 81
                   :summary "spend 6 ki to cast wall of stone"})]})])

(defn monk-elemental-disciplines []
  (t/selection-cfg
   {:name "Elemental Disciplines"
    :tags #{:class}
    :ref [:class :monk :elemental-disciplines]
    :multiselect? true
    :options elemental-disciplines}))

(defn language-option [{:keys [name key]}]
  (t/option-cfg
   {:name name
    :modifiers [(modifiers/language key)]
    :prereqs [(t/option-prereq
               "You already have this language"
               (fn [c] (not (get @(subscribe [::character/languages nil c]) key))))]}))

(defn key-to-name [key]
  (s/join " " (map s/capitalize (s/split (name key) #"-"))))

(defn spell-field [name value]
  [:div.m-b-2
   [:span.f-w-b (str name ": ")]
   [:span.f-w-n value]])

(defn spell-help [{:keys [school casting-time range duration components description summary source page]}]
  [:div
   [:div.m-b-5
    (spell-field "School" school)
    (spell-field "Casting Time" casting-time)
    (spell-field "Range" range)
    (spell-field "Duration" duration)
    (let [{:keys [verbal somatic material material-component]} components]
      (spell-field "Components" (str
          (s/join ", " (remove nil?
              [(if verbal "V")
               (if somatic "S")
               (if material "M")]))
          (if material-component (str " (" material-component ")")))))]
   [:div.f-w-n (if (or description summary)
                 (doall
                  (map-indexed
                   (fn [i p]
                     ^{:key i} [:p.m-t-5 p])
                   (s/split (or description summary) #"\n"))))]
   #_(if source
     (let [{:keys [abbr url]} (disp/sources source)]
       [:div.f-w-n
        [:span "(see"]
        [:a.m-l-5 {:href url :target :_blank} abbr]
        [:span.m-l-5 (str "page " page)]
        [:span " for more details)"]]))])

(defn using-source? [option-sources source]
  (or (nil? source)
      (= :phb source)
      (get option-sources source)))

(defn spell-option [spells-map spellcasting-ability class-name key & [prepend-level? qualifier prereq-fn]]
  (let [{:keys [name level source edit-event] :as spell} (spells-map key)]
    (t/option-cfg
     {:name (if prepend-level? (str level " - " name) name)
      :key key
      :edit-event edit-event
      :help (spell-help spell)
      :prereqs [prereq-fn
                (t/option-prereq
                 "You already know this spell"
                 (fn [c] (let [spells-known @(subscribe [::character/spells-known nil c])]
                           (or (not spells-known)
                               (not-any?
                                (fn [[[_ kw]]]
                                  (= key kw))
                                (get spells-known level))))))]
      :modifiers [(modifiers/spells-known level key spellcasting-ability class-name nil qualifier)]})))


(def memoized-spell-option (memoize spell-option))

(defn spell-options [spells-map spells spellcasting-ability class-name & [prepend-level? qualifier prereq-fn]]
  (map
   #(memoized-spell-option spells-map spellcasting-ability class-name % prepend-level? qualifier prereq-fn)
   (sort spells)))

(defn spell-level-title [class-name level]
  (str class-name (if (and level (zero? level)) " Cantrips Known" (str " Spells Known" (if level (str " " level))))))

(defn prereq-level-fn [prereq-level]
  (fn [c] (let [total-levels @(subscribe [::character/total-levels nil c])]
                               (>= total-levels prereq-level))))

(defn spell-selection [spell-lists spells-map {:keys [title class-key level spellcasting-ability class-name num prepend-level? spell-keys options min max exclude-ref? ref prereq-fn]}]
  (let [title (or title (spell-level-title class-name level))
        kw (common/name-to-kw title)
        ref (or ref (if (not exclude-ref?) [:class class-key kw]))]
     (t/selection-cfg
      {:name title
       :key kw
       :ref ref
       :order (if (and level (zero? level)) 0 1)
       :multiselect? true
       :prereq-fn prereq-fn
       :options (or options
                    (spell-options
                    spells-map
                    (or spell-keys (get-in spell-lists [class-key level]))
                    spellcasting-ability
                    class-name
                    prepend-level?
                    nil))
       :min (or min num)
       :max (or max num)
       :tags #{:spells}})))

      ;;  :options (flatten (concat (or options     
      ;;                (spell-options
      ;;                spells-map
      ;;                (or spell-keys (get-in spell-lists [class-key level]))
      ;;                spellcasting-ability
      ;;                class-name
      ;;                prepend-level?
      ;;                nil)
      ;;                )
      ;;                (map (fn [[race-name]]
      ;;                 (spell-options
      ;;                 spells-map
      ;;                 (get-in sl/race-spell-lists [race-name level])
      ;;                 spellcasting-ability
      ;;                 class-name
      ;;                 prepend-level?
      ;;                 nil
      ;;                 (race-prereq race-name false))) sl/race-spell-lists)
      ;;                (map (fn [[subrace-name race-name]]
      ;;                 (spell-options
      ;;                 spells-map
      ;;                 (get-in sl/subrace-spell-lists [subrace-name 1 level])
      ;;                 spellcasting-ability
      ;;                 class-name
      ;;                 prepend-level?
      ;;                 nil
      ;;                 (subrace-prereq race-name subrace-name false)))
      ;;                   (into {} (map (fn [subrace-list] {(get subrace-list 0) (get-in subrace-list [1 0])}) sl/subrace-spell-lists)))
      ;;                 )
      ;;                 )

(defn spell-slot-schedule [level-factor]
  (case level-factor
    1 {1 {1 2}
       2 {1 1}
       3 {1 1
          2 2}
       4 {2 1}
       5 {3 2}
       6 {3 1}
       7 {4 1}
       8 {4 1}
       9 {4 1
          5 1}
       10 {5 1}
       11 {6 1}
       13 {7 1}
       15 {8 1}
       17 {9 1}
       18 {5 1}
       19 {6 1}
       20 {7 1}}
    2 {2 {1 2}
       3 {1 1}
       5 {1 1
          2 2}
       7 {2 1}
       9 {3 2}
       11 {3 1}
       13 {4 1}
       15 {4 1}
       17 {4 1
           5 1}
       19 {5 1}}
    3 {3 {1 2}
       4 {1 1}
       7 {1 1
          2 2}
       10 {2 1}
       13 {3 2}
       16 {3 1}
       19 {4 1}}
    4 {1 {1 2}
       3 {1 1}
       5 {1 1
          2 2}
       7 {2 1}
       9 {3 2}
       11 {3 1}
       13 {4 1}
       15 {4 1}
       17 {4 1
           5 1}
       19 {5 1}}
    5 {1 {1 1}
       2 {1 2}
       3 {2 2}
       4 {2 2}
       5 {3 2}
       6 {3 2}
       7 {4 2}
       8 {4 2}
       9 {5 2}
       10 {5 2}
       11 {5 3}
       12 {5 3}
       13 {5 3}
       14 {5 3}
       15 {5 3}
       16 {5 3}
       17 {5 4}
       18 {5 4}
       19 {5 4}
       20 {5 4}}
    6 {3 {1 1}
       4 {1 2}
       5 {2 2}
       6 {2 2}
       7 {3 2}
       8 {3 2}
       9 {4 2}
       10 {4 2}
       11 {5 2}
       12 {5 2}
       13 {5 3}
       14 {5 3}
       15 {5 3}
       16 {5 3}
       17 {5 3}
       18 {5 3}
       19 {5 4}
       20 {5 4}}
    {}))

(defn total-slots [level level-factor]
  (let [schedule (spell-slot-schedule level-factor)]
    (reduce
     (fn [m lvl]
       (merge-with + m (schedule lvl)))
     {}
     (range 1 (inc level)))))

(defn spell-tags [cls-key-nm level]
  #{:spells (keyword (str cls-key-nm "-spells")) (keyword (str "level-" level))})

(defn bard-magical-secrets [spells-map min-level]
  (let [max-level (key (last (total-slots min-level 1)))
        spells-by-level (group-by :level (vals spells-map))
        filtered-spells-by-level (select-keys spells-by-level (range 0 (inc max-level)))]
    (t/selection-cfg
     {:name "Bard Magical Secrets"
      :tags #{:spells}
      :min 2
      :max 2
      :ref [:class :bard :magical-secrets]
      :options (mapcat
                (fn [[lvl spells]]
                  (map
                   (fn [{:keys [name] :as spell}]
                     (let [key (or (:key spell) (common/name-to-kw name))]
                       (spell-option spells-map ::character/cha "Bard" key true)))
                   spells))
                filtered-spells-by-level)})))

(defn cantrip-selections [spell-lists spells-map class-key class-name ability cantrips-known]
  (reduce
   (fn [m [k v]]
     (assoc m k [(spell-selection spell-lists
                                  spells-map
                                  {:class-key class-key
                                   :level 0
                                   :spellcasting-ability ability
                                   :class-name class-name
                                   :num v})]))
   {}
   cantrips-known))

(defn apply-spell-restriction [spells-map spell-keys restriction]
  (if restriction
    (filter
     (fn [spell-key]
       (restriction (spells-map spell-key)))
     spell-keys)
    spell-keys))

(defn class-key-name [cls-key cls-nm]
  (if cls-key
    (name cls-key)
    (common/name-to-kw cls-nm)))

(defn spell-selection-key [cls-key-nm]
  (keyword (str cls-key-nm "-spells-known")))


(defn spells-known-selections [spell-lists
                               spells-map
                               {:keys [class-key
                                       level-factor
                                       spells-known
                                       spell-list-kw
                                       known-mode
                                       spells
                                       ability
                                       slot-schedule] :as cfg}
                               cls-cfg]
  (reduce
   (fn [m [cls-lvl v]]
     (let [[num restriction] (if (number? v) [v] ((juxt :num :restriction) v))
           slots (or (if slot-schedule (slot-schedule cls-lvl)) (total-slots cls-lvl level-factor))
           all-spells (select-keys
                       (or spells (spell-lists (or spell-list-kw class-key)))
                       (keys slots))
           acquire? (= :acquire known-mode)
           options (flatten
                      (map
                       (fn [[lvl spell-keys]]
                         (let [spell-keys (vec spell-keys)
                               filtered-keys (apply-spell-restriction spells-map spell-keys restriction)]
                           (map
                            (fn [spell-key]
                              (let [spell (spells-map spell-key)]
                                #?@(:cljs
                                    [(if (nil? spell) (js/console.warn (str "No spell found for key: " spell-key)))
                                     (if (nil? (:name spell)) (js/console.warn (str "Spell is missing name: " spell-key)))])
                                (memoized-spell-option
                                 spells-map
                                 ability
                                 (:name cls-cfg)
                                 spell-key
                                 true)))
                            filtered-keys)))
                       all-spells))]
         (assoc m cls-lvl
                [(let [cls-key-nm (class-key-name (:key cls-cfg) (:name cls-cfg))
                       kw (spell-selection-key cls-key-nm)
                       cls-nm (:name cls-cfg)]
                   (spell-selection
                    spell-lists
                    spells-map
                    {:class-key class-key
                     :class-name cls-nm
                     :min num
                     :max (if (not acquire?) num)
                     :options options}))])))
   {}
   spells-known))

(defn spellcasting-template [spell-lists
                             spells-map
                             {:keys [class-key
                                     level-factor
                                     cantrips-known
                                     spells-known
                                     known-mode
                                     ability
                                     spell-list] :as cfg}
                             cls-cfg]
  (let [spell-lists (if spell-list
                      (assoc spell-lists class-key spell-list)
                      spell-lists)
        spell-selections (spells-known-selections spell-lists spells-map cfg cls-cfg)
        cantrip-selections (cantrip-selections spell-lists spells-map class-key (:name cls-cfg) ability cantrips-known)]
    {:selections (merge-with
                  concat
                  cantrip-selections
                  spell-selections)}))

(defn cantrip-selection [class-key class-name spellcasting-ability num]
  (t/selection-cfg
    {:name "Cantrip"
      :order 1
      :tags #{:spells}
      :options (spell-options spells/spell-map (get-in sl/spell-lists [class-key 0]) spellcasting-ability class-name)
      :min num
      :max num}))



(defn fey-touched-spell-selection [spellcasting-ability]
  (spell-selection sl/spell-lists
                    spells/spell-map
                    {:title "Fey Touched Divination or Enchantment Spell"
                    :spellcasting-ability spellcasting-ability
                    :class-name "Fey Touched"
                    :num 1
                    :prepend-level? false
                    :spell-keys (map :key (filter #(and (= 1 (:level %)) (fey-touched-spell? %)) spells/spells))
                    :exclude-ref? true
                    }))

(defn shadow-touched-spell-selection [spellcasting-ability]
  (spell-selection sl/spell-lists
                    spells/spell-map
                    {:title "Shadow Touched Illusion or Necromancy Spell"
                    :spellcasting-ability spellcasting-ability
                    :class-name "Shadow Touched"
                    :num 1
                    :prepend-level? false
                    :spell-keys (map :key (filter #(and (= 1 (:level %)) (shadow-touched-spell? %)) spells/spells))
                    :exclude-ref? true
                    }))

(defn fey-touched-ability-increase-selection [ability-keys num-increases & [different?]]
  (ability-increase-selection-2 {:ability-keys ability-keys
                                 :num-increases num-increases
                                 :different? different?
                                 :selection-fn (fn [k] (fey-touched-spell-selection k))
                                 :modifier-fns [(fn [k] (modifiers/spells-known 2 :misty-step k "Fey Touched"))
                                                ]}))

(defn shadow-touched-ability-increase-selection [ability-keys num-increases & [different?]]
  (ability-increase-selection-2 {:ability-keys ability-keys
                                 :num-increases num-increases
                                 :different? different?
                                 :selection-fn (fn [k] (shadow-touched-spell-selection k))
                                 :modifier-fns [(fn [k] (modifiers/spells-known 2 :invisibility k "Shadow Touched"))
                                                ]}))

(defn gift-of-the-gem-dragon-ability-increase-selection [ability-keys num-increases & [different? modifier-fns]]
  (ability-increase-selection-2 {:ability-keys ability-keys
                                 :num-increases num-increases
                                 :different? different?
                                 :modifier-fns [(fn [k] (modifiers/reaction
                                                         {:name "Gift of the Gem Dragon: Telekinetic Reprisal"
                                                          :frequency (units5e/long-rests ?prof-bonus)
                                                          :summary (str "When you take damage from a creature within 10 ft., the creature must make a DC " (?spell-save-dc k) " STR save or take 2d8 force damage and be pushed up to 10 ft. away, half damage and isn't pushed if successful")}))
                                                ]}))

(defn magic-initiate-option [spells-map class-key class-name spellcasting-ability spell-lists]
  (t/option-cfg
   {:name (name class-key)
    :selections [(t/selection-cfg
                  {:name "Cantrip"
                   :order 1
                   :tags #{:spells}
                   :options (spell-options spells-map (get-in spell-lists [class-key 0]) spellcasting-ability class-name)
                   :min 2
                   :max 2})
                 (t/selection-cfg
                  {:name "Level 1 Spell"
                   :order 2
                   :tags #{:spells}
                   :options (spell-options spells-map (get-in spell-lists [class-key 1]) spellcasting-ability class-name)
                   :min 1
                   :max 1})]}))

(defn ritual-spell? [spell]
  (:ritual spell))

(defn ritual-caster-option [spells-map class-key class-name spellcasting-ability spell-lists]
  (t/option-cfg
   {:name (name class-key)
    :key class-key
    :selections [(t/selection-cfg
                  {:name "Level 1 Ritual Spells"
                   :tags #{:spells}
                   :options (spell-options
                             spells-map
                             (filter (fn [spell-kw] (ritual-spell? (spells-map spell-kw))) (get-in spell-lists [class-key 1]))
                             spellcasting-ability
                             class-name
                             false
                             "Ritual Only")
                   :min 2
                   :max 2
                   :order 7})
                 (t/selection-cfg
                  {:name "Additional Ritual Spells"
                   :tags #{:spells}
                   :show-if-zero? true
                   :multiselect? true
                   :min 0
                   :max nil
                   :order 8
                   :options (spell-options
                             spells-map
                             (filter
                              (fn [spell-kw] (ritual-spell? (spells-map spell-kw)))
                              (apply concat
                                     (vals (get spell-lists class-key))))
                             spellcasting-ability
                             class-name
                             false
                             "Ritual Only")})]}))

(defn spell-sniper-option [spells-map class-key class-name spellcasting-ability spell-lists]
  (let [options (spell-options spells-map (filter (fn [spell-kw] (:attack-roll? (spells-map spell-kw))) (get-in spell-lists [class-key 0])) spellcasting-ability class-name)]
    (t/option-cfg
     {:name (name class-key)
      :key class-key
      :prereqs [(t/option-prereq
                 "There are no attack cantrips for this class"
                 (fn [_] (seq options)))]
      :selections [(t/selection-cfg
                    {:name "Attack Cantrip"
                     :tags #{:spells}
                     :options options})]})))

(defn weapon-proficiency-selection-2 [weapon-map weapon-proficiency-options]
  (let [{num :choose options :options} weapon-proficiency-options
        weapons (if (:any options)
                  (vals weapon-map)
                  (map weapon-map (keys options)))]
    (t/selection-cfg
     {:name "Weapon Proficiency"
      :options (map
                (fn [{:keys [name key :db/id] :as item}]
                  (t/option-cfg
                   {:name (or name (::mi/name item))
                    :key key}))
                weapons)
      :multiselect? true
      :tags #{:profs}
      :min (or num 1)
      :max (or num 1)})))

(defn language-selection-aux [languages num]
  (t/selection-cfg
   {:name "Languages"
    :options (map
              (fn [lang]
                (language-option lang))
              languages)
    :ref [:languages]
    :multiselect? true
    :tags #{:profs :language-profs}
    :min (or num 0)
    :max num}))

(defn language-selection [language-map language-options]
  (let [{lang-num :choose lang-options :options} language-options
        languages (if (:any lang-options)
                    (vals language-map)
                    (map language-map (keys lang-options)))]
    (language-selection-aux languages lang-num)))

(defn any-language-selection [language-map & [num]]
  (language-selection-aux (vals language-map) num))

(defn maneuver-option [name & [desc]]
  (t/option-cfg
   {:name name
    :modifiers [(modifiers/trait (str name " Maneuver")
                      desc)]}))

(defn mod-maneuver-option [name mods]
  (t/option-cfg
   {:name name
    :modifiers mods}))

(defn proficiency-help [num singular plural]
  (str "Select additional " (if (> num 1) plural singular) " for which you are proficient."))

(defn size-selection [sizes]
  (t/selection-cfg
   {:name "Size"
    :tags #{:race}
    :options (map
              (fn [size]
               (t/option-cfg
                {:name (common/kw-to-name size true)
                 :modifiers [(modifiers/size size)]}))
              sizes)
    ;; :options [(t/option-cfg
    ;;             {:name "Small"
    ;;             :modifiers [(mod5e/size :small)]})
    ;;           (t/option-cfg
    ;;             {:name "Medium"
    ;;             :modifiers [(mod5e/size :medium)]})]
                }))

(defn skill-selection-2 [{:keys [options num min max order key prereq-fn]}]
  (t/selection-cfg
   {:name "Skill Proficiency"
    :key key
    :order (or order 0)
    :help (proficiency-help (or num min) "a skill" "skills")
    :options (let [key-set (set options)]
               (skill-options
                (filter
                 (comp key-set :key)
                 skills/skills)))
    :min (or min num)
    :max (or max num)
    :multiselect? true
    ;;:ref [:skill-profs]
    :tags #{:skill-profs :profs}
    :prereq-fn prereq-fn}))

(defn skill-prof-or-expertise [skill-kw source]
  [(modifiers/skill-proficiency skill-kw source)
   (modifiers/skill-expertise skill-kw [(some
                                         (fn [[k v]]
                                           (not= k source))
                                         (?skill-profs skill-kw))])])

;; Does not work; always gives expertise
(defn tool-prof-or-expertise [tool-kw source]
  [(modifiers/tool-proficiency tool-kw false nil source)
   (modifiers/tool-expertise tool-kw [(some
                                         (fn [[k v]]
                                           (not= k source))
                                         (?tool-profs tool-kw))])])

(defn skill-or-expertise-selection [num skill-kws option-source]
  (t/selection-cfg
   {:name "Skill Proficiency"
    :order 0
    :tags #{:skill-profs :profs}
    :options (map
              (fn [skill-kw]
                (let [{:keys [name icon]} (skills/skills-map skill-kw)]
                  (t/option-cfg
                   {:name name
                    :icon icon
                    :modifiers [(skill-prof-or-expertise skill-kw option-source)]})))
              skill-kws)}))

(defn skill-expertise-selection [skill-kws num]
  (t/selection-cfg
   {:name "Skill Expertise (Double Proficiency)"
    :tags #{:profs}
    :min num
    :max num
    :options (map
              (fn [skill-kw]
                (let [{:keys [name icon]} (skills/skills-map skill-kw)]
                  (t/option-cfg
                   {:name name
                    :icon icon
                    :modifiers [(modifiers/skill-proficiency skill-kw)
                                (modifiers/skill-expertise skill-kw)]})))
              skill-kws)}))

(defn skill-selection
  ([num]
   (skill-selection-2 {:num num
                       :options (map :key skills/skills)}))
  ([options num & [order key prereq-fn]]
   (skill-selection-2 {:options options
                       :num num
                       :order order
                       :key key
                       :prereq-fn prereq-fn})))

(defn tool-proficiency-selection-2 [{:keys [num min max] :as cfg}]
  (t/selection-cfg
   (merge
    {:name "Tool Proficiency"
     :help (proficiency-help (or num min) "a tool" "tools")
     :multiselect 2
     :tags #{:tool-profs :profs}}
    (if num {:min num :max num})
    cfg)))

(defn tool-proficiency-selection [cfg]
  (tool-proficiency-selection-2
   cfg))

(defn tool-selection
  ([num]
   (tool-proficiency-selection
    {:options (tool-options equipment/tools)
     :num num}))
  ([options num]
   (tool-proficiency-selection
    {:options (tool-options
               (filter
                (comp (set options) :key)
                equipment/tools))
     :num num})))


(defn weapon-proficiency-selection
  ([num custom-and-standard-weapons]
   (t/selection-cfg
    {:name "Weapon Proficiency"
     :help (proficiency-help num "a weapon" "weapons")
     :options [(weapon-proficiency-options custom-and-standard-weapons)]
     :min num
     :max num
     :tags #{:weapon-profs :profs}}))
  ([options num custom-and-standard-weapons]
   (t/selection-cfg
    {:name "Weapon Proficiency"
     :help (proficiency-help num "a weapon" "weapons")
     :options (weapon-proficiency-options
               (filter
                (comp (set options) :key)
                custom-and-standard-weapons))
     :min num
     :max num
     :tags #{:weapon-profs :profs}})))

(defn skilled-selection [title]
  (t/selection-cfg
   {:name title
    :tags #{:profs}
    :options [(t/option-cfg
              {:name "Skill"
               :selections [(skill-selection 1)]})
             (t/option-cfg
              {:name "Tool"
               :selections [(tool-selection 1)]})]}))

(defn expertise-selection [num & [key]]
  (t/selection-cfg
   {:name "Skill Expertise"
    :key (or key :skill-expertise)
    :order 2
    :options (map
              (fn [{:keys [name key icon]}]
                (t/option-cfg
                 {:name name
                  :key key
                  :icon icon
                  :modifiers [(modifiers/skill-expertise key)]
                  :prereqs [(t/option-prereq (str "Requires proficiency in " name)
                                             (fn [built-char]
                                               (let [skill-profs @(subscribe [::character/skill-profs nil built-char])]
                                                 (and skill-profs (skill-profs key)))))]}))
              skills/skills)
    :min num
    :max num
    :multiselect? true
    :ref [:skill-expertise]
    :tags #{:profs :expertise}}))

(def maneuver-options
  [(maneuver-option "Ambush"
                    "When you make a Dexterity (Stealth) check or an initiative roll, you can expend one superiority die and add the die to the roll, provided you aren't incapacitated")
   (maneuver-option "Bait and Switch"
                    (str "When you're within 5 feet of a creature on your turn, you can expend one superiority die and switch places with that creature, provided you spend at least 5 feet of movement and the creature is willing and isn't incapacitated. This movement doesn't provoke opportunity attacks."
                         "\n\nRoll the superiority die. Until the start of your next turn, you or the other creature (your choice) gains a bonus to AC equal to the number rolled."))
   (mod-maneuver-option
    "Brace"
    [(modifiers/reaction
      {:name "Brace Maneuver"
       :summary "When a creature you can see moves into the reach you have with the melee weapon you're wielding, you can use your reaction to expend one superiority die and make one attack against the creature, using that weapon. If the attack hits, add the superiority die to the weapon's damage roll"})])
   (maneuver-option "Commander's Strike"
                    "When you take the Attack action on your turn, you can forgo one of your attacks and use a bonus action to direct one of your companions to strike. When you do so, choose a friendly creature who can see or hear you and expend one superiority die. That creature can immediately use its reaction to make one weapon attack, adding the superiority die to the attack's damage roll")
   (maneuver-option "Commanding Presense"
                    "When you make a Charisma (Intimidation), a Charisma (Performance), or a Charisma (Persuasion) check, you can expend one superiority die and add the superiority die to the ability check")
   (maneuver-option "Disarming Attack"
                    "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to disarm the target, forcing it to drop one item of your choice that it's holding. You add the superiority die to the attack's damage roll, and the target must make a Strength saving throw. On a failed save, it drops the object you choose. The object lands at its feet")
   (maneuver-option "Distracting Strike"
                    "When you hit a creature with a weapon attack, you can expend one superiority die to distract the creature, giving your allies an opening. You add the superiority die to the attack's damage roll. The next attack roll against the target by an attacker other than you has advantage if the attack is made before the start of your next turn")
   (maneuver-option "Evasive Footwork"
                    "When you move, you can expend one superiority die, rolling the die and adding the number rolled to your AC until you stop moving")
   (mod-maneuver-option
    "Feinting Attack"
    [(modifiers/bonus-action
      {:name "Feinting Attack Maneuver"
       :page 74
       :summary "You can expend one superiority die and use a bonus action on your turn to feint, choosing one creature within 5 feet of you as your target. You have advantage on your next attack roll against that creature this turn. If that attack hits, add the superiority die to the attack's damage roll"})])
   (mod-maneuver-option
    "Goading Attack"
    [(modifiers/dependent-trait
      {:name "Goading Attack Maneuver"
       :page 74
       :summary "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to goad the target into attacking you. You add the superiority die to the attack's damage roll, and the target must make a Wisdom saving throw. On a failed save, the target has disadvantage on all attack rolls against targets other than you until the end of your next turn"})])
   (mod-maneuver-option
    "Grappling Strike"
    [(modifiers/bonus-action
      {:name "Grappling Strike Maneuver"
       :summary "Immediately after you hit a creature with a melee attack on your turn, you can expend one superiority die and then try to grapple the target as a bonus action (see the Player's Handbook for rules on grappling). Add the superiority die to your Strength (Athletics) check"})])
   (maneuver-option "Lunging Attack"
                    "When you make a melee weapon attack on your turn, you can expend one superiority die to increase your reach for that attack by 5 feet. If you hit, you add the superiority die to the attack's damage roll")
   (maneuver-option "Manuevering Attack"
                    "When you hit a creature with a weapon attack, you can expend one superiority die to maneuver one of your comrades into a more advantageous position. You add the superiority die to the attack's damage roll, and you choose a friendly creature who can see or hear you. That creature can use its reaction to move up to half its speed without provoking opportunity attacks from the target of your attack")
   (mod-maneuver-option
    "Menacing Attack"
    [(modifiers/dependent-trait
      {:name "Menacing Attack Maneuver"
       :page 74
       :summary "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to frighten the target. You add the superiority die to the attack's damage roll, and the target must make a Wisdom saving throw. On a failed save, it is frightened of you until the end of your next turn"})])
   (mod-maneuver-option
    "Parry"
    [(modifiers/reaction
      {:name "Parry Maneuver"
       :page 74
       :summary "When another creature damages you with a melee attack, you can use your reaction and expend one superiority die to reduce the damage by the number you roll on your superiority die + your Dexterity modifier"})])
   (maneuver-option "Precision Attack"
                    "When you make a weapon attack roll against a creature, you can expend one superiority die to add it to the roll. You can use this maneuver before or after making the attack roll, but before any effects of the attack are applied")
   (mod-maneuver-option
    "Pushing Attack"
    [(modifiers/dependent-trait
      {:name "Pushing Attack Maneuver"
       :page 74
       :summary "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to drive the target back. You add the superiority die to the attack's damage roll, and if the target is Large or smaller, it must make a Strength saving throw. On a failed save, you push the target up to 15 feet away from you"})])
   (mod-maneuver-option
    "Quick Toss"
    [(modifiers/bonus-action
      {:name "Quick Toss Maneuver"
       :summary "As a bonus action, you can expend one superiority die and make a ranged attack with a weapon that has the thrown property. You can draw the weapon as part of making this attack. If you hit, add the superiority die to the weapon's damage roll"})])
   (mod-maneuver-option
    "Rally"
    [(modifiers/bonus-action
      {:name "Rally Maneuver"
       :page 74
       :summary "On your turn, you can use a bonus action and expend one superiority die to bolster the resolve of one of your companions. When you do so, choose a friendly creature who can see or hear you. That creature gains temporary hit points equal to the superiority die roll + your Charisma modifier"})])
   (mod-maneuver-option
    "Riposte"
    [(modifiers/reaction
      {:name "Riposte Maneuver"
       :page 74
       :summary "When a creature misses you with a melee attack, you can use your reaction and expend one superiority die to make a melee weapon attack against the creature. If you hit, you add the superiority die to the attack's damage roll"})])
   (maneuver-option "Sweeping Attack"
                    "When you hit a creature with a melee weapon attack, you can expend one superiority die to attempt to damage another creature with the same attack. Choose another creature within 5 feet of the original target and within your reach. If the original attack roll would hit the second creature, it takes damage equal to the number you roll on your superiority die. The damage is of the same type dealt by the original attack")
   (maneuver-option "Tactical Assessment"
                    "When you make an Intelligence (Investigation), an Intelligence (History), or a Wisdom (Insight) check, you can expend one superiority die and add the superiority die to the ability check")
   (mod-maneuver-option
    "Trip Attack"
    [(modifiers/dependent-trait
      {:name "Trip Attack Maneuver"
       :page 74
       :summary "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to knock the target down. You add the superiority die to the attack's damage roll, and if the target is Large or smaller, it must make a Strength saving throw. On a failed save, you knock the target prone"})])])

(defn rune-selection [num order prereq]
  (t/selection-cfg
   {:name "Rune Carver"
    :ref [:class :fighter :levels :level-3 :martial-archetype :rune-knight :rune-carver]
    :tags #{:class}
    :min num
    :max num
    :options [(t/option-cfg
               {:name "Cloud Rune"
                :help "Sleight of Hand and Deception advantage; use reaction to change target of attack"
                :modifiers [(modifiers/trait-cfg
                              {:name "Cloud Rune"
                              :summary "Advantage on Sleight of Hand and Deception checks"})
                            (modifiers/reaction
                              {:name "Cloud Rune"
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                              :summary "When you or a creature you can see within 30 feet of you is hit by an attack roll, choose a different creature within 30 feet of you, other than the attacker. The chosen creature becomes the target of the attack, using the same roll."})]})
              (t/option-cfg
               {:name "Fire Rune"
                :help "Expertise on proficient tools; restrain enemy hit by weapon attack with fiery shackles"
                :modifiers [(modifiers/trait-cfg
                              {:name "Fire Rune"
                              :summary "2X proficiency for any proficient tool"})
                            (modifiers/dependent-trait
                              {:name "Fire Rune"
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                              :summary (str "When you hit a creature with an attack using a weapon, summon fiery shackles: the target takes an extra 2d6 fire damage, and it must succeed on a DC " (?spell-save-dc ::character/con) " STR save or be restrained for 1 minute. While restrained by the shackles, the target takes 2d6 fire damage at the start of each of its turns. The target can repeat the saving throw at the end of each of its turns")})]})
              (t/option-cfg
               {:name "Frost Rune"
                :help "Animal Handling and Charisma advantage; +2 to STR and CON ability checks for 10 min"
                :modifiers [(modifiers/trait-cfg
                              {:name "Frost Rune"
                              :summary "Advantage on Animal Handling and Charisma checks"})
                            (modifiers/bonus-action
                              {:name "Frost Rune"
                              :duration units5e/minutes-10
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                              :summary "+2 to all ability checks using Strength or Constitution"})]})
              (t/option-cfg
               {:name "Stone Rune"
                :help "Insight advantage; 120 ft. darkvision; use reaction to charm creature that ends turn within 30 ft."
                :modifiers [(modifiers/darkvision 120 1)
                            (modifiers/trait-cfg
                              {:name "Stone Rune"
                              :summary "Advantage on Insight checks and 120 ft. darkvision"})
                            (modifiers/reaction
                              {:name "Stone Rune"
                              :duration units5e/minutes-1
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                              :summary (str "When a creature you can see ends its turn within 30 feet of you, force the creature to make a DC " (?spell-save-dc ::character/con) " WIS Save, charming the creature for 1 minute. While charmed in this way, the creature has a speed of 0 and is incapacitated. The creature repeats the saving throw at the end of each of its turns")})]})
              (t/option-cfg
               {:name "Hill Rune"
                :help "Poison save advantage; resistance to poison; gain resistance to bludgeoning, piercing, and slashing for 1 min"
                :prereqs [prereq]
                :modifiers [(modifiers/damage-resistance :poison)
                             (modifiers/saving-throw-advantage [:poisoned])
                             (modifiers/trait-cfg
                              {:name "Hill Rune"
                               :summary "Advantage on poison saves, resistance to poison damage"})
                             (modifiers/bonus-action
                              {:name "Hill Rune"
                               :duration units5e/minutes-1
                               :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                               :summary "Gain resistance to bludgeoning, piercing, and slashing damage"})]})
              (t/option-cfg
               {:name "Storm Rune"
                :help "Arcana advantage; surprise immunity; for 1 min., use reaction to give advantage to rolls"
                :prereqs [prereq]
                :modifiers [(modifiers/trait-cfg
                              {:name "Storm Rune"
                              :summary "Advantage on Arcana checks, and can't be surprised while you aren't incapacitated"})
                            (modifiers/bonus-action
                              {:name "Storm Rune"
                              :duration units5e/minutes-1
                              :frequency (units5e/rests (if (>= (?class-level :fighter) 15) 2 1))
                              :summary "Enter a prophetic state for 1 minute or until you're incapacitated. Until the state ends, when you or another creature you can see within 60 feet of you makes an attack roll, a saving throw, or an ability check, you can use your reaction to cause the roll to have advantage or disadvantage"})]})
              ]}))

(def can-cast-spell-prereq
  (t/option-prereq "Requires the ability to cast at least one spell."
                   (fn [c] (some (fn [[k v]] (seq v)) @(subscribe [::character/spells-known nil c])))))

(defn does-not-have-feat-prereq [kw]
  {::t/label "You already have this feat."
   ::t/prereq-fn (fn [c] (let [feats @(subscribe [::character/feats nil c])]
                           (not (and feats (feats kw)))))})

(defn feat-option [cfg & [multiselect?]]
  (let [kw (common/name-to-kw (:name cfg))
        summary (:summary cfg)]
    (t/option-cfg
     (cond-> cfg
       true (assoc :key kw :help summary)
       (not (:exclude-trait? cfg)) (update :modifiers
                                           conj
                                           (modifiers/trait-cfg
                                            {:name (str (:name cfg) " Feat")
                                             :page (:page cfg)
                                             :source (:source cfg)
                                             :summary summary}))
       true (update :modifiers
                    conj
                    (mods/set-mod ?feats kw))
       (not multiselect?) (update :prereqs conj (does-not-have-feat-prereq kw))))))

(def elven-accuracy-summary "Reroll one die on attacks with advantage using dex, int, wis, or cha")

(def gift-of-the-metallic-dragon-summary "You learn the cure wounds spell. You can cast this spell without expending a spell slot. Once you cast this spell in this way, you can't do so again until you finish a long rest. You can also cast this spell using spell slots you have. The spell's spellcasting ability is ")

#_(defn homebrew-spell-selection [spell-lists spells-map]
  (spell-selection
   spell-lists
   spells-map
   {:class-key :homebrew
    :class-name "Homebrew"
    :ref [:optional-content :homebrew :spells-known]
    :min 0
    :max nil
    :spell-keys (keys spells-map)}))

(def homebrew-tool-prof-selection
  (tool-proficiency-selection-2
   {:min 0
    :max nil
    :multiselect? true
    :ref [:tool-profs]
    :options (tool-options equipment/tools)}))

(def homebrew-skill-prof-selection
  (skill-selection-2 {:min 0
                      :max nil
                      :options (map :key skills/skills)}))

(defn homebrew-language-selection [language-map & [min max]]
  (t/selection-cfg
   {:name "Languages"
    :options (map
              (fn [lang]
                (language-option lang))
              (vals language-map))
    :multiselect? true
    :tags #{:profs :language-profs}
    :min (or min 0)
    :max max}))

(def homebrew-armor-prof-selection
  (t/selection-cfg
   {:name "Armor Proficiency"
    :key :armor-prof
    :tags #{:profs}
    :min 0
    :max nil
    :multiselect? true
    :options (map
              (fn [armor-type]
                (t/option-cfg
                 {:name (s/capitalize (name armor-type))
                  :key armor-type
                  :modifiers [(modifiers/armor-proficiency armor-type)]}))
              [:light :medium :heavy :shields])}))

(defn homebrew-weapon-prof-selection [weapon-map]
  (t/selection-cfg
   {:name "Weapon Proficiency"
    :key :weapon-prof
    :tags #{:profs}
    :min 0
    :max nil
    :multiselect? true
    :options (map
              (fn [{:keys [name key]}]
                (t/option-cfg
                 {:name name
                  :key key
                  :modifiers [(modifiers/weapon-proficiency key)]}))
              (conj
               (vals weapon-map)
               {:name "Simple"
                :key :simple}
               {:name "Martial"
                :key :martial}))}))

(def dual-wield-ac-mod
  (mods/vec-mod ?ac-bonus-fns
                (fn [_ _] 1)
                nil
                nil
                [(let [main-hand-weapon ?orcpub.dnd.e5.character/main-hand-weapon
                       off-hand-weapon ?orcpub.dnd.e5.character/off-hand-weapon
                       all-weapons-map @(subscribe [::mi/all-weapons-map])]
                   (and (and main-hand-weapon
                             (-> all-weapons-map
                                 main-hand-weapon
                                 ::weapons/melee?))
                        (and off-hand-weapon
                             (-> all-weapons-map
                                 off-hand-weapon
                                 ::weapons/melee?))))]))

(def dual-wield-weapon-mod
  (mods/modifier ?dual-wield-weapon? weapons/one-handed-weapon?))

(def medium-armor-master-max-bonus
  (mods/modifier ?max-medium-armor-bonus 3))

(def medium-armor-master-stealth
  (mods/fn-mod ?armor-stealth-disadvantage?
               (fn [armor]
                 (if (= :medium (:type armor))
                   false
                   (?armor-stealth-disadvantage? armor)))))

(defn custom-option-builder [name-sub name-event]
  [:div.m-t-10
   [:span "Name"]
   [comps/input-field
    :input
    @(subscribe name-sub)
    (fn [value]
      (dispatch (conj name-event value)))
    {:class-name "input"}]])

(defn feat-options [spell-lists spells-map language-map]
  [(feat-option
      {:name "Alert"
       :icon "look-at"
       :page 165
       :exclude-trait? true
       :summary "+5 initiative; can't be surprised; creatures don't gain advantage on attacks against you for being unseen"
       :modifiers [(modifiers/initiative 5)
                   (modifiers/trait-cfg
                    {:name "Alert"
                     :summary (str "\u2022 You gain a +5 bonus to initiative."
                                   "\n\u2022 You can't be surprised while you are conscious."
                                   "\n\u2022 Other creatures don't gain advantage on attack rolls against you as a result of being unseen by you.")})]})
   (feat-option
      {:name "Athlete"
       :icon "weight-lifting-up"
       :page 165
       :exclude-trait? true
       :summary "increase STR or DEX by 1; standing up only uses 5 ft movement; climbing doesn't cost extra movement; make running long or high jump after moving only 5 ft."
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Athlete"
                     :summary (str "\u2022 When you are prone, standing up uses only 5 feet of your movement."
                                   "\n\u2022 Climbing doesn't cost you extra movement."
                                   "\n\u2022 You can make a running long jump or a running high jump after moving only 5 feet on foot, rather than 10 feet.")})]})
   (feat-option
      {:name "Athlete (WoPV)"
       :icon "weight-lifting-up"
       :page 165
       :exclude-trait? true
       :summary "increase STR, DEX, or CON by 1; gain prof in Athletics or Acrobatics; standing up only uses 5 ft movement; climbing doesn't cost extra movement; make running long or high jump after moving only 5 ft.; advantage on Athletics and Acrobatics checks for traversing terrain"
       :selections [(ability-increase-selection [::character/str ::character/dex ::character/con] 1 false)
                    (skill-selection [:athletics :acrobatics] 1)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Athlete"
                     :summary (str "\u2022 When you are prone, standing up uses only 5 feet of your movement."
                                   "\n\u2022 Climbing doesn't cost you extra movement."
                                   "\n\u2022 You can make a running long jump or a running high jump after moving only 5 feet on foot, rather than 10 feet."
                                   "\n\u2022 You have advantage on Strength (Athletics) and Dexterity (Acrobatics) checks for traversing terrain.")})]})
   (feat-option
      {:name "Athlete (LRC)"
       :icon "weight-lifting-up"
       :page 165
       :exclude-trait? true
       :summary "increase STR, DEX, or CON by 1; gain prof in Athletics or Acrobatics; standing up only uses 5 ft movement; climbing doesn't cost extra movement; make running long or high jump after moving only 5 ft.; advantage on Athletics and Acrobatics checks for traversing terrain, being knocked prone, and for grappling or shoving"
       :selections [(ability-increase-selection [::character/str ::character/dex ::character/con] 1 false)
                    (skill-selection [:athletics :acrobatics] 1)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Athlete"
                     :summary (str "\u2022 When you are prone, standing up uses only 5 feet of your movement."
                                   "\n\u2022 Climbing doesn't cost you extra movement."
                                   "\n\u2022 You can make a running long jump or a running high jump after moving only 5 feet on foot, rather than 10 feet."
                                   "\n\u2022 You have advantage on Strength (Athletics) and Dexterity (Acrobatics) checks for traversing terrain, avoiding being knocked prone, and for contests involving grappling or shoving.")})]})
   (feat-option
      {:name "Actor"
       :icon "drama-masks"
       :page 165
       :exclude-trait? true
       :summary "increase CHA by 1; advantage on Deception and Performance when trying to pass as someone else; mimic the speech of a person or sounds of a creature you have heard for 1 at least 1 minute. Determined fake by Insight vs Deception"
       :modifiers [(modifiers/ability ::character/cha 1)
                   (modifiers/trait-cfg
                    {:name "Actor"
                     :summary (str "\u2022 You have advantage on Charisma (Deception) and Charisma (Performance) checks when trying to pass yourself off as a different person."
                                   "\n\u2022 You can mimic the speech of another person or the sounds made by other creatures. You must have heard the person speaking, or heard the creature make the sound, for at least 1 minute. A successful Wisdom (Insight) check contested by your Charisma (Deception) check allows a listener to determine that the effect is faked.")})]})
   (feat-option
      {:name "Actor (WoPV/LRC)"
       :icon "drama-masks"
       :page 165
       :exclude-trait? true
       :summary "increase CHA by 1; advantage on Deception and Performance when trying to pass as someone else; mimic the speech of a person or sounds of a creature you have heard for 1 at least 1 minute. Determined fake by Insight vs Deception; gain proficiency or expertise with Disguise Kit"
       :modifiers [(modifiers/ability ::character/cha 1)
                   (tool-prof-or-expertise :disguise-kit :actor)
                   (modifiers/tool-proficiency :disguise-kit)
                   (modifiers/trait-cfg
                    {:name "Actor"
                     :summary (str "\u2022 You have advantage on Charisma (Deception) and Charisma (Performance) checks when trying to pass yourself off as a different person."
                                   "\n\u2022 You can mimic the speech of another person or the sounds made by other creatures. You must have heard the person speaking, or heard the creature make the sound, for at least 1 minute. A successful Wisdom (Insight) check contested by your Charisma (Deception) check allows a listener to determine that the effect is faked.")})]})
   (feat-option
      {:name "Bountiful Luck"
       :exclude-trait? true
       :summary "as a reaction, have an ally reroll a 1 on the d20 for an attack roll, ability check, or saving throw"
       :modifiers [(modifiers/reaction
                    {:name "Bountiful Luck"
                     :summary (str "Your people have extraordinary luck, which you have learned to mystically lend to your companions when you see them falter. You're not sure how you do it; you just wish it, and it happens. Surely a sign of fortune's favor!"
                                   "\n\nWhen an ally you can see within 30 feet of you rolls a 1 on the d20 for an attack roll, an ability check, or a saving throw, you can use your reaction to let the ally reroll the die. The ally must use the new roll."
                                   "\n\nWhen you use this ability, you can't use your Lucky racial trait before the end of your next turn.")})]
       :prereqs [(race-prereq ["Halfling"])]})
   (feat-option
      {:name "Cartomancer"
       :exclude-trait? true
       :summary "learn prestidigitation and hide its components using conversation and card handling; imbue a card with a spell to cast it as a bonus action once"
       :modifiers [(modifiers/spells-known 0 :prestidigitation nil "Cartomancer")
                   (modifiers/trait-cfg
                    {:name "Cartomancer: Card Tricks"
                     :summary "You learn the Prestidigitation cantrip and can use it to create to create illusions that duplicate the effects of stage magic. When you use Prestidigitation in this way, you can conceal the components of the spell as ordinary conversation and card handling."})
                   (modifiers/bonus-action
                    {:name "Cartomancer: Hidden Ace"
                     :summary "When you finish a long rest, you can choose one spell from your class's spell list and imbue that spell into a card. The chosen spell must have a casting time of 1 action, and it must be a level for which you have spell slots. The card remains imbued with this spell for 8 hours. While the card is imbued with the spell, you can use a bonus action to flourish the card and cast the spell within. The card then immediately loses its magic."})]})
   (feat-option
      {:name "Charger"
       :icon "charging-bull"
       :page 165
       :exclude-trait? true
       :summary "when you Dash, you can make 1 melee attack or shove as a bonus action; if you move 10 ft. before taking this bonus action you gain +5 damage to attack or shove 10 ft."
       :modifiers [(modifiers/bonus-action
                    {:name "Charge"
                     :page 165
                     :summary (str "When you use your action to Dash, you can use a bonus action to make one melee weapon attack or to shove a creature."
                                   "\n\nIf you move at least 10 feet in a straight line immediately before taking this bonus action, you either gain a +5 bonus to the attack's damage roll (if you chose to make a melee attack and hit) or push the target up to 10 feet away from you (if you chose to shove and you succeed).")})]})
   (feat-option
      {:name "Charger (LRC)"
       :icon "charging-bull"
       :page 165
       :exclude-trait? true
       :summary "when you Dash, you can make 1 melee attack or shove as a bonus action; if you move 10 ft. before taking attacking or shoving you gain 1d8 damage to attack or shove 10 ft."
       :modifiers [(modifiers/bonus-action
                    {:name "Charger: Dash"
                     :page 165
                     :summary "When you use your action to Dash, you can use a bonus action to make one melee weapon attack or to shove a creature."})
                   (modifiers/trait-cfg
                    {:name "Charger: Charge"
                     :summary "Once per turn, if you move at least 10 feet in a straight line immediately before making a melee attack or shoving a creature, you either gain a 1d8 bonus to the attack’s damage roll (if you chose to make a melee attack and hit) or push the target up to 10 feet away from you (if you chose to shove and you succeed)."})]})
   (feat-option
      {:name "Chef"
       :exclude-trait? true
       :summary "increase CON or WIS by 1; gain proficiency with cook's utensils; you and allies regain extra HP during a short rest; make treats that give temp HP when eaten"
       :modifiers [(modifiers/dependent-trait
                    {:name "Chef"
                     :summary (str "\u2022 As part of a short rest, you can cook special food, provided you have ingredients and cook's utensils on hand. You can prepare enough of this food for a number of creatures equal to 4 + your proficiency bonus. At the end of the short rest, any creature who eats the food and spends one or more Hit Dice to regain hit points regains an extra 1d8 hit points."
                                   "\n\u2022 With one hour of work or when you finish a long rest, you can cook a number of treats equal to your proficiency bonus. These special treats last 8 hours after being made. A creature can use a bonus action to eat one of those treats to gain temporary hit points equal to your proficiency bonus.")})
                   (modifiers/tool-proficiency :cooks-utensils)]
       :selections [(ability-increase-selection [::character/con ::character/wis] 1)]})
   (feat-option
      {:name "Crossbow Expert"
       :icon "crossbow"
       :page 165
       :exclude-trait? true
       :summary "ignore loading property of crossbows you are proficient with; don't have disadvantage from being within 5 ft. of hostile creature; attack with a hand crossbow as bonus action"
       :modifiers [(modifiers/bonus-action
                    {:name "Crossbow Expert"
                     :page 165
                     :summary "When you use the Attack action and attack with a one-handed weapon, you can use a bonus action to attack with a hand crossbow you are holding."})
                   (modifiers/trait-cfg
                    {:name "Crossbow Expert"
                     :page 165
                     :summary (str "\u2022 You ignore the loading quality of crossbows with which you are proficient."
                                   "\n\u2022 Being within 5 feet of a hostile creature doesn't impose disadvantage on your ranged attack rolls.")})]})
   (feat-option
      {:name "Crusher"
       :exclude-trait? true
       :summary "increase STR or DEX by 1; when dealing bludgeoning damage, move target 5 ft. to an unoccupied space, provided it's no more than one size larger than you (once/turn); on critical dealing bludgeoning damage, attack rolls are made with advantage against the target until your next turn"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Crusher"
                     :summary (str "\u2022 Once per turn, when you hit a creature with an attack that deals bludgeoning damage, you can move it 5 feet to an unoccupied space, provided the target is no more than one size larger than you."
                                   "\n\u2022 When you score a critical hit that deals bludgeoning damage to a creature, attack rolls against that creature are made with advantage until the start of your next turn.")})]})
   (feat-option
      {:name "Defensive Duelist"
       :icon "spinning-sword"
       :page 165
       :exclude-trait? true
       :summary "when you are hit with a melee attack, you can add your prof bonus to AC for the attack if you are wielding a finesse weapon you are proficient with"
       :modifiers [(modifiers/reaction
                    {:name "Defensive Duelist"
                     :page 165
                     :summary "When you are wielding a finesse weapon with which you are proficient and another creature hits you with a melee attack, you can use your reaction to add your proficiency bonus to your AC for that attack, potentially causing the attack to miss you."})]
       :prereqs [(ability-prereq ::character/dex 13)]})
   (feat-option
      {:name "Defensive Duelist (WoPV)"
       :icon "spinning-sword"
       :page 165
       :exclude-trait? true
       :summary "increase STR or DEX by 1; when you are hit with a melee attack, you can add your prof bonus to AC for the attack if you are wielding a finesse weapon you are proficient with; gain proficiency with all finesse weapons"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/weapon-proficiency :finesse)
                   (modifiers/reaction
                    {:name "Defensive Duelist"
                     :page 165
                     :summary "When you are wielding a finesse weapon with which you are proficient and another creature hits you with a melee attack, you can use your reaction to add your proficiency bonus to your AC for that attack, potentially causing the attack to miss you."})]})
   (feat-option
      {:name "Defensive Duelist (LRC)"
       :icon "spinning-sword"
       :page 165
       :exclude-trait? true
       :summary "increase DEX by 1; when you are hit with a melee attack, you can add your prof bonus to AC for the attack if you are wielding a finesse weapon you are proficient with; gain proficiency with all finesse weapons"
       :modifiers [(modifiers/ability ::character/dex 1)
                   (modifiers/weapon-proficiency :finesse)
                   (modifiers/reaction
                    {:name "Defensive Duelist"
                     :page 165
                     :summary "When you are wielding a finesse weapon with which you are proficient and another creature hits you with a melee attack, you can use your reaction to add your proficiency bonus to your AC for that attack, potentially causing the attack to miss you."})]})
   (feat-option
      {:name "Dragon Fear"
       :exclude-trait? true
       :summary "increase STR, CON, or CHA by 1; use Breath Weapon to force each create of your choice within 30 ft. to make a WIS save or become frightened for 1 minute"
       :modifiers [(modifiers/action
                    {:name "Dragon Fear"
                     :summary (str "Instead of exhaling destructive energy, you can expend a use of your Breath Weapon trait to roar, forcing each creature of your choice within 30 feet of you to make a Wisdom saving throw (DC 8 + your proficiency bonus + your Charisma modifier = " (?spell-save-dc ::character/cha) "). A target automatically succeeds on the save if it can't hear or see you. On a failed save, a target becomes frightened of you for 1 minute. If the frightened target takes any damage, it can repeat the saving throw, ending the effect on itself on a success.")})]
       :selections [(ability-increase-selection [::character/str ::character/con ::character/cha] 1 false)]
       :prereqs [(race-prereq ["Dragonborn" "Dragonborn (Standard)"])]})
   (feat-option
      {:name "Dragon Hide"
       :exclude-trait? true
       :summary "increase STR, CON, or CHA by 1; calculate AC as 13 + DEX; use claws for unarmed strikes, dealing 1d4 + STR slashing damage"
       :modifiers [(modifiers/trait-cfg
                    {:name "Dragon Hide"
                     :summary (str "You manifest scales and claws reminiscent of your draconic ancestors. You gain the following benefits:"
                                   "\n\u2022 Your scales harden. While you aren't wearing armor, you can calculate your AC as 13 + your Dexterity modifier. You can use a shield and still gain this benefit."
                                   "\n\u2022 You grow retractable claws from the tips of your fingers. Extending or retracting the claws requires no action. The claws are natural weapons, which you can use to make unarmed strikes. If you hit with them, you deal slashing damage equal to 1d4 + your Strength modifier, instead of the normal bludgeoning damage for an unarmed strike.")})
                   (mods/vec-mod ?unarmored-defense :dragon-hide)
                   (mods/cum-sum-mod ?unarmored-ac-bonus 3
                                    nil
                                    nil
                                    [(= :dragon-hide (first ?unarmored-defense))])
                   (mods/cum-sum-mod ?unarmored-with-shield-ac-bonus 3
                                    nil
                                    nil
                                    [(= :dragon-hide (first ?unarmored-defense))])
                   (modifiers/attack
                    {:name "Claws"
                     :damage-die 4
                     :damage-die-count 1
                     :damage-modifier (?ability-bonuses ::character/str)
                     :summary "Unarmed strike"})]
       :selections [(ability-increase-selection [::character/str ::character/con ::character/cha] 1 false)]
       :prereqs [(race-prereq ["Dragonborn" "Dragonborn (Standard)"])]})
   (feat-option
      {:name "Drow High Magic"
       :exclude-trait? true
       :summary "Learn detect magic and cast at will; learn levitate and dispel magic to cast once per long rest; CHA spellcasting ability"
       :modifiers [(modifiers/trait-cfg
                    {:name "Drow High Magic"
                     :summary "You learn more of the magic typical of dark elves. You learn the detect magic spell and can cast it at will, without expending a spell slot. You also learn levitate and dispel magic, each of which you can cast once without expending a spell slot. You regain the ability to cast those two spells in this way when you finish a long rest. Charisma is your spellcasting ability for all three spells."})
                   (modifiers/spells-known 1 :detect-magic ::character/cha "Drow High Magic")
                   (modifiers/spells-known 2 :levitate ::character/cha "Drow High Magic")
                   (modifiers/spells-known 3 :dispel-magic ::character/cha "Drow High Magic")]
       :prereqs [(subrace-prereq "Elf (AoA)" "Dark Elf")]})
   (feat-option
      {:name "Dual Wielder"
       :icon "rogue"
       :page 165
       :exclude-trait? true
       :summary "+1 AC bonus when wielding two melee weapons; two-weapon fighting with any one-handed melee weapon; draw or stow two one-handed weapons"
       :modifiers [dual-wield-weapon-mod
                   dual-wield-ac-mod
                   (modifiers/trait-cfg
                    {:name "Dual Wielder"
                     :summary (str "\u2022 You gain a +1 bonus to AC while you are wielding a separate melee weapon in each hand."
                                   "\n\u2022 You can use two-weapon fighting even when the one-handed melee weapons you are wielding aren't light."
                                   "\n\u2022 You can draw or stow two one-handed weapons when you would normally be able to draw or stow only one.")})]})
   (feat-option
      {:name "Dungeon Delver"
       :icon "dungeon-gate"
       :page 166
       :exclude-trait? true
       :summary "advantage to detect secret doors; advantage on saves against and resistance to trap damage; fast pace doesn't impose passive Perception penalty"
       :modifiers [(modifiers/damage-resistance :trap)
                   (modifiers/saving-throw-advantage [:traps])
                   (modifiers/trait-cfg
                    {:name "Dungeon Delver"
                     :summary (str "\u2022 You have advantage on Wisdom (Perception) and Intelligence (Investigation) checks made to detect the presence of secret doors."
                                   "\n\u2022 You have advantage on saving throws made to avoid or resist traps."
                                   "\n\u2022 You have resistance to the damage dealt by traps."
                                   "\n\u2022 Traveling at a fast pace doesn't impose the normal -5 penalty on your passive Wisdom (Perception) score.")})]})
   (feat-option
      {:name "Durable"
       :icon "hospital-cross"
       :page 166
       :exclude-trait? true
       :summary "increase CON by 1; when you roll Hit Die to regain HPs, the min points regained is 2X your CON modifier"
       :modifiers [(modifiers/ability ::character/con 1)
                   (modifiers/dependent-trait
                    {:name "Durable"
                     :page 166
                     :summary (str "When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from the roll equals twice your Constitution modifier (minimum of 2) (" (max 2 (* 2 (?ability-bonuses ::character/con))) ").")})]})
   (feat-option
      {:name "Durable (WoPV)"
       :icon "defensive-wall"
       :exclude-trait? true
       :summary "increase CON by 1; when you roll Hit Die to regain HPs, the min points regained is 2X your CON modifier; recover all hit dice on a long rest"
       :modifiers [(modifiers/ability ::character/con 1)
                   (modifiers/dependent-trait
                    {:name "Durable"
                     :summary (str "\u2022 When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from the roll equals twice your Constitution modifier (minimum of 2) (" (max 2 (* 2 (?ability-bonuses ::character/con))) ")."
                                   "\n\u2022 You recover all hit die on a long rest instead of just half.")})]})
   (feat-option
      {:name "Durable (LRC)"
       :icon "hospital-cross"
       :exclude-trait? true
       :summary "increase CON by 1; when you roll Hit Die to regain HPs, the min points regained is 2X your CON modifier; use bonus action to heal from Hit Dice"
       :modifiers [(modifiers/ability ::character/con 1)
                   (modifiers/dependent-trait
                    {:name "Durable"
                     :page 166
                     :summary (str "When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from the roll equals twice your Constitution modifier (minimum of 2) (" (max 2 (* 2 (?ability-bonuses ::character/con))) ").")})
                   (modifiers/bonus-action
                    {:name "Durable"
                     :summary "As a Bonus Action, you can expend one of your Hit Dice, roll the die, and regain a number of Hit Points equal to th e roll."})]})
   (feat-option
      {:name "Dwarven Fortitude"
       :summary "increase CON by 1; when taking the dodge action, spend a Hit Die to heal yourself equal to the roll + CON"
       :modifiers [(modifiers/ability ::character/con 1)
                   (modifiers/trait-cfg
                    {:name "Dwarven Fortitude"
                     :summary "Whenever you take the Dodge action in combat, you can spend one Hit Die to heal yourself. Roll the die, add your Constitution modifier, and regain a number of hit points equal to the total (minimum of 1)."})]
       :prereqs [(race-prereq ["Dwarf"])]})
   (feat-option
      {:name "Elemental Adept"
       :icon "wind-hole"
       :page 166
       :summary "select a damage type (acid, cold, fire, lightning, or thunder), your spells ignore resistance to that type and min damage die roll is 2; can select this feat multiple times"
       :prereqs [can-cast-spell-prereq]}
      true)
   (feat-option
    {:name "Elven Accuracy"
     :page 74
     :exclude-trait? true
     :summary (str "increase DEX, INT, WIS, or CHA by 1; " elven-accuracy-summary)
     :modifiers [(modifiers/trait-cfg
                  {:name "Elven Accuracy"
                   :summary "Whenever you have advantage on an attack roll using Dexterity, Intelligence, Wisdom, or Charisma, you can reroll one of the dice once."})]
     :selections [(ability-increase-selection [::character/dex ::character/int ::character/wis ::character/cha] 1 false)]
     :prereqs [(race-prereq ["Elf" "Half-Elf" "Half-Elf (AoA)"])]})
   (feat-option
    {:name "Fade Away"
     :exclude-trait? true
     :summary "increase DEX or INT by 1; once per rest, use reaction to turn invisible after taking damage until the end of next turn, or until attacking, dealing damage, or forcing a saving throw"
     :modifiers [(modifiers/reaction
                  {:name "Fade Away"
                   :frequency units5e/rests-1
                   :summary "Immediately after you take damage, you can use a reaction to magically become invisible until the end of your next turn or until you attack, deal damage, or force someone to make a saving throw"})]
     :selections [(ability-increase-selection [::character/dex ::character/int] 1 false)]
     :prereqs [(race-prereq ["Gnome"])]})
   (feat-option
    {:name "Fey Teleportation"
     :exclude-trait? true
     :summary "increase INT or CHA by 1; learn to speak, read, and write Sylvan; learn and cast misty step once per rest"
     :selections [(ability-increase-selection [::character/int ::character/cha] 1 false)]
     :modifiers [(modifiers/language :sylvan)
                 (modifiers/trait-cfg
                  {:name "Fey Teleportation"
                   :summary "You learn the misty step spell and can cast it once without expending a spell slot. You regain the ability to cast it in this way when you finish a short or long rest. Intelligence is your spellcasting ability for this spell."})
                 (modifiers/spells-known 2 :misty-step ::character/int "Fey Teleportation")]
     :prereqs [(subrace-prereq "Elf (AoA)" "High Elf")]})
   (feat-option
    {:name "Fey Touched"
     :exclude-trait? true
     :summary "increase INT, WIS, or CHA by 1; learn misty step; learn 1 divination or enchantment 1st-level spell that can be casted without expending a spell slot once per long rest"
     :modifiers [(modifiers/spells-known 2 :misty-step nil "Fey Touched")
                 (modifiers/trait-cfg
                  {:name "Fey Touched"
                   :summary "You learn the misty step spell and one 1st-level spell of your choice. The 1st-level spell must be from the divination or enchantment school of magic. You can cast each of these spells without expending a spell slot. Once you cast either of these spells in this way, you can't cast that spell in this way again until you finish a long rest. You can also cast these spells using spell slots you have of the appropriate level. The spells' spellcasting ability is the ability increased by this feat."})]
     :selections [ ;;(fey-touched-spell-selection)
                  (fey-touched-ability-increase-selection [::character/int ::character/wis ::character/cha] 1 false)]})
   (feat-option
    {:name "Gift of the Gem Dragon"
     :exclude-trait? true
     :summary "When you take damage from a creature within 10 ft., use reaction to force the creature to make a STR save or take 2d8 force damage and be pushed up to 10 ft. away, half damage and isn't pushed if successful"
     :selections [(gift-of-the-gem-dragon-ability-increase-selection [::character/int ::character/wis ::character/cha] 1 false)]
     :modifiers [(modifiers/reaction
                  {:name "Gift of the Gem Dragon"
                   :frequency (units5e/long-rests ?prof-bonus)
                   :summary "When you take damage from a creature that is within 10 feet of you, you can use your reaction to emanate telekinetic energy. The creature that dealt damage to you must make a Strength saving throw (DC equals 8 + your proficiency bonus + the ability modifier of the score increased by this feat). On a failed save, the creature takes 2d8 force damage and is pushed up to 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed."})]})
   (feat-option
    {:name "Gift of the Metallic Dragon"
     :exclude-trait? true
     :summary "cast cure wounds once/long rest; use reaction to grant bonus to AC"
     :selections [(t/selection-cfg
                   {:name "Gift of the Metallic Dragon: Spellcasting ability"
                    :tags #{:spells}
                    :options [(t/option-cfg
                               {:name "Intelligence"
                                :modifiers [(modifiers/spells-known 1 :cure-wounds ::character/int "Gift of the Metallic Dragon")
                                            (modifiers/trait-cfg
                                             {:name "Gift of the Metallic Dragon: Draconic Healing"
                                              :summary (str gift-of-the-metallic-dragon-summary " Intelligence")})]})
                              (t/option-cfg
                               {:name "Wisdom"
                                :modifiers [(modifiers/spells-known 1 :cure-wounds ::character/wis "Gift of the Metallic Dragon")
                                            (modifiers/trait-cfg
                                             {:name "Gift of the Metallic Dragon: Draconic Healing"
                                              :summary (str gift-of-the-metallic-dragon-summary " Wisdom")})]})
                              (t/option-cfg
                               {:name "Charisma"
                                :modifiers [(modifiers/spells-known 1 :cure-wounds ::character/cha "Gift of the Metallic Dragon")
                                            (modifiers/trait-cfg
                                             {:name "Gift of the Metallic Dragon: Draconic Healing"
                                              :summary (str gift-of-the-metallic-dragon-summary " Charisma")})]})]})]
     :modifiers [(modifiers/spells-known 1 :cure-wounds nil "Gift of the Metallic Dragon")
                 (modifiers/reaction
                  {:name "Gift of the Metallic Dragon: Protective Wings"
                   :frequency (units5e/long-rests ?prof-bonus)
                   :summary "You can manifest protective wings that can shield you or others. When you or another creature you can see within 5 feet of you is hit by an attack roll, you can use your reaction to manifest spectral wings from your back for a moment. You grant a bonus to the target's AC equal to your proficiency bonus against that attack roll, potentially causing it to miss"})]})
   (feat-option
    {:name "Grappler"
     :icon "muscle-up"
     :page 167
     :exclude-trait? true
     :summary "advantage on attacks against creature you grapple; can use an action to pin the creature"
     :modifiers [(modifiers/action
                  {:name "Grappler"
                   :page 167
                   :summary (str "\u2022 You have advantage on attack rolls against a creature you are grappling."
                                 "\n\u2022 You can use your action to try to pin a creature grappled by you. To do so, make another grapple check. If you succeed, you and the creature are both restrained until the grapple ends.")})]
     :prereqs [(ability-prereq ::character/str 13)]})
   (feat-option
    {:name "Grappler (LRC)"
     :icon "muscle-up"
     :page 167
     :exclude-trait? true
     :summary "once per turn, deal both damage and attempt to grapple a creature you hit; advantage on attacks against creature you grapple; attempt to grapple with opportunity attack; speed isn't halved when moving a creature your size or smaller"
     :modifiers [(modifiers/action
                  {:name "Grappler"
                   :page 167
                   :summary (str "\u2022 When you hit a creature with an Unarmed Strike as part of the Attack action on your turn, you can both deal damage and attempt to grapple the creature. You can use this benefit only once per turn."
                                 "\n\u2022 You have advantage on attack rolls against a creature you are grappling."
                                 "\n\u2022 When making an opportunity attack, you can attempt to grapple the provoking creature instead of making an attack."
                                 "\n\u2022 Your speed isn’t halved when you move a creature grappled by you if the creature is your size or smaller.")})]
     :prereqs [(ability-prereq ::character/str 13)]})
   (feat-option
      {:name "Great Weapon Master"
       :icon "broadsword"
       :page 167
       :exclude-trait? true
       :summary "When you critical or reduce a creature to 0 HPs with melee weapon, make one melee weapon attack as bonus action. When you melee Attack with heavy weapon, you can take -5 on attack to deal +10 damage."
       :modifiers [(modifiers/bonus-action
                    {:name "Great Weapon Master"
                     :page 167
                     :summary "On your turn, when you score a critical hit with a melee weapon or reduce a creature to 0 hit points with one, you can make one melee weapon attack as a bonus action."})
                   (modifiers/trait-cfg
                    {:name "Great Weapon Master"
                     :summary "Before you make a melee attack with a heavy weapon that you are proficient with, you can choose to take a -5 penalty to the attack roll. If the attack hits, you add +10 to the attack's damage."})]})
   (feat-option
      {:name "Healer"
       :icon "medical-pack-alt"
       :page 167
       :exclude-trait? true
       :summary "When you stabilize with healer's kit, the creature regains 1 HP; use a healer's kit to restore 1d6 + 4 + creature's max hit dice HPs (use once/rest/person)"
       :modifiers [(modifiers/action
                    {:name "Healer"
                     :page 167
                     :summary "As an action, you can spend one use of a healer's kit to tend to a creature and restore 1d6 + 4 hit points to it, plus additional hit points equal to the creature's maximum number of Hit Dice. The creature can't regain hit points from this feat again until it finishes a short or long rest."})
                   (modifiers/trait-cfg
                    {:name "Healer"
                     :page 167
                     :summary "When you use a healer's kit to stabilize a dying creature, that creature also regains 1 hit point."})]})
   (feat-option
      {:name "Heavily Armored"
       :icon "lamellar"
       :exclude-trait? true
       :summary "increase STR by 1; proficiency in heavy armor"
       :page 167
       :modifiers [(modifiers/heavy-armor-proficiency)
                   (modifiers/ability ::character/str 1)]
       :prereqs [(armor-prereq :medium)]})
   (feat-option
      {:name "Heavy Armor Master"
       :icon "gauntlet"
       :page 167
       :exclude-trait? true
       :summary "increase STR by 1; when wearing heavy armor, slashing, piercing, and bludgeoning damage from non-magical attacks is 3 less"
       :modifiers [(modifiers/ability ::character/str 1)
                   (modifiers/trait-cfg
                    {:name "Heavy Armor Master"
                     :summary "While you are wearing heavy armor, bludgeoning, piercing, and slashing damage that you take from nonmagical attacks is reduced by 3."})]
       :prereqs [(armor-prereq :heavy)]})
   (feat-option
      {:name "Heavy Armor Master (LRC)"
       :icon "gauntlet"
       :page 167
       :exclude-trait? true
       :summary "increase CON or STR by 1; when wearing heavy armor, take prof bonus less slashing, piercing, and bludgeoning damage"
       :selections [(ability-increase-selection [::character/con ::character/str] 1 false)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Heavy Armor Master"
                     :summary "While you are wearing heavy armor, bludgeoning, piercing, and slashing damage that you take is reduced by an amount equal to your proficiency bonus (minimum of 3)."})]
       :prereqs [(armor-prereq :heavy)]})
   (feat-option
    {:name "Infernal Constitution"
     :page 75
     :exclude-trait? true
     :summary (str "increase CON by 1; resistance to cold and poison damage; advantage on saves against being poisoned")
     :modifiers [(modifiers/ability ::character/con 1)
                 (modifiers/trait-cfg
                  {:name "Infernal Constitution"
                   :summary (str "\u2022 You have resistance to cold and poison damage."
                                 "\n\u2022 You have advantage on saving throws against being poisoned.")})]
     :prereqs [(race-prereq ["Tiefling" "Tiefling (AoA)"])]})
   (feat-option
      {:name "Inspiring Leader"
       :icon "public-speaker"
       :page 167
       :exclude-trait? true
       :summary "spend 10 min to give 6 friendly creatures within 30 ft. temp HPs equal to you CHA mod + your level"
       :modifiers [(modifiers/dependent-trait
                    {:name "Inspiring Leader"
                     :page 167
                     :summary (str "You can spend 10 minutes inspiring your companions, shoring up their resolve to fight. When you do so, choose up to six friendly creatures (which can include yourself) within 30 feet of you who can see or hear you and who can understand you. Each creature can gain temporary hit points equal to your level + your Charisma modifier (" (+ (?ability-bonuses ::character/cha) ?total-levels) "). A creature can't gain temporary hit points from this feat again until it has finished a short or long rest.")})]
       :prereqs [(ability-prereq ::character/cha 13)]})
   (feat-option
      {:name "Keen Mind"
       :icon "brain"
       :page 167
       :exclude-trait? true
       :summary "increase INT by 1; always know which direction is north; know hours before sunset or sunrise; recall anything heard or seen within a month"
       :modifiers [(modifiers/ability ::character/int 1)
                   (modifiers/trait-cfg
                    {:name "Keen Mind"
                     :summary (str "\u2022 You always know which way is north."
                                   "\n\u2022 You always know the number of hours left before the next sunrise or sunset."
                                   "\n\u2022 You can accurately recall anything you have seen or heard within the past month.")})]})
   (feat-option
      {:name "Lightly Armored"
       :icon "scale-mail"
       :page 167
       :exclude-trait? true
       :summary "increase STR or DEX by 1; proficiency in light armor"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/light-armor-proficiency)]})
   (feat-option
      {:name "Linguist"
       :icon "lips"
       :page 167
       :exclude-trait? true
       :summary "increase INT by 1; learn 3 languages; create written ciphers"
       :selections [(language-selection-aux (vals language-map) 1)]
       :modifiers [(modifiers/ability ::character/int 1)
                   (modifiers/dependent-trait
                    {:name "Linguist Feat"
                     :page 167
                     :summary (str "You can ably create written ciphers. Others can't decipher a code you create unless you teach them, they succeed on an Intelligence check (DC equal to your Intelligence score + your proficiency bonus (" (+ (?abilities ::character/int) ?prof-bonus) ")), or they use magic to decipher it.")})]})
   (feat-option
      {:name "Lucky"
       :icon "clover"
       :page 167
       :exclude-trait? true
       :summary "3 luck points per long rest, which you can use to roll an additional d20 when rolling an attack, save, or ability check, or when an attack is made against you, and choose which one to use"
       :modifiers [(modifiers/trait-cfg
                    {:name "Lucky"
                     :frequency (units5e/long-rests 3)
                     :summary (str "You have inexplicable luck that seems to kick in at just the right moment."
                                   "\nYou have 3 luck points. Whenever you make an attack roll, an ability check, or a saving throw, you can spend one luck point to roll an additional d20. You can choose to spend one of your luck points after you roll the die, but before the outcome is determined. You choose which of the d20s is used for the attack roll, ability check, or saving throw."
                                   "\nYou can also spend one luck point when an attack roll is made against you. Roll a d20, and then choose whether the attack uses the attacker's roll or yours. If more than one creature spends a luck point to influence the outcome of a roll, the points cancel each other out; no additional dice are rolled."
                                   "\nYou regain your expended luck points when you finish a long rest")})]})
   (feat-option
      {:name "Mage Slayer"
       :icon "zeus-sword"
       :page 168
       :exclude-trait? true
       :summary "use reaction to attack a caster within 5 ft.; impose disadvantage to a caster's concentration check when you attack; advantage on saves against spells cast within 5 ft."
       :modifiers [(modifiers/reaction
                    {:name "Mage Slayer"
                     :page 168
                     :summary "When a creature within 5 feet of you casts a spell, you can use your reaction to make a melee weapon attack against that creature."})
                   (modifiers/trait-cfg
                    {:name "Mage Slayer"
                     :summary (str "\u2022 When you damage a creature that is concentrating on a spell, that creature has disadvantage on the saving throw it makes to maintain its concentration."
                                   "\n\u2022 You have advantage on saving throws against spells cast by creatures within 5 feet of you.")})]})
   (feat-option
      {:name "Magic Initiate"
       :icon "magic-palm"
       :page 168
       :summary "gain 2 cantrips and 1 1st level spell from a chosen class, that can also be cast free once/long rest"
       :selections [(t/selection-cfg
                     {:name "Spell Class"
                      :order 0
                      :tags #{:spells}
                      :options [(magic-initiate-option spells-map :bard "Bard" ::character/cha spell-lists)
                                (magic-initiate-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                (magic-initiate-option spells-map :druid "Druid" ::character/wis spell-lists)
                                (magic-initiate-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                (magic-initiate-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                (magic-initiate-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]})
   (feat-option
      {:name "Martial Adept"
       :icon "visored-helm"
       :page 168
       :frequency units5e/rests-1
       :summary (str "You have martial training that allows you to perform special combat maneuvers. You gain the following benefits:"
                     "\n\u2022 You learn two maneuvers of your choice from among those available to the Battle Master archetype in the fighter class. If a maneuver you use requires your target to make a saving throw to resist the maneuver's effects, the saving throw DC equals 8 + your proficiency bonus + your Strength or Dexterity modifier (your choice)."
                     "\n\u2022 You gain one superiority die, which is a d6 (this die is added to any superiority dice you have from another source). This die is used to fuel your maneuvers. A superiority die is expended when you use it. You regain your expended superiority dice when you finish a short or long rest")
       :selections [(t/selection-cfg
                     {:name "Martial Maneuvers"
                      :tags #{:class}
                      :options maneuver-options
                      :min 2
                      :max 2})]})
   (feat-option
      {:name "Medium Armor Master"
       :icon "bracers"
       :page 168
       :exclude-trait? true
       :summary "medium armor doesn't give disadvantage to Stealth; max DEX bonus to AC is 3 for medium armor"
       :modifiers [medium-armor-master-max-bonus
                   medium-armor-master-stealth
                   (modifiers/trait-cfg
                    {:name "Medium Armor Master"
                     :summary (str "\u2022 Wearing medium armor doesn't impose disadvantage on your Dexterity (Stealth) checks."
                                   "\n\u2022 When you wear medium armor, you can add 3, rather than 2, to your AC if you have a Dexterity of 16 or higher.")})]
       :prereqs [(armor-prereq :medium)]})
   (feat-option
      {:name "Medium Armor Master (LRC)"
       :icon "bracers"
       :page 168
       :exclude-trait? true
       :summary "increase STR or DEX by 1; medium armor doesn't give disadvantage to Stealth; max DEX bonus to AC is 3 for medium armor"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [medium-armor-master-max-bonus
                   medium-armor-master-stealth
                   (modifiers/trait-cfg
                    {:name "Medium Armor Master"
                     :summary (str "\u2022 Wearing medium armor doesn't impose disadvantage on your Dexterity (Stealth) checks."
                                   "\n\u2022 When you wear medium armor, you can add 3, rather than 2, to your AC if you have a Dexterity of 16 or higher.")})]
       :prereqs [(armor-prereq :medium)]})
   (feat-option
      {:name "Mobile"
       :icon "move"
       :page 168
       :exclude-trait? true
       :summary "speed increases by 10 ft.; Dash through difficult terrain doesn't cost extra movement; don't provoke opportunity attacks from a creature you made a melee attack against"
       :modifiers [(modifiers/speed 10)
                   (modifiers/trait-cfg
                    {:name "Mobile"
                     :summary (str "\u2022 Your speed increases by 10 feet."
                                   "\n\u2022 When you use the Dash action, difficult terrain doesn't cost you extra movement on that turn."
                                   "\n\u2022 When you make a melee attack against a creature, you don't provoke opportunity attacks from that creature for the rest of the turn, whether you hit or not.")})]})
   (feat-option
      {:name "Moderately Armored"
       :icon "shoulder-armor"
       :page 168
       :exclude-trait? true
       :summary "increase STR or DEX by 1; gain proficiency with shields and medium armor"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/medium-armor-proficiency)
                   (modifiers/shield-armor-proficiency)]
       :prereqs [(armor-prereq :light)]})
   (feat-option
      {:name "Mounted Combatant"
       :icon "cavalry"
       :page 168
       :exclude-trait? true
       :summary "while mounted and not incapacitated: advantage on attacks against unmounted creatures smaller than mount, force attack on mount to target you; mount takes no damage on sucessful DEX saves and half on fail"
       :modifiers [(modifiers/trait-cfg
                    {:name "Mounted Combatant"
                     :summary (str "\u2022 You have advantage on melee attack rolls against any unmounted creature that is smaller than your mount."
                                   "\n\u2022 You can force an attack targeted at your mount to target you instead."
                                   "\n\u2022 If your mount is subjected to an effect that allows it to make a Dexterity saving throw to take only half damage, it instead takes no damage if it succeeds on the saving throw, and only half damage if it fails.")})]})
   (feat-option
      {:name "Observant"
       :icon "surrounded-eye"
       :page 168
       :exclude-trait? true
       :summary "increase INT or WIS by 1; read lips; +5 bonus to passive Perception and passive Investigation"
       :selections [(ability-increase-selection [::character/int ::character/wis] 1 false)]
       :modifiers [(modifiers/passive-perception 5)
                   (modifiers/passive-investigation 5)
                   (modifiers/trait-cfg
                    {:name "Observant"
                     :summary (str "\u2022 If you can see a creature's mouth while it is speaking a language you understand, you can interpret what it's saying by reading its lips."
                                   "\n\u2022 You have a +5 bonus to your passive Wisdom (Perception) and passive Intelligence (Investigation) scores.")})]})
   (feat-option
      {:name "Orcish Fury"
       :exclude-trait? true
       :summary "increase STR or CON by 1; once per rest, on a hit with a simple or martial weapon, add one of the weapon's damage die again; after using Relentless Endurance, use reaction to make a weapon attack"
       :selections [(ability-increase-selection [::character/str ::character/con] 1 false)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Orcish Fury: Attack"
                     :frequency units5e/rests-1
                     :summary "When you hit with an attack using a simple or martial weapon, you can roll one of the weapon's damage dice an additional time and add it as extra damage of the weapon's damage type"})
                   (modifiers/reaction
                    {:name "Orcish Fury: Relentless Endurance"
                     :summary "Immediately after you use your Relentless Endurance trait, you can use your reaction to make one weapon attack."})]
       :prereqs [(race-prereq ["Half-Orc"])]})
   (feat-option
      {:name "Piercer"
       :page 80
       :exclude-trait? true
       :summary "increase STR or DEX by 1; reroll one damage die when dealing piercing damage; roll one additional damage die on critical hit with piercing damage"
       :modifiers [(modifiers/trait-cfg
                    {:name (str "\u2022 Once per turn, when you hit a creature with an attack that deals piercing damage, you can reroll one of the attack's damage dice, and you must use the new roll."
                                "\n\u2022 When you score a critical hit that deals piercing damage to a creature, you can roll one additional damage die when determining the extra piercing damage the target takes.")})]
       :selections [(ability-increase-selection [::character/str ::character/dex] 1)]})
   (feat-option
      {:name "Poisoner"
       :exclude-trait? true
       :summary "damage rolls ignore resistance to poison; apply poison using bonus action instead of action; gain proficiency with the poisoner's kit; spend 1 hour and 50 gp to create poison"
       :modifiers [(modifiers/trait-cfg
                    {:name "Poisoner"
                     :summary (str "\u2022 When you make a damage roll that deals poison damage, it ignores resistance to poison damage."
                                   "\n\u2022 With one hour of work using a poisoner's kit and expending 50 gp worth of materials, you can create a number of doses of potent poison equal to your proficiency bonus. Once applied to a weapon or piece of ammunition, the poison retains its potency for 1 minute or until you hit with the weapon or ammunition. When a creature takes damage from the coated weapon or ammunition, that creature must succeed on a DC 14 Constitution saving throw or take 2d8 poison damage and become poisoned until the end of your next turn.")})
                   (modifiers/bonus-action
                    {:name "Poisoner: Apply Poison"
                     :summary "You can apply poison to a weapon or piece of ammunition as a bonus action, instead of an action."})]})
   (feat-option
      {:name "Polearm Master"
       :icon "halberd"
       :page 168
       :exclude-trait? true
       :summary "bonus attack with opposite end of quarterstaff, glaive, or halberd; opportunity attacks have the reach of glaive, pike, halberd, or quarterstaff"
       :modifiers [(modifiers/bonus-action
                    {:name "Polearm Master"
                     :page 168
                     :summary "When you take the Attack action and attack with only a glaive, halberd, quarterstaff, or spear, you can use a bonus action to make a melee attack with the opposite end of the weapon; this attack uses the same ability modifier as the primary attack. The weapon's damage die for this attack is a d4, and the attack deals bludgeoning damage."})
                   (modifiers/reaction
                    {:name "Polearm Master"
                     :page 168
                     :summary "While you are wielding a glaive, halberd, pike, quarterstaff, or spear, other creatures provoke an opportunity attack from you when they enter the reach you have with that weapon."})]})
   (feat-option
      {:name "Prodigy"
       :exclude-trait? true
       :summary "gain one skill proficiency, one tool proficiency, and one language proficiency; gain expertise with a skill you have proficiency in"
       :selection [(skill-selection 1)
                   (tool-selection 1)
                   (language-selection-aux (vals language-map) 1)
                   (expertise-selection 1)]
       :prereqs [(race-prereq ["Half-Elf" "Half-Elf (AoA)" "Half-Orc" "Human"])]})
   (feat-option
      {:name "Resilient"
       :icon "dodging"
       :page 168
       :exclude-trait? true
       :summary "increase ability by 1 and gain proficiency in saves with that ability"
       :selections [(ability-increase-selection
                     character/ability-keys
                     1
                     false
                     [(fn [k] (modifiers/saving-throws nil k))])]})
   (feat-option
      {:name "Revenant Blade"
       :exclude-trait? true
       :summary "increase STR or DEX by 1; +1 to AC while holding a double-bladed scimitar with two hands; double-bladed scimitar have the finesse property"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Revenant Blade"
                     :summary (str "\u2022 While you are holding a double-bladed scimitar with two hands, you gain a +1 bonus to Armor Class."
                                   "\n\u2022 A double-bladed scimitar has the finesse property when you wield it.")})]
       :prereqs [(race-prereq ["Elf" "Elf (AoA)"])]})
   (feat-option
      {:name "Ritual Caster"
       :icon "gift-of-knowledge"
       :page 169
       :exclude-trait? true
       :summary "choose a spellcaster class and learn 2 rituals from that class; add found ritual spells to your book"
       :modifiers [(modifiers/dependent-trait
                    {:name "Ritual Caster Feat"
                     :page 169
                     :summary (str "choose a spellcaster class and learn 2 rituals from that class; add found ritual spells of the class to your book (max level " (common/round-up (/ ?total-levels 2)) "spending 2 hours and 50 gp per level")})]
       :selections [(t/selection-cfg
                     {:name "Ritual Caster: Spell Class"
                      :tags #{:spells}
                      :options [(ritual-caster-option spells-map :bard "Bard" ::character/cha spell-lists)
                                (ritual-caster-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                (ritual-caster-option spells-map :druid "Druid" ::character/wis spell-lists)
                                (ritual-caster-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                (ritual-caster-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                (ritual-caster-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]
       :prereqs [(t/option-prereq "Requires Intelligence or Wisdom 13 or higher"
                                  (fn [c]
                                    (let [{:keys [::character/wis ::character/int] :as abilities} @(subscribe [::character/abilities nil c])]
                                      (or (and wis (>= wis 13))
                                          (and int (>= int 13))))))]})
   (feat-option
      {:name "Savage Attacker"
       :icon "saber-slash"
       :page 169
       :exclude-trait? true
       :summary "reroll melee weapon attack damage and use either total (use once/turn)"
       :modifiers [(modifiers/trait-cfg
                    {:name "Savage Attacker"
                     :summary "Once per turn when you roll damage for a melee weapon attack, you can reroll the weapon's damage dice and use either total."})]})
   (feat-option
      {:name "Savage Attacker (WoPV)"
       :icon "saber-slash"
       :page 169
       :exclude-trait? true
       :summary "increase STR, DEX, or CON by 1; reroll melee weapon attack damage and use either total (use once/turn); score critical hit on a 19"
       :selections [(ability-increase-selection [::character/str ::character/dex ::character/con] 1 false)]
       :modifiers [(modifiers/trait-cfg
                    {:name "Savage Attacker"
                     :summary (str "\u2022 Once per turn when you roll damage for a melee weapon attack, you can reroll the weapon's damage dice and use either total."
                                   "\n\u2022 The roll on the d20 needed to score a critical hit is reduced by 1. This stacks with other features that increase your crit range, down to 17.")})]})
   (feat-option
      {:name "Savage Attacker (LRC)"
       :icon "saber-slash"
       :page 169
       :exclude-trait? true
       :summary "reroll melee weapon attack damage and use either total; score critical hit on a 19"
       :modifiers [(modifiers/trait-cfg
                    {:name "Savage Attacker"
                     :summary (str "\u2022 When you roll damage for a melee weapon attack, you can reroll the weapon’s damage dice and use either total."
                                   "\n\u2022 The roll on the d20 needed to score a critical hit is reduced by 1. This stacks with other features that increase your crit range, down to 17.")})]})
   (feat-option
      {:name "Second Chance"
       :exclude-trait? true
       :summary "increase DEX, CON, or CHA by 1"
       :modifiers [(modifiers/reaction
                    {:name "Second Chance"
                     :summary "When a creature you can see hits you with an attack roll, you can use your reaction to force that creature to reroll. Once you use this ability, you can't use it again until you roll initiative at the start of combat or until you finish a short or long rest."})]
       :prereqs [(race-prereq ["Halfling"])]})
   (feat-option
      {:name "Sentinel"
       :icon "guards"
       :page 169
       :exclude-trait? true
       :summary "reduce target's speed to 0 when you hit with opportunity attack; opportunity attacks even when target Disengages; use reaction to make a weapon attack against a creature within 5 ft. that attacks another target"
       :modifiers [(modifiers/trait-cfg
                    {:name "Sentinel"
                     :summary (str "\u2022 When you hit a creature with an opportunity attack, the creature's speed becomes 0 for the rest of the turn."
                                   "\n\u2022 Creatures provoke opportunity attacks from you even if they take the Disengage action before leaving your reach."
                                   "\n\u2022 When a creature within 5 feet of you makes an attack against a target other than you (and that target doesn't have this feat), you can use your reaction to make a melee weapon attack against the attacking creature.")})]})
   (feat-option
    {:name "Shadow Touched"
     :exclude-trait? true
     :summary "increase INT, WIS, or CHA by 1; learn invisibility; learn 1 illusion or necromancy 1st-level spell that can be casted without expending a spell slot once per long rest"
     :modifiers [(modifiers/spells-known 2 :invisibility nil "Shadow Touched")
                 (modifiers/trait-cfg
                  {:name "Shadow Touched"
                   :summary "You learn the invisibility spell and one 1st-level spell of your choice. The 1st-level spell must be from the illusion or necromancy school of magic. You can cast each of these spells without expending a spell slot. Once you cast either of these spells in this way, you can't cast that spell in this way again until you finish a long rest. You can also cast these spells using spell slots you have of the appropriate level. The spells' spellcasting ability is the ability increased by this feat."})]
     :selections [(shadow-touched-ability-increase-selection [::character/int ::character/wis ::character/cha] 1 false)]})
   (feat-option
      {:name "Sharpshooter"
       :icon "bullseye"
       :page 170
       :exclude-trait? true
       :summary "no disadvantage for long range for ranged weapon attacks; ranged weapons ignore half and 3/4 cover; take -5 to ranged attack to gain +10 on damage"
       :modifiers [(modifiers/trait-cfg
                    {:name "Sharpshooter"
                     :summary (str "\u2022 Attacking at long range doesn't impose disadvantage on your ranged weapon attack rolls."
                                   "\n\u2022 Your ranged weapon attacks ignore half cover and three-quarters cover."
                                   "\n\u2022 Before you make an attack with a ranged weapon that you are proficient with, you can choose to take a -5 penalty to the attack roll. If the attack hits, you add +10 to the attack's damage.")})]})
   (feat-option
      {:name "Shield Master"
       :icon "attached-shield"
       :page 170
       :exclude-trait? true
       :summary "when Attacking use bonus action to shove; add shield's AC bonus to saves that target just you and not incapacitated; use reaction to take no damage on a sucessful DEX save"
       :modifiers [(modifiers/bonus-action
                    {:name "Shield Master: Shove"
                     :page 170
                     :summary "If you take the Attack action on your turn, you can use a bonus action to try to shove a creature within 5 feet of you with your shield."})
                   (modifiers/trait-cfg
                    {:name "Shield Master: Single DEX Save"
                     :page 170
                     :summary "If you aren't incapacitated, you can add your shield's AC bonus to any Dexterity saving throw you make against a spell or other harmful effect that targets only you."})
                   (modifiers/reaction
                    {:name "Shield Master: DEX Save"
                     :page 170
                     :summary "If you are subjected to an effect that allows you to make a Dexterity saving throw to take only half damage, you can use your reaction to take no damage if you succeed on the saving throw, interposing your shield between yourself and the source of the effect."})]})
   (feat-option
      {:name "Skill Expert"
       :exclude-trait? true
       :summary "increase ability by one; proficiency in one skill; expertise in one proficient skill"
       :selections [(ability-increase-selection character/ability-keys 1)
                    (skill-selection 1)
                    (expertise-selection 1)
                    ]})
   (feat-option
      {:name "Skilled"
       :icon "juggler"
       :page 170
       :exclude-trait? true
       :summary "proficiency in three skills and/or tools"
       :selections [(skilled-selection "Skill/Tool 1")
                    (skilled-selection "Skill/Tool 2")
                    (skilled-selection "Skill/tool 3")]})
   (feat-option
      {:name "Skulker"
       :icon "ghost-ally"
       :page 170
       :exclude-trait? true
       :summary "try to hide when lightly obscured; when hiding, missing a ranged weapon attack doesn't reveal you; no disadvantage on Perception checks in dim light for sight"
       :modifiers [(modifiers/trait-cfg
                    {:name "Skulker"
                     :summary (str "\u2022 You can try to hide when you are lightly obscured from the creature from which you are hiding."
                                   "\n\u2022 When you are hidden from a creature and miss it with a ranged weapon attack, making the attack doesn't reveal your position."
                                   "\n\u2022 Dim light doesn't impose disadvantage on your Wisdom (Perception) checks relying on sight.")})]
       :prereqs [(ability-prereq ::character/dex 13)]})
   (feat-option
    {:name "Slasher"
     :page 81
     :exclude-trait? true
     :summary "increase STR or DEX by 1; when dealing slashing damage, reduce speed of target by 10 ft. until your next turn (once/turn); on critical dealing slashing damage, target has disadvantage on attacks until your next turn"
     :modifiers [(modifiers/trait-cfg
                  {:name "Slasher"
                   :summary (str "\u2022 Once per turn when you hit a creature with an attack that deals slashing damage, you can reduce the speed of the target by 10 feet until the start of your next turn."
                                 "\n\u2022 When you score a critical hit that deals slashing damage to a creature, you grievously wound it. Until the start of your next turn, the target has disadvantage on all attack rolls.")})]
     :selections [(ability-increase-selection [::character/str ::character/dex] 1)]})
   (feat-option
      {:name "Spell Sniper"
       :icon "laser-precision"
       :page 170
       :exclude-trait? true
       :summary "attack spells have double range; ranged spells ignore half and 3/4 cover; learn a cantrip that requires an attack roll"
       :modifiers [(modifiers/trait-cfg
                    {:name "Spell Sniper"
                     :summary (str "\u2022 When you cast a spell that requires you to make an attack roll, the spell's range is doubled."
                                   "\n\u2022 Your ranged spell attacks ignore half cover and three-quarters cover.")})]
       :prereqs [can-cast-spell-prereq]
       :selections [(t/selection-cfg
                     {:name "Spell Sniper: Spell Class"
                      :tags #{:spells}
                      :options [(spell-sniper-option spells-map :bard "Bard" ::character/cha spell-lists)
                                (spell-sniper-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                (spell-sniper-option spells-map :druid "Druid" ::character/wis spell-lists)
                                (spell-sniper-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                (spell-sniper-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                (spell-sniper-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]})
   (feat-option
      {:name "Tavern Brawler"
       :icon "broken-bottle"
       :page 170
       :exclude-trait? true
       :summary "increase STR or CON by 1; improvised weapon proficiency; d4 damage on unarmed strike; grapple as bonus action"
       :selections [(ability-increase-selection [::character/str ::character/con] 1 false)]
       :modifiers [(modifiers/weapon-proficiency :improvised)
                   (modifiers/trait-cfg
                    {:name "Tavern Brawler"
                     :page 170
                     :summary (str "\u2022 You are proficient with improvised weapons."
                                   "\n\u2022 Your unarmed strike uses a d4 for damage."
                                   "\n\u2022 When you hit a creature with an unarmed strike or an improvised weapon on your turn, you can use a bonus action to attempt to grapple the target.")})]})
   (feat-option
      {:name "Telekinetic"
       :exclude-trait? true
       :summary "increase INT, WIS, or CHA by 1; cast invisible mage hand without components; shove as bonus action"
       :selections [(ability-increase-selection [::character/int ::character/wis ::character/cha] 1 false [(fn [k] (mods/modifier ?telekinetic-ability k)) (fn [k] (modifiers/spells-known 0 :mage-hand k "Telekinetic"))])]
       :modifiers [(modifiers/spells-known 0 :mage-hand nil "Telekinetic")
                   (modifiers/trait-cfg
                    {:name "Telekinetic: Mage Hand"
                     :summary "You learn the mage hand cantrip. You can cast it without verbal or somatic components, and you can make the spectral hand invisible. If you already know this spell, its range increases by 30 feet when you cast it. Its spellcasting ability is the ability increased by this feat."})
                   (modifiers/bonus-action
                    {:name "Telekinetic: Shove"
                     :summary (str "As a bonus action, you can try to telekinetically shove one creature you can see within 30 feet of you. When you do so, the target must succeed on a Strength saving throw (DC 8 + your proficiency bonus + the ability modifier of the score increased by this feat (" (?spell-save-dc ?telekinetic-ability) ")) or be moved 5 feet toward you or away from you. A creature can willingly fail this save.")})]})
   (feat-option
      {:name "Telepathic"
       :exclude-trait? true
       :summary "increase INT, WIS, or CHA by 1; speak telepathically to any creature within 60 ft.; cast detect thoughts hand without components"
       :selections [(ability-increase-selection [::character/int ::character/wis ::character/cha] 1 false [(fn [k] (mods/modifier ?telepathic-ability k)) (fn [k] (modifiers/spells-known 2 :detect-thoughts k "Telepathic"))])]
       :modifiers [(modifiers/spells-known 2 :detect-thoughts nil "Telepathic")
                   (modifiers/trait-cfg
                    {:name "Telepathic: Telepathy"
                     :summary "You can speak telepathically to any creature you can see within 60 feet of you. Your telepathic utterances are in a language you know, and the creature understands you only if it knows that language. Your communication doesn't give the creature the ability to respond to you telepathically."})
                   (modifiers/trait-cfg
                    {:name "Telepathic: Detect Thoughts"
                     :summary "You can cast the detect thoughts spell, requiring no spell slot or components, and you must finish a long rest before you can cast it this way again. Your spellcasting ability for the spell is the ability increased by this feat. If you have spell slots of 2nd level or higher, you can cast this spell with them."})]})
   (feat-option
      {:name "Tough"
       :icon "defensive-wall"
       :page 170
       :exclude-trait? true
       :summary "2 extra HPs per level"
       :modifiers [(mods/modifier ?hit-point-level-bonus (+ 2 ?hit-point-level-bonus))
                   (modifiers/trait-cfg
                    {:name "Tough"
                     :summary "Your hit point maximum increases by an amount equal to twice your level when you gain this feat. Whenever you gain a level thereafter, your hit point maximum increases by an additional 2 hit points."})]})
   (feat-option
      {:name "Tough (LRC)"
       :icon "defensive-wall"
       :page 170
       :exclude-trait? true
       :summary "increase CON by 1; 2 extra HPs per level"
       :modifiers [(mods/modifier ?hit-point-level-bonus (+ 2 ?hit-point-level-bonus))
                   (modifiers/ability ::character/con 1)
                   (modifiers/trait-cfg
                    {:name "Tough"
                     :summary "Your hit point maximum increases by an amount equal to twice your level when you gain this feat. Whenever you gain a level thereafter, your hit point maximum increases by an additional 2 hit points."})]})
   (feat-option
      {:name "War Caster"
       :icon "deadly-strike"
       :page 170
       :exclude-trait? true
       :summary "adv. on CON saves for spell concentration; somatic components with weapons or shield in hand; cast 1 action single target spell as opportunity attack"
       :modifiers [(modifiers/trait-cfg
                    {:name "War Caster"
                     :summary (str "\u2022 You have advantage on Constitution saving throws that you make to maintain your concentration on a spell when you take damage."
                                   "\n\u2022 You can perform the somatic components of spells even when you have weapons or a shield in one or both hands."
                                   "\n\u2022 When a hostile creature's movement provokes an opportunity attack from you, you can use your reaction to cast a spell at the creature, rather than making an opportunity attack. The spell must have a casting time of 1 action and must target only that creature.")})]
       :prereqs [can-cast-spell-prereq]})
   (feat-option
      {:name "Weapon Master"
       :icon "sword-slice"
       :page 170
       :exclude-trait? true
       :summary "increase STR or DEX by 1; proficiency with 4 weapons"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)
                    (weapon-proficiency-selection-2 weapons/weapons-map {:choose 4 :options {:any true}})]})
   (feat-option
      {:name "Weapon Master (LRC)"
       :icon "sword-slice"
       :page 170
       :exclude-trait? true
       :summary "increase STR or DEX by 1; proficiency with Simple and Martial weapons; score critical hit on 19"
       :selections [(ability-increase-selection [::character/str ::character/dex] 1 false)]
       :modifiers [(modifiers/weapon-proficiency :simple)
                   (modifiers/weapon-proficiency :martial)]})
   (feat-option
    {:name "Wood Elf Magic"
     :exclude-trait? true
     :summary "learn one druid cantrip; learn longstrider and pass without trace that can be cast without expending a spell slot once per long rest"
     :modifiers [(modifiers/spells-known 1 :longstrider ::character/wis "Wood Elf Magic")
                 (modifiers/spells-known 2 :pass-without-trace ::character/wis "Wood Elf Magic")]
     :selections [(spell-selection spell-lists spells-map
                  {:class-key :druid
                   :level 0
                   :exclude-ref? true
                   :spellcasting-ability ::character/wis
                   :class-name "Wood Elf Magic"
                   :num 1})]
     :prereqs [(subrace-prereq "Elf (AoA)" "Wood Elf")]})]
  #_(map
   (fn [i]
     (t/option-cfg
      (let [kw (keyword (str "custom-feat-" i))]
        {:name (str "Custom Feat " (inc i))
         :key kw
         :icon "beer-stein"
         :order (inc i)
         :ui-fn #(custom-option-builder
                  [:custom-feat-name [:feats kw]]
                  [:set-custom-feat-name [:feats kw]])
         :selections [(t/selection-cfg
                       {:name "Feat Modifiers"
                        :min 0
                        :max nil
                        :multiselect? true
                        :order 2
                        :tags #{:feats}
                        :options [(t/option-cfg
                                   {:name "Tool Proficiency or Expertise"
                                    :help "Gain proficiency in a particular tool or expertise if you already have a proficiency in the tool (select on the 'Proficiencies' tab)."
                                    :selections [(t/selection-cfg
                                                  {:name "Tool Proficiency or Expertise"
                                                   :tags #{:profs}
                                                   :options (map
                                                             (fn [{:keys [name key]}]
                                                               (t/option-cfg
                                                                {:name name
                                                                 :key key
                                                                 :modifiers [(tool-prof-or-expertise key kw)]}))
                                                             equipment/tools)})]})
                                  (t/option-cfg
                                   {:name "Skill Proficiency or Expertise"
                                    :help "Gain proficiency in a particular skill or expertise if you already have proficiency in it"
                                    :selections [(t/selection-cfg
                                                  {:name "Skill Proficiency or Expertise"
                                                   :tags #{:profs}
                                                   :options (map
                                                             (fn [{:keys [name key]}]
                                                               (t/option-cfg
                                                                {:name name
                                                                 :key key
                                                                 :modifiers [(skill-prof-or-expertise key kw)]}))
                                                             skills/skills)})]})
                                  (t/option-cfg
                                   {:name "Ability Score Increase"
                                    :help "This will allow you to select and ability score to increase by 1 (see the 'Abilities Variant' section above)"
                                    :selections [(ability-increase-selection character/ability-keys 1 false)]})
                                  (t/option-cfg
                                   {:name "Extra 2 HPs Per Level"
                                    :help "This will give you an extra 2 HPs per level"
                                    :modifiers [(mods/modifier ?hit-point-level-bonus (+ 2 ?hit-point-level-bonus))]})
                                  (t/option-cfg
                                   {:name "Speed +10"
                                    :help "Increase your speed by 10 ft."
                                    :modifiers [(modifiers/speed 10)]})
                                  (t/option-cfg
                                   {:name "Passive Perception +5"
                                    :help "Increase your passive perception by 5"
                                    :modifiers [(modifiers/passive-perception 5)]})
                                  (t/option-cfg
                                   {:name "Passive Investigation +5"
                                    :help "Increase your passive investigation by 5"
                                    :modifiers [(modifiers/passive-perception 5)]})
                                  (t/option-cfg
                                   {:name "Save Proficiency"
                                    :help "Select proficiency in saving throws with a particular ability (select on the 'Proficiencies' tab)"
                                    :selections [(t/selection-cfg
                                                  {:name "Saving Throw Proficiency"
                                                   :tags #{:profs}
                                                   :options (map
                                                             (fn [k]
                                                               (t/option-cfg
                                                                {:name (:name (abilities-map k))
                                                                 :key k
                                                                 :modifiers [(modifiers/saving-throws nil k)]}))
                                                             character/ability-keys)})]})
                                  (t/option-cfg
                                   {:name "Initiative +5"
                                    :help "This will increase your initiative by 5."
                                    :modifiers [(modifiers/initiative 5)]})
                                  (t/option-cfg
                                   {:name "Weapon Proficiency"
                                    :help "This will allow you to select weapon proficiencies, from 'Simple', 'Martial', or specific weapons (select on the 'Proficiencies' tab)."
                                    :selections [homebrew-weapon-prof-selection]})
                                  (t/option-cfg
                                   {:name "Improvised Weapons Proficiency"
                                    :help "Gain proficiency in improvised weapons, such as broken bottles"})
                                  (t/option-cfg
                                   {:name "Armor Proficiency"
                                    :help "This will allow you to select armor proficiencies, from 'Shields', 'Light', 'Medium', or 'Heavy' (select on the 'Proficiencies' tab)."
                                    :selections [homebrew-armor-prof-selection]})
                                  (t/option-cfg
                                   {:name "Medium Armor: Max DEX Bonus of 3"
                                    :help "This will set your max dexterity bonus with medium armor to 3 instead of 2"
                                    :modifiers [medium-armor-master-max-bonus]})
                                  (t/option-cfg
                                   {:name "Ritual Spells"
                                    :help "Learn 2 ritual spells from a particular class"
                                    :selections [(t/selection-cfg
                                                  {:name "Spellaster Class"
                                                   :tags #{:spells}
                                                   :options [(ritual-caster-option spells-map :bard "Bard" ::character/cha spell-lists)
                                                             (ritual-caster-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                                             (ritual-caster-option spells-map :druid "Druid" ::character/wis spell-lists)
                                                             (ritual-caster-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                                             (ritual-caster-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                                             (ritual-caster-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]})
                                  (t/option-cfg
                                   {:name "Three Skills or Tools"
                                    :help "Select proficiency in three skills or tools"
                                    :selections [(skilled-selection "Skill/Tool 1")
                                                 (skilled-selection "Skill/Tool 2")
                                                 (skilled-selection "Skill/tool 3")]})
                                  (t/option-cfg
                                   {:name "Attack Cantrip"
                                    :help "Select a cantrip that requires an attack roll"
                                    :selections [(t/selection-cfg
                                                  {:name "Attack Cantrip Class"
                                                   :tags #{:spells}
                                                   :options [(spell-sniper-option spells-map :bard "Bard" ::character/cha spell-lists)
                                                             (spell-sniper-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                                             (spell-sniper-option spells-map :druid "Druid" ::character/wis spell-lists)
                                                             (spell-sniper-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                                             (spell-sniper-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                                             (spell-sniper-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]})
                                  (t/option-cfg
                                   {:name "Medium Armor: Stealthy"
                                    :help "This will allow you to use medium armor without stealth disadvantage"
                                    :modifiers [medium-armor-master-stealth]})
                                  (t/option-cfg
                                   {:name "Language Proficiency"
                                    :help "This will allow you to select language proficiencies"
                                    :selections [(homebrew-language-selection)]})
                                  (t/option-cfg
                                   {:name "Dual Wielding: AC +1"
                                    :help "When wielding two-weapons, this will give you a +1 bonus to AC."
                                    :key :dual-wield-ac-mod
                                    :modifiers [dual-wield-ac-mod]})
                                  (t/option-cfg
                                   {:name "Dual Wielding: Any One-Handed Melee Weapon"
                                    :help "This will allow you to engage in two-weapon fighting with any two single-handed melee weapons"
                                    :key :dual-wield-weapon-mod
                                    :modifiers [dual-wield-weapon-mod]})
                                  (t/option-cfg
                                   {:name "Spellcasting"
                                    :help "Select low-level spells from a particular class"
                                    :selections [(t/selection-cfg
                                                  {:name "Spell Class"
                                                   :order 0
                                                   :tags #{:spells}
                                                   :options [(magic-initiate-option spells-map :bard "Bard" ::character/cha spell-lists)
                                                             (magic-initiate-option spells-map :cleric "Cleric" ::character/wis spell-lists)
                                                             (magic-initiate-option spells-map :druid "Druid" ::character/wis spell-lists)
                                                             (magic-initiate-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
                                                             (magic-initiate-option spells-map :warlock "Warlock" ::character/cha spell-lists)
                                                             (magic-initiate-option spells-map :wizard "Wizard" ::character/int spell-lists)]})]})]})]})))
   (range 10)))

(def fighting-style-options
  [(t/option-cfg
    {:name "Archery"
     :modifiers [(modifiers/ranged-attack-bonus 2)
      (modifiers/trait-cfg
       {:name "Archery Fighting Style"
        :page 72
        :description "You gain a +2 bonus to attack rolls you make with ranged weapons."})]})
   (t/option-cfg
    {:name "Blind Fighting"
     :modifiers [(modifiers/trait-cfg
       {:name "Blind Fighting Style"
        :description "You have blindsight with a range of 10 feet. Within that range, you can effectively see anything that isn't behind total cover, even if you're blinded or in darkness. Moreover, you can see an invisible creature within that range, unless the creature successfully hides from you."})]})
   (t/option-cfg
    {:name "Defense"
     :modifiers [(modifiers/armored-ac-bonus 1)
      (modifiers/trait-cfg
       {:name "Defense Fighting Style"
        :page 72
        :description "While you are wearing armor, you gain a +1 bonus to AC."})]})
   (t/option-cfg
    {:name "Druidic Warrior"
     :selections [(cantrip-selection :druid "Druidic Warrior" ::character/wis 2)]
     :modifiers [(modifiers/trait-cfg
       {:name "Druidic Warrior Fighting Style"
        :description "You learn two cantrips of your choice from the Druid spell list. They count as ranger spells for you, and Wisdom is your spellcasting ability for them. Whenever you gain a level in this class, you can replace one of these cantrips with another cantrip from the Druid spell list."})]})
   (t/option-cfg
    {:name "Dueling"
     :modifiers [(modifiers/trait-cfg
       {:name "Dueling Fighting Style"
        :page 72
        :description "When you are wielding a melee weapon in one hand and no other weapons, you gain a +2 bonus to damage rolls with that weapon."})
                (mods/vec-mod ?damage-bonus-fns ;vec-mod prop
                              (fn [weapon _] (if (or (weapon ::weapons/two-handed?)
                                                     (weapon ::weapons/ranged?)) 0 2)) ;vec-mod val ... maybe?
                              nil ;vec-mod nm
                              nil ;vec-mod value ... maybe?
                              [(let [main-hand-weapon ?orcpub.dnd.e5.character/main-hand-weapon
                                     off-hand-weapon ?orcpub.dnd.e5.character/off-hand-weapon
                                     all-weapons-map @(subscribe [::mi/all-weapons-map])]
                                 (and (and main-hand-weapon
                                           (-> all-weapons-map
                                               main-hand-weapon
                                               ::weapons/melee?)
                                           (not (-> all-weapons-map
                                                    main-hand-weapon
                                                    ::weapons/two-handed?)))
                                      (and off-hand-weapon
                                           (not (-> all-weapons-map  ;ensure no weapons in off hand
                                                    off-hand-weapon
                                                    ::weapons/type)))))])
                 ]})
   (t/option-cfg
    {:name "Great Weapon Fighting"
     :modifiers [(modifiers/trait-cfg
       {:name "Great Weapon Fighting Style"
        :page 72
        :description "When you roll a 1 or 2 on a damage die for an attack you make with a melee weapon that you are wielding with two hands, you can reroll the die and must use the new roll, even if the new roll is a 1 or a 2. The weapon must have the two-handed or versatile property for you to gain this benefit."})]})
   (t/option-cfg
    {:name "Interception"
     :modifiers [(modifiers/trait-cfg
       {:name "Interception Fighting Style"
        :description "When a creature you can see hits a target, other than you, within 5 feet of you with an attack, you can use your reaction to reduce the damage the target takes by 1d10 + your proficiency bonus (to a minimum of 0 damage). You must be wielding a shield or a simple or martial weapon to use this reaction."})]})
   (t/option-cfg
    {:name "Protection"
     :modifiers [(modifiers/reaction
       {:name "Protection Fighting Style"
        :page 72
        :description "When a creature you can see attacks a target other than you that is within 5 feet of you, you can use your reaction to impose disadvantage on the attack roll. You must be wielding a shield."})]})
   (t/option-cfg
    {:name "Superior Technique"
     :selections [(t/selection-cfg
               {:name "Martial Maneuvers"
               :tags #{:class}
               :options maneuver-options})]
     :modifiers [(modifiers/trait-cfg
       {:name "Superior Technique Fighting Style"
        :description "Learn one Battle Master martial maneuvers using 1 d6 superiority die (regain on rest). Save DC is DEX or STR."})]})
   (t/option-cfg
    {:name "Thrown Weapon Fighting"
     :modifiers [(modifiers/trait-cfg
       {:name "Thrown Weapon Fighting Style"
        :description "You can draw a weapon that has the thrown property as part of the attack you make with the weapon.\n\nWhen you hit with a ranged attack using a thrown weapon, you gain a +2 bonus to the damage roll."})]})
   (t/option-cfg
    {:name "Two Weapon Fighting"
     :modifiers [(modifiers/trait-cfg
                  {:name "Two Weapon Fighting"
                   :description "When you engage in two-weapon fighting, you can add your ability modifier to the damage of the second attack."})
                 (mods/modifier ?weapon-ability-damage-modifier
                                (fn [weapon finesse? _]
                                  (?weapon-ability-modifier weapon finesse?)))]})
   (t/option-cfg
    {:name "Unarmed Fighting"
     :modifiers [(modifiers/dependent-trait
       {:name "Unarmed Fighting Style"
        :description (str "Your unarmed strikes deal 1d6+" (?ability-bonuses ::character/str) " bludgeoning damage, 1d8 if you aren't wielding any weapons or a shield")})
                 (modifiers/attack
                  {:name "Unarmed Fighting"
                   :damage-die 6
                   :damage-die-count 1
                   :damage-modifier (?ability-bonuses ::character/str)
                   :summary "Unarmed strike"})
                 (modifiers/attack
                  {:name "Unarmed Fighting"
                   :damage-die 8
                   :damage-die-count 1
                   :damage-modifier (?ability-bonuses ::character/str)
                   :summary "Unarmed strike (unarmed)"})]})])

(defn fighting-style-selection-2 [class-kw num options]
  (t/selection-cfg
   {:name "Fighting Style"
    :tags #{:class}
    :ref [:class class-kw :fighting-style]
    :multiselect? true
    :min num
    :max num
    :options options}))

(defn fighting-style-selection [class-kw & [restrictions additional-options]]
  (fighting-style-selection-2
   class-kw
   1
   (if restrictions
     (filter
      (fn [o]
        (restrictions (::t/key o)))
      fighting-style-options)
     fighting-style-options)))

(defn feat-selection [spell-lists spells-map language-map num]
  (t/selection-cfg
   {:name "Feats"
    :options (feat-options spell-lists spells-map language-map)
    :multiselect? true
    :tags #{:feats}
    :order 2
    :ref [:feats]
    :show-if-zero? true
    :min num
    :max num}))

(defn ability-score-improvement-selection [spell-lists spells-map language-map cls lvl]
  (t/selection-cfg
   {:name "Ability Score Improvement or Feat"
    :key :asi-or-feat
    :tags #{:ability-scores}
    :options [(ability-increase-option 2 false character/ability-keys)
              (t/option-cfg
               {:name "Feat"
                :selections [(feat-selection spell-lists spells-map language-map 1)]})]}))

(def rogue-expertise-selection
  (t/selection-cfg
   {:name "Expertise"
    :tags #{:profs :skill-profs :expertise}
    :order 1
    :options [(t/option-cfg
               {:name "Two Skills"
                :selections [(expertise-selection 2 :two-skills)]})
              (t/option-cfg
               {:name "One Skill/Thieves' Tools"
                :selections [(expertise-selection 1 :one-skill-thieves-tools)]
                :modifiers [(modifiers/tool-proficiency :thieves-tools)
                            (modifiers/tool-expertise :thieves-tools)]})]}))

(defn artificer-spell [spell-level spell-key min-level]
  (modifiers/spells-known-cfg
   spell-level
   {:key spell-key
    :ability ::character/int
    :class "Artificer"
    :qualifier "Specialist"
    :class-key :artificer
    :always-prepared? true}
   min-level
   nil))

(defn cleric-spell [spell-level spell-key min-level]
  (modifiers/spells-known-cfg
   spell-level
   {:key spell-key
    :ability ::character/wis
    :class "Cleric"
    :qualifier "Domain"
    :class-key :cleric
    :always-prepared? true}
   min-level
   nil))


(defn potent-spellcasting [page & [source]]
  (modifiers/dependent-trait
   {:level 8
    :page page
    :source source
    :summary (str "You add your Wisdom modifiers ("
                  (common/bonus-str (?ability-bonuses ::character/wis))
                  ") to the damage you deal with any cleric cantrip")
    :name "Potent Spellcasting"}))

(def monk-base-cfg
  {:name "Monk"
   :subclass-level 3
   :subclass-title "Monastic Tradition"})

(def paladin-base-cfg
  {:name "Paladin"
   :subclass-level 3
   :subclass-title "Sacred Oath"})

(def ua-al-illegal (modifiers/al-illegal "Unearthed Arcana options are not allowed"))

(defn subclass-plugin [class-base-cfg source subclasses ua-al-illegal?]
  (merge
   class-base-cfg
   {:source source
    :plugin? true
    :subclasses (if ua-al-illegal?
                  (map
                   (fn [subclass]
                     (update subclass :modifiers conj ua-al-illegal))
                   subclasses)
                  subclasses)}))

(defn paladin-spell [spell-level key]
  (modifiers/spells-known-cfg spell-level
                              {:key key
                               :ability ::character/cha
                               :class "Paladin"
                               :always-prepared? true
                               :class-key :paladin}
                              (case spell-level
                                1 3
                                2 5
                                3 9
                                4 13
                                5 17)
                              nil))

(defn subclass-spell-selection [spell-lists spells-map class-key class-name ability spells num]
  (spell-selection
   spell-lists
   spells-map
   {:class-key class-key
    :spell-keys spells
    :spellcasting-ability ability
    :class-name class-name
    :num num
    :prepend-level? true}))

(defn subclass-cantrip-selection [spell-lists spells-map class-key class-name ability spells num]
  (spell-selection
   spell-lists
   spells-map
   {:class-key class-key
    :level 0
    :spellcasting-ability ability
    :class-name class-name
    :spell-keys spells
    :num num}))

(defn warlock-subclass-spell-selection [spell-lists spells-map class-kw spellcasting-ability spells]
  (subclass-spell-selection spell-lists spells-map class-kw (if (= class-kw :warlock-int) "Warlock (Int)" "Warlock") spellcasting-ability spells 0))

(def classes
  {:artificer "Artificer"
   :bard "Bard"
   :cleric "Cleric"
   :druid "Druid"
   :paladin "Paladin"
   :ranger "Ranger"
   :sorcerer "Sorcerer"
   :warlock-int "Warlock (Int)"
   :warlock-cha "Warlock"
   :wizard "Wizard"})

(def prepared-cantrip-classes
  {:artificer "Artificer"
   :cleric "Cleric"
   :druid "Druid"})

(def unprepared-classes
  {:bard "Bard"
   :sorcerer "Sorcerer"
   :warlock-int "Warlock (Int)"
   :warlock-cha "Warlock"
   :wizard "Wizard"})

(def prepared-classes
  {:artificer "Artificer"
   :cleric "Cleric"
   :druid "Druid"
   :paladin "Paladin"
   :ranger "Ranger"})

(def class-prepares-spells?
  {:artificer true
   :bard false
   :cleric true
   :druid true
   :paladin true
   :ranger true
   :sorcerer false
   :warlock-int false
   :warlock-cha false
   :wizard false})

(def class-spellcasting-ability
  {:artificer ::character/int
   :bard ::character/cha
   :cleric ::character/wis
   :druid ::character/wis
   :paladin ::character/cha
   :ranger ::character/wis
   :sorcerer ::character/cha
   :warlock-int ::character/cha
   :warlock-cha ::character/cha
   :wizard ::character/int})

(def class-level-factors
  {:artificer 4
   :bard 1
   :cleric 1
   :druid 1
   :paladin 2
   :ranger 2
   :sorcerer 1
   :warlock-int 5
   :warlock-cha 5
   :wizard 1})

(def max-spell-slot-levels
  {1 [1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9 9]
   2 [0 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5]
   3 [0 0 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4]
   4 [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5]
   5 [1 1 2 2 3 3 4 4 5 5 5 5 5 5 5 5 5 5 5 5]})

(defn spell-level-prereq [spell-level class-key]
  (fn [c] (let [level-factor (get class-level-factors class-key)
                class-level (@(subscribe [::character/class-level-fn nil c]) class-key)
                max-spell-slot-level (get (get max-spell-slot-levels level-factor) (- class-level 1))]
               (>= max-spell-slot-level spell-level))))

(defn min-level-prereq [spell-level class-key]
  (let [level-factor (get class-level-factors class-key)
        level-prereq (+ 1 (.indexOf (max-spell-slot-levels level-factor) spell-level))]
        (if (= 0 level-prereq) 21 level-prereq)))

(defn race-spell-selection [spell-lists spells-map spells num]
  (into []
    (map
      (fn [[class-key class-name]]
        (let [spell-level (get-in spells-map [(get spells 0) :level])]
        (spell-selection
          spell-lists
          spells-map
          {:class-key class-key
            :level (if (zero? spell-level) 0 nil)
            :spell-keys spells
            :spellcasting-ability (get class-spellcasting-ability class-key)
            :class-name class-name
            ;; :ref [:class class-key :bard-cantrips-known]
            :num 0
            :prepend-level? (if (zero? spell-level) false true)
            :prereq-fn (spell-level-prereq spell-level class-key)})))
      unprepared-classes
      )))

(defn race-cantrip-selection [spell-lists spells-map spells num]
  (into []
    (map
      (fn [[class-key class-name]]
        (spell-selection
          spell-lists
          spells-map
          {:class-key class-key
            :level 0
            :spell-keys spells
            :spellcasting-ability (get class-spellcasting-ability class-key)
            :class-name class-name
            ;; :ref [:class class-key :bard-cantrips-known]
            :num 0
            :prepend-level? false
            :prereq-fn (spell-level-prereq 0 class-key)}))
      prepared-cantrip-classes
      )))

(defn race-spell-prepared-class [spell-lists spells-map spells num]
  (into []
    (map
      (fn [[class-key class-name]]
        (let [spell-level (get-in spells-map [(get spells 0) :level])]
        (map (fn [spell] (modifiers/spells-known 1 spell (get class-spellcasting-ability class-key) class-name (min-level-prereq spell-level class-key) nil class-key)) spells)))
      prepared-classes
      )))

(defn subrace-spell-selections [spell-lists spells-map subrace-nm min-lvl max-lvl]
  (mapcat (fn [spell-level] (race-spell-selection spell-lists spells-map (get-in sl/subrace-spell-lists [subrace-nm 1 spell-level]) 0)) (range min-lvl (+ max-lvl 1))))

(defn race-spell-selections [spell-lists spells-map race-nm min-lvl max-lvl]
  (mapcat (fn [spell-level] (race-spell-selection spell-lists spells-map (get-in sl/race-spell-lists [race-nm spell-level]) 0)) (range min-lvl (+ max-lvl 1))))

(defn race-cantrip-selections [spell-lists spells-map race-nm min-lvl max-lvl]
  (race-cantrip-selection spell-lists spells-map (get-in sl/race-spell-lists [race-nm 0]) 0))

(defn subrace-spells-known [spell-lists spells-map subrace-nm min-lvl max-lvl]
  (mapcat (fn [spell-level] (race-spell-prepared-class spell-lists spells-map (get-in sl/subrace-spell-lists [subrace-nm 1 spell-level]) 0)) (range min-lvl (+ max-lvl 1))))

(defn race-spells-known [spell-lists spells-map race-nm min-lvl max-lvl]
  (mapcat (fn [spell-level] (race-spell-prepared-class spell-lists spells-map (get-in sl/race-spell-lists [race-nm spell-level]) 0)) (range min-lvl (+ max-lvl 1))))

(defn traits-modifiers [traits & [class-key source]]
  (map
   (fn [trait]
     (modifiers/trait-cfg (merge {:source source
                                  :class-key class-key}
                                 trait)))
   traits))

(defn armor-prof-modifiers [armor-proficiencies & [cls-kw]]
  (map
   (fn [armor-prof]
     (let [[armor-kw first-class?] (if (keyword? armor-prof) [armor-prof false] armor-prof)]
       (modifiers/armor-proficiency armor-kw first-class? cls-kw)))
   armor-proficiencies))

(defn tool-prof-modifiers [tool-proficiencies & [cls-kw]]
  (map
   (fn [tool-prof]
     (let [[tool-kw first-class?] (if (keyword? tool-prof) [tool-prof false] tool-prof)]
       (modifiers/tool-proficiency tool-kw first-class? cls-kw)))
   tool-proficiencies))

(defn weapon-prof-modifiers [weapon-proficiencies & [cls-kw]]
  (map
   (fn [weapon-prof]
     (let [[weapon-kw first-class?] (if (keyword? weapon-prof) [weapon-prof false] weapon-prof)]
       (if (#{:simple :martial} weapon-kw)
         (modifiers/weapon-proficiency weapon-kw first-class? cls-kw)
         (modifiers/weapon-proficiency weapon-kw first-class? cls-kw))))
   weapon-proficiencies))


(defn subrace-option [race
                      spell-lists
                      spells-map
                      languages
                      source
                      {:keys [name
                              abilities
                              profs
                              size
                              sizes
                              speed
                              darkvision
                              subrace-options
                              armor-proficiencies
                              weapon-proficiencies
                              modifiers
                              selections
                              traits
                              source
                              edit-event]}]
  (let [{:keys [skill-options tool]} profs
        {skill-num :choose options :options} skill-options
        skill-kws (if (:any options)
                    (map :key skills/skills)
                    (map
                     clojure.core/key
                     (filter val options)))]
    (t/option-cfg
     {:name name
      :edit-event edit-event
      :selections (concat
                   (if (seq sizes)
                    [(size-selection sizes)])
                   (if (seq skill-kws)
                     [(skill-selection skill-kws (or skill-num 1))])
                   selections)
      :modifiers (concat
                  [(modifiers/subrace name)]
                  (if (and speed
                           (not= speed (:speed race)))
                    [(modifiers/speed (- speed (:speed race)))])
                  (if (and darkvision
                           (not= darkvision (:darkvision race)))
                    [(modifiers/darkvision darkvision)])
                  modifiers
                  (armor-prof-modifiers armor-proficiencies)
                  (weapon-prof-modifiers weapon-proficiencies)
                  (tool-prof-modifiers (common/true-keys tool))
                  (map
                   (fn [[k v]]
                     (modifiers/subrace-ability k v))
                   abilities)
                  (traits-modifiers traits nil source)
                  (if source [(modifiers/used-resource source name)]))})))

(defn ability-modifiers [abilities]
  (map
   (fn [[k v]]
     (modifiers/ability k v))
   abilities))

(defn darkvision-modifiers [range]
  [(modifiers/darkvision range)])

(defn feat-selection-2 [cfg]
  (t/selection-cfg
   (merge
    {:name "Feats"
     :ref [:feats]
     :show-if-zero? true
     :tags #{:feats}
     :order 1
     :multiselect? true}
    cfg)))

(def homebrew-ability-increase-selection
  (ability-increase-selection-2
   {:min 0}))

(defn homebrew-feat-selection [spell-lists spells-map language-map]
  (feat-selection-2
   {:min 0
    :max nil
    :options (feat-options spell-lists spells-map language-map)}))

(def homebrew-al-illegal
  (modifiers/al-illegal "Homebrew options are not allowed"))

(defn none-option [path]
  (t/option-cfg
   {:name "<none>"
    :key :none
    :order 1001
    :prereqs [(t/option-prereq
               nil
               (fn [_] @(subscribe [:homebrew? path]))
               true)]}))


(defn custom-subrace-builder []
  (custom-option-builder
   [:custom-subrace-name]
   [:set-custom-subrace]))

(def homebrew-speed-selection
  (t/selection-cfg
   {:name "Speed"
    :tags #{:race}
    :min 0
    :max 1
    :options (map
              (fn [speed]
                (t/option-cfg
                 {:name (str speed " ft.")
                  :key (keyword (str "ft-" speed))
                  :modifiers [(modifiers/speed speed)]}))
              (range -10 55 5))}))

(def homebrew-darkvision-selection
  (t/selection-cfg
   {:name "Darkvision"
    :tags #{:race}
    :min 0
    :max 1
    :options (map
              (fn [distance]
                (t/option-cfg
                 {:name (str distance " ft.")
                  :key (keyword (str "ft-" distance))
                  :modifiers [(modifiers/darkvision distance)]}))
              (range 0 150 30))}))

(defn custom-subrace-option [spell-lists spells-map language-map weapon-map path]
  (t/option-cfg
   {:name "Custom"
    :icon "beer-stein"
    :ui-fn custom-subrace-builder
    :help "Homebrew subrace. This allows you to use a subrace that is not on the list. This will allow unrestricted access to skill and tool proficiencies, racial ability increases, and feats."
    :modifiers [(modifiers/deferred-subrace)
                homebrew-al-illegal]
    :order 1000
    :selections [homebrew-skill-prof-selection
                 homebrew-tool-prof-selection
                 homebrew-ability-increase-selection
                 (homebrew-feat-selection spell-lists spells-map language-map)
                 homebrew-speed-selection
                 homebrew-darkvision-selection
                 homebrew-armor-prof-selection
                 (homebrew-weapon-prof-selection weapon-map)
                 (homebrew-language-selection language-map)]}))

(defn custom-race-builder []
  (custom-option-builder
   [:custom-race-name]
   [:set-custom-race]))

(defn subrace-selection [race spell-lists spells-map language-map weapon-map plugin? source subraces path]
  (let [subrace-path (conj path :subrace)]
    (t/selection-cfg
     {:name "Subrace"
      :tags #{:subrace}
      :min (if subraces 1 0)
      :order 1
      :options (cond->
                (if (seq subraces)
                  (map
                   (partial subrace-option race spell-lists spells-map language-map source)
                   (if source
                     (map (fn [sr] (assoc sr :source source)) subraces)
                     subraces))
                  [(none-option subrace-path)])

                 (not plugin?)
                 (conj (custom-subrace-option spell-lists spells-map language-map weapon-map subrace-path)))})))

(defn custom-race-option [spell-lists spells-map language-map weapon-map]
  (t/option-cfg
   {:name "Custom"
    :icon "beer-stein"
    :ui-fn custom-race-builder
    :help "Homebrew race. This allows you to use a race that is not on the list. This will allow unrestricted access to skill and tool proficiencies, racial ability increases, and feats."
    :modifiers [(modifiers/deferred-race)
                homebrew-al-illegal]
    #_:prereqs #_[(t/option-prereq
               nil
               (fn [_] @(subscribe [:homebrew? [:race]]))
               true)]
    :order 1000
    :selections [(subrace-selection {} spell-lists spells-map language-map weapon-map false nil nil [:race :custom])
                 homebrew-skill-prof-selection
                 homebrew-tool-prof-selection
                 homebrew-ability-increase-selection
                 (homebrew-feat-selection spell-lists spells-map language-map)
                 homebrew-speed-selection
                 homebrew-darkvision-selection
                 homebrew-armor-prof-selection
                 (homebrew-weapon-prof-selection weapon-map)
                 (homebrew-language-selection language-map)]}))

(defn custom-background-builder []
  (custom-option-builder
   [:custom-background-name]
   [:set-custom-background]))

(defn custom-background-option [language-map]
  (t/option-cfg
   {:name "Custom"
    :ui-fn custom-background-builder
    :order 1000
    :modifiers [(modifiers/deferred-background)]
    :selections [(skill-selection 2)
                 (t/selection-cfg
                  {:name "Tool / Language Proficiencies"
                   :tags #{:profs}
                   :options [(t/option-cfg
                              {:name "Two Tools"
                               :selections [(tool-selection 2)]})
                             (t/option-cfg
                              {:name "One Tool / One Language"
                               :selections [(tool-selection 1)
                                            (homebrew-language-selection language-map 1 1)]})
                             (t/option-cfg
                              {:name "Two Languages"
                               :selections [(homebrew-language-selection language-map 2 2)]})]})]}))

(defn hollow-selection []
  (t/selection-cfg
    {:name "Additional Options"
    :tags #{:race}
    :order 9
    :min 0
    :max 1
    :options [(t/option-cfg
               {:name "<none>"
                :key :none})
              (t/option-cfg
                {:name "Hollow"
                :modifiers [(modifiers/trait-cfg
                              {:name "Ageless"
                              :summary "You don't age and you're unaffected by any effects that would cause you to."})
                            (modifiers/trait-cfg
                              {:name "Cling to Life"
                              :summary "When you make a death save and roll 16 or higher, regain 1 hp."})
                            (modifiers/trait-cfg
                              {:name "Revenance"
                              :summary "You retain your creature type, but register as undead for any spells and other effects that require you to."})
                            (modifiers/action
                              {:name "Unsettling Presence"
                              :frequency units5e/long-rests-1
                              :summary "Unsettle a target within 15 ft. The target has disadvantage on the next saving throw it makes within the next minute. Constructs, undead, and creatures that can't be frightened are immune"})]})]}))

(defn race-option [spell-lists
                   spells-map
                   language-map
                   weapon-map
                   {:keys [name
                           icon
                           key
                           help
                           abilities
                           size
                           sizes
                           speed
                           darkvision
                           subraces
                           modifiers
                           selections
                           traits
                           source
                           languages
                           language-options
                           armor-proficiencies
                           weapon-proficiencies
                           custom-ability-scores
                           custom-ability-scores-2
                           profs
                           source
                           plugin?
                           edit-event]
                    :as race}]
  (let [key (or key (common/name-to-kw name))
        {:keys [armor weapon save skill-options weapon-proficiency-options tool-options tool language-options]} profs
        {skill-num :choose options :options} skill-options
        skill-kws (if (:any options)
                    (map :key skills/skills)
                    (map
                     clojure.core/key
                     (filter val options)))]
    (t/option-cfg
     {:name name
      :icon icon
      :key key
      :help help
      :edit-event edit-event
      :selections (concat
                   (if (seq sizes)
                    [(size-selection sizes)])
                   (if (seq skill-kws)
                     [(skill-selection skill-kws (or skill-num 1))])
                   (if (seq subraces)
                     [(subrace-selection race spell-lists spells-map language-map weapon-map plugin? source subraces [:race key])])
                   (if (seq language-options) [(language-selection language-map language-options)])
                   (if (seq weapon-proficiency-options)
                     [(weapon-proficiency-selection-2 weapon-map weapon-proficiency-options)])
                   (if custom-ability-scores
                     [(ability-increase-selection-2
                      {:ability-keys character/ability-keys
                        :num-increases 3
                        :max-ability 2})])
                   (if custom-ability-scores-2
                     [(ability-increase-selection-2
                      {:ability-keys character/ability-keys
                        :num-increases 4
                        :max-ability 2})])
                   [(hollow-selection)]
                   selections)
      :modifiers (concat
                  (if (not plugin?)
                    (remove
                     nil?
                     [(modifiers/race name)
                      (if size (modifiers/size size))
                      (if speed (modifiers/speed speed))]))
                  (if darkvision
                    (darkvision-modifiers darkvision))
                  (map
                   (fn [language]
                     (modifiers/language (common/name-to-kw language)))
                   languages)
                  (map
                   (fn [[k v]]
                     (modifiers/race-ability k v))
                   abilities)
                  modifiers
                  (tool-prof-modifiers (common/true-keys tool))
                  (traits-modifiers traits nil source)
                  (armor-prof-modifiers armor-proficiencies)
                  (weapon-prof-modifiers weapon-proficiencies)
                  (if source [(modifiers/used-resource source name)]))})))

(defn add-sources [source background]
  (-> background
      (assoc :source source)
      (update :traits (fn [traits] (map (fn [t] (assoc t :source source)) traits)))))

(def artisans-tools-choice-cfg
  {:name "Artisan's Tool"
   :options (zipmap (map :key equipment/artisans-tools) (repeat 1))})

(def gaming-set-choice-cfg
  {:name "Gaming Set"
   :options (zipmap (map :key equipment/gaming-sets) (repeat 1))})

(defn starting-equipment-option [equipment num]
  (t/option-cfg
   {:name (:name equipment)
    :key (:key equipment)
    :modifiers [(modifiers/equipment (:key equipment) num)]}))

(defn class-starting-equipment-entity-options [key items]
  (eh/starting-equipment-entity-options ::char-equip/class-starting-equipment? key items))

(defn tool-prof-selection-aux [tool num & [key prereq-fn]]
  (t/selection-cfg
   {:name (str "Tool Proficiency: " (:name tool))
    :key (if key (keyword (str (name key) "--" (common/name-to-kw (:name tool)))))
    :help (str "Select " (s/lower-case (:name tool)) " for which you are proficient.")
    :options (map
              (fn [{:keys [name key icon]}]
                (t/option-cfg
                 {:name name
                  :key key
                  :icon icon
                  :modifiers [(modifiers/tool-proficiency key)]}))
              (:values tool))
    :min num
    :max num
    :prereq-fn prereq-fn
    :tags #{:tool-profs :profs}}))

(defn tool-prof-selection [tool-options & [key prereq-fn]]
  (let [[first-key first-num] (-> tool-options first)
        first-option (equipment/tools-map first-key)]
    (if (and (= 1 (count tool-options))
             (seq (:values first-option)))
      (tool-prof-selection-aux first-option first-num key prereq-fn)
      (t/selection-cfg
       {:name "Tool Proficiencies"
        :key key
        :options (map
                  (fn [[k num]]
                    (let [tool (equipment/tools-map k)]
                      (if (:values tool)
                        (t/option-cfg
                         {:name (:name tool)
                          :selections [(tool-prof-selection-aux tool num key prereq-fn)]})
                        (t/option-cfg
                         {:name (:name tool)
                          :key (:key tool)
                          :icon (:icon tool)
                          :modifiers [(modifiers/tool-proficiency (:key tool))]}))))
                  tool-options)
        :prereq-fn prereq-fn
        :tags #{:profs :tool-profs}}))))

(defn first-class? [class-kw & [classes]]
  (fn [c] (= class-kw (first (or classes @(subscribe [::character/classes nil c]))))))

(defn new-starting-equipment-selection [class-kw {:keys [name options] :as cfg}]
  (t/selection-cfg
   (merge
    cfg
    {:name (str "Starting Equipment: " name)
     :tags #{:equipment :starting-equipment}
     :order 1
     :options (conj options
                    (t/option-cfg
                     {:name "<none>"
                      :key :none}))
     :prereq-fn (if class-kw (first-class? class-kw))})))

(defn simple-weapon-selection [num class-kw weapon-map]
  (new-starting-equipment-selection
   class-kw
   {:name "Simple Weapon"
    :tags #{:starting-equipment}
    :options (weapon-options (weapons/simple-weapons (vals weapon-map)))
    :min num
    :max num
    :prereq-fn (first-class? class-kw)}))

(defn simple-weapon-selection-numbered [n class-kw weapon-map]
  (new-starting-equipment-selection
   class-kw
   {:name (str "Simple Weapon " n)
    :tags #{:starting-equipment}
    :options (weapon-options (weapons/simple-weapons (vals weapon-map)))
    :min 1
    :max 1
    :prereq-fn (first-class? class-kw)}))

(defn weapon-option-2 [class-kw weapon-map [k num]]
  (case k
    :simple (t/option-cfg
             {:name "Any Simple Weapon"
              :selections [(simple-weapon-selection num class-kw weapon-map)]})
    :martial (t/option-cfg
              {:name "Any Martial Weapon"
               :selections [(new-starting-equipment-selection
                             class-kw
                             {:name "Martial Weapon"
                              :options (weapon-options (weapons/martial-weapons (vals weapon-map)))
                              :min num
                              :max num})]})
    (t/option-cfg
     {:name (-> k weapon-map :name (str (if (> num 1) (str " (" num ")") "")))
      :modifiers [(modifiers/weapon k num)]})))

(defn class-options [class-kw option-fn choices help]
  (map
   (fn [{:keys [name options]}]
     (new-starting-equipment-selection
      class-kw
      {:name name
       :help help
       :options (map
                 option-fn
                 options)}))
   choices))

(defn class-weapon-options [weapon-choices class-kw weapon-map]
  (class-options class-kw (partial weapon-option-2 class-kw weapon-map) weapon-choices "Select a weapon to begin your adventuring career with."))

(defn armor-option [[k num]]
  (t/option-cfg
     {:name (-> k armor/armor-map :name)
      :modifiers [(modifiers/armor k num)]}))

(defn class-armor-options [armor-choices class-kw]
  (class-options class-kw armor-option armor-choices "Select armor to begin your adventuring career with."))

(defn equipment-option [class-kw [k num]]
  (let [equipment (equipment/equipment-map k)]
    (if (:values equipment)
      (t/option-cfg
       {:name (:name equipment)
        :selections [(t/selection-cfg
                      {:name (:name equipment)
                       :tags #{:equipment :starting-equipment}
                       :options (map
                                 #(equipment-option class-kw %)
                                 (zipmap (map :key (:values equipment)) (repeat num)))
                       :prereq-fn (first-class? class-kw)})]})
      (t/option-cfg
       {:name (-> equipment :name (str (if (> num 1) (str " (" num ")") "")))
        :modifiers (if (:items equipment)
                     (map
                      (fn [[kw num]]
                        (modifiers/equipment kw num))
                      (:items equipment))
                     [(modifiers/equipment k num)])}))))

(defn class-equipment-options [equipment-choices class-kw]
  (class-options class-kw (partial equipment-option class-kw) equipment-choices "Select equipment to start your adventuring career with."))

(defn background-skills-cfg [background-nm skill-kws]
  {:modifiers (map
               (fn [skill-kw]
                 (modifiers/skill-proficiency skill-kw
                                              background-nm
                                              [(not (get ?skill-profs skill-kw))]))
               skill-kws)
   :selections (map
                (fn [skill-kw]
                  (skill-selection (map :key skills/skills)
                                   1
                                   0
                                   nil
                                   (fn [c]
                                     (let [skill-profs @(subscribe [::character/skill-profs nil c])
                                           skill-sources (get skill-profs skill-kw)
                                           passes? (and skill-sources
                                                        (not (skill-sources background-nm)))]
                                       passes?))))
                skill-kws)})

(defn background-option [language-map
                         weapon-map
                         {:keys [name
                                 help
                                 page
                                 profs
                                 selections
                                 modifiers
                                 weapon-choices
                                 weapons
                                 equipment
                                 custom-equipment
                                 equipment-choices
                                 armor
                                 armor-choices
                                 treasure
                                 custom-treasure
                                 traits
                                 source
                                 edit-event]
                          :as background}]
  (let [kw (common/name-to-kw name)
        {:keys [skill skill-options tool-options tool language-options]
         armor-profs :armor weapon-profs :weapon} profs
        {skill-num :choose options :options} skill-options
        skill-kws (if (:any options) (map :key skills/skills) (keys options))]
    (t/option-cfg
     (merge-with
      concat
      (background-skills-cfg name (keys skill))
      {:name name
       :key kw
       :help help
       :edit-event edit-event
       :page page
       :select-fn (fn [_ _]
                    (dispatch [:add-background-starting-equipment background]))
       :selections (concat
                    selections
                    (if (seq tool-options) [(tool-prof-selection tool-options)])
                    (class-weapon-options weapon-choices nil weapon-map)
                    (class-armor-options armor-choices nil)
                    (class-equipment-options equipment-choices nil)
                    (if (seq skill-kws) [(skill-selection skill-kws skill-num)])
                    (if (seq language-options) [(language-selection
                                                 language-map
                                                 language-options)]))
       :modifiers (concat
                   [(modifiers/background name)]
                   (traits-modifiers traits)
                   modifiers
                   (armor-prof-modifiers (keys armor-profs))
                   (weapon-prof-modifiers (keys weapon-profs))
                   (tool-prof-modifiers (keys tool)))}))))

(defn total-levels-prereq [level & [class-key]]
  (fn [c] (>= (if class-key
                (if (= class-key :warlock)
                  (max (@(subscribe [::character/class-level-fn nil c]) :warlock-cha)
                       (@(subscribe [::character/class-level-fn nil c]) :warlock-int))
                  (@(subscribe [::character/class-level-fn nil c]) class-key))
                @(subscribe [::character/total-levels nil c]))
              level)))

(defn total-levels-prereq-2 [level & [class-key]]
  (fn [c]
    (and c
         (character/class-level-fn c)
         level
         (>= (or (if class-key
                   ((character/class-level-fn c) class-key)
                   (character/total-levels c))
                 0)
             (or level 0)))))

(defn total-levels-option-prereq [level & [class-key]]
  (t/option-prereq
   (str "You must have at least " level " " (name class-key) " levels")
   (total-levels-prereq level class-key)))

(defn add-mod-total-levels-prereq [lvl cls modifier]
  (if (sequential? modifier)
    (map
     add-mod-total-levels-prereq lvl cls
     modifier)
    (update
     modifier
     ::mods/conditions
     conj
     (total-levels-prereq-2 lvl (:key cls)))))

(defn subclass-option [spell-lists
                       spells-map
                       language-map
                       cls
                       {:keys [name
                               source
                               edit-event
                               profs
                               selections
                               spellcasting
                               modifiers
                               level-modifiers
                               traits
                               prereqs
                               levels]
                        :as subcls}]
  (let [kw (common/name-to-kw name)
        {:keys [armor weapon save skill-options skill-expertise-options tool-options tool language-options]} profs
        {skill-num :choose options :options} skill-options
        {level-factor :level-factor} spellcasting
        skill-kws (if (:any options) (map :key skills/skills) (keys options))
        skill-expertise-kws (if (get-in skill-expertise-options [:options :any])
                              (map :key skills/skills)
                              (keys (:options skill-expertise-options)))
        armor-profs (keys armor)
        weapon-profs (keys weapon)
        tool-profs (keys tool)
        spellcasting-template (spellcasting-template
                               spell-lists
                               spells-map
                               (assoc
                                spellcasting
                                :class-key
                                (or (:spell-list spellcasting) kw))
                               subcls)
        spell-selections (mapcat
                          (fn [[lvl selections]]
                            (map
                             (fn [selection]
                               (assoc selection
                                      ::t/prereq-fn
                                      (fn [c] (let [total-levels @(subscribe [::character/total-levels nil c])]
                                                (>= lvl total-levels)))))
                             selections))
                          (:selections spellcasting-template))
        level-selections (mapcat
                          (fn [[lvl {selections :selections}]]
                            (map
                             (fn [selection]
                               (assoc
                                selection
                                ::t/prereq-fn
                                (total-levels-prereq lvl (:key cls))))
                             selections))
                          levels)
        level-modifiers (mapcat
                         (fn [[lvl {modifiers :modifiers}]]
                           (map
                            (partial add-mod-total-levels-prereq lvl cls)
                            modifiers))
                         levels)]
    (t/option-cfg
     {:name name
      :edit-event edit-event
      :prereqs prereqs
      :selections (map
                   (fn [selection]
                     (update selection ::t/tags sets/union #{(:key cls) kw}))
                   (concat
                    selections
                    level-selections
                    spell-selections
                    (if (seq tool-options) [(tool-prof-selection tool-options)])
                    (if (seq skill-kws) [(skill-selection skill-kws skill-num)])
                    (if (seq skill-expertise-kws)
                      [(skill-expertise-selection skill-expertise-kws (:choose skill-expertise-options))])
                    (if (seq language-options) [(language-selection language-map language-options)])))
      :modifiers (concat
                  modifiers
                  level-modifiers
                  [(modifiers/subclass (:key cls) kw)
                   (modifiers/subclass-name (:key cls) name)]
                  (if (:known-mode spellcasting)
                    [(modifiers/spells-known-mode name (:known-mode spellcasting))])
                  (armor-prof-modifiers armor-profs)
                  (weapon-prof-modifiers weapon-profs)
                  (tool-prof-modifiers tool-profs)
                  (traits-modifiers traits (:key cls))
                  (if level-factor [(modifiers/spell-slot-factor (:key cls) level-factor)])
                  (if source [(modifiers/used-resource source name)]))})))

(defn level-key [index]
  (keyword (str "level-" index)))

(defn level-name [index]
  (str "Level " index))

(defn subclass-level-option [{:keys [name
                                     levels] :as subcls}
                             kw
                             spellcasting-template
                             i]
  (let [selections (some-> levels (get i) :selections)]
    (t/option-cfg
     {:name (level-name i)
      :key (level-key i)
      :order i
      :selections (concat
                   selections
                   (some-> spellcasting-template :selections (get i)))
      :modifiers (some-> levels (get i) :modifiers)})))

(defn al-illegal-hit-points-mod [reason]
  (modifiers/al-illegal (str reason " The only legal option is 'Average'.")))

(defn hit-points-selection [die class-nm level]
  (t/selection-cfg
   {:name (str "Hit Points: " class-nm " " level)
    :key :hit-points
    :require-value? true
    :help "Select the method with which to determine this level's hit points."
    :tags #{:class}
    :options [{::t/name "Manual Entry"
               ::t/key :manual-entry
               ::t/help "This option allows you to manually type in the value for this level's hit points. Use this if you want to roll dice yourself or if you already have a character with known hit points for this level."
               ::t/modifiers [(modifiers/deferred-max-hit-points)
                              (al-illegal-hit-points-mod "Manual entry for hit points is not legal.")]}
              {::t/name (str "Roll (1D" die ")")
               ::t/key :roll
               ::t/help "This option rolls virtual dice for you and sets that value for this level's hit points. It could pay off with a high roll, but you might also roll a 1."
               ::t/modifiers [(modifiers/deferred-max-hit-points)
                              (al-illegal-hit-points-mod "Rolling for hit points is not legal.")]}
              (let [average (dice/die-mean-round-up die)]
                (t/option-cfg
                 {:name "Average"
                  :key :average
                  :help (str "This option just gives you the average value (" average ") for the die roll (1D" die ").")
                  :modifiers [(modifiers/max-hit-points average)]}))]}))

(defn custom-subclass-builder [path]
  (custom-option-builder
   [:custom-subclass-name path]
   [:set-custom-subclass path]))

#_(defn custom-subclass-spell-selection [ability-kw level]
  (t/selection-cfg
   {:name (if (zero? level)
            "Cantrips Known"
            (str (common/ordinal level) "-Level Spells Known"))
    :key (keyword (str "lvl-" level "-spells-known"))
    :min 0
    :max nil
    :multiselect? true
    :tags #{:spells}
    :order level
    :prereq-fn (fn [c] (or (zero? level)
                           (-> @(subscribe [::character/total-levels nil c])
                               (total-slots 3)
                               (get level)
                               pos?)))

    :options (sequence
              (comp
               (filter
                (fn [s]
                  (= level (:level s))))
               (map :key)
               (map (partial memoized-spell-option ability-kw "Custom")))
              spells/spells)}))

#_(defn custom-subclass-spellcasting-selection [cls-key]
  (t/selection-cfg
   {:name "Spellcasting Ability"
    :key :spellcasting-ability
    :min 0
    :max 1
    :tags #{:class}
    :options (conj
              (map
               (fn [{ability-kw :key name :name}]
                 (t/option-cfg
                  {:name name
                   :key ability-kw
                   :modifiers [(modifiers/spell-slot-factor cls-key 3)]
                   :selections (map
                                (fn [level]
                                  (custom-subclass-spell-selection ability-kw level))
                                (range 0 5))}))
               abilities)
              (t/option-cfg
               {:name "<none>"
                :key :none}))}))

(defn custom-subclass-option [spell-lists spells-map language-map weapon-map cls-key level-key subclass-selection-key spellcasting-class?]
  (let [path [:class cls-key :levels level-key subclass-selection-key]]
    (t/option-cfg
     {:name "Custom"
      :icon "beer-stein"
      :ui-fn #(custom-subclass-builder path)
      :help "Homebrew subclass. This allows you to use a subclass that is not on the list. This will allow unrestricted access to skill and tool proficiencies and feats."
      #_:prereqs #_[(t/option-prereq
                     nil
                     (fn [_] @(subscribe [:homebrew? path]))
                     true)]
      :order 1000
      :modifiers [(modifiers/deferred-subclass-name cls-key)
                  homebrew-al-illegal]
      :selections (let [selections
                        [homebrew-skill-prof-selection
                         homebrew-tool-prof-selection
                         (homebrew-feat-selection spell-lists spells-map language-map)
                         homebrew-armor-prof-selection
                         (homebrew-weapon-prof-selection weapon-map)]]
                    selections
                    #_(if spellcasting-class?
                      selections
                      (conj selections
                            (custom-subclass-spellcasting-selection cls-key))))})))

(defn level-option [spell-lists
                    spells-map
                    language-map
                    weapon-map
                    {:keys [name
                            plugin?
                            hit-die
                            profs
                            levels
                            traits
                            spellcasting
                            ability-increase-levels
                            subclass-title
                            subclass-help
                            subclass-level
                            subclasses
                            source] :or {subclass-level 1} :as cls}
                    kw
                    spellcasting-template
                    i]
  (let [ability-inc-set (set ability-increase-levels)
        level-kw (level-key i)]
    (t/option-cfg
     {:name (level-name i)
      :key level-kw
      :order i
      :selections (map
                   (fn [selection]
                     (update selection ::t/tags sets/union #{:level level-kw}))
                   (concat
                    (some-> levels (get i) :selections)
                    (some-> spellcasting-template :selections (get i))
                    (if (= i subclass-level)
                      (let [subclass-selection-key (common/name-to-kw subclass-title)]
                        [(t/selection-cfg
                          {:name (or subclass-title (str name " Archetype"))
                           :adder-key-fn (fn [_] [:subclass])
                           :key subclass-selection-key
                           :help subclass-help
                           :tags #{:subclass}
                           :order 2
                           :options (conj
                                     (map
                                      #(subclass-option spell-lists spells-map language-map (assoc cls :key kw) %)
                                      (if source (map (fn [sc] (assoc sc :source source)) subclasses) subclasses))
                                     (custom-subclass-option spell-lists spells-map language-map weapon-map kw level-kw subclass-selection-key (some? spellcasting)))})]))
                    (if (and (not plugin?) (ability-inc-set i))
                      [(ability-score-improvement-selection spell-lists spells-map language-map name i)])
                    (if (not plugin?)
                      [(assoc
                        (hit-points-selection hit-die name i)
                        ::t/prereq-fn
                        (fn [c] (or (not (= kw (first @(subscribe [::character/classes nil c]))))
                                    (> i 1))))])))
      :modifiers (concat
                  (some-> levels (get i) :modifiers)
                  (traits-modifiers
                   (filter
                    (fn [{level :level :or {level 1}}]
                      (= level i))
                    traits)
                   kw)
                  (if (and (not plugin?)
                           (= i 1))
                    [(mods/cum-sum-mod
                      ?hit-point-level-increases
                      hit-die
                      nil
                      nil
                      [(= kw (first ?classes))])])
                  (if (not plugin?)
                    [(modifiers/level kw name i hit-die)]))})))



(defn class-skill-selection [{skill-num :choose options :options skill-select-order :order} key prereq-fn]
  (let [skill-kws (if (:any options) (map :key skills/skills) (keys options))]
    (skill-selection skill-kws skill-num skill-select-order key prereq-fn)))

(defn class-help-field [name value]
  [:div.m-t-5
    [:span.f-w-b (str name ":")]
   [:span.m-l-10 value]])


(defn class-help [hd saves weapon-profs armor-profs]
  [:div
   (class-help-field "Hit Die" (str "d" hd))
   (class-help-field "Saving Throw Proficiencies" (s/join ", " (map (comp s/upper-case name) saves)))
   (class-help-field "Weapon Proficiencies" (s/join ", " (map (comp name key) weapon-profs)))
   (class-help-field "Armor Proficiencies" (s/join ", " (map (comp name key) armor-profs)))])

(defn class-option [spell-lists
                    spells-map
                    plugin-subclasses-map
                    language-map
                    weapon-map
                    {:keys [name
                            key
                            help
                            hit-die
                            plugin?
                            profs
                            levels
                            ability-increase-levels
                            subclass-title
                            subclass-level
                            subclasses
                            selections
                            modifiers
                            source
                            weapon-choices
                            weapons
                            equipment
                            equipment-choices
                            armor
                            armor-choices
                            spellcasting
                            multiclass-prereqs]
                     :as cls}]
  (let [merged-class (update cls :subclasses #(into (sorted-set-by (fn [x y] (compare (:name x) (:name y)))) (concat (reverse (get plugin-subclasses-map key)) %)))
        kw (or key (common/name-to-kw name))
        {:keys [save skill-options skill-expertise-options multiclass-skill-options tool-options multiclass-tool-options tool]
         armor-profs :armor weapon-profs :weapon} profs
        {level-factor :level-factor} spellcasting
        skill-expertise-kws (if (get-in skill-expertise-options [:options :any])
                              (map :key skills/skills)
                              (keys (:options skill-expertise-options)))
        save-profs (keys save)
        spellcasting-template (spellcasting-template
                               spell-lists
                               spells-map
                               (assoc spellcasting :class-key kw)
                               merged-class)
        first-class? (fn [c] (let [first-class (first @(subscribe [::character/classes nil c]))]
                               (= kw first-class)))]
    (t/option-cfg
     {:name name
      :key kw
      :help [:div.p-t-5.p-l-10.p-r-10
             (class-help hit-die save-profs weapon-profs armor-profs)
             [:div.m-t-10 help]]
      :prereqs multiclass-prereqs
      :selections (map
                   (fn [selection]
                     (update selection ::t/tags sets/union #{kw}))
                   (concat
                    selections
                    (if (seq tool-options)
                      [(tool-prof-selection tool-options :tool-selection first-class?)])
                    (if (seq multiclass-tool-options)
                      [(tool-prof-selection multiclass-tool-options :multiclass-tool-selection (fn [c] (not= kw (first (:classes c)))))])
                    (if weapon-choices (class-weapon-options weapon-choices kw weapon-map))
                    (if armor-choices (class-armor-options armor-choices kw))
                    (if equipment-choices (class-equipment-options equipment-choices kw))
                    (if skill-options
                      [(class-skill-selection skill-options :skill-proficiency first-class?)])
                    (if (seq skill-expertise-kws)
                      [(skill-expertise-selection skill-expertise-kws (:choose skill-expertise-options))])
                    (if multiclass-skill-options
                      [(class-skill-selection multiclass-skill-options :multiclass-skill-proficiency (complement first-class?))])
                    [(t/selection-cfg
                      {:name (str name " Levels")
                       :key :levels
                       :help "These are your levels in the containing class. You can add levels by clicking the 'Add Levels' button below."
                       :new-item-fn (fn [selection options current-values]
                                      {::entity/key (-> current-values count inc level-key)})
                       :tags #{kw}
                       :options (map
                                 (partial level-option spell-lists spells-map language-map weapon-map merged-class kw spellcasting-template)
                                 (range 1 21))
                       :min 1
                       :sequential? true
                       :multiselect? true
                       :max nil})]))
      :associated-options (remove
                           nil?
                           [(class-starting-equipment-entity-options :weapons weapons)
                            (class-starting-equipment-entity-options :armor armor)
                            (class-starting-equipment-entity-options :equipment equipment)])
      :modifiers (concat
                  modifiers
                  (if (:prepares-spells? spellcasting)
                    [(mods/map-mod ?prepares-spells name true)])
                  (if (= :all (:known-mode spellcasting))
                    (let [spell-list (spell-lists kw)]
                      (mapcat
                       (fn [[lvl spell-keys]]
                         (map
                          (fn [spell-key]
                            (modifiers/spells-known-cfg lvl
                                                        {:class-key kw
                                                         :key spell-key
                                                         :class name
                                                         :ability (:ability spellcasting)}
                                                        1
                                                        [(let [slots (?class-spell-slots kw)]
                                                           (slots lvl))
                                                         (let [spell (spells-map spell-key)]
                                                           (using-source? ?option-sources (:source spell)))]))
                          spell-keys))
                       spell-list)))
                  (if armor-profs (armor-prof-modifiers armor-profs kw))
                  (if weapon-profs (weapon-prof-modifiers weapon-profs kw))
                  (if tool (tool-prof-modifiers tool kw))
                  (if level-factor [(modifiers/spell-slot-factor kw level-factor)])
                  (if (and source (not plugin?))
                    [(modifiers/used-resource source name)])
                  (if (:known-mode spellcasting)
                    [(modifiers/spells-known-mode name (:known-mode spellcasting))])
                  (remove
                   nil?
                   [(modifiers/cls kw)
                    (if save-profs (apply modifiers/saving-throws kw save-profs))]))})))

#_(defn source-url [source]
  (some-> source disp/sources :url))

(def ranger-base-cfg
  {:name "Ranger"
   :subclass-level 3
   :subclass-title "Ranger Archetype"})

(defn background-selection [cfg]
  (t/selection-cfg
   (merge
    {:name "Background"
     :tags #{:background}}
    cfg)))

(defn class-selection [cfg]
  (t/selection-cfg
   (merge
    {:name "Class"
     :order 0
     :tags #{:class}
     :multiselect? true
     :min 1
     :max nil}
    cfg)))

(defn race-selection [cfg]
  (t/selection-cfg
   (merge
    {:name "Race"
     :order 0
     :help "Race determines your appearance and helps shape your culture and background. It also affects your ability scores, size, speed, languages and many other crucial inherent traits."
     :tags #{:race}}
    cfg)))

(def ranger-skills {:animal-handling true :athletics true :insight true :investigation true :nature true :perception true :stealth true :survival true})

(defn evasion [level page]
  {:name "Evasion"
   :page page
   :level level
   :summary "your instinctive agility lets you dodge out of the way of certain area effects, such as a blue dragon's lightning breath or a fireball spell. When you are subjected to an effect that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on the saving throw, and only half damage if you fail"})

(defn uncanny-dodge-modifier [page]
  (modifiers/reaction
   {:name "Uncanny Dodge"
    :page page
    :summary "when an attacker that you can see hits you with an attack, you can use your reaction to halve the attack's damage against you"}))

(defn divine-strike [damage-desc page & [source]]
  (modifiers/dependent-trait
   {:level 8
    :name "Divine Strike"
    :page page
    :source source
    :frequency units5e/turns-1
    :summary (str "you gain the ability to infuse your weapon strikes with divine energy. Once on each of your turns when you hit a creature with a weapon attack, you can cause the attack to deal an extra "
                  (if (>= (?class-level :cleric) 14) 2 1)
                  "d8 "
                  damage-desc
                  " damage to the target")}))

(defn blessed-strikes []
  (modifiers/dependent-trait
   {:level 8
    :name "Blessed Strikes"
    :frequency units5e/rounds-1
    :summary "you are blessed with divine might in battle. When a creature takes damage from one of your cantrips or weapon attacks, you can also deal 1d8 radiant damage to that creature. Once you deal this damage, you can't use this feature again until the start of your next turn"}))

(defn divine-strike-selection [damage-desc page & [source]]
  (t/selection-cfg
   {:level 8
    :name "Cleric Level 8 Feature"
    :tags #{:class}
    :options [(t/option-cfg
               {:name "Divine Strike"
                :modifiers [(divine-strike damage-desc page source)]})
              (t/option-cfg
               {:name "Blessed Strikes"
                :modifiers [(blessed-strikes)]})]}))

(defn potent-spellcasting-selection [page & [source]]
  (t/selection-cfg
   {:level 8
    :name "Cleric Level 8 Feature"
    :tags #{:class}
    :options [(t/option-cfg
               {:name "Potent Spellcasting"
                :modifiers [(potent-spellcasting page source)]})
              (t/option-cfg
               {:name "Blessed Strikes"
                :modifiers [(blessed-strikes)]})]}))

(defn favored-enemy-types [language-map]
  {:aberration [:deep-speech :undercommon :grell :slaad]
   :beast [:giant-elk :giant-eagle :giant-owl]
   :celestial (keys language-map)
   :construct [:modron]
   :dragon [:aquan :draconic :sylvan]
   :elemental [:auran :terran :ignan :aquan]
   :fey [:draconic :elvish :sylvan :abyssal :infernal :primoridial :aquan :giant]
   :fiend (keys language-map)
   :giant [:giant :orc :undercommon]
   :monstrosity [:draconic :sylvan :elvish :hook-horror :abyssal :celestial :infernal :primordial :aquan :sphynx :umber-hulk :yeti :winter-wolf :goblin :worg]
   :ooze []
   :plant [:druidic :elvish :sylvan]
   :undead (keys language-map)})

(def humanoid-enemies
  {:bugbear [:goblin]
   :bullywug [:bullywug]
   :githyanki [:gith]
   :gitzerai [:gith]
   :gnoll [:gnoll :abyssal]
   :goblin [:goblin]
   :grimlock [:undercommon]
   :hobgoblin [:goblin]
   :human [:common]
   :kobold [:draconic]
   :koa-toa [:undercommon]
   :lizardfolk [:draconic :abyssal]
   :merfolk [:aquan]
   :orc [:orc]
   :thri-kreen [:thri-kreen]
   :troglodyte [:troglodyte]
   :yuan-ti-pureblood {:name "Yuan-Ti Pureblood"
                       :languages [:abyssal :draconic]}})

(defn druid-cantrip-selection [spell-lists spells-map class-nm]
  (t/selection-cfg
   {:name "Druid Cantrip"
    :tags #{:spells}
    :options (spell-options spells-map (get-in spell-lists [:druid 0]) ::character/wis class-nm)}))

(defn eldritch-invocation-selection [cfg class-kw]
  (t/selection-cfg
   (merge
    {:name "Eldritch Invocations"
     :multiselect? true
     :ref [:class class-kw :eldritch-invocations]
     :tags #{:spells}}
    cfg)))

(defn artificer-infusion-selection [cfg]
  (t/selection-cfg
   (merge
    {:name "Artificer Infusions"
     :multiselect? true
     :ref [:class :artificer :artificer-infusions]
     :tags #{:spells}}
    cfg)))

(defn infusion-item-field [name value attunement?]
  [:div.m-b-2
   [:span.f-w-b (str name ": ")]
   [:span.f-w-n value]
   (when attunement?
     [:span.f-w-i " (requires attunement)"])])

(defn infusion-help [{:keys [level attunement? item description summary]}]
  [:div
   [:div.m-b-5
    (spell-field "Level" level)
    (infusion-item-field "Item" item attunement?)]
   [:div.f-w-n (if (or description summary)
                 (doall
                  (map-indexed
                   (fn [i p]
                     ^{:key i} [:p.m-t-5 p])
                   (s/split (or description summary) #"\n"))))]])

(defn infusion-option [infusions-map key]
  (let [{:keys [name level edit-event] :as infusion} (infusions-map key)]
    (t/option-cfg
     {:name (str level " - " name)
      :key key
      :edit-event edit-event
      :help (infusion-help infusion)
      :prereqs [(total-levels-option-prereq level :artificer)]
      :modifiers [(modifiers/infusions-known key)]})))

(def memoized-infusion-option (memoize infusion-option))

(defn infusion-options [infusions-map]
  (map
   #(memoized-infusion-option infusions-map %)
   (sort (keys infusions-map))))

(defn infusion-selection [infusions-map num]
  (let []
     (t/selection-cfg
      {:name "Artificer Infusions"
       :key :artificer-infusions
       :ref [:class :artificer :artificer-infusions]
       :order 2
       :multiselect? true
       :options (infusion-options infusions-map)
       :min num
       :max num
       :tags #{:spells}})))


(def pact-of-the-tome-name "Pact Boon: Pact of the Tome")
(def pact-of-the-chain-name "Pact Boon: Pact of the Chain")
(def pact-of-the-blade-name "Pact Boon: Pact of the Blade")
(def pact-of-the-talisman-name "Pact Boon: Pact of the Talisman")

(defn has-trait-with-name-prereq [name]
  (t/option-prereq
   (str "You must have " name)
   (fn [c] (some #(= name (:name %)) @(subscribe [::character/traits nil c])))))

(def pact-of-the-tome-prereq
  (has-trait-with-name-prereq pact-of-the-tome-name))

(def pact-of-the-blade-prereq
  (has-trait-with-name-prereq pact-of-the-blade-name))

(def pact-of-the-chain-prereq
  (has-trait-with-name-prereq pact-of-the-chain-name))

(def pact-of-the-talisman-prereq
  (has-trait-with-name-prereq pact-of-the-talisman-name))

(def has-eldritch-blast-prereq
  (t/option-prereq
   "You must know the edritch blast cantrip"
   (fn [c]
     (or (get-in @(subscribe [::character/spells-known nil c])
             [0 ["Warlock (Int)" :eldritch-blast]])
         (get-in @(subscribe [::character/spells-known nil c])
             [0 ["Warlock" :eldritch-blast]])))))

(defn deep-gnome-option-cfg [key source page]
  {:name "Gnome"
   :plugin? true
   :subraces
   [{:name (str "Deep Gnome (" (s/upper-case (name source)) ")")
     :key key
     :abilities {::character/dex 1}
     :modifiers [(modifiers/darkvision 120)
                 (modifiers/language :undercommon)]
     :source source
     :traits [{:name "Stone Camouflage"
               :source source
               :page page
               :summary "Advantage on hide checks in rocky terrain"}]}]})

(defmacro eldritch-invocation-option [{:keys [name summary source page prereqs modifiers trait-type frequency range]}]
  `(t/option-cfg
    {:name ~name
     :prereqs ~prereqs
     :modifiers (conj
                 ~modifiers
                 (~(case trait-type
                    :action `modifiers/action
                    :bonus-action `modifiers/bonus-action
                    :reaction `modifiers/reaction
                    `modifiers/dependent-trait)
                  {:name (str "Eldritch Invocation: " ~name)
                   :page ~page
                   :source ~source
                   :summary ~summary
                   :frequency ~frequency
                   :range ~range}))}))

#_(def deep-gnome-prereq
  (t/option-prereq
   "Deep Gnome only"
   (fn [c] (let [subrace @(subscribe [::character/subrace nil c])]
             (or (= "Deep Gnome (EE)" subrace)
                 (= "Deep Gnome (SCAG)" subrace))))))

#_(defn svirfneblin-magic-feat [source page]
  (feat-option
   {:name (str "Svirfneblin Magic (" (s/upper-case (name source)) ")")
    :page page
    :source source
    :summary "Can cast 'nondetection', 'blindness/deafness', 'blur', and 'disguise self'"
    :prereqs [deep-gnome-prereq]
    :modifiers [(modifiers/spells-known 3 :nondetection ::character/cha "Deep Gnome" 0 "at will")
                (modifiers/spells-known 2 :blindness-deafness ::character/cha "Deep Gnome" 0 "once per long rest")
                (modifiers/spells-known 2 :blur ::character/cha "Deep Gnome" 0 "once per long rest")
                (modifiers/spells-known 1 :disguise-self ::character/cha "Deep Gnome" 0 "once per long rest")]}))


(defn feat-prereqs [prereqs path-prereqs]
  (concat
   (map
    (fn [prereq]
      (cond
        ((into #{} character/ability-keys) prereq)
        (ability-prereq prereq 13)

        (= :spellcasting prereq)
        can-cast-spell-prereq

        :else
        (armor-prereq prereq)))
    prereqs)
   (let [race-prereqs (:race path-prereqs)
         race-keys (sequence
                    (comp
                     (filter
                      val)
                     (map
                      key))
                    race-prereqs)]
     (if (seq race-keys)
       (let [race-map @(subscribe [::races/race-map])
             race-names (map (comp :name race-map) race-keys)]
         [(race-prereq race-names)])))))

(def filter-true (filter val))

(defn magic-initiate-selection [spells-map spell-lists]
  (t/selection-cfg
   {:name "Spell Class"
    :order 0
    :tags #{:spells}
    :options [(magic-initiate-option spells-map :bard "Bard" ::character/cha spell-lists)
              (magic-initiate-option spells-map :cleric "Cleric" ::character/wis spell-lists)
              (magic-initiate-option spells-map :druid "Druid" ::character/wis spell-lists)
              (magic-initiate-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
              (magic-initiate-option spells-map :warlock "Warlock" ::character/cha spell-lists)
              (magic-initiate-option spells-map :wizard "Wizard" ::character/int spell-lists)]}))

(defn ritual-caster-selection [spells-map spell-lists]
  (t/selection-cfg
   {:name "Ritual Caster: Spell Class"
    :tags #{:spells}
    :order 6
    :options [(ritual-caster-option spells-map :bard "Bard" ::character/cha spell-lists)
              (ritual-caster-option spells-map :cleric "Cleric" ::character/wis spell-lists)
              (ritual-caster-option spells-map :druid "Druid" ::character/wis spell-lists)
              (ritual-caster-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
              (ritual-caster-option spells-map :warlock "Warlock" ::character/cha spell-lists)
              (ritual-caster-option spells-map :wizard "Wizard" ::character/int spell-lists)]}))

(defn spell-sniper-selection [spells-map spell-lists]
  (t/selection-cfg
   {:name "Spell Sniper: Spell Class"
    :tags #{:spells}
    :options [(spell-sniper-option spells-map :bard "Bard" ::character/cha spell-lists)
              (spell-sniper-option spells-map :cleric "Cleric" ::character/wis spell-lists)
              (spell-sniper-option spells-map :druid "Druid" ::character/wis spell-lists)
              (spell-sniper-option spells-map :sorcerer "Sorcerer" ::character/cha spell-lists)
              (spell-sniper-option spells-map :warlock "Warlock" ::character/cha spell-lists)
              (spell-sniper-option spells-map :wizard "Wizard" ::character/int spell-lists)]}))

(defn make-feat-selections [language-map spells-map spell-lists proficiency-weapons k v]
  (if v
    (case k
      :weapon-prof-choice [(weapon-proficiency-selection v proficiency-weapons)]
      :language-choice [(language-selection-aux (vals language-map) v)]
      :skill-tool-choice (map
                          (fn [i]
                            (skilled-selection (str "Skill/Tool " (inc i))))
                          (range v))
      :ritual-casting [(ritual-caster-selection spells-map
                                                spell-lists)]
      :magic-novice [(magic-initiate-selection spells-map
                                               spell-lists)]
      :attack-spell [(spell-sniper-selection spells-map
                                             spell-lists)]
      nil)))

(defn collect-map-modifiers [m modifier-fn]
  (sequence
   (comp
    filter-true
    (map
     (fn [[k]]
       (modifier-fn k))))
   m))

(defn make-feat-modifiers [k v option-key]
  (if v
    (case k
      :initiative [(modifiers/initiative v)]
      :two-weapon-ac-1 [dual-wield-ac-mod]
      :two-weapon-any-one-handed [dual-wield-weapon-mod]
      :max-hp-bonus [(mods/modifier ?hit-point-level-bonus (+ v ?hit-point-level-bonus))]
      :passive-investigation-5 [(modifiers/passive-investigation 5)]
      :passive-perception-5 [(modifiers/passive-perception 5)]
      :medium-armor-max-dex-3 [medium-armor-master-max-bonus]
      :medium-armor-stealth [medium-armor-master-stealth]
      :speed [(modifiers/speed v)]
      :flying-speed [(modifiers/flying-speed-override v)]
      :flying-speed-equals-walking-speed [(modifiers/flying-speed-equal-to-walking)]
      :swimming-speed [(modifiers/swimming-speed-override v)]
      :saving-throw-advantage-traps [(modifiers/saving-throw-advantage [:traps])]
      :lizardfolk-ac (if v
                       [(mods/modifier ?natural-ac-bonus 3)
                        (mods/modifier ?armor-class-with-armor
                                      (fn [armor & [shield]]
                                        (max (+ ?base-armor-class
                                                (if shield (?shield-ac-bonus shield) 0))
                                             (?armor-class-with-armor armor shield))))])
      :tortle-ac (if v
                   [(mods/modifier ?natural-ac-bonus 7)
                    (mods/modifier ?armor-class-with-armor
                                  (fn [armor & [shield]]
                                    (+ 17
                                       (if shield (?shield-ac-bonus shield) 0))))])
      :language (collect-map-modifiers
                 v
                 #(modifiers/language %))
      :saving-throw-advantage (collect-map-modifiers
                               v
                               #(modifiers/saving-throw-advantage [%]))
      :skill-prof (collect-map-modifiers
                   v
                   #(modifiers/skill-proficiency %))
      :tool-prof-or-expertise (collect-map-modifiers
                                v
                                #(tool-prof-or-expertise % option-key))
      :skill-prof-or-expertise (collect-map-modifiers
                                v
                                #(skill-prof-or-expertise % option-key))
      :armor-prof (collect-map-modifiers
                   v
                   #(modifiers/armor-proficiency %))
      :weapon-prof (collect-map-modifiers
                   v
                   #(modifiers/weapon-proficiency %))
      :damage-resistance (collect-map-modifiers
                          v
                          #(modifiers/damage-resistance %))
      :damage-immunity (collect-map-modifiers
                        v
                        #(modifiers/damage-immunity %))
      nil)))

(defn plugin-modifiers [props option-key]
  (reduce
   (fn [mods [k v]]
     (let [feat-mods (make-feat-modifiers k v option-key)]
       (if feat-mods
         (concat mods feat-mods)
         mods)))
   []
   props))

(defn feat-modifiers [key name description props ability-increases]
  (let [without-saves (sets/intersection ability-increases
                                         (into #{} character/ability-keys))]
    (concat
     (plugin-modifiers props key)
     (if (= 1 (count without-saves))
       (let [ability-kw (first without-saves)
             ability-mod (modifiers/ability ability-kw 1)]
         (if (:saves? ability-increases)
           [ability-mod
            (modifiers/saving-throws nil ability-kw)]
           [ability-mod]))
       [])
     [(modifiers/trait-cfg
       {:name name
        :description description})])))

(defn feat-selections [language-map spells-map spell-lists proficiency-weapons props ability-increases]
  (let [without-saves (sets/intersection ability-increases
                                         (into #{} character/ability-keys))]
    (reduce
     (fn [selections [k v]]
       (let [feat-selections (make-feat-selections language-map spells-map spell-lists proficiency-weapons k v)]
         (if feat-selections
           (concat selections feat-selections)
           selections)))
     (if (< 1 (count without-saves))
       [(if (:saves? ability-increases)
          (ability-increase-selection
           without-saves
           1
           false
           [(fn [k] (modifiers/saving-throws nil k))])
          (ability-increase-selection
           without-saves
           1
           false))]
       [])
     props)))


(defn feat-option-from-cfg [language-map
                            spells-map
                            spell-lists
                            custom-and-standard-weapons
                            {:keys [name
                                    key
                                    icon
                                    description
                                    prereqs
                                    path-prereqs
                                    props
                                    ability-increases
                                    edit-event]}]
  (let [feat-mods (feat-modifiers key
                                  name
                                  description
                                  props
                                  ability-increases)
        feat-selections (feat-selections language-map
                                         spells-map
                                         spell-lists
                                         custom-and-standard-weapons
                                         props
                                         ability-increases)]
    (t/option-cfg
     {:name name
      :key key
      :icon icon
      :edit-event edit-event
      :modifiers feat-mods
      :selections feat-selections
      :summary description
      :prereqs (feat-prereqs prereqs path-prereqs)})))

(def draconic-ancestries
  [{:name "Black"
    :breath-weapon {:damage-type :acid
                    :area-type :line
                    :line-width 5
                    :line-length 30
                    :save ::character/dex}}
   {:name "Blue"
    :breath-weapon {:damage-type :lightning
                    :area-type :line
                    :line-width 5
                    :line-length 30
                    :save ::character/dex}}
   {:name "Brass"
    :breath-weapon {:damage-type :fire
                    :area-type :line
                    :line-width 5
                    :line-length 30
                    :save ::character/dex}}
   {:name "Bronze"
    :breath-weapon {:damage-type :lightning
                    :area-type :line
                    :line-width 5
                    :line-length 30
                    :save ::character/dex}}
   {:name "Copper"
    :breath-weapon {:damage-type :acid
                    :area-type :line
                    :line-width 5
                    :line-length 30
                    :save ::character/dex}}
   {:name "Gold"
    :breath-weapon {:damage-type :fire
                    :area-type :cone
                    :length 15
                    :save ::character/dex}}
   {:name "Green"
    :breath-weapon {:damage-type :poison
                    :area-type :cone
                    :length 15
                    :save ::character/con}}
   {:name "Red"
    :breath-weapon {:damage-type :fire
                    :area-type :cone
                    :length 15
                    :save ::character/dex}}
   {:name "Silver"
    :breath-weapon {:damage-type :cold
                    :area-type :cone
                    :length 15
                    :save ::character/con}}
   {:name "White"
    :breath-weapon {:damage-type :cold
                    :area-type :cone
                    :length 15
                    :save ::character/con}}])

(defn artificer-infusion-options [spell-lists spells-map]
   [(t/option-cfg
     {:name "Arcane Propulsion Armor"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Arcane Propulsion Armor"
                    :summary (str "Item: A suit of armor (requires attunement)"
                    "\nThe wearer of this armor gains these benefits:"
                    "\n\u2022 The wearer's walking speed increases by 5 feet."
                    "\n\u2022 The armor includes gauntlets, each of which is a magic melee weapon that can be wielded only when the hand is holding nothing. The wearer is proficient with the gauntlets, and each one deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the gauntlet detaches and flies at the attack's target, then immediately returns to the wearer and reattaches."
                    "\n\u2022 The armor can't be removed against the wearer's will."
                    "\n\u2022 If the wearer is missing any limbs, the armor replaces those limbs - hands, arms, feet, legs, or similar appendages. The replacements function identically to the body parts they replace.")})]})
    (t/option-cfg
      {:name "Armor of Magical Strength"
       :modifiers [(modifiers/trait-cfg
                    {:name "Infusion: Armor of Magical Strength"
                     :summary (str "Item: A suit of armor (requires attunement)"
                     "\nThis armor has 6 charges. The wearer can expend the armor's charges in the following ways:"
                     "\n\u2022 When the wearer makes a Strength check or a Strength saving throw, it can expend 1 charge to add a bonus to the roll equal to its Intelligence modifier."
                     "\n\u2022 If the creature would be knocked prone, it can use its reaction to expend 1 charge to avoid being knocked prone."
                     "\nThe armor regains 1d6 expended charges daily at dawn.")})]})
    (t/option-cfg
     {:name "Boots of the Winding Path"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Boots of the Winding Path"
                    :summary (str "Item: A pair of boots (requires attunement)"
                    "\nWhile wearing these boots, a creature can teleport up to 15 feet as a bonus action to an unoccupied space the creature can see. The creature must have occupied that space at some point during the current turn.")})]})
    (t/option-cfg
     {:name "Enhanced Arcane Focus"
      :modifiers [(modifiers/dependent-trait
                   {:name "Infusion: Enhanced Arcane Focus"
                    :summary (str "Item: A rod, staff or wand (requires attunement)"
                    "\nWhile holding this item, a creature gains +" (if (>= (?class-level :artificer) 10) 2 1) " bonus to spell attack rolls. In addition, the creature ignores half cover when making a spell attack.")})]})
    (t/option-cfg
     {:name "Enhanced Defense"
      :modifiers [(modifiers/dependent-trait
                   {:name "Infusion: Enhanced Defense"
                    :summary (str "Item: A suit of armor or a shield"
                                  "\nA creature gains a +" (if (>= (?class-level :artificer) 10) 2 1) " bonus to Armor Class while wearing (armor) or wielding (shield) the infused item.")})]})
    (t/option-cfg
     {:name "Enhanced Weapon"
      :modifiers [(modifiers/dependent-trait
                   {:name "Infusion: Enhanced Weapon"
                    :summary (str "Item: A simple or martial weapon"
                                  "\nThis magic weapon grants a +" (if (>= (?class-level :artificer) 10) 2 1) " bonus to attack and damage rolls made with it.")})]})
    (t/option-cfg
     {:name "Helm of Awareness"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Helm of Awareness"
                    :summary (str "Item: A helmet (requires attunement)"
                                  "\nWhile wearing this helmet, a creature has advantage on initiative rolls. In addition, the wearer can’t be surprised, provided it isn’t incapacitated.")})]})
    (t/option-cfg
     {:name "Mind Sharpener"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Mind Sharpener"
                    :summary (str "Item: A suit of armor or robes"
                                  "\nThe infused item can send a jolt to the wearer to refocus their mind. The item has 4 charges. When the wearer fails a Constitution saving throw to maintain concentration on a spell, the wearer can use its reaction to expend 1 of the item's charges to succeed instead. The item regains 1d4 expended charges daily at dawn.")})]})
    (t/option-cfg
     {:name "Radiant Weapon"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Radiant Weapon"
                    :summary (str "Item: A simple or martial weapon (requires attunement)"
                                  "\nThis magic weapon grants a +1 bonus to attack and damage rolls made with it. While holding it, the wielder can take a bonus action to cause it to shed bright light in a 30-foot radius and dim light for an additional 30 feet. The wielder can extinguish the light as a bonus action."
                                  "\n\nThe weapon has 4 charges. As a reaction immediately after being hit by an attack, the wielder can expend 1 charge and cause the attacker to be blinded until the end of the attacker's next turn, unless the attacker succeeds on a Constitution saving throw against your spell save DC. The weapon regains 1d4 expended charges daily at dawn.")})]})
    (t/option-cfg
     {:name "Repeating Shot"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Repeating Shot"
                    :summary (str "Item: A simple or martial weapon with the ammunition property (requires attunement)"
                                  "\nThis magic weapon grants a +1 bonus to attack and damage rolls made with it when it's used to make a ranged attack, and it ignores the loading property if it has it."
                                  "\n\nIf the weapon lacks ammunition, it produces its own, automatically creating one piece of magic ammunition when the wielder makes a ranged attack with it. The ammunition created by the weapon vanishes the instant after it hits or misses a target.")})]})
    (t/option-cfg
     {:name "Repulsion Shield"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Repulsion Shield"
                    :summary (str "Item: A shield (requires attunement)"
                                  "\nA creature gains a +1 bonus to Armor Class while wielding this shield."
                                  "\n\nThe shield has 4 charges. While holding it, the wielder can use a reaction immediately after being hit by a melee attack to expend 1 of the shield's charges and push the attacker up to 15 feet away. The shield regains 1d4 expended charges daily at dawn.")})]})
    (t/option-cfg
     {:name "Resistant Armor"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Resistant Armor"
                    :summary (str "Item: A suit of armor (requires attunement)"
                                  "\nWhile wearing this armor, a creature has resistance to one of the following damage types, which you choose when you infuse the item: acid, cold, fire, force, lightning, necrotic, poison, psychic, radiant, or thunder.")})]})
    (t/option-cfg
     {:name "Returning Weapon"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Returning Weapon"
                    :summary (str "Item: A simple or martial weapon with the thrown property"
                                  "\nThis magic weapon grants a +1 bonus to attack and damage rolls made with it, and it returns to the wielder’s hand immediately after it is used to make a ranged attack.")})]})
    (t/option-cfg
     {:name "Spell-Refueling Ring"
      :modifiers [(modifiers/trait-cfg
                   {:name "Infusion: Spell-Refueling Ring"
                    :summary (str "Item: A ring (requires attunement)"
                                  "\nWhile wearing this ring, the creature can recover one expended spell slot as an action. The recovered slot can be of 3rd level or lower. Once used, the ring can't be used again until the next dawn.")})]})
    
    ]
  )
