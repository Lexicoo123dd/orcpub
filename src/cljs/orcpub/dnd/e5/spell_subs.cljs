(ns orcpub.dnd.e5.spell-subs
  (:require [re-frame.core :refer [reg-sub]]
            [orcpub.common :as common]
            [orcpub.template :as t]
            [orcpub.modifiers :as mod]
            [orcpub.dnd.e5 :as e5]
            [orcpub.dnd.e5.backgrounds :as bg5e]
            [orcpub.dnd.e5.languages :as langs5e]
            [orcpub.dnd.e5.races :as races5e]
            [orcpub.dnd.e5.classes :as classes5e]
            [orcpub.dnd.e5.feats :as feats5e]
            [orcpub.dnd.e5.modifiers :as mod5e]
            [orcpub.dnd.e5.magic-items :as mi5e]
            [orcpub.dnd.e5.units :as units5e]
            [orcpub.dnd.e5.character :as char5e]
            [orcpub.dnd.e5.weapons :as weapon5e]
            [orcpub.dnd.e5.skills :as skill5e]
            [orcpub.dnd.e5.spells :as spells5e]
            [orcpub.dnd.e5.monsters :as monsters5e]
            [orcpub.dnd.e5.selections :as selections5e]
            [orcpub.dnd.e5.encounters :as encounters5e]
            [orcpub.dnd.e5.combat :as combat5e]
            [orcpub.dnd.e5.spell-lists :as sl5e]
            [orcpub.dnd.e5.armor :as armor5e]
            [orcpub.dnd.e5.template :as t5e]
            [orcpub.dnd.e5.equipment :as equipment5e]
            [orcpub.dnd.e5.options :as opt5e]
            [orcpub.route-map :as routes]
            [orcpub.dnd.e5.events :as events]
            [orcpub.dnd.e5.template-base :as t-base]
            [reagent.ratom :as ra]
            [clojure.string :as s]
            [clojure.set :as set]
            [cljs-http.client :as http]))

(reg-sub
 ::e5/plugins
 (fn [db _]
   (get db :plugins)))

(reg-sub
 ::e5/plugin-vals
 :<- [::e5/plugins]
 (fn [plugins]
   (let [result (map
                 (fn [p]
                   (into
                    {}
                    (map
                     (fn [[type-k type-m]]
                       [type-k
                        (if (coll? type-m)
                          (into
                           {}
                           (remove
                            (fn [[k {:keys [disabled?]}]]
                              disabled?)
                            type-m))
                          type-m)])
                     p)))
                 (filter (comp not :disabled?)
                         (vals plugins)))]
     result)))

(reg-sub
 ::bg5e/plugin-backgrounds
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (map
    (fn [background]
      (assoc background :edit-event [::bg5e/edit-background background]))
    (mapcat (comp vals ::e5/backgrounds) plugins))))

(reg-sub
 ::langs5e/plugin-languages
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/languages) plugins)))

(reg-sub
 ::selections5e/plugin-selections
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/selections) plugins)))

(reg-sub
 ::selections5e/selection-map
 :<- [::selections5e/plugin-selections]
 (fn [selections]
   (common/map-by-key selections)))

(defn spell-modifiers [{:keys [spells]} class-name]
  (map
   (fn [{:keys [level value]}]
     (let [{:keys [ability key]} value]
       (mod5e/spells-known (or (:level value) 0)
                           key
                           (if (keyword? ability)
                             (keyword "orcpub.dnd.e5.character" (name ability)))
                           class-name
                           level)))
   spells))

(reg-sub
 ::races5e/plugin-races
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (map
    (fn [race]
      (assoc race
             :modifiers
             (concat (opt5e/plugin-modifiers (:props race)
                                             (:key race))
                     (spell-modifiers race (:name race)))
             :edit-event [::races5e/edit-race race]))
    (mapcat (comp vals ::e5/races) plugins))))

(reg-sub
 ::races5e/plugin-subraces
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (map
    (fn [subrace]
      (assoc subrace
             :modifiers (concat (opt5e/plugin-modifiers (:props subrace)
                                                        (:key subrace))
                                (spell-modifiers subrace (:name subrace)))
             :edit-event [::races5e/edit-subrace subrace]))
    (mapcat (comp vals ::e5/subraces) plugins))))

(defn level-modifier [class-key {:keys [type value]}]
  (case type
    :weapon-prof (mod5e/weapon-proficiency value)
    :num-attacks (mod5e/num-attacks value)
    :damage-resistance (mod5e/damage-resistance value)
    :damage-immunity (mod5e/damage-immunity value)
    :saving-throw-advantage (mod5e/saving-throw-advantage value)
    :skill-prof (mod5e/skill-proficiency value)
    :armor-prof (mod5e/armor-proficiency value)
    :tool-prof (mod5e/tool-proficiency value)
    :flying-speed (mod5e/flying-speed-override value)
    :swimming-speed (mod5e/swimming-speed-override value)
    :flying-speed-equals-walking-speed (mod5e/flying-speed-equal-to-walking)
    :spell (mod5e/spells-known (:level value)
                               (:key value)
                               (:ability value)
                               (if (keyword? class-key)
                                 (common/safe-capitalize-kw class-key)))))

(defn eldritch-knight-spell? [s]
    (let [school (:school s)]
      (or (= school "evocation")
          (= school "abjuration"))))

(defn arcane-trickster-spell? [s]
    (let [school (:school s)]
      (or (= school "enchantment")
          (= school "illusion"))))


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

(defn eldritch-knight-ref [subclass-key subpath]
    (concat
     [:class :fighter :levels :level-3 :martial-archetype subclass-key]
     subpath))

(defn arcane-trickster-ref [subclass-key subpath]
    (concat
     [:class :rogue :levels :level-3 :roguish-archetype subclass-key]
     subpath))

(defn eldritch-knight-spell-selection [subclass-key spell-lists spells-map num spell-levels]
  (subclass-wizard-spell-selection spell-lists
                                   spells-map
                                   "Fighter Abjuration or Evocation Spells"
                                   (eldritch-knight-ref subclass-key [:abjuration-or-evocation-spells-known])
                                   :fighter
                                   "Fighter"
                                   num
                                   spell-levels
                                   eldritch-knight-spell?))

(defn arcane-trickster-spell-selection [subclass-key spell-lists spells-map num spell-levels]
  (subclass-wizard-spell-selection spell-lists
                                   spells-map
                                   "Rogue Enchantment or Illusion Spells"
                                     (arcane-trickster-ref subclass-key [:enchantment-or-illusion-spells-known])
                                     :rogue
                                     "Rogue"
                                     num
                                     spell-levels
                                     arcane-trickster-spell?))

(defn eldritch-knight-any-spell-selection [subclass-key spell-lists spells-map num spell-levels]
  (subclass-wizard-spell-selection spell-lists
                                   spells-map
                                   "Fighter Spells: Any School"
                                     (eldritch-knight-ref subclass-key [:spells-known-any-school])
                                     :fighter
                                     "Fighter"
                                     num
                                     spell-levels))


(defn arcane-trickster-any-spell-selection [subclass-key spell-lists spells-map num spell-levels]
  (subclass-wizard-spell-selection spell-lists
                                   spells-map
                                   "Rogue Spells: Any School"
                                     (arcane-trickster-ref subclass-key [:spells-known-any-school])
                                     :rogue
                                     "Rogue"
                                     num
                                     spell-levels))

(defn eldritch-knight-cantrip [subclass-key spell-lists spells-map num]
  (opt5e/spell-selection spell-lists
                         spells-map
                         {:class-key :fighter
                          :level 0
                          :ref (eldritch-knight-ref subclass-key [:cantrips-known])
                          :spellcasting-ability ::char5e/int
                          :class-name "Fighter"
                          :num num
                          :spell-keys (get-in spell-lists [:wizard 0])}))

(defn arcane-trickster-cantrip [subclass-key spell-lists spells-map num]
  (opt5e/spell-selection spell-lists
                         spells-map
                         {:class-key :rogue
                          :level 0
                          :ref (arcane-trickster-ref subclass-key [:cantrips-known])
                          :spellcasting-ability ::char5e/int
                          :class-name "Rogue"
                          :num num
                          :spell-keys (get-in spell-lists [:wizard 0])}))

(defn spellcaster-subclass-levels [subclass-key spell-lists spells-map class-name]
  (case class-name
    :rogue {3 {:selections [(arcane-trickster-cantrip subclass-key spell-lists spells-map 2)
                            (arcane-trickster-spell-selection subclass-key spell-lists spells-map 2 [1])
                            (arcane-trickster-any-spell-selection subclass-key spell-lists spells-map 1 [1])]
               :modifiers [(mod5e/spells-known 0 :mage-hand ::char5e/int "Arcane Trickster")]}
            4 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1])]}
            7 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
            8 {:selections [(arcane-trickster-any-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
            10 {:selections [(arcane-trickster-cantrip subclass-key spell-lists spells-map 1)
                             (arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
            11 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
            13 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
            14 {:selections [(arcane-trickster-any-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
            16 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
            19 {:selections [(arcane-trickster-spell-selection subclass-key spell-lists spells-map 1 [1 2 3 4])]}
            20 {:selections [(arcane-trickster-any-spell-selection subclass-key spell-lists spells-map 1 [1 2 3 4])]}}
    :fighter {3 {:selections [(eldritch-knight-cantrip subclass-key spell-lists spells-map 2)
                              (eldritch-knight-spell-selection subclass-key spell-lists spells-map 2 [1])
                              (eldritch-knight-any-spell-selection subclass-key spell-lists spells-map 1 [1])]}
              4 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1])]}
              7 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
              8 {:selections [(eldritch-knight-any-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
              10 {:selections [(eldritch-knight-cantrip subclass-key spell-lists spells-map 1)
                               (eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
              11 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2])]}
              13 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
              14 {:selections [(eldritch-knight-any-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
              16 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2 3])]}
              19 {:selections [(eldritch-knight-spell-selection subclass-key spell-lists spells-map 1 [1 2 3 4])]}
              20 {:selections [(eldritch-knight-any-spell-selection subclass-key spell-lists spells-map 1 [1 2 3 4])]}}
    nil))

(defn merge-level [level-1 level-2]
  (merge-with
   concat
   level-1
   level-2))

(defn merge-levels [& level-specs]
  (apply
   merge-with
   merge-level
   level-specs))

(defn to-class-level [spell-level]
  (dec (* 2 spell-level)))

(defn level-selection [class-key selection-map {:keys [type num]}]
  (let [{:keys [name options]} (selection-map type)]
    (t/selection-cfg
     {:name name
      :key type
      :tags #{:class}
      :min (or num 1)
      :max (or num 1)
      :options (map
                (fn [{:keys [name description]}]
                  (t/option-cfg
                   {:name name
                    :modifiers [(mod5e/trait-cfg
                                 {:name name
                                  :summary description})]}))
                options)})))

(defn make-level-selections [class selections selection-map]
  (reduce
   (fn [levels {:keys [level] :as s}]
     (update-in levels
               [(or level 1) :selections]
               conj
               (level-selection class selection-map s)))
   {}
   selections))

(defn make-cleric-spell-mods [cleric-spells]
  (vec
   (reduce-kv
    (fn [mods spell-level spells]
      (concat
       mods
       (let [spell-kws (vals spells)]
         (mapv
          (fn [spell-kw]
            (opt5e/cleric-spell spell-level spell-kw (to-class-level spell-level)))
          spell-kws))))
    []
    cleric-spells)))

(defn make-levels [spell-lists spells-map selection-map {:keys [key class spellcasting] :as option}]
  (let [modifiers (:level-modifiers option)
        selections (:level-selections option)
        by-level (group-by :level modifiers)
        add-spellcasting? (and spellcasting
                               (#{:fighter :rogue} class))
        spellcaster-levels (spellcaster-subclass-levels key spell-lists spells-map class)
        selections-levels (make-level-selections class selections selection-map)]
    (reduce-kv
     (fn [levels level level-modifiers]
       (update-in levels
                  [(or level 1) :modifiers]
                  concat
                  (map (partial level-modifier class) level-modifiers)))
     (merge-levels
      selections-levels
      (if add-spellcasting?
        spellcaster-levels)
      (if (and (= class :paladin)
               (:paladin-spells option))
        {1 {:modifiers (reduce-kv
                        (fn [mods spell-level spells]
                          (concat
                           mods
                           (map
                            (fn [spell-kw]
                              (opt5e/paladin-spell spell-level
                                                   spell-kw))
                            (vals spells))))
                        []
                        (:paladin-spells option))}})
      (let [cleric-spells (:cleric-spells option)]
        (if (and (= class :cleric)
                 cleric-spells)
          (let [cleric-spell-mods (make-cleric-spell-mods cleric-spells)]
            {1 {:modifiers cleric-spell-mods}})))
      (if (and (or (= class :warlock-cha) (= class :warlock-int))
               (:warlock-spells option))
        (reduce-kv
         (fn [levels spell-level spells]
           (let [level (to-class-level spell-level)]
             (if (and spell-level (seq (vals spells)))
               (assoc-in levels
                         [level :selections]
                         [(opt5e/warlock-subclass-spell-selection spell-lists spells-map class (if (= class :warlock-cha) ::char5e/cha (if (= class :warlock-int) ::char5e/int)) (vals spells))]))))
         {}
         (:warlock-spells option))))
     by-level)))

(reg-sub
 ::classes5e/plugin-subclasses
 :<- [::e5/plugin-vals]
 :<- [::spells5e/spell-lists]
 :<- [::spells5e/spells-map]
 :<- [::selections5e/selection-map]
 (fn [[plugins spell-lists spells-map selection-map] _]
   (map
    (fn [subclass]
      (let [levels (make-levels spell-lists spells-map selection-map subclass)]
        (assoc subclass
               :modifiers (opt5e/plugin-modifiers (:props subclass)
                                                  (:key subclass))
               :levels levels
               :edit-event [::classes5e/edit-subclass subclass])))
    (mapcat (comp vals ::e5/subclasses) plugins))))

(reg-sub
 ::classes5e/plugin-classes
 :<- [::e5/plugin-vals]
 :<- [::spells5e/spell-lists]
 :<- [::spells5e/spells-map]
 :<- [::selections5e/selection-map]
 (fn [[plugins spell-lists spells-map selection-map]]
   (map
    (fn [class]
      (let [levels (make-levels spell-lists spells-map selection-map class)]
        (assoc class
               :modifiers (opt5e/plugin-modifiers (:props class)
                                                  (:key class))
               :levels levels)))
    (mapcat (comp vals ::e5/classes) plugins))))

(reg-sub
 ::feats5e/plugin-feats
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/feats) plugins)))

(reg-sub
 ::classes5e/plugin-invocations
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/invocations) plugins)))

(reg-sub
 ::classes5e/plugin-boons
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat #(-> % ::e5/boons vals) plugins)))

(defn criminal-background [nm]
  {:name nm
   :help "You have a history of criminal activity."
   :traits [{:name "Criminal Contact"
             :page 129
             :summary "You have a contact into a network of criminals"}]
   :profs {:skill {:deception true, :stealth true}
           :tool {:thieves-tools true}
           :tool-options {:gaming-set 1}}
   :equipment {:crowbar 1
               :clothes-common 1
               :pouch 1}
   :treasure {:gp 15}})

(def ships-passage-trait-cfg
  {:name "Ship's Passage"
   :page 139
   :summary "You are able to secure free passage on a sailing ship"})

(def acolyte-bg
  {:name "Acolyte"
   :help "Your life has been devoted to serving a god or gods."
   :profs {:skill {:insight true, :religion true}
           :language-options {:choose 2 :options {:any true}}}
   :equipment {:clothes-common 1
               :pouch 1
               :incense 5
               :vestements 1}
   :selections [(opt5e/new-starting-equipment-selection
                 nil
                 {:name "Holy Symbol"
                  :options (map
                            #(opt5e/starting-equipment-option % 1)
                            equipment5e/holy-symbols)})
                ]
   :equipment-choices [{:name "Prayer Book/Wheel"
                        :options {:prayer-book 1
                                  :prayer-wheel 1}}]
   :treasure {:gp 15}
   :traits [{:name "Shelter the Faithful"
             :page 127
             :summary "You and your companions can expect free healing at an establishment of your faith."}]})

(def archaeologist-bg
  {:name "Archaeologist"
   :help ""
   :profs {:skill {:history true :survival true}
           :language-options {:choose 1 :options {:any true}}}
   :selections [(t/selection-cfg
                 {:name "Proficiency: Cartographer's Tools or Navigator's Tools"
                  :tags #{:profs}
                  :options [(t/option-cfg
                             {:name "Cartographer's Tools"
                              :modifiers [(mod5e/tool-proficiency :cartographers-tools)]})
                            (t/option-cfg
                             {:name "Navigator's Tools"
                              :modifiers [(mod5e/tool-proficiency :navigators-tools)]})]})]
   :equipment {:case-map-or-scroll 1
               :lantern-bullseye 1
               :pick-miner-s 1
               :clothes-traveler-s 1
               :shovel 1
               :tent-two-person 1
               :pouch 1}
   :custom-equipment {"Trinket" 1}
   :treasure {:gp 25}
  })

(def athlete-bg
  {:name "Athlete"
   :help "You have participated in physical contests."
   :profs {:skill {:acrobatics true, :athletics true}
           :language-options {:choose 1 :options {:any true}}
           :tool {:land-vehicles true}}
   :equipment {:clothes-traveler-s 1
               :pouch 1}
   :custom-equipment {"Bronze discus or leather ball" 1
                      "Lucky charm or past trophy" 1}
   :treasure {:gp 10}
   :traits [{:name "Echoes of Victory"
             :summary "50% chance there's an admirer who is willing to provide information and shelter when visiting a settlement within 100 miles of where you grew up. During downtime, compete in athletic events sufficient enough to provide a comfortable lifestyle."}]})

(def charlatan-bg
  {:name "Charlatan"
   :help "You have a history of being able to work people to your advantage."
   :traits [{:name "False Identity"
             :page 128
             :summary "you have a false identity; you can forge documents"}]
   :profs {:skill {:deception true :sleight-of-hand true}
           :tool {:disguise-kit true :forgery-kit true}}
   :equipment {:clothes-fine 1
               :disguise-kit 1
               :pouch 1}
   :treasure {:gp 15}})

(def city-watch-bg
  {:name "City Watch"
  ;;  :help "You have a history of being able to work people to your advantage."
   :traits [{:name "Watcher's Eye"
             :summary "You can easily find the local outpost of the watch or a similar organization, and just as easily pick out the dens of criminal activity in a community, although you're more likely to be welcome in the former locations rather than the latter."}]
   :profs {:skill {:athletics true :insight true}
           :language-options {:choose 2 :options {:any true}}}
   :equipment {:horn 1
               :manacles 1
               :pouch 1}
   :custom-equipment {"Uniform" 1}
   :treasure {:gp 10}})

(def entertainer-bg
  {:name "Entertainer"
   :help "You have a history of entertaining people."
   :traits [{:name "By Popular Demand"
             :page 130
             :summary "you are able to find a place to perform, in which you will recieve free food and lodging"}]
   :profs {:skill {:acrobatics true :performance true}
           :tool {:disguise-kit true}
           :tool-options {:musical-instrument 1}}
   :equipment-choices [classes5e/musical-instrument-choice-cfg]
   :equipment {:costume 1
               :pouch 1}
   :treasure {:gp 15}})

(def gladiator-bg
  {:name "Gladiator"
   :help "You have a history of gladiatorial entertainment."
   :traits [{:name "By Popular Demand"
             :page 130
             :summary "you are able to find a place to perform, in which you will recieve free food and lodging"}]
   :profs {:skill {:acrobatics true :performance true}
           :tool {:disguise-kit true}
           :tool-options {:musical-instrument 1}}
   :selections [(opt5e/new-starting-equipment-selection
                 nil
                 {:name "Gladiator Weapon"
                  :options (opt5e/weapon-options weapon5e/weapons)})]
   :equipment {:costume 1
               :pouch 1}
   :treasure {:gp 15}})

(def feylost-bg
  {:name "Feylost"
   :help "You grew up in the Feywild."
   :traits [{:name "Feywild Visitor"
             :summary "A spirit of the Feywild might visit you while you're sound asleep."}
            {:name "Feywild Connection"
             :summary "Your mannerisms and knowledge of fey customs are recognized by natives of the Feywild, who see you as one of their own. Friendly Fey creatures are inclined to come to your aid if you are lost or need help in the Feywild."}]
   :profs {:skill {:deception true :survival true}
           :tool-options {:musical-instrument 1}
           :language-options {:choose 1 :options {:elvish true :gnomish true :goblin true :sylvan true}}}
   :equipment-choices [classes5e/musical-instrument-choice-cfg]
   :equipment {:clothes-traveler-s 1
               :pouch 1}
   :custom-equipment {"Feywild trinket" 3}
   :treasure {:gp 8}})

(def far-traveler-bg
  {:name "Far Traveler"
   :help "You come from a distant place."
   :traits [{:name "All eyes on you"
             :summary "You get curious glances because of your foreign accent, mannerisms, figures of speech, and perhaps appearance. You can use this attention to gain access to people and and places you might otherwise not have."}]
   :selections [(t/selection-cfg
                 {:name "Tool Proficiency"
                  :tags #{:profs}
                  :options [(t/option-cfg
                             {:name "Musical Instrument"
                              :selections [(opt5e/tool-selection (map :key equipment5e/musical-instruments) 1)]})
                            (t/option-cfg
                             {:name "Gaming Set"
                              :selections [(opt5e/tool-selection (map :key equipment5e/gaming-sets) 1)]})]})
                (t/selection-cfg
                 {:name "Musical Instrument or Gaming Set"
                  :tags #{:equipment}
                  :options [(t/option-cfg
                             {:name "Musical Instrument"
                              :selections [(opt5e/new-starting-equipment-selection
                                            nil
                                            {:name "Musical Instrument"
                                             :options (opt5e/tool-options (filter (comp (set (map :key equipment5e/musical-instruments)) :key) equipment5e/tools))})]})
                            (t/option-cfg
                             {:name "Gaming Set"
                              :selections [(opt5e/new-starting-equipment-selection
                                            nil
                                            {:name "Gaming Set"
                                             :options (opt5e/tool-options (filter (comp (set (map :key equipment5e/gaming-sets)) :key) equipment5e/tools))})]})]})]
   :profs {:skill {:insight true :perception true}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:clothes-traveler-s 1
               :pouch 1}
   :custom-equipment {"Poorly wrought maps" 1
                      "Jewelry (10 gp)" 1}
   :treasure {:gp 5}})

(def folk-hero-bg
  {:name "Folk Hero"
   :help "You are regarded as a hero by the people of your home village."
   :traits [{:name "Rustic Hospitality"
             :page 131
             :summary "find a place to rest, hide, or recuperate among commoners"}]
   :profs {:skill {:animal-handling true :survival true}
           :tool {:land-vehicles true}
           :tool-options {:artisans-tool 1}}
   :equipment-choices [opt5e/artisans-tools-choice-cfg]
   :equipment {:shovel 1
               :pot-iron 1
               :clothes-common 1
               :pouch 1}
   :treasure {:gp 10}})

(def guild-artisan-bg
  {:name "Guild Artisan"
   :help "You are an artisan and a member of a guild in a particular field."
   :traits [{:name "Guild Membership"
             :page 133
             :summary "fellow guild members will provide you with food and lodging; you have powerful political connections through your guild"}]
   :profs {:skill {:insight true :persuasion true}
           :tool-options {:artisans-tool 1}
           :language-options {:choose 1 :options {:any true}}}
   :equipment-choices [opt5e/artisans-tools-choice-cfg]
   :equipment {:clothes-traveler-s 1
               :pouch 1}
   :custom-equipment {"Letter of introduction" 1}
   :treasure {:gp 15}})

(def guild-merchant-bg
  {:name "Guild Merchant"
   :help "You are member of a guild of merchants"
   :traits [{:name "Guild Membership"
             :page 133
             :summary "fellow guild members will provide you with food and lodging; you have powerful political connections through your guild"}]
   :profs {:skill {:insight true :persuasion true}
           :language-options {:choose 1 :options {:any true}}}
   :selections [(t/selection-cfg
                 {:name "Proficiency: Navigator's Tools or Language"
                  :tags #{:profs}
                  :options [(t/option-cfg
                             {:name "Navigator's Tools"
                              :modifiers [(mod5e/tool-proficiency :navigators-tools)]})
                            (t/option-cfg
                             {:name "Language"
                              :selections [(opt5e/language-selection ::langs5e/language-map 1)]})]})]
   :equipment {:clothes-traveler-s 1
               :pouch 1
               :mule 1
               :cart 1}
   :custom-equipment {"Letter of introduction" 1}
   :treasure {:gp 15}})

(def haunted-one-bg
  {:name "Haunted One"
   :help "You are haunted by something so terrible that you dare not speak of it"
   :traits [{:name "Heart of Darkness"
             :summary "Though commoners might fear you, they will extend you every courtesy and do their utmost to help you. Unless you have shown yourself to be a danger to them, they will even take up arms to fight alongside you, should you find yourself facing an enemy alone"}]
   :profs {:skill-options {:choose 2 :options {:arcana true :investigation true :religion true :survival true}}
           :language-options {:choose 1 :options {:abyssal true :celestial true :deep-speech true :draconic true :infernal true :primordial true :sylvan true :undercommon true}}}
   :selections [(opt5e/language-selection ::langs5e/language-map 1)] ;;fix
   :equipment {:monster-hunters-pack 1
               :chest 1
               :crowbar 1
               :hammer 1
               :wooden-stake 3
               :holy-symbol 1
               :holy-water 1
               :manacles 1
               :mirror-steel 1
               :oil 1
               :tinderbox 1
               :torch 3
               :clothes-common 1}
   :custom-equipment {"Horror Trinket" 1}
   :treasure {:sp 1}})

(def hermit-bg
  {:name "Hermit"
   :help "You have lived a secluded life."
   :traits [{:name "Discovery"
             :page 134
             :summary "You have made a powerful and unique discovery"}]
   :profs {:skill {:medicine true :religion true}
           :tool {:herbalism-kit true}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:case-map-or-scroll 1
               :clothes-common 1
               :herbalism-kit 1}
   :custom-equipment {"Winter Blanket" 1
                      "Notes from studies/prayers" 1}
   :treasure {:gp 5}})

(def investigator-bg
  {:name "Investigator"
  ;;  :help "You are haunted by something so terrible that you dare not speak of it"
   :traits [{:name "Official Inquiry"
             :summary "Through a combination of fast-talking, determination, and official-looking documentation, you can gain access to a place or an individual related to a crime you're investigating. Those who aren't involved in your investigation avoid impeding you or pass along your requests. Additionally, local law enforcement has firm opinions about you, viewing you as either a nuisance or one of their own"}]
   :profs {:skill-options {:choose 2 :options {:insight true :investigation true :perception true}}
           :tool {:disguise-kit true :thieves-tools true}}
   :equipment {:magnifying-glass 1
               :clothes-common 1}
   :custom-equipment {"Evidence from a past case" 1}
   :treasure {:gp 10}})

(def noble-bg
  {:name "Noble"
   :help "You are of noble birth."
   :traits []
   :profs {:skill {:history true :persuasion true}
           :tool-options {:gaming-set 1}
           :language-options {:choose 1 :options {:any true}}}
   :selections [(t/selection-cfg
                 {:name "Noble Feature"
                  :tags #{:background}
                  :options [(t/option-cfg
                             {:name "Position of Privilege"
                              :modifiers [(mod5e/trait-cfg
                                           {:name "Position of Privilege"
                                            :page 135
                                            :summary "you are welcome in high society and common folk try to accomodate you"})]})
                            (t/option-cfg
                             {:name "Retainers"
                              :modifiers [(mod5e/trait-cfg
                                           {:name "Retainers"
                                            :page 136
                                            :summary "You have 3 commoner retainers"})]})]})]
   :equipment {:clothes-fine 1
               :signet-ring 1
               :purse 1}
   :custom-equipment {"Scroll of Pedigree" 1}
   :treasure {:gp 25}})

(def knight-bg
  {:name "Knight"
   :help "You are a knight."
   :traits [{:name "Retainers"
             :page 136
             :summary "You have 2 commoner retainers and 1 noble squire"}]
   :profs {:skill {:history true :persuasion true}
           :tool-options {:gaming-set 1}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:clothes-fine 1
               :signet-ring 1
               :purse 1}
   :custom-equipment {"Scroll of Pedigree" 1
                      "Emblem of Chivalry" 1}
   :treasure {:gp 25}})

(def mafia-member-bg
  {:name "Mafia Member"
   :help "You belong or have belonged to a mafia"
   :traits [{:name "Mafia Connections"
             :summary (str "As an established and respected member of the mafia, you can rely on certain benefits that membership provides."
                      "Your fellow guild members will provide you with lodging and food if necessary, and pay for your funeral if needed."
                      "In some cities and towns, a mafia family offers a central place to meet other members of your profession, which can be a good place to meet potential patrons, allies, or hirelings.")}]
   :profs {:skill-options {:choose 2 :options {:deception true :insight true :intimidation true :persuasion true}}
           :tool-options {:gaming-set 1}
           :tool {:forgery-kit true}}
   :equipment {:clothes-fine 1
               :forgery-kit 1
               :pouch 1}
   :treasure {:gp 15}})

(def marine-bg
  {:name "Marine"
   :help ""
   :traits [{:name "Steady"
             :page 31
             :summary "Can move twice the normal amount of time (16 hours). Can automatically find a safe route to land a boat on a shore, if one exists."}]
   :profs {:skill {:athletics true :survival true}
           :tool {:water-vehicles true :land-vehicles true}}
   :equipment {:dagger 1
               :clothes-traveler-s 1
               :pouch 1}
   :treasure {:gp 10}})

(def mercenary-veteran-bg
  {:name "Mercenary Veteran"
   :help ""
   :traits [{:name "Mercenary Life"
             :page 152
             :summary "Identify and know a little about mercenary companies by their emblems, including who has hired them recently. Find the taverns and festhalls where mercenaries abide in any area, as long as you speak the language. Find mercenary work between adventures sufficient to maintain a comfortable lifestyle."}]
   :profs {:skill {:athletics true :persuasion true}
           :tool-options {:gaming-set 1}
           :tool {:land-vehicles true}}
   :equipment {:clothes-traveler-s 1
               :pouch 1}
   :equipment-choices [opt5e/gaming-set-choice-cfg]
   :custom-equipment {"Insignia of your rank" 1}
   :treasure {:gp 10}
  })

(def outlander-bg
  {:name "Outlander"
   :help "You were raised in the wilds."
   :traits [{:name "Wanderer"
             :page 136
             :summary "Your memory of maps, geography, settlements, and terrain is excellent. You can find fresh food and water for you and 5 other people."}]
   :profs {:skill {:athletics true :survival true}
           :tool-options {:musical-instrument 1}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:staff 1
               :clothes-traveler-s 1
               :pouch 1
               :hunting-trap 1}
   :custom-equipment {"Trophy from Animal You Killed" 1}
   :treasure {:gp 10}})

(def sage-bg
  {:name "Sage"
   :help "You spent your life studying lore."
   :traits [{:name "Researcher"
             :page 139
             :summary "If you don't know a piece of info you often know where to find it"}]
   :profs {:skill {:arcana true :history true}
           :language-options {:choose 2 :options {:any true}}}
   :equipment {:ink 1
               :clothes-common 1
               :pouch 1
               :knife-small 1}
   :custom-equipment {"Quill" 1
                      "Letter with question from dead colleague" 1}
   :treasure {:gp 10}})

(def sailor-bg
  {:name "Sailor"
   :help "You were a member of a crew for a seagoing vessel."
   :traits [ships-passage-trait-cfg]
   :profs {:skill {:athletics true :perception true}
           :tool {:navigators-tools true :water-vehicles true}}
   :weapons {:club 1}
   :equipment {:rope-silk 1
               :clothes-common 1
               :pouch 1}
   :custom-equipment {"Belaying Pin" 1
                      "Lucky Charm" 1}
   :treasure {:gp 10}})


(def pirate-bg
  {:name "Pirate"
   :help "You were a member of a crew for a seagoing vessel."
   :profs {:skill {:athletics true :perception true}
           :tool {:navigators-tools true :water-vehicles true}}
   :weapons {:club 1}
   :equipment {:rope-silk 1
               :clothes-common 1
               :pouch 1}
   :selections [(t/selection-cfg
                 {:name "Feature"
                  :tags #{:background}
                  :options [(t/option-cfg
                             {:name "Ship's Passage"
                              :modifiers [(mod5e/trait-cfg
                                           ships-passage-trait-cfg)]})
                            (t/option-cfg
                             {:name "Bad Reputation"
                              :modifiers [(mod5e/trait-cfg
                                           {:name "Bad Reputation"
                                            :page 139
                                            :summary "People in a civilized settlement are afraid of you and will let you get away with minor crimes"})]})]})]
   :custom-equipment {"Belaying Pin" 1
                      "Lucky Charm" 1}
   :treasure {:gp 10}})

(def soldier-bg
  {:name "Soldier"
   :help "You have spent your living by the sword."
   :traits [{:name "Military Rank"
             :page 140
             :summary "Where recognized, your previous rank provides influence among military"}]
   :profs {:skill {:athletics true :intimidation true}
           :tool {:land-vehicles true}
           :tool-options {:gaming-set 1}}
   :equipment {:clothes-common 1
               :pouch 1}
   :equipment-choices [{:name "Dice or Cards"
                        :options {:dice-set 1
                                  :playing-card-set 1}}]
   :custom-equipment {"Insignia of Rank" 1
                      "Trophy from Fallen Enemy" 1}
   :treasure {:gp 10}})

(def urchin-bg
  {:name "Urchin"
   :help "You were a poor orphan living on the streets."
   :traits [{:name "City Streets"
             :page 141
             :summary "You can travel twice your normal speed between city locations"}]
   :profs {:skill {:sleight-of-hand true :stealth true}
           :tool {:disguise-kit true :thieves-tools true}}
   :equipment {:knife-small 1
               :clothes-common 1
               :pouch 1}
   :custom-equipment {"Map of city you grew up in" 1
                      "Pet mouse" 1
                      "Token to remember your parents" 1}
   :treasure {:gp 10}})

(def uthgardt-tribe-member-bg
  {:name "Uthgardt Tribe Member"
   :help "You belong to the Uthgardt tribe."
   :traits [{:name "Uthgardt Heritage"
             :summary "You have an excellent knowledge of the terrain and natural resources of the North. You can find twice as much food and water as you normally would when you forage there.
You can call upon the hospitality of your people, and those allied with your tribe, often including members of the druid circles, tribes of nomadic elves, the Harpers, and the priesthoods devoted to the gods of the First Circle."}]
   :profs {:skill {:athletics true :survival true}
           :language-options {:choose 1 :options {:any true}}}
   :selections [(t/selection-cfg
                 {:name "Tool Proficiency"
                  :tags #{:profs}
                  :options [(t/option-cfg
                             {:name "Musical Instrument"
                              :selections [(opt5e/tool-selection (map :key equipment5e/musical-instruments) 1)]})
                            (t/option-cfg
                             {:name "Artisan's Tools"
                              :selections [(opt5e/tool-selection (map :key equipment5e/artisans-tools) 1)]})]})]
   :equipment {:hunting-trap 1
               :clothes-traveler-s 1
               :pouch 1}
   :custom-equipment {"Totemic token or tattoos" 1}
   :treasure {:gp 10}})

(reg-sub
 ::bg5e/backgrounds
 :<- [::bg5e/plugin-backgrounds]
 (fn [plugin-backgrounds]
   (vec
      (concat
      (reverse plugin-backgrounds)
      [acolyte-bg
       archaeologist-bg
       athlete-bg
       charlatan-bg
       city-watch-bg
       (criminal-background "Criminal")
       (criminal-background "Spy")
       entertainer-bg
       gladiator-bg
       feylost-bg
       far-traveler-bg
       folk-hero-bg
       guild-artisan-bg
       guild-merchant-bg
       haunted-one-bg
       hermit-bg
       investigator-bg
       noble-bg
       knight-bg
       mafia-member-bg
       marine-bg
       mercenary-veteran-bg
       outlander-bg
       sage-bg
       sailor-bg
       pirate-bg
       soldier-bg
       urchin-bg
       uthgardt-tribe-member-bg
       ])
   )))

#_(reg-sub
 ::bg5e/backgrounds
 :<- [::bg5e/plugin-backgrounds]
 (fn [plugin-backgrounds]
   (cons
    acolyte-bg
    plugin-backgrounds)))

(def languages
  [{:name "Common"
    :key :common}
   {:name "Centaur"
    :key :centaur}
   {:name "Dwarvish"
    :key :dwarvish}
   {:name "Elvish"
    :key :elvish}
   {:name "Giant"
    :key :giant}
   {:name "Gnomish"
    :key :gnomish}
   {:name "Goblin"
    :key :goblin}
   {:name "Halfling"
    :key :halfling}
   {:name "Harpy"
    :key :harpy}
   {:name "Lenuboon"
    :key :lenuboon}
   {:name "Lunar"
    :key :lunar}
   {:name "Orc"
    :key :orc}
   {:name "Abyssal"
    :key :abyssal}
   {:name "Celestial"
    :key :celestial}
   {:name "Draconic"
    :key :draconic}
   {:name "Deep Speech"
    :key :deep-speech}
   {:name "Infernal"
    :key :infernal}
   {:name "Primordial"
    :key :primordial}
   {:name "Sylvan"
    :key :sylvan}
   {:name "Undercommon"
    :key :undercommon}])

(reg-sub
 ::langs5e/languages
 :<- [::langs5e/plugin-languages]
 (fn [plugin-languages]
   (concat
    languages
    plugin-languages)))

(reg-sub
 ::langs5e/language-map
 :<- [::langs5e/languages]
 (fn [languages]
   (common/map-by-key languages)))

(defn powerful-build [page]
  {:name "Powerful Build"
   :page page
   :source :vgm
   :summary "Count as one size larger for purposes of determining weight you can carry, push, drag, or lift."})

(def elf-weapon-training-mods
  (opt5e/weapon-prof-modifiers [:longsword :shortsword :shortbow :longbow]))

(defn sunlight-sensitivity [page & [source]]
  {:name "Sunlight Sensitivity"
   :summary "Disadvantage on attack and perception rolls when there's direct sunlight"
   :source (or source :phb)
   :page 24})

(def mask-of-the-wild-mod
  (mod5e/trait-cfg
   {:name "Mask of the Wild"
    :page 24
    :summary "Hide when lightly obscured by natural phenomena."}))

(defn high-elf-cantrip-selection [spell-lists spells-map]
  (opt5e/spell-selection
   spell-lists
   spells-map
   {:class-key :wizard
    :level 0
    :exclude-ref? true
    :spellcasting-ability ::char5e/int
    :class-name "High Elf"
    :num 1}))

#_(def drow-magic-mods
  [(mod5e/spells-known 0 :dancing-lights ::char5e/cha "Dark Elf")
   (mod5e/spells-known 1 :faerie-fire ::char5e/cha "Dark Elf" 3)
   (mod5e/spells-known 2 :darkness ::char5e/cha "Dark Elf" 5)])

(defn elf-option-cfg [spell-lists spells-map language-map]
  {:name "Elf"
   :key :elf
   :help "Elves are graceful, magical creatures, with a slight build."
   :abilities {::char5e/dex 2}
   :size :medium
   :speed 30
   :languages ["Elvish" "Common"]
   :darkvision 60
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/immunity :magical-sleep)
               (mod5e/skill-proficiency :perception)]
   :subraces
   [{:name "High Elf"
     :abilities {::char5e/int 1}
     :selections [(high-elf-cantrip-selection spell-lists spells-map)
                  (opt5e/language-selection-aux (vals language-map) 1)]
     :modifiers [elf-weapon-training-mods]}
    #_{:name "Wood Elf"
     :abilities {::char5e/wis 1}
     :modifiers [(mod5e/speed 5)
                 mask-of-the-wild-mod
                 elf-weapon-training-mods]}
    #_{:name "Dark Elf (Drow)"
     :abilities {::char5e/cha 1}
     :traits [(sunlight-sensitivity 24)]
     :modifiers (conj drow-magic-mods
                      (mod5e/weapon-proficiency :rapier)
                      (mod5e/weapon-proficiency :shortsword)
                      (mod5e/weapon-proficiency :crossbow-hand)
                      (mod5e/darkvision 120))}]
   :traits [{:name "Fey Ancestry"
             :page 23
             :summary "advantage on charmed saves and immune to sleep magic"}
            {:name "Trance"
             :page 23
             :summary "Trance 4 hrs. instead of sleep 8"}]})

(defn high-elf-aoa-spell-selection [spell-lists spells-map spell-level prereq-level]
  (opt5e/spell-selection
   spell-lists
   spells-map
   {:class-key :wizard
    :level spell-level
    :exclude-ref? true
    :spellcasting-ability ::char5e/int
    :class-name "High Elf"
    :num 1
    :prereq-fn (opt5e/prereq-level-fn prereq-level)}))

(defn wood-elf-aoa-cantrip-selection [spell-lists spells-map]
  (opt5e/spell-selection
   spell-lists
   spells-map
   {:class-key :druid
    :level 0
    :exclude-ref? true
    :spellcasting-ability ::char5e/wis
    :class-name "Wood Elf"
    :num 1}))

(defn elf-aoa-option-cfg [spell-lists spells-map language-map]
  {:name "Elf (AoA)"
   :key :elf-aoa
   :help "Elves are graceful, magical creatures, with a slight build."
   :abilities {::char5e/dex 2}
   :size :medium
   :speed 30
   :languages ["Elvish" "Common"]
   :darkvision 60
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/immunity :magical-sleep)
               (mod5e/skill-proficiency :perception)]
   :subraces [{:name "High Elf"
               :abilities {::char5e/int 1}
               :selections [(opt5e/language-selection-aux (vals language-map) 1)
                            (high-elf-aoa-spell-selection spell-lists spells-map 0 1)
                            (high-elf-aoa-spell-selection spell-lists spells-map 1 3)
                            (high-elf-aoa-spell-selection spell-lists spells-map 2 5)]
               :modifiers [elf-weapon-training-mods
                           ]}
              {:name "Dark Elf"
               :abilities {::char5e/cha 1}
               :darkvision 120
               :modifiers [(mod5e/dependent-trait
                            {:name "Dark Magic"
                             :summary (str "You know Dancing Lights and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Silent Image")
                                                (>= lvl 5) (conj "Darkness"))))
                                          " once per long rest. CHA is your spellcasting ability.")})
                           (mod5e/spells-known 0 :dancing-lights ::char5e/cha "Dark Elf")
                           (mod5e/spells-known 1 :silent-image ::char5e/cha "Dark Elf" 3)
                           (mod5e/spells-known 2 :darkness ::char5e/cha "Dark Elf" 5)]
               :weapon-proficiencies [:rapier :whip :shortsword :crossbow-hand]}
              {:name "Wood Elf"
               :abilities {::char5e/wis 1}
               :speed 35
               :selections [(wood-elf-aoa-cantrip-selection spell-lists spells-map)]
               :modifiers [(mod5e/skill-proficiency :nature)]
               :weapon-proficiencies [:longsword :shortsword :shortbow :longbow]
               :traits [{:name "Mask of the Wild"
                         :summary "You can attempt to hide even when you are only lightly obscured by foliage, heavy rain, falling snow, mist, and other natural phenomena"}]}
              {:name "Sea Elf"
               :abilities {::char5e/con 1}
               :modifiers [(mod5e/damage-resistance :cold)
                           (mod5e/swimming-speed-equal-to-walking)
                           (mod5e/dependent-trait
                            {:name "Sea Magic"
                             :summary (str "You know Shape Water and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Destroy Water")
                                                (>= lvl 5) (conj "Waterwalk"))))
                                          " once per long rest, without material component. INT is your spellcasting ability.")})
                           (mod5e/spells-known 0 :shape-water ::char5e/int "Sea Elf")
                           (mod5e/spells-known 1 :destroy-water ::char5e/int "Sea Elf" 3)
                           (mod5e/spells-known 2 :waterwalk ::char5e/int "Sea Elf" 5)]
               :weapon-proficiencies [:trident :glaive :net :rapier]
               :traits [{:name "Child of the Sea"
                         :summary "Breathe air and water, resistance to cold damage"}
                        {:name "Friend of the Sea"
                         :summary "Communicate simple ideas to any Beast that has a swimming speed. It can understand your words, though you have no special ability to understand it in return. You have a swimspeed equal to your walking speed"}]}
              {:name "Snow Elf"
               :abilities {::char5e/wis 1}
               :modifiers [(mod5e/damage-resistance :cold)
                           (mod5e/dependent-trait
                            {:name "Snow Elf Magic"
                             :summary (str "You know Ray of Frost and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Armor of Agathys")
                                                (>= lvl 5) (conj "Warding Wind"))))
                                          " once per long rest, without material component. INT is your spellcasting ability.")})
                           (mod5e/spells-known 0 :ray-of-frost ::char5e/wis "Snow Elf")
                           (mod5e/spells-known 1 :armor-of-agathys ::char5e/wis "Snow Elf" 3)
                           (mod5e/spells-known 2 :warding-wind ::char5e/wis "Snow Elf" 5)
                           (mod5e/reaction
                            {:name "Freezing Breeze"
                             :summary (str "Cause an enemy within 20 ft. using their reaction to make a DC " (?spell-save-dc ::char5e/wis) " CON save, losing their reaction on a fail")})]
               :weapon-proficiencies [:longbow :morningstar :pike :whip]
               }
              ]
   :traits [{:name "Fey Ancestry"
             :page 23
             :summary "advantage on charmed saves and immune to sleep magic"}
            {:name "Trance"
             :page 23
             :summary "Trance 4 hrs. instead of sleep 8. After trance, gain proficiency with a weapon or tool"}]
   })

(defn aasimar-option-cfg [language-map]
  {:name "Aasimar"
   :key :aasimar
   :help ""
   :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 1 true)
                (opt5e/language-selection-aux (vals language-map) 1)]
   :subraces [{:name "Necrotic Shroud"
               :modifiers [(mod5e/bonus-action
                            {:name "Necrotic Shroud"
                             :level 3
                             :page 7
                             :source :mpmm
                             :summary (str "Creatures other than allies within 10 ft. that you can see must succeed on a DC " (?spell-save-dc ::char5e/cha) " cha save or be frightened of you until the end of your next turn. For 1 minute, once per turn, deal an additional " ?prof-bonus " necrotic damage to one target you deal damage to with a spell or attack.")})]}
              {:name "Radiant Consumption"
               :modifiers [(mod5e/bonus-action
                            {:name "Radiant Consumption"
                             :level 3
                             :page 7
                             :source :mpmm
                             :summary (str "For 1 minute, shed 10 ft. bright light and 10 ft. dim light, deal " ?prof-bonus " radiant damage to each creature within 10 ft. at the end of your turn and once per turn, deal an additional " ?prof-bonus " radiant damage to one target you deal damage to with a spell or attack.")})]}
              {:name "Radiant Soul"
               :modifiers [(mod5e/bonus-action
                            {:name "Radiant Soul"
                             :level 3
                             :page 7
                             :source :mpmm
                             :summary (str "For 1 minute, sprout wings (flying speed equal to walking speed) and once per turn, deal an additional " ?prof-bonus " radiant damage to one target you deal damage to with a spell or attack.")})]}]
   :modifiers [(mod5e/action
                {:name "Healing Hands"
                 :page 7
                 :summary "Touch and heal a creature equal to your proficiency bonus d6 (use once/long rest)."})
               (mod5e/damage-resistance :necrotic)
               (mod5e/damage-resistance :radiant)
               (mod5e/spells-known 0 :light ::char5e/cha "Aasimar")]})

(def centaur-option-cfg
  {:name "Centaur"
   :key :centaur
   :help ""
   :languages ["Common" "Centaur"]
   :traits [{:name "Strong Build"
             :summary "Count as one size larger for purposes of determining weight you can carry, push, drag, or lift.\nAny climb that requires hands and feet costs 4 more ft. instead of 1."}]
   :modifiers [(mod5e/attack
                {:name "Hoove/Horn"
                 :attack-type :melee
                 :damage-type :bludgeoning
                 :damage-die 6
                 :damage-die-count 1
                 :damage-modifier (if (= (?class-level :monk) 0) (::char5e/str ?ability-bonuses) (max (::char5e/str ?ability-bonuses) (::char5e/dex ?ability-bonuses)))})]
   :subraces [{:name "Equine"
               :abilities {::char5e/str 2 ::char5e/con 1}
               :size :medium
               :speed 40
               :profs {:skill-options {:choose 1 :options {:animal-handling true :athletics true :perception true :nature true :survival true}}}
               :weapon-proficiencies [:battleaxe :flail :glaive :greataxe :greatsword :halberd :lance :longsword :maul :morningstar :pike :rapier :scimitar :shortsword :trident :war-pick :warhammer :whip :longbow]
               :modifiers [(mod5e/bonus-action
                            {:name "Charge"
                            :summary "If you move at least 30 feet in a straight line, you can make an attack with your hooves or Dash"})]}
              {:name "Ovine"
               :abilities {::char5e/con 2 ::char5e/dex 1}
               :size :medium
               :speed 35
               :weapon-proficiencies [:longbow]
               :profs {:skill-options {:choose 1 :options {:animal-handling true :athletics true :perception true :nature true :survival true}}}
               :modifiers [(mod5e/tool-proficiency :weavers-tools)]
               :traits [{:name "Soft Pelt"
                        :summary "Resistance to bludgeoning damage from melee weapon attacks while not wearing heavy armor"}]
               :selections [(t/selection-cfg
                             {:name "Tool Proficiencies"
                              :tags #{:profs}
                              :options [(t/option-cfg
                                         {:name "Artisan's Tool"
                                          :selections [(opt5e/tool-selection (map :key equipment5e/artisans-tools) 1)]})
                                        (t/option-cfg
                                         {:name "Musical Instrument"
                                          :selections [(opt5e/tool-selection (map :key equipment5e/musical-instruments) 1)]})]})]}
              {:name "Caprine"
               :abilities {::char5e/dex 2 ::char5e/con 1}
               :size :small
               :speed 30
               :profs {:skill {:athletics true}
                       :tool {:masons-tools true}
                       :skill-options {:choose 1 :options {:acrobatics true :perception true :nature true :survival true}}}
               :traits [{:name "Skilled Climber"
                         :summary "Unlike other centaurs, climbing does not cost you the extra feet. Climbing speed equals walking speed if the climb is less than 90 degrees with minimal footing"}
                        {:name "Evasive Bounce"
                         :summary "If you move at least 10 feet towards an enemy in a straight line and make a melee weapon attack, you can bounce off the enemy and move yourself 5 feet away from them without provoking opportunity attacks.\nYou can only bounce away toward where you came from and only once off the same target."}]}
              {:name "Cervine"
               :abilities {::char5e/dex 1 ::char5e/con 1 ::char5e/wis 1}
               :size :medium
               :speed 40
               :profs {:skill {:nature true}
                       :tool {:herbalism-kit true}
                       :skill-options {:choose 1 :options {:acrobatics true :athletics true :perception true :stealth true :medicine true}}}
               :modifiers [(mod5e/spells-known 2 :locate-animals-or-plants ::char5e/wis "Cervine Centaur")]
               :traits [{:name "Connection to the Wilds"
                        :summary "You can cast Locate Animals or Plants at will with a radius of 500 ft., 5 miles if cast as a Ritual"}
                       {:name "Undergrowth Mobility"
                        :summary "Treat difficult terrain created by plants as regular terrain, magical or not.\n   In terrain with plants of medium size or larger nearby, whether creature or part of the surroundings, you can Hide behind them with a bonus action"}]
               :selections [(opt5e/cantrip-selection :druid "Cervine Centaur" ::char5e/wis 1)]
              }
              ]})

(defn changeling-option-cfg [language-map]
  {:name "Changeling"
   :key :changeling
   :help ""
   :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :languages ["Common"]
   :profs {:skill-options {:choose 2 :options {:deception true :insight true :intimidation true :performance true :persuasion true}}}
   :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 1 true)
                (opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/action
                {:name "Shapechanger"
                 :page 10
                 :summary "you change your appearance and your voice. You determine the specifics of the changes, including your coloration, hair length, and sex. You can also adjust your height between Medium and Small. You can make yourself appear as a member of another race, though none of your game statistics change. You can't duplicate the appearance of an individual you've never seen, and you must adopt a form that has the same basic arrangement of limbs that you have. Your clothing and equipment aren't changed by this trait.
                 You stay in the new form until you use an action to revert to your true form or until you die."})]})

(def dwarf-option-cfg
  {:name "Dwarf",
   :key :dwarf
   :help "Dwarves are short and stout and tend to be skilled warriors and craftmen in stone and metal."
   :abilities {::char5e/con 2},
   :size :medium
   :speed 25,
   :darkvision 60
   :languages ["Dwarvish" "Common"]
   :weapon-proficiencies [:handaxe :battleaxe :light-hammer :warhammer]
   :selections [(opt5e/tool-selection [:smiths-tools :brewers-supplies :masons-tools] 1)]
   :traits [{:name "Dwarven Resilience"
             :summary "Advantage on poison saves, resistance to poison damage"
             :page 20},
            {:name "Stonecunning"
             :summary "2X prof bonus on stonework-related history checks"
             :page 20}]
   :subraces [{:name "Hill Dwarf",
               :abilities {::char5e/wis 1}
               :modifiers [(mod/modifier ?hit-point-level-bonus (+ 1 ?hit-point-level-bonus))]}
              {:name "Mountain Dwarf"
               :abilities {::char5e/str 2}
               :armor-proficiencies [:light :medium]}
              {:name "Duergar"
               :abilities {::char5e/str 1}
               :darkvision 120
               :modifiers [(mod5e/saving-throw-advantage [:charmed])
                           (mod5e/saving-throw-advantage [:paralyzed])
                           (mod5e/spells-known 1 :enlarge-reduce ::char5e/int "Duergar Dwarf" 3)
                           (mod5e/spells-known 2 :invisibility ::char5e/int "Duergar Dwarf" 5)
                           (mod5e/action
                            {:name "Duergar Magic"
                            :page 81
                            :summary (str "You can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Enlarge/Reduce, using only Enlarge,")
                                                (>= lvl 5) (conj "Invisibility"))))
                                          " on yourself once per day while not in direct sunlight, without needing material components. INT is your spellcasting ability.")})]
               :traits [{:name "Duergar Resilience"
                         :summary "Advantage on saving throws against illusions, being charmed and paralyzed."}
                        (sunlight-sensitivity 81)]}]
   :modifiers [(mod5e/damage-resistance :poison)
               (mod5e/saving-throw-advantage [:poisoned])]})

(defn goliath-option-cfg [language-map]
  {:name "Goliath"
   :key :goliath
   :abilities {::char5e/str 2 ::char5e/con 1}
   :size :medium
   :speed 30
   :languages ["Common" "Giant"]
   :profs {:skill {:athletics true}}
   :modifiers [(mod5e/damage-resistance :cold)
               (mod5e/reaction
                {:name "Stone's Endurance"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "When you take damage, reduce the damage taken by 1d12+" (?ability-bonuses ::char5e/con))})]
   :traits [(powerful-build 21)
            {:name "Mountain Born"
             :summary "You have resistance to cold damage. You also naturally acclimate to high altitudes, including elevations above 20,000 feet."}]})

(defn halfling-option-cfg [spell-lists spells-map]
  {:name "Halfling"
   :key :halfling
   :help "Halflings are small and nimble, half the height of a human, but fairly stout. They are cheerful and practical."
   :abilities {::char5e/dex 2}
   :size :small
   :speed 25
   :languages ["Halfling" "Common"]
   :modifiers [(mod5e/saving-throw-advantage [:frightened])]
   :subraces
   [{:name "Lightfoot"
     :abilities {::char5e/cha 1}
     :traits [{:name "Naturally Stealthy"
               :page 28
               :summary "Can attempt to hide behind creatures larger than you"}]}
    {:name "Stout"
     :abilities {::char5e/con 1}
     :modifiers [(mod5e/damage-resistance :poison)
                 (mod5e/saving-throw-advantage [:poisoned])]
     :traits [{:name "Stout Resilience"
               :summary "Advantage on poison saves, resistance to poison damage"}]}
    {:name "Lotusden"
     :abilities {::char5e/wis 1}
     :modifiers [(mod5e/spells-known 0 :druidcraft ::char5e/wis "Lotusden Halfling")
                 (mod5e/spells-known 1 :entangle ::char5e/wis "Lotusden Halfling" 3)
                 (mod5e/spells-known 2 :spike-growth ::char5e/wis "Lotusden Halfling" 5)
                 (mod5e/dependent-trait
                  {:name "Children of the Woods"
                  :summary (str "You know druidcraft and can cast "
                                (common/list-print
                                  (let [lvl ?total-levels]
                                    (cond-> []
                                      (>= lvl 3) (conj "Entangle")
                                      (>= lvl 5) (conj "Spike Growth"))))
                                " once per long rest, without material components. WIS is your spellcasting ability.")})]
     :traits [{:name "Timberwalk"
               :summary "Ability checks made to track you are at disadvantage and you can move through difficult terrain made of non-magical plants and overgrowth without expending extra movement."}]}
    {:name "Mark of Hospitality"
     :abilities {::char5e/cha 1}
     :modifiers (into [] (concat
                 [(mod5e/spells-known 0 :prestidigitation ::char5e/cha "Halfling")
                 (mod5e/spells-known 1 :purify-food-and-drink ::char5e/cha "Halfling")
                 (mod5e/spells-known 1 :unseen-servant ::char5e/cha "Halfling")]
                 (opt5e/subrace-spells-known spell-lists spells-map "Mark of Hospitality" 1 5)))
     :selections (opt5e/subrace-spell-selections spell-lists spells-map "Mark of Hospitality" 1 5)
     :traits [{:name "Ever Hospitable"
               :summary "Add 1d4 to any Persuasion check and ability checks involving Brewer's Tools or Cook's Utensils."}
              {:name "Innkeeper's Magic"
               :summary "You know prestidigitation and can cast Purify Foods and Drink and Unseen Servant once per long rest. Cha is your spellcasting ability."}]}]
   :traits [{:name "Lucky"
             :page 28
             :summary "Reroll 1s on d20 once"}
            {:name "Nimble"
             :page 28
             :summary "Move through the space of creatures larger than you"}
            {:name "Brave"
             :page 28
             :summary "you have advantage on saves against being frightened"}]})

;; (opt5e/race-spell-selection spell-lists spells-map (get-in sl5e/subrace-spell-lists ["Mark of Hospitality" 1 1]) 0)
;;                  (opt5e/race-spell-selection spell-lists spells-map (get-in sl5e/subrace-spell-lists ["Mark of Hospitality" 1 2]) 0)
;;                  (opt5e/race-spell-selection spell-lists spells-map (get-in sl5e/subrace-spell-lists ["Mark of Hospitality" 1 3]) 0)
;;                  (opt5e/race-spell-selection spell-lists spells-map (get-in sl5e/subrace-spell-lists ["Mark of Hospitality" 1 4]) 0)
;;                  (opt5e/race-spell-selection spell-lists spells-map (get-in sl5e/subrace-spell-lists ["Mark of Hospitality" 1 5]) 0)

(defn human-option-cfg [spell-lists spells-map language-map]
  {:name "Human"
   :key :human
   :help "Humans are physically diverse and highly adaptable. They excel in nearly every profession."
   :size :medium
   :speed 30
   :languages ["Common"]
   :subraces
   [{:name "Calishite"
     :help "test"}
    {:name "Chondathan"}
    {:name "Damaran"}
    {:name "Illuskan"}
    {:name "Mulan"}
    {:name "Rashemi"}
    {:name "Shou"}
    {:name "Tethyrian"}
    {:name "Turami"}]
   :selections [(opt5e/language-selection-aux (vals language-map) 1)
                (t/selection-cfg
                 {:name "Variant"
                  :tags #{:subrace}
                  :options [(t/option-cfg
                             {:name "Standard Human"
                              :modifiers [(mod5e/race-ability ::char5e/str 1)
                                          (mod5e/race-ability ::char5e/con 1)
                                          (mod5e/race-ability ::char5e/dex 1)
                                          (mod5e/race-ability ::char5e/int 1)
                                          (mod5e/race-ability ::char5e/wis 1)
                                          (mod5e/race-ability ::char5e/cha 1)]
                              :selections [(t/selection-cfg
                                            {:name "Proficiency"
                                             :tags #{:profs}
                                             :options [(t/option-cfg
                                                        {:name "Skill"
                                                         :selections [(t/selection-cfg
                                                                       {:name "Skill Proficiency"
                                                                        :tags #{:profs}
                                                                        :options (opt5e/skill-options skill5e/skills)})]})
                                                       (t/option-cfg
                                                        {:name "Artisan's Tool"
                                                         :selections [(opt5e/tool-selection (map :key equipment5e/artisans-tools) 1)]})
                                                       (t/option-cfg
                                                        {:name "Musical Instrument"
                                                         :selections [(opt5e/tool-selection (map :key equipment5e/musical-instruments) 1)]})
                                                       (t/option-cfg
                                                        {:name "Language"
                                                         :selections [(opt5e/language-selection-aux (vals language-map) 1)]})]})]})
                            (t/option-cfg
                             {:name "Variant Human"
                              :selections [(opt5e/feat-selection spell-lists spells-map 1)
                                           (opt5e/skill-selection 1)
                                           (opt5e/ability-increase-selection char5e/ability-keys 2 true)]})]})]})

(defn draconic-ancestry-option [{:keys [name breath-weapon]}]
  (t/option-cfg
   {:name name
    :modifiers [(mod5e/damage-resistance (:damage-type breath-weapon))
                (mod/modifier ?draconic-ancestry-breath-weapon breath-weapon)]}))

(def dragonborn-standard-option-cfg
  {:name "Dragonborn (Standard)"
   :key :dragonborn-standard
   :help "Kin to dragons, dragonborn resemble humanoid dragons, without wings or tail and standing erect. They tend to make excellent warriors."
   :abilities {::char5e/str 2 ::char5e/cha 1}
   :size :medium
   :speed 30
   :languages ["Draconic" "Common"]
   :modifiers [(mod5e/attack
                (let [breath-weapon ?draconic-ancestry-breath-weapon
                      damage-type (:damage-type breath-weapon)]
                  (merge
                   breath-weapon
                   {:name "Breath Weapon"
                    :summary (if damage-type
                               (common/safe-capitalize-kw damage-type))
                    :attack-type :area
                    :damage-die 6
                    :page 34
                    :damage-die-count (condp <= ?total-levels
                                        16 5
                                        11 4
                                        6 3
                                        2)
                    :save-dc (?spell-save-dc ::char5e/con)})))]
   :selections [(t/selection-cfg
                 {:name "Draconic Ancestry"
                  :tags #{:subrace}
                  :options (map
                            draconic-ancestry-option
                            opt5e/draconic-ancestries)})]})

(def dragonborn-option-cfg
  {:name "Dragonborn"
   :key :dragonborn
   :help "Kin to dragons, dragonborn resemble humanoid dragons, without wings or tail and standing erect. They tend to make excellent warriors."
   :size :medium
   :speed 30
   :languages ["Draconic" "Common"]
   :subraces [{:name "Draconblood"
               :abilities {::char5e/int 2 ::char5e/cha 1}
               :darkvision 60
               :modifiers [(mod5e/trait-cfg
                            {:name "Forceful Presence"
                             :page 168
                             :source :egw
                             :summary (str "When you make a Intimidation or Persuasion check, you can do so with advantage once per long rest.")})]}
              {:name "Ravenite"
               :abilities {::char5e/str 2 ::char5e/con 1}
               :darkvision 60
               :modifiers [(mod5e/reaction
                            {:name "Vengeful Assault"
                             :page 168
                             :source :egw
                             :frequency units5e/rests-1
                             :summary (str "When you take damage from a creature in range of a weapon you are wielding, you can make an attack with the weapon against that creature.")})]}]
   :modifiers [(mod5e/attack
                (let [breath-weapon ?draconic-ancestry-breath-weapon
                      damage-type (:damage-type breath-weapon)]
                  (merge
                   breath-weapon
                   {:name "Breath Weapon"
                    :summary (if damage-type
                               (common/safe-capitalize-kw damage-type))
                    :attack-type :area
                    :damage-die 6
                    :page 34
                    :damage-die-count (condp <= ?total-levels
                                        16 5
                                        11 4
                                        6 3
                                        2)
                    :save-dc (?spell-save-dc ::char5e/con)})))]
   :selections [(t/selection-cfg
                 {:name "Draconic Ancestry"
                  :tags #{:subrace}
                  :options (map
                            draconic-ancestry-option
                            opt5e/draconic-ancestries)})]})

(def gnome-option-cfg
  {:name "Gnome"
   :key :gnome
   :help "Gnomes are small, intelligent humanoids who live life with the utmost of enthusiasm."
   :abilities {::char5e/int 2}
   :size :small
   :speed 25
   :darkvision 60
   :languages ["Gnomish" "Common"]
   :modifiers [(mod5e/saving-throw-advantage [:magic] [::char5e/int ::char5e/wis ::char5e/cha])]
   :traits [{:name "Gnome Cunning"
             :page 37
             :summary "Advantage on INT, WIS, and CHA saves against magic"}]
   :subraces
   [{:name "Rock Gnome"
     :abilities {::char5e/con 1}
     :modifiers [(mod5e/tool-proficiency :tinkers-tools)]
     :traits [{:name "Artificer's Lore"
               :page 37
               :summary "Add 2X prof bonus on magical, alchemical, or technological item-related history checks."}
              {:name "Tinker"
               :page 37
               :summary "Using tinker's tools, you can spend 1 hour and 10 gp worth of materials to construct a Tiny clockwork device (AC 5, 1 hp). The device ceases to function after 24 hours (unless you spend 1 hour repairing it to keep the device functioning), or when you use your action to dismantle it; at that time, you can reclaim the materials used to create it. You can have up to three such devices active at a time. When you create a device, choose one of the following options:

Clockwork Toy: This toy is a clockwork animal, monster, or person, such as a frog, mouse, bird, dragon, or soldier. When placed on the ground, the toy moves 5 feet across the ground on each of your turns in a random direction. It makes noises as appropriate to the creature it represents.

Fire Starter: The device produces a miniature flame, which you can use to light a candle, torch, or campfire. Using the device requires your action.

Music Box: When opened, this music box plays a single song at a moderate volume. The box stops playing when it reaches the song's end or when it is closed.

May make other objects at the DM's discretion."}]}
    {:name "Forest Gnome"
     :abilities {::char5e/dex 1}
     :modifiers [(mod5e/spells-known 0 :minor-illusion ::char5e/int "Forest Gnome")]
     :traits [{:name "Speak with Small Beasts"
               :page 37
               :summary "Through sound and gestures, you may communicate simple ideas with Small or smaller beasts."}]}
    {:name "Svirfneblin (Deep Gnome)"
     :abilities {::char5e/dex 1}
     :darkvision 120
     :traits [{:name "Stone Camouflage"
               :summary "Advantage on Stealth checks to hide in rocky terrain."}]}]})

(defn deep-gnome-option-cfg [language-map]
  {:name "Deep Gnome"
   :key :deep-gnome
   :help "Gnomes are small, intelligent humanoids who live life with the utmost of enthusiasm."
   :abilities {::char5e/int 2 ::char5e/dex 1}
   :size :small
   :speed 30
   :darkvision 120
   :languages ["Common"]
   :selection [(opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/saving-throw-advantage [:magic] [::char5e/int ::char5e/wis ::char5e/cha])
              (mod5e/spells-known 1 :disguise-self ::char5e/int "Deep Gnome" 3)
              (mod5e/spells-known 2 :nondetection ::char5e/int "Deep Gnome" 5)
              (mod5e/action
              {:name "Gift of the Svirfneblin"
              :page 11
              :summary (str "You can cast "
                            (common/list-print
                              (let [lvl ?total-levels]
                                (cond-> []
                                  (>= lvl 3) (conj "Disguise Self")
                                  (>= lvl 5) (conj "Nondetection"))))
                            " on yourself once per long rest without needing material components. You can also cast these using spell slots of the appropriate level. INT, WIS, or CHA is your spellcasting ability.")})
               (mod5e/action
                {:name "Svirfneblin Camouflage"
                 :page 11
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "Make a Stealth check with advantage"})]
   :traits [{:name "Gnome"
             :page 11
             :summary "You are considered a gnome for any prerequisite or effect that requires you to be a gnome."}]})

(defn half-elf-option-cfg [language-map]
  {:name "Half-Elf"
   :key :half-elf
   :help "Half-elves are charismatic, and bear a resemblance to both their elvish and human parents and share many of the traits of each."
   :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Elvish"]
   :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 2 true)
                (assoc
                 (opt5e/skill-selection 2)
                 ::t/ref
                 [:race :half-elf :skill-proficiency])
                (opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/saving-throw-advantage [:charmed])]
   :traits [{:name "Fey Ancestry"
             :page 39
             :summary "advantage on charmed saves and immune to sleep magic"}]})

(defn half-elf-aoa-option-cfg [spell-lists spells-map]
  {:name "Half-Elf (AoA)"
   :key :half-elf-aoa
   :help "Half-elves are charismatic, and bear a resemblance to both their elvish and human parents and share many of the traits of each."
   :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Elvish"]
   :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 2 true)
                (t/selection-cfg
                 {:name "Half-Elf Heritage"
                  :tags #{:race}
                  :options [(t/option-cfg
                             {:name "Human"
                              :selections [(opt5e/skill-selection 2)]})
                            (t/option-cfg
                             {:name "High Elf"
                              :selections [(high-elf-cantrip-selection spell-lists spells-map)]})
                            (t/option-cfg
                             {:name "Dark Elf"
                              :modifiers [(mod5e/darkvision 120)]})
                            (t/option-cfg
                             {:name "Wood Elf"
                              :modifiers [(mod5e/speed 5)]})
                            (t/option-cfg
                             {:name "Sea Elf"
                              :modifiers [(mod5e/swimming-speed-equal-to-walking)]})
                            (t/option-cfg
                             {:name "Snow Elf"
                              :modifiers [(mod5e/damage-resistance :cold)]})]})]
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/immunity :magical-sleep)]
   :traits [{:name "Magical Ancestry"
             :page 39
             :summary "advantage on charmed saves and immune to sleep magic"}]})

(def half-orc-option-cfg
  {:name "Half-Orc"
   :key :half-orc
   :help "Half-orcs are strong and bear an unmistakable resemblance to their orcish parent. They tend to make excellent warriors, especially Barbarians."
   :abilities {::char5e/str 2 ::char5e/con 1}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Orc"]
   :modifiers [(mod5e/skill-proficiency :intimidation)]
   :traits [{:name "Relentless Endurance"
             :page 41
             :frequency units5e/long-rests-1
             :summary "Drop to 1 hp instead of being reduced to 0."}
            {:name "Savage Attacks"
             :page 41
             :summary "On critical hit with melee weapon attack, add additional damage dice roll"}]})

(def lenuboon-option-cfg
  {:name "Lenuboon"
   :key :lenuboon
   :help "Monkey-like race that inhabit the jungles of Ngbutu, living in tribes in homes high in the trees."
   :abilities {::char5e/dex 2 ::char5e/wis 1}
   :size :medium
   :speed 35
   :languages ["Common" "Lenuboon"]
   :modifiers [(mod5e/bonus-action
                {:name "Dextrous Feet"
                 :summary "Use your feet to manupulate an object, open or close a door or container, or pick up or set down a Tiny object"})
               (mod5e/reaction
                {:name "Lenuboon Dodge"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "Reduce damage taken by 1d6+" ?prof-bonus)})
               (mod5e/climbing-speed-equal-to-walking)]
   :traits [{:name "Glide"
             :summary "When you fall and aren't incapacitated, subtract up to 100 ft. from fall damage, and move 3 ft. horizontally for every foot descended"}
            {:name "Natural Climber"
             :summary "You have a climbing speed equal to your walking speed"}]
   })

(defn lumini-spell-selection [spell-lists spells-map spellcasting-ability]
  (opt5e/spell-selection spell-lists spells-map 
   {:spell-keys (get-in sl5e/race-spell-lists ["Lumini" 1])
    :spellcasting-ability spellcasting-ability
    :class-name "Lumini"
    :num 1
    :title "Graviturgy Spell Known"
    :exclude-ref? true}))

(defn lumini-spellcasting-ability-selection [spell-lists spells-map]
  (t/selection-cfg
   {:name "Lumini Spellcasting Ability"
    :tags #{:spells}
    :order 1
    :options [(t/option-cfg
               {:name "Intelligence"
                :selections [(lumini-spell-selection spell-lists spells-map ::char5e/int)]})
              (t/option-cfg
               {:name "Wisdom"
                :selections [(lumini-spell-selection spell-lists spells-map ::char5e/wis)]})
              (t/option-cfg
               {:name "Charisma"
                :selections [(lumini-spell-selection spell-lists spells-map ::char5e/cha)]})]}))

(defn lumini-option-cfg [spell-lists spells-map]
  {:name "Lumini"
   :key :lumini
   :help "Humanoid rabbit folk that inhabit the surface of Luminus."
   :abilities {::char5e/dex 2}
   :size :medium
   :speed 35
   :languages ["Common" "Lunar"]
   :selections (into [] (concat
                [(opt5e/ability-increase-selection [::char5e/wis ::char5e/int] 1 true)
                (lumini-spellcasting-ability-selection spell-lists spells-map)]
                (opt5e/race-spell-selections spell-lists spells-map "Lumini" 0 9)
                (opt5e/race-cantrip-selections spell-lists spells-map "Lumini" 0 0)))
   :modifiers (into [] (concat
               [(mod5e/skill-proficiency :perception)
               (mod/cum-sum-mod ?initiative ?prof-bonus)
               (mod5e/dependent-trait
                {:name "Nimble Rabbit"
                 :summary (str "Advantage on skill checks and saves that would cause you to become grappled, prone, or restrained. Initiative increases by " (common/bonus-str ?prof-bonus))})]
               (opt5e/race-spells-known spell-lists spells-map "Lumini" 1 9)))
   :traits [{:name "Moon Jump"
             :summary "High jump distance increases by 5 ft. and long jump by 10 ft."}
            {:name "Luminusborn"
             :summary "As a spellcaster, you gain access to the graviturgy spell list.\n  You learn one 1st level spell from the graviturgy spell list, and can cast it for free once per long rest. WIS, INT, or CHA is your spellcasting ability for it."}]})

(defn orc-option-cfg [language-map]
  {:name "Orc"
   :key :orc
   :help ""
   :abilities {::char5e/str 2 ::char5e/con 1}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/bonus-action
                {:name "Adrenaline Rush"
                 :page 28
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "Take the Dash action as a bonus action"})]
   :traits [{:name "Relentless Endurance"
             :page 28
             :frequency units5e/long-rests-1
             :summary "Drop to 1 hp instead of being reduced to 0"}
            (powerful-build 28)]})

(defn shifter-option-cfg [language-map]
  {:name "Shifter"
   :key :shifter
   :help ""
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :profs {:skill-options {:choose 1 :options {:acrobatics true :athletics true :intimidation true :survival true}}}
   :selections [(opt5e/language-selection-aux (vals language-map) 1)
                (opt5e/ability-increase-option 2 true char5e/ability-keys)
                (opt5e/ability-increase-selection char5e/ability-keys 1 true)
                ]
   :subraces [{:name "Beasthide"
               :traits [{:name "Beasthide"
                         :page 32
                         :summary "Gain 1d6 more temp HP when shifting. +1 AC while shifted"}]}
              {:name "Longtooth"
               :modifiers [(mod5e/bonus-action
                            {:name "Longtooth"
                             :page 32
                             :summary (str "When shifting and then while shifted, make an unarmed strike with your fangs, dealing 1d6 + " (::char5e/str ?ability-bonuses) " piercing damage")})]}
              {:name "Swiftstride"
               :modifiers [(mod5e/bonus-action
                            {:name "Longtooth"
                             :page 32
                             :summary "While shifted, your walking speed increases by 10 ft. When a creature ends its turn within 5 ft., move up to 10 ft. without provoking opportunity attacks"})]}
              {:name "Wildhunt"
               :traits [{:name "Wildhunt"
                         :page 32
                         :summary "While shifted, advantage on WIS checks, and no creature within 30 ft. can attack with advantage against you unless incapacitated"}]}]
   :modifiers [(mod5e/bonus-action
                {:name "Shifting"
                 :page 32
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "Transform for 1 minute, until you die, or revert back. While shifted, gain " (* 2 ?prof-bonus) " temp HP")})]})

(def tiefling-option-cfg
  {:name "Tiefling"
   :key :tiefling
   :help "Tieflings bear the distinct marks of their infernal ancestry: horns, a tail, pointed teeth, and solid-colored eyes. They are smart and charismatic."
   :abilities {::char5e/int 1 ::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Infernal"]
   :modifiers [(mod5e/trait-cfg
                {:name "Hellish Resistance"
                 :page 43
                 :summary "Resistance to fire damage"})
               (mod5e/dependent-trait
                {:name "Infernal Legacy"
                 :page 43
                 :summary (str "You know thaumaturgy and can cast "
                               (common/list-print
                                (let [lvl ?total-levels]
                                  (cond-> []
                                    (>= lvl 3) (conj "Hellish Rebuke")
                                    (>= lvl 5) (conj "Darkness"))))
                               " once per day. CHA is the spellcasting ability.")})
               (mod5e/damage-resistance :fire)
               (mod5e/spells-known 0 :thaumaturgy ::char5e/cha "Tiefling")
               (mod5e/spells-known 1 :hellish-rebuke ::char5e/cha "Tiefling" 3)
               (mod5e/spells-known 2 :darkness ::char5e/cha "Tiefling" 5)]})

(reg-sub
 ::races5e/plugin-subraces-map
 :<- [::races5e/plugin-subraces]
 (fn [plugin-subraces]
   (group-by :race plugin-subraces)))

(reg-sub
 ::classes5e/plugin-subclasses-map
 :<- [::classes5e/plugin-subclasses]
 (fn [plugin-subclasses]
   (group-by :class plugin-subclasses)))

(defn compare-keys [x y]
  (compare (:key x) (:key y)))

(reg-sub
 ::races5e/races
 :<- [::races5e/plugin-races]
 :<- [::races5e/plugin-subraces-map]
 :<- [::spells5e/spell-lists]
 :<- [::spells5e/spells-map]
 :<- [::langs5e/language-map]
 (fn [[plugin-races subraces-map spell-lists spells-map language-map]]
   (vec
    (into
     (sorted-set-by compare-keys)
     (map
      (fn [{:keys [key] :as race}]
        (if (subraces-map key)
          (update race :subraces concat (subraces-map key))
          race))
      (concat
       (reverse plugin-races)
       [(aasimar-option-cfg language-map)
        centaur-option-cfg
        (changeling-option-cfg language-map)
        dwarf-option-cfg
        (elf-option-cfg spell-lists spells-map language-map)
        (elf-aoa-option-cfg spell-lists spells-map language-map)
        (goliath-option-cfg language-map)
        (halfling-option-cfg spell-lists spells-map)
        (human-option-cfg spell-lists spells-map language-map)
        dragonborn-standard-option-cfg
        dragonborn-option-cfg
        gnome-option-cfg
        (deep-gnome-option-cfg language-map)
        (half-elf-option-cfg language-map)
        (half-elf-aoa-option-cfg spell-lists spells-map)
        half-orc-option-cfg
        lenuboon-option-cfg
        (lumini-option-cfg spell-lists spells-map)
        (orc-option-cfg language-map)
        (shifter-option-cfg language-map)
        tiefling-option-cfg]))))))

(defn base-class-options [spell-lists spells-map plugin-subclasses-map language-map weapons-map invocations boons]
  [(classes5e/barbarian-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/bard-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/cleric-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/druid-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/fighter-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/monk-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/paladin-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/ranger-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/rogue-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/sorcerer-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)
   (classes5e/warlock-cha-option spell-lists spells-map plugin-subclasses-map language-map  weapons-map invocations boons)
   (classes5e/warlock-int-option spell-lists spells-map plugin-subclasses-map language-map  weapons-map invocations boons)
   (classes5e/wizard-option spell-lists spells-map plugin-subclasses-map language-map weapons-map)])

(reg-sub
 ::classes5e/classes
 :<- [::spells5e/spell-lists]
 :<- [::spells5e/spells-map]
 :<- [::classes5e/plugin-subclasses-map]
 :<- [::langs5e/language-map]
 :<- [::classes5e/plugin-classes]
 :<- [::classes5e/invocations]
 :<- [::classes5e/boons]
 :<- [::mi5e/custom-and-standard-weapons-map]
 (fn [[spell-lists spells-map plugin-subclasses-map language-map plugin-classes invocations boons weapons-map] _]
   (vec
    (into
     (sorted-set-by #(compare (::t/key %1) (::t/key %2)))
     (concat
      (reverse
       (map
        (fn [plugin-class]
          (opt5e/class-option
           spell-lists
           spells-map
           plugin-subclasses-map
           language-map
           weapons-map
           plugin-class))
        plugin-classes))
      (base-class-options spell-lists spells-map plugin-subclasses-map language-map weapons-map invocations boons))))))

(reg-sub
 ::classes5e/class-map
 :<- [::classes5e/classes]
 (fn [classes]
   (common/map-by ::t/key classes)))

(reg-sub
 ::classes5e/class
 :<- [::classes5e/class-map]
 (fn [class-map [_ key]]
   (class-map key)))

(reg-sub
 ::races5e/race-map
 :<- [::races5e/races]
 (fn [races]
   (common/map-by-key races)))

(reg-sub
 ::races5e/race
 :<- [::races5e/race-map]
 (fn [race-map [_ key]]
   (race-map key)))

(reg-sub
 ::feats5e/feats
 :<- [::feats5e/plugin-feats]
 (fn [plugin-feats]
   (map
    (fn [feat]
      (assoc feat :edit-event [::feats5e/edit-feat feat]))
    plugin-feats)))

(reg-sub
 ::classes5e/invocations
 :<- [::classes5e/plugin-invocations]
 (fn [plugin-invocations]
   (map
    (fn [invocation]
      (assoc invocation :edit-event [::classes5e/edit-invocation invocation]))
    plugin-invocations)))

(reg-sub
 ::classes5e/boons
 :<- [::classes5e/plugin-boons]
 (fn [plugin-boons]
   (map
    (fn [boon]
      (assoc boon :edit-event [::classes5e/edit-boon boon]))
    plugin-boons)))

(reg-sub
 ::spells5e/plugin-spells
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (map
    (fn [spell]
      (assoc spell :edit-event [::spells5e/edit-spell spell]))
    (mapcat (comp vals ::e5/spells) plugins))))

(reg-sub
 ::spells5e/plugin-spells-map
 :<- [::spells5e/plugin-spells]
 (fn [plugin-spells _]
   (common/map-by-key plugin-spells)))

(reg-sub
 ::monsters5e/plugin-monsters
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/monsters) plugins)))

(reg-sub
 ::encounters5e/plugin-encounters
 :<- [::e5/plugin-vals]
 (fn [plugins _]
   (mapcat (comp vals ::e5/encounters) plugins)))

(defn true-types [m]
  (sequence
   (comp
    (filter val)
    (map key)
    (map common/kw-to-name))
   m))

(defn process-plugin-monster [{:keys [props traits] :as monster}]
  (let [{:keys [damage-resistance
                damage-immunity
                damage-vulnerability
                condition-immunity
                language]} props
        filtered-languages (true-types language)
        filtered-resistances (true-types damage-resistance)
        filtered-damage-immunities (true-types damage-immunity)
        filtered-vulnerabilities (true-types damage-vulnerability)
        filtered-condition-immunities (true-types condition-immunity)]
    (cond-> monster      
      (seq filtered-languages)
      (assoc :languages (s/join ", " filtered-languages))
      
      (seq filtered-resistances)
      (assoc :damage-resistances (s/join ", " filtered-resistances))

      (seq filtered-damage-immunities)
      (assoc :damage-immunities (s/join ", " filtered-damage-immunities))

      (seq filtered-vulnerabilities)
      (assoc :damage-vulnerabilities (s/join ", " filtered-vulnerabilities))

      (seq filtered-condition-immunities)
      (assoc :condition-immunities (s/join ", " filtered-condition-immunities)))))

(reg-sub
 ::monsters5e/monsters
 :<- [::monsters5e/plugin-monsters]
 (fn [plugin-monsters]
   (concat
    monsters5e/monsters
    (map
     process-plugin-monster
     plugin-monsters))))

(reg-sub
 ::encounters5e/encounters
 :<- [::encounters5e/plugin-encounters]
 (fn [plugin-encounters]
   plugin-encounters))

(reg-sub
 ::encounters5e/encounter-map
 :<- [::encounters5e/encounters]
 (fn [encounters]
   (common/map-by-key encounters)))

(reg-sub
 ::monsters5e/monster-map
 :<- [::monsters5e/monsters]
 (fn [monsters]
   (common/map-by-key monsters)))

(reg-sub
 ::monsters5e/monster
 :<- [::monsters5e/monster-map]
 (fn [monster-map [_ key]]
   (get monster-map key)))

(reg-sub
 ::monsters5e/sorted-monsters
 :<- [::monsters5e/monsters]
 :<- [::char5e/monster-sort-criteria]
 :<- [::char5e/monster-sort-direction]
 (fn [[monsters sort-criteria sort-direction]]
   (let [comparator (if (= sort-direction "asc") compare #(compare %2 %1))]
     (case sort-criteria
       "name" (sort-by :name comparator monsters)
       "cr" (sort-by :challenge comparator monsters)))))

(defn all-subtypes-removed? [subtypes hidden-subtypes]
  (and (seq subtypes)
       (seq hidden-subtypes)
       (->> subtypes
            (remove
              hidden-subtypes)
            empty?)))

(defn filter-monsters [monsters filter-text monster-filters]
  (let [lower-case-filter-text (s/lower-case filter-text)]
    (filter
      (fn [{:keys [name type subtypes size]}]
        (and (or (< (count filter-text) 3)
                 (s/includes? (s/lower-case name) lower-case-filter-text))
             (not (or (-> monster-filters :size size)
                      (-> monster-filters :type type)
                      (all-subtypes-removed? subtypes (:subtype monster-filters))))))
      monsters)))

(reg-sub
 ::monsters5e/filtered-monsters
 :<- [::monsters5e/sorted-monsters]
 :<- [::char5e/monster-text-filter]
 :<- [::char5e/monster-filters]
 (fn [[sorted-monsters filter-text monster-filters]]
   (filter-monsters sorted-monsters (or filter-text "") monster-filters)))

(reg-sub
  ::monsters5e/filtered-monster-names
  :<- [::monsters5e/filtered-monsters]
  (fn [filtered-monsters]
    (set (map :name filtered-monsters))))

(reg-sub
 ::spells5e/base-spells
 (fn [_]
   spells5e/spells))

(reg-sub
 ::spells5e/base-spells-map
 :<- [::spells5e/base-spells]
 (fn [spells]
   (common/map-by-key spells)))

(reg-sub
 ::spells5e/spells
 :<- [::spells5e/plugin-spells]
 (fn [plugin-spells]
   (into
    (sorted-set-by compare-keys)
    (concat
     (reverse plugin-spells)
     spells5e/spells))))

(reg-sub
 ::spells5e/spells-for-level
 :<- [::spells5e/spells]
 (fn [spells [_ level]]
   (filter
    #(= (:level %) level)
    spells)))

(reg-sub
 ::spells5e/spells-map
 :<- [::spells5e/spells]
 (fn [spells]
   (reduce
    (fn [m {:keys [name key level] :as spell}]
      (assoc m (or key (common/name-to-kw name)) spell))
    {}
    spells)))

(defn merge-spell-lists [& spell-lists]
  (apply
   merge-with
   concat
   spell-lists))

(reg-sub
 ::spells5e/plugin-spell-lists
 :<- [::spells5e/plugin-spells]
 (fn [plugin-spells _]
   (reduce
    (fn [lists {:keys [key level spell-lists]}]
      (reduce-kv
       (fn [l k v]
         (if v
           (update-in l [k level] conj key)
           l))
       lists
       spell-lists))
    {}
    plugin-spells)))

(reg-sub
 ::spells5e/spell-lists
 :<- [::spells5e/plugin-spell-lists]
 (fn [plugin-spell-lists]
   (merge-with
    merge-spell-lists
    sl5e/spell-lists
    plugin-spell-lists)))

(reg-sub
 ::spells5e/spellcasting-classes
 (fn []
   (map
    (fn [kw]
      {:key kw
       :name (common/kw-to-name kw)})
    [:bard :cleric :druid :paladin :ranger :sorcerer :warlock :wizard])))

(defn spell-option [spells-map [_ spell-key ability-key class-name]]
   (let [spell (spells-map spell-key)
         level (:level spell)]
     (t/option-cfg
      {:name (str level " - " (:name spell))
       :key spell-key
       :modifiers [(mod5e/spells-known
                    (:level spell)
                    spell-key
                    ability-key
                    class-name)]})))

(reg-sub
 ::spells5e/spell-option
 :<- [::spells5e/spells-map]
 spell-option)

(reg-sub
 ::spells5e/spell-options
 :<- [::spells5e/spells-map]
 :<- [::spells5e/spell-lists]
 (fn [[spells-map spell-lists] [_ ability-key class-name levels]]
   (apply concat
          (sequence
           (comp
            (map spell-lists)
            (map (fn [spell-key]
                   (spell-option spells-map [nil spell-key ability-key class-name]))))
           levels))))

(reg-sub
 ::spells5e/builder-item
 (fn [db _]
   (::spells5e/builder-item db)))

(reg-sub
 ::bg5e/builder-item
 (fn [db _]
   (::bg5e/builder-item db)))

(reg-sub
 ::races5e/builder-item
 (fn [db _]
   (::races5e/builder-item db)))

(reg-sub
 ::races5e/subrace-builder-item
 (fn [db _]
   (::races5e/subrace-builder-item db)))

(reg-sub
 ::classes5e/subclass-builder-item
 (fn [db _]
   (::classes5e/subclass-builder-item db)))

(reg-sub
 ::classes5e/invocation-builder-item
 (fn [db _]
   (::classes5e/invocation-builder-item db)))

(reg-sub
 ::classes5e/boon-builder-item
 (fn [db _]
   (::classes5e/boon-builder-item db)))

(reg-sub
 ::classes5e/builder-item
 (fn [db _]
   (::classes5e/builder-item db)))

(reg-sub
 ::feats5e/builder-item
 (fn [db _]
   (::feats5e/builder-item db)))

(reg-sub
 ::langs5e/builder-item
 (fn [db _]
   (::langs5e/builder-item db)))

(reg-sub
 ::monsters5e/builder-item
 (fn [db _]
   (::monsters5e/builder-item db)))

(reg-sub
 ::encounters5e/builder-item
 (fn [db _]
   (::encounters5e/builder-item db)))

(reg-sub
 ::selections5e/builder-item
 (fn [db _]
   (::selections5e/builder-item db)))

(reg-sub
 ::combat5e/tracker-item
 (fn [db _]
   (::combat5e/tracker-item db)))


(reg-sub
 ::monsters5e/alignments
 (fn [db _]
   (into (sorted-set) (map :alignment) monsters5e/monsters)))

(reg-sub
 ::monsters5e/challenge-ratings
 (fn [db _]
   (into (sorted-set)
         (keep
          (fn [pair]
            (when (pos? (second pair))
            (first pair)))
            monsters5e/challenge-ratings))))

(reg-sub
 ::classes5e/has-prof?
 :<- [::classes5e/builder-item]
 (fn [class [_ prof-type prof-key]]
   (some? (get-in class [:profs prof-type prof-key]))))
