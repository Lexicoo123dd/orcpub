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

(def faceless-bg
  {:name "Faceless"
   :help "You have two personas."
   :traits [{:name "Dual Personalities"
             :summary "Upon donning a disguise and behaving as your persona, you are unidentifiable as your true self. By removing your disguise and revealing your true face, you are no longer identifiable as your persona. This allows you to change appearances between your two personalities as often as you wish, using one to hide the other or serve as convenient camouflage. However, should someone realize the connection between your persona and your true self, your deception might lose its effectiveness."}]
   :profs {:skill {:deception true :intimidation true}
           :tool {:disguise-kit true}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:disguise-kit 1
               :costume 1
               :pouch 1}
   :treasure {:gp 10}})

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

(def monastic-bg
  {:name "Monastic"
   :help ""
   :traits [{:name "Monastic Influence"
             :summary "Your monastic insignia tells those familiar with your monastery who you are and where you come from. This can bring you great respect or make you a target, depending on how those who see it feel about it. If you are in good standing with them, they may offer you shelter or help that they may not offer someone else."}]
   :profs {:skill {:athletics true :perception true}
           :tool-options {:artisans-tool 1}
           :language-options {:choose 1 :options {:any true}}}
   :equipment {:clothes-common 1
               :pouch 1}
   :equipment-choice [opt5e/artisans-tools-choice-cfg]
   :custom-equipment {"Monastic Symbol" 1}
   :treasure {:gp 10}})

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

(def smuggler-bg
  {:name "Smuggler"
   :help "You are acquainted with a network of smugglers who are willing to help you out of tight situations."
   :traits [{:name "Down Low"
             :summary "You are acquainted with a network of smugglers who are willing to help you out of tight situations. While in a particular town, city, or other similarly sized community (DM's discretion), you and your companions can stay for free in safe houses. Safe houses provide a poor lifestyle. While staying at a safe house, you can choose to keep your presence (and that of your companions) a secret."}]
   :profs {:skill {:athletics true :deception true}
           :tool {:water-vehicles true}}
   :equipment {:clothes-common 1
               :pouch 1}
   :equipment-choices [{:name "Dice or Cards"
                        :options {:dice-set 1
                                  :playing-card-set 1}}]
   :custom-equipment {"Fancy leather vest or a pair of boots" 1}
   :treasure {:gp 15}})

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

(def urban-bounty-hunter-bg
  {:name "Urban Bounty Hunter"
   :traits [{:name "Ear to the Ground"
             :summary "You are in frequent contact with people in the segment of society that your chosen quarries move through. These people might be associated with the criminal underworld, the rough-and-tumble folk of the streets, or members of high society. This connection comes in the form of a contact in any city you visit, a person who provides information about the people and places of the local area."}]
   :profs {:skill-options {:choose 2 :options {:deception true :insight true :persuasion true :stealth true}}}
   :selections [(t/selection-cfg
                 {:name "Tool Proficiency"
                  :tags #{:profs}
                  :min 2
                  :max 2
                  :options [(t/option-cfg
                             {:name "Gaming Set"
                              :selections [(opt5e/tool-selection (map :key equipment5e/gaming-sets) 1)]})
                            (t/option-cfg
                             {:name "Musical Instrument"
                              :selections [(opt5e/tool-selection (map :key equipment5e/musical-instruments) 1)]})
                            (t/option-cfg
                             {:name "Theives' Tools"
                              :modifiers [(mod5e/tool-proficiency :thieves-tools)]})]})]
   :equipment {:pouch 1}
   :equipment-choices [{:name "Set of Clothes"
                        :options {:clothes-common 1
                                  :costume 1
                                  :clothes-fine 1
                                  :clothes-traveler-s 1}}]
   :treasure {:gp 20}
   })

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
       faceless-bg
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
       monastic-bg
       outlander-bg
       sage-bg
       sailor-bg
       pirate-bg
       smuggler-bg
       soldier-bg
       urban-bounty-hunter-bg
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
   {:name "Harpian"
    :key :harpian}
   {:name "Lamia"
    :key :lamia}
   {:name "Lenuboon"
    :key :lenuboon}
   {:name "Lunar"
    :key :lunar}
   {:name "Mystic"
    :key :mystic}
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
   :summary "You have disadvantage on attack rolls and on Wisdom (Perception) checks that rely on sight when you, the target of your attack, or whatever you are trying to perceive is in direct sunlight."
   :source (or source :phb)
   :page 24})

(def mask-of-the-wild-mod
  (mod5e/trait-cfg
   {:name "Mask of the Wild"
    :page 24
    :summary "You can attempt to hide even when you are only lightly obscured by foliage, heavy rain, falling snow, mist, and other natural phenomena."}))

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
   ;; :abilities {::char5e/dex 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Elvish" "Common"]
   :darkvision 60
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/immunity :magical-sleep)
               (mod5e/skill-proficiency :perception)]
   :subraces
   [{:name "High Elf"
     ;; :abilities {::char5e/int 1}
     :selections [(high-elf-cantrip-selection spell-lists spells-map)
                  (opt5e/language-selection-aux (vals language-map) 1)]
     :modifiers [elf-weapon-training-mods]}
    #_{:name "Wood Elf"
     ;; :abilities {::char5e/wis 1}
     :modifiers [(mod5e/speed 5)
                 mask-of-the-wild-mod
                 elf-weapon-training-mods]}
    #_{:name "Dark Elf (Drow)"
     ;; :abilities {::char5e/cha 1}
     :traits [(sunlight-sensitivity 24)]
     :modifiers (conj drow-magic-mods
                      (mod5e/weapon-proficiency :rapier)
                      (mod5e/weapon-proficiency :shortsword)
                      (mod5e/weapon-proficiency :crossbow-hand)
                      (mod5e/darkvision 120))}]
   :traits [{:name "Fey Ancestry"
             :page 23
             :summary "You have advantage on saving throws against being charmed, and magic can't put you to sleep"}
            {:name "Trance"
             :page 23
             :summary "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep"}]})

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
   ;; :abilities {::char5e/dex 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Elvish" "Common"]
   :darkvision 60
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/immunity :magical-sleep)
               (mod5e/skill-proficiency :perception)]
   :subraces [{:name "High Elf"
               ;; :abilities {::char5e/int 1}
               :selections [(opt5e/language-selection-aux (vals language-map) 1)
                            (high-elf-aoa-spell-selection spell-lists spells-map 0 1)
                            (high-elf-aoa-spell-selection spell-lists spells-map 1 3)
                            (high-elf-aoa-spell-selection spell-lists spells-map 2 5)]
               :modifiers [elf-weapon-training-mods
                           ]}
              {:name "Dark Elf"
               ;; :abilities {::char5e/cha 1}
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
               ;; :abilities {::char5e/wis 1}
               :speed 35
               :selections [(wood-elf-aoa-cantrip-selection spell-lists spells-map)]
               :modifiers [(mod5e/skill-proficiency :nature)]
               :weapon-proficiencies [:longsword :shortsword :shortbow :longbow]
               :traits [{:name "Mask of the Wild"
                         :summary "You can attempt to hide even when you are only lightly obscured by foliage, heavy rain, falling snow, mist, and other natural phenomena"}]}
              {:name "Sea Elf"
               ;; :abilities {::char5e/con 1}
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
                         :summary "You can breathe air and water, and you have resistance to cold damage"}
                        {:name "Friend of the Sea"
                         :summary "Aquatic animals have an extraordinary affinity with your people. You can communicate simple ideas to any Beast that has a swimming speed. It can understand your words, though you have no special ability to understand it in return. Additionally, you have a swimspeed equal to your walking speed"}]}
              {:name "Snow Elf"
               ;; :abilities {::char5e/wis 1}
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
                             :summary (str "By using a reaction against a creature using their reaction, you can cause a cold wind to blow around you, causing an enemy within 20 feet of you to shiver and make a constitution saving throw against DC 8 + Your Constitution + your Proficiency bonus ("
                                           (?spell-save-dc ::char5e/con) "). On a failed save, the creature loses its reaction until the end of its turn. This feature works very similarly to the spell counterspell")})]
               :weapon-proficiencies [:longbow :morningstar :pike :whip]
               }
              ]
   :traits [{:name "Magical Ancestry"
             :page 23
             :summary "You have advantage on saving throws against being charmed, and magic can’t put you to sleep"}
            {:name "Trance"
             :page 23
             :summary (str "Elves don’t need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep."
                           "\n\nWhenever you finish this trance, you can gain a proficiency that you don’t have, either with a weapon or a tool of your choice selected from the Player’s Handbook. You mystically acquire these proficiencies by drawing them from shared elven memory, and you retain them until you finish your next long rest")}]
   })

(def genasi-option-cfg
  {:name "Genasi"
   :key :genasi
   :custom-ability-scores true
   :sizes [:small :medium]
   :darkvision 60
   :languages ["Common" "Primordial"]
  ;;  :selections [(t/selection-cfg
  ;;                {:name "Size"
  ;;                 :tags #{:race}
  ;;                 :options [(t/option-cfg
  ;;                            {:name "Small"
  ;;                             :modifiers [(mod5e/size :small)]})
  ;;                           (t/option-cfg
  ;;                            {:name "Medium"
  ;;                             :modifiers [(mod5e/size :medium)]})]})]
   :subraces [{:name "Fire Genasi"
               ;; :abilities {::char5e/con 2 ::char5e/int 1}
               :speed 30
               :modifiers [(mod5e/damage-resistance :fire)
                           (mod5e/dependent-trait
                            {:name "Reach to the Blaze"
                             :summary (str "You know produce flame and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Burning Hands")
                                                (>= lvl 5) (conj "Flame Blade"))))
                                          " once per long rest, without requiring material components. You can also cast these spells using spell slots of the appropriate level. INT is the spellcasting ability.")})
                           (mod5e/spells-known 0 :produce-flame ::char5e/int "Genasi")
                           (mod5e/spells-known 1 :burning-hands ::char5e/int "Genasi" 3)
                           (mod5e/spells-known 2 :flame-blade ::char5e/int "Genasi" 5)]}
              {:name "Air Genasi"
               ;; :abilities {::char5e/con 2 ::char5e/dex 1}
               :speed 35
               :modifiers [(mod5e/damage-resistance :thunder)
                           (mod5e/dependent-trait
                            {:name "Mingle with the Wind"
                             :summary (str "You know gust and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Feather Fall")
                                                (>= lvl 5) (conj "Levitate"))))
                                          " once per long rest, without requiring material components. You can also cast these spells using spell slots of the appropriate level. DEX is the spellcasting ability.")})
                           (mod5e/spells-known 0 :gust ::char5e/dex "Genasi")
                           (mod5e/spells-known 1 :feather-fall ::char5e/dex "Genasi" 3)
                           (mod5e/spells-known 2 :levitate ::char5e/dex "Genasi" 5)]
               :traits [{:name "Unending Breath"
                         :summary "You can hold your breath indefinitely while you’re not incapacitated"}]}
              {:name "Earth Genasi"
               ;; :abilities {::char5e/con 2 ::char5e/str 1}
               :speed 30
               :modifiers [(mod5e/dependent-trait
                            {:name "Merge with Stone"
                             :summary (str "You know blade ward and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Earth Tremor")
                                                (>= lvl 5) (conj "Maximilian's Earthen Grasp"))))
                                          " once per long rest, without requiring material components. You can also cast these spells using spell slots of the appropriate level. STR is the spellcasting ability.")})
                           (mod5e/spells-known 0 :blade-ward ::char5e/str "Genasi")
                           (mod5e/spells-known 1 :earth-tremor ::char5e/str "Genasi" 3)
                           (mod5e/spells-known 2 :maximilians-earthen-grasp ::char5e/str "Genasi" 5)
                           (mod5e/bonus-action
                            {:name "Cast Blade Ward"
                             :frequency (units5e/long-rests ?prof-bonus)
                             :summary "Cast Blade Ward using a bonus action"})]
               :traits [{:name "Earth Walk"
                         :summary "You can move across difficult terrain without expending extra movement if you are using your walking speed on the ground or a floor"}]}
              {:name "Lightning Genasi"
               ;; :abilities {::char5e/con 2 ::char5e/cha 1}
               :speed 30
               :modifiers [(mod5e/damage-resistance :lightning)
                           (mod5e/skill-proficiency :insight)
                           (mod5e/dependent-trait
                            {:name "Rightous Lightning"
                             :summary (str "You know lightning grasp and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Thunderwave")
                                                (>= lvl 5) (conj "Kinetic Jaunt"))))
                                          " once per long rest, without requiring material components. You can also cast these spells using spell slots of the appropriate level. CHA is the spellcasting ability.")})
                           (mod5e/spells-known 0 :lightning-grasp ::char5e/cha "Genasi")
                           (mod5e/spells-known 1 :thunderwave ::char5e/cha "Genasi" 3)
                           (mod5e/spells-known 2 :kinetic-jaunt ::char5e/cha "Genasi" 5)]
               :traits [{:name "Lightning Adapted Hearing"
                         :summary "you cannot be blinded through magical means"}]}
              {:name "Water Genasi"
               ;; :abilities {::char5e/con 2 ::char5e/wis 1}
               :speed 30
               :modifiers [(mod5e/damage-resistance :acid)
                           (mod5e/dependent-trait
                            {:name "Call to the Wave"
                             :summary (str "You know shape water and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Create and Destroy Water")
                                                (>= lvl 5) (conj "Water Walk"))))
                                          " once per long rest, without requiring material components. You can also cast these spells using spell slots of the appropriate level. WIS is the spellcasting ability.")})
                           (mod5e/spells-known 0 :shape-water ::char5e/wis "Genasi")
                           (mod5e/spells-known 1 :create-and-destroy-water ::char5e/wis "Genasi" 3)
                           (mod5e/spells-known 2 :water-walk ::char5e/wis "Genasi" 5)]
               :traits [{:name "Amphibious"
                         :summary "you can breathe air and water"}]}
               ]
   }
)

(defn goblin-aoa-option-cfg [spell-lists spells-map]
  {:name "Goblin (AoA)"
   :key :goblin-aoa
   ;; :abilities {::char5e/dex 1}
   :custom-ability-scores true
   :size :small
   :speed 30
   :darkvision 60
   :languages ["Common" "Goblin"]
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/bonus-action
                {:name "Nimble Escape"
                 :frequency units5e/turns-1
                 :summary "You can take the Disengage or Hide action as a bonus action on each of your turns"})
               (mod5e/dependent-trait
                {:name "Fury of the Small"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "When you damage a creature with an attack or a spell and the creature’s size is larger than yours, you can cause the attack or spell to deal extra damage to the creature. The extra damage equals your proficiency bonus")})]
   :traits [{:name "Fey Ancestry"
             :summary "You have advantage on saving throws you make to avoid or end the charmed condition on yourself"}]
   :subraces [{:name "Forest Goblin"
               ;; :abilities {::char5e/dex 1 ::char5e/con 1}
               :modifiers [(mod5e/tool-proficiency :leatherworkers-tools)]
               :traits [{:name "Keen Hearing"
                         :summary "You have advantage on Perception checks that rely on hearing"}
                        {:name "Resourceful Hunter"
                         :summary "During a short rest, you can use the corpse of a small or larger beast with usable materials to create a dagger, spear, light shield, or 1d6 arrows, darts or blowing needles"}]}
              {:name "Desert Goblin"
               ;; :abilities {::char5e/str 1 ::char5e/con 1}
               :modifiers [(mod5e/damage-resistance :fire)]
               :traits [{:name "Shield Expert"
                         :summary (str "Handling shields of all kinds in creative assaults, you can use them to their maximal potential."
                                       "\n\nYou can carry a shield with the bulky property without a movement speed penalty."
                                       "\n\nAdditionally, you can use shields of armour one level above the armours you are proficient with. (e.g If you are proficient with light and medium armour, you can use a heavy shield)")}]}
              {:name "Swamp Goblin"
               ;; :abilities {::char5e/con 1}
               :selections [
                            ;; (opt5e/ability-increase-selection [::char5e/wis ::char5e/cha] 1 true)
                            (t/selection-cfg
                             {:name "Cantrip"
                              :order 1
                              :tags #{:spells}
                              :options (concat (opt5e/spell-options spells-map (get-in spell-lists [:druid 0]) ::char5e/wis "Druid") (opt5e/spell-options spells-map (get-in spell-lists [:warlock 0]) ::char5e/cha "Warlock"))
                              :min 1
                              :max 1})]
               :modifiers [(mod5e/tool-proficiency :poisoners-kit)]
               :traits [{:name "Swamp Hunter"
                         :summary "You have advantage on Nature checks used to extract poison from a beasts"}]}
              {:name "Frost Goblin"
               ;; :abilities {::char5e/con 1 ::char5e/int 1}
               :modifiers [(mod5e/damage-resistance :cold)
                           (mod5e/weapon-proficiency :crossbow-hand)
                           (mod5e/weapon-proficiency :crossbow-light)
                           (mod5e/tool-proficiency :tinkers-tools)]}
             ]
  })

(defn aasimar-option-cfg [language-map]
  {:name "Aasimar"
   :key :aasimar
   :help ""
   ;; :abilities {::char5e/cha 2}
   :custom-ability-scores true
   :sizes [:small :medium]
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :selections [
                ;; (opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 1 true)
                (opt5e/language-selection-aux (vals language-map) 1)
                ;; (t/selection-cfg
                ;;  {:name "Size"
                ;;   :tags #{:race}
                ;;   :options [(t/option-cfg
                ;;              {:name "Small"
                ;;               :modifiers [(mod5e/size :small)]})
                ;;             (t/option-cfg
                ;;              {:name "Medium"
                ;;               :modifiers [(mod5e/size :medium)]})]})
                              ]
   :subraces [{:name "Necrotic Shroud"
               :modifiers [(mod5e/bonus-action
                            {:name "Necrotic Shroud"
                             :level 3
                             :page 7
                             :source :mpmm
                             :summary (str "Your eyes briefly become pools of darkness, and ghostly, flightless wings sprout from your back temporarily. Creatures other than your allies within 10 feet of you that can see you must succeed on a Charisma saving throw (DC 8 + your proficiency bonus + your Charisma modifier ["
                                           (?spell-save-dc ::char5e/cha) "]) or become frightened of you until the end of your next turn. Until the transformation ends, once on each of your turns, you can deal extra necrotic damage to one target when you deal damage to it with an attack or a spell. The extra damage equals your proficiency bonus")})]}
              {:name "Radiant Consumption"
               :modifiers [(mod5e/bonus-action
                            {:name "Radiant Consumption"
                             :level 3
                             :page 7
                             :source :mpmm
                             :summary (str "Searing light temporarily radiates from your eyes and mouth. For the duration, you shed bright light in a 10-foot radius and dim light for an additional 10 feet, and at the end of each of your turns, each creature within 10 feet of you takes radiant damage equal to your proficiency bonus. Until the transformation ends, once on each of your turns, you can deal extra radiant damage to one target when you deal damage to it with an attack or a spell. The extra damage equals your proficiency bonus")})]}
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
                 :frequency units5e/long-rests-1
                 :summary "As an action, you can touch a creature and roll a number of d4s equal to your proficiency bonus. The creature regains a number of hit points equal to the total rolled. Once you use this trait, you can't use it again until you finish a long rest"})
               (mod5e/damage-resistance :necrotic)
               (mod5e/damage-resistance :radiant)
               (mod5e/spells-known 0 :light ::char5e/cha "Aasimar")]})

(def centaur-option-cfg
  {:name "Centaur"
   :key :centaur
   :custom-ability-scores true
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
               ;; :abilities {::char5e/str 2 ::char5e/con 1}
               :size :medium
               :speed 40
               :profs {:skill-options {:choose 1 :options {:animal-handling true :athletics true :perception true :nature true :survival true}}}
               :weapon-proficiencies [:battleaxe :flail :glaive :greataxe :greatsword :halberd :lance :longsword :maul :morningstar :pike :rapier :scimitar :shortsword :trident :war-pick :warhammer :whip :longbow]
               :modifiers [(mod5e/bonus-action
                            {:name "Charge"
                            :summary "If you move at least 30 feet in a straight line, you can make an attack with your hooves or take the Dash action as a bonus action"})]}
              {:name "Ovine"
               ;; :abilities {::char5e/con 2 ::char5e/dex 1}
               :size :medium
               :speed 35
               :weapon-proficiencies [:longbow]
               :profs {:skill-options {:choose 1 :options {:animal-handling true :athletics true :perception true :nature true :survival true}}}
               :modifiers [(mod5e/tool-proficiency :weavers-tools)]
               :traits [{:name "Soft Pelt"
                        :summary "You have resistance to bludgeoning damage from weapon attacks while not wearing heavy armor"}]
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
               ;; :abilities {::char5e/dex 2 ::char5e/con 1}
               :size :small
               :speed 30
               :profs {:skill {:athletics true}
                       :tool {:masons-tools true}
                       :skill-options {:choose 1 :options {:acrobatics true :perception true :nature true :survival true}}}
               :traits [{:name "Skilled Climber"
                         :summary "You have a climbing speed equal to your walking speed as long as the climb is not 90 degrees or more and provides a minimal amount of footing (Not completely smooth surface)"}
                        {:name "Evasive Bounce"
                         :summary (str "If you move at least 10 feet towards an enemy in a straight line and make a melee weapon attack, you can bounce off the enemy and move yourself 5 feet away from them without provoking opportunity attacks."
                                       "\n\nYou can only bounce away toward where you came from and only once off the same target.")}]}
              {:name "Cervine"
               ;; :abilities {::char5e/dex 1 ::char5e/con 1 ::char5e/wis 1}
               :size :medium
               :speed 40
               :profs {:skill {:nature true}
                       :tool {:herbalism-kit true}
                       :skill-options {:choose 1 :options {:acrobatics true :athletics true :perception true :stealth true :medicine true}}}
               :modifiers [(mod5e/spells-known 2 :locate-animals-or-plants ::char5e/wis "Cervine Centaur")]
               :traits [{:name "Connection to the Wilds"
                        :summary "You can cast Locate Animals or Plants at will with a radius of 500 feet, or 5 miles if cast as a Ritual"}
                       {:name "Undergrowth Mobility"
                        :summary (str "You can treat difficult terrain created by plants as regular terrain, magical or not."
                                      "\n\nIn terrain with plants of medium size or larger nearby, whether creature or part of the surroundings, you can take the Hide action behind them as a bonus action")}]
               :selections [(opt5e/cantrip-selection :druid "Cervine Centaur" ::char5e/wis 1)]
              }
              ]})

(defn changeling-option-cfg [language-map]
  {:name "Changeling"
   :key :changeling
   :help ""
   ;; :abilities {::char5e/cha 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Common"]
   :profs {:skill-options {:choose 2 :options {:deception true :insight true :intimidation true :performance true :persuasion true}}}
   :selections [
                ;; (opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 1 true)
                (opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/action
                {:name "Shapechanger"
                 :page 10
                 :summary (str "As an action, you change your appearance and your voice. You determine the specifics of the changes, including your coloration, hair length, and sex. You can also adjust your height between Medium and Small. You can make yourself appear as a member of another race, though none of your game statistics change. You can't duplicate the appearance of an individual you've never seen, and you must adopt a form that has the same basic arrangement of limbs that you have. Your clothing and equipment aren't changed by this trait."
                               "\n\nYou stay in the new form until you use an action to revert to your true form or until you die.")})]})

(defn duergar-magic-option [ability]
  [(mod5e/spells-known 2 :enlarge-reduce ability "Duergar" 3)
   (mod5e/spells-known 2 :invisibility ability "Duergar" 5)])

(def duergar-option-cfg
  {:name "Duergar"
   :key :duergar
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 120
   :languages ["Dwarvish" "Common"]
   :modifiers [(mod5e/damage-resistance :poison)
               (mod5e/saving-throw-advantage [:poisoned])
               (mod5e/saving-throw-advantage [:charmed])
               (mod5e/saving-throw-advantage [:paralyzed])
               (mod5e/spells-known 1 :enlarge-reduce nil "Duergar" 3)
               (mod5e/spells-known 2 :invisibility nil "Duergar" 5)
               (mod5e/action
                {:name "Duergar Magic"
                 :summary (str "You can cast "
                              (common/list-print
                                (let [lvl ?total-levels]
                                  (cond-> []
                                    (>= lvl 3) (conj "Enlarge/Reduce")
                                    (>= lvl 5) (conj "Invisibility"))))
                              " on yourself once per long rest, without needing material components. INT, WIS, or CHA is your spellcasting ability.")})]
   :selections [(t/selection-cfg
                 {:name "Duergar Spellcasting Ability"
                  :tags #{:race}
                  :options [(t/option-cfg
                             {:name "Intelligence"
                              :modifiers (duergar-magic-option ::char5e/int)})
                            (t/option-cfg
                             {:name "Wisdom"
                              :modifiers (duergar-magic-option ::char5e/wis)})
                            (t/option-cfg
                             {:name "Charisma"
                              :modifiers (duergar-magic-option ::char5e/cha)})]})]
   :traits [{:name "Dwarven Resilience"
             :summary "You have advantage on saving throws you make to avoid or end the poisoned condition on yourself. You also have resistance to poison damage"}
            {:name "Psionic Fortitude"
             :summary "You have advantage on saving throws you make to avoid or end the charmed or stunned condition on yourself"}]})

(def dwarf-option-cfg
  {:name "Dwarf",
   :key :dwarf
   :help "Dwarves are short and stout and tend to be skilled warriors and craftmen in stone and metal."
   ;; :abilities {::char5e/con 2},
   :custom-ability-scores true
   :size :medium
   :speed 25,
   :darkvision 60
   :languages ["Dwarvish" "Common"]
   :weapon-proficiencies [:handaxe :battleaxe :light-hammer :warhammer]
   :selections [(opt5e/tool-selection [:smiths-tools :brewers-supplies :masons-tools] 1)]
   :traits [{:name "Dwarven Resilience"
             :summary "You have advantage on saving throws against poison, and you have resistance against poison damage"
             :page 20},
            {:name "Stonecunning"
             :summary "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus"
             :page 20}]
   :subraces [{:name "Hill Dwarf",
               ;; :abilities {::char5e/wis 1}
               :modifiers [(mod/modifier ?hit-point-level-bonus (+ 1 ?hit-point-level-bonus))]}
              {:name "Mountain Dwarf"
               ;; :abilities {::char5e/str 2}
               :armor-proficiencies [:light :medium]}
              {:name "Duergar"
               ;; :abilities {::char5e/str 1}
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
                         :summary "You have advantage on saving throws against poison, and you have resistance against poison damage. You also have advantage on saving throws against illusions and against being charmed or paralyzed"}
                        (sunlight-sensitivity 81)]}]
   :modifiers [(mod5e/damage-resistance :poison)
               (mod5e/saving-throw-advantage [:poisoned])]})

(defn goliath-option-cfg [language-map]
  {:name "Goliath"
   :key :goliath
   ;; :abilities {::char5e/str 2 ::char5e/con 1}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Common" "Giant"]
   :profs {:skill {:athletics true}}
   :modifiers [(mod5e/damage-resistance :cold)
               (mod5e/reaction
                {:name "Stone's Endurance"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "You can supernaturally draw on unyielding stone to shrug off harm. When you take damage, you can use your reaction to roll a d12. Add your Constitution modifier to the number rolled and reduce the damage by that total")})]
   :traits [(powerful-build 21)
            {:name "Mountain Born"
             :summary "You have resistance to cold damage. You also naturally acclimate to high altitudes, even if you've never been to one. This includes elevations above 20,000 feet"}]})

(defn halfling-option-cfg [spell-lists spells-map]
  {:name "Halfling"
   :key :halfling
   :help "Halflings are small and nimble, half the height of a human, but fairly stout. They are cheerful and practical."
   ;; :abilities {::char5e/dex 2}
   :custom-ability-scores true
   :size :small
   :speed 25
   :languages ["Halfling" "Common"]
   :modifiers [(mod5e/saving-throw-advantage [:frightened])]
   :subraces
   [{:name "Lightfoot"
     ;; :abilities {::char5e/cha 1}
     :traits [{:name "Naturally Stealthy"
               :page 28
               :summary "You can attempt to hide even when you are obscured only by a creature that is at least one size larger than you"}]}
    {:name "Stout"
     ;; :abilities {::char5e/con 1}
     :modifiers [(mod5e/damage-resistance :poison)
                 (mod5e/saving-throw-advantage [:poisoned])]
     :traits [{:name "Stout Resilience"
               :summary "You have advantage on saving throws against poison, and you have resistance against poison damage"}]}
    {:name "Ghostwise"
    ;;  :abilities {::char5e/wis 1}
     :traits [{:name "Silent Speech"
               :summary "You can speak telepathically to any creature within 30 feet of you. The creature understands you only if the two of you share a language. You can speak telepathically in this way to one creature at a time"}]}
    {:name "Lotusden"
     ;; :abilities {::char5e/wis 1}
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
     ;; :abilities {::char5e/cha 1}
     :modifiers (into [] (concat
                 [(mod5e/spells-known 0 :prestidigitation ::char5e/cha "Halfling")
                 (mod5e/spells-known 1 :purify-food-and-drink ::char5e/cha "Halfling")
                 (mod5e/spells-known 1 :unseen-servant ::char5e/cha "Halfling")]
                 (opt5e/subrace-spells-known spell-lists spells-map "Mark of Hospitality" 1 5)))
     :selections (opt5e/subrace-spell-selections spell-lists spells-map "Mark of Hospitality" 1 5)
     :traits [{:name "Ever Hospitable"
               :summary "When you make a Charisma (Persuasion) check or an ability check involving brewer's supplies or cook's utensils, you can roll a d4 and add the number rolled to the ability check"}
              {:name "Innkeeper's Magic"
               :summary "You know the prestidigitation cantrip. You can also cast the purify food and drink and unseen servant spells with this trait. Once you cast either spell with this trait, you can't cast that spell with it again until you finish long rest. Charisma is your spellcasting ability for these spells"}]}]
   :traits [{:name "Lucky"
             :page 28
             :summary "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll"}
            {:name "Halfling Nimbleness"
             :page 28
             :summary "You can move through the space of any creature that is of a size larger than yours"}
            {:name "Brave"
             :page 28
             :summary "You have advantage on saving throws against being frightened"}]})

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
  ;;  :subraces
  ;;  [{:name "Calishite"
  ;;    :help "test"}
  ;;   {:name "Chondathan"}
  ;;   {:name "Damaran"}
  ;;   {:name "Illuskan"}
  ;;   {:name "Mulan"}
  ;;   {:name "Rashemi"}
  ;;   {:name "Shou"}
  ;;   {:name "Tethyrian"}
  ;;   {:name "Turami"}]
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

(defn kitsune-spellcasting-ability-option [name ability]
  (t/option-cfg
    {:name name
     :modifiers [(mod5e/spells-known 1 :disguise-self ability "Kitsune")
                 (mod5e/spells-known 2 :misty-step ability "Kitsune")
                 (mod5e/spells-known 5 :modify-memory ability "Kitsune")]}))

(def kitsune-option-cfg
  {:name "Kitsune"
   :key :kitsune
   ;; :abilities {::char5e/cha 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Common" "Sylvan"]
   :darkvision 60
   :profs {:skill-options {:choose 1 :options {:persuasion true :deception true}}}
   :traits [{:name "Type"
             :summary "You are humanoid but count as Fey for the purpose of any effects, abilities, or features"}
            {:name "Fey Resistance"
             :summary "You have advantage on saves against being charmed"}]
   :modifiers [(mod5e/saving-throw-advantage [:charmed])
               (mod5e/bonus-action
                {:name "Kitsune Ancestry"
                 :summary (str "You possess Kitsune like features, which mostly include fox ears and a tail. You can hide/reveal these by using a bonus action. Spells like Detect Magic are able to discern that you are hiding something with illusion magic and when you drop unconcious or die, your Kitsune features are revealed if hidden previously."
                               "\n\nWhile your true form with ears and tails is revealed, your Spellsave DC for spells that cause the charmed condition is calculated as 8 + Spellcasting Modifier + Proficiency Bonus + your Character level/4 (minimum of 1)."
                               "\n\nAdditionally, you can speak with foxes like under the influence of the Speak with Animals spell.")})
               (mod5e/dependent-trait
                {:name "Kitsune Magic"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "You get charges equal to your proficiency bonus with which you can cast a list of spells. All these spells pull from the same charges, so you can cast any combination of these spells only an amount equal to your proficiency bonus. You regain one charge of this feature upon finishing a long rest (At most = Proficiency Bonus)."
                               "\n\nThe Spells are Disguise Self and Misty Step. Additionally, you get access to a special weaker variant of Modify Memory, which you can cast only with this feature."
                               "\n\nThe changes of this Modify Memory are that you can only affect the target’s memory of an event that happened within the last 10 minutes that lasted no longer than 1 minute. You choose if your spellcasting ability for these spells is Intelligence, Wisdom or Charisma while creating your character.")})
               (mod5e/spells-known 1 :disguise-self nil "Kitsune")
               (mod5e/spells-known 2 :misty-step nil "Kitsune")
               (mod5e/spells-known 5 :modify-memory nil "Kitsune")]
   :selections [
                ;; (opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 1)
                (t/selection-cfg
                 {:name "Kitsune Spellcasting Ability"
                  :tags #{:spells}
                  :options [(kitsune-spellcasting-ability-option "Intelligence" ::char5e/int)
                            (kitsune-spellcasting-ability-option "Wisdom" ::char5e/wis)
                            (kitsune-spellcasting-ability-option "Charisma" ::char5e/cha)]})]})

(def lamia-option-cfg
  {:name "Lamia"
   :key :lamia
   ;; :abilities {::char5e/con 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :languages ["Common" "Lamia"]
   :profs {:armor {:light true}
           :weapon {:scimitar true}}
  ;;  :selections [(opt5e/ability-increase-selection [::char5e/str ::char5e/dex] 1 true)]
   :modifiers [(mod5e/damage-resistance :poison)
               (mod5e/saving-throw-advantage [:poisoned])
               (mod5e/dependent-trait
                {:name "Frightening Gaze"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "You can try to frighten a target within 30 feet with your gaze. The target must make a Wisdom Save against 8 + your proficiency bonus + your Charisma modifier. (DC " (?spell-save-DC ::char5e/cha) "). On a failed save, the target is frightened of you"
                           (common/bonus-str 
                            (condp <= ?total-levels 5 
                             ", and has its movement speed reduced to 0 for it's next turn if it fails by 5 or more" ""))
                 ". The target must be able to see you, otherwise they automatically suceed the save. The target can repeat the save at the end of its turns. Once the target has succeeded on a save against this effect, it's immune against it for 24 hours.")})]
   :traits [{:name "Tremor Sense"
             :summary "You have tremorsense of 15 feet. You have advantage on perception checks for feeling things through the ground."}
            {:name "Cold Blooded Anatomy"
             :summary (str "You have resistance to poison damage and advantage on saving throws against being poisoned."
                           "\n\nAdditionally, you can survive 3 times longer without food or drink than an average humanoid."
                           "\n\nHowever, you have a susceptibility to the cold. When hit by cold damage or when in an environment of 0 degrees or lower, your movement speed is halved until the end of your next turn.")}]})

(defn draconic-ancestry-option [{:keys [name breath-weapon]}]
  (t/option-cfg
   {:name name
    :modifiers [(mod5e/damage-resistance (:damage-type breath-weapon))
                (mod/modifier ?draconic-ancestry-breath-weapon breath-weapon)]}))

(def dragonborn-standard-option-cfg
  {:name "Dragonborn (Standard)"
   :key :dragonborn-standard
   :help "Kin to dragons, dragonborn resemble humanoid dragons, without wings or tail and standing erect. They tend to make excellent warriors."
   ;; :abilities {::char5e/str 2 ::char5e/cha 1}
   :custom-ability-scores true
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
   :custom-ability-scores true
   :help "Kin to dragons, dragonborn resemble humanoid dragons, without wings or tail and standing erect. They tend to make excellent warriors."
   :size :medium
   :speed 30
   :languages ["Draconic" "Common"]
   :subraces [{:name "Draconblood"
               ;; :abilities {::char5e/int 2 ::char5e/cha 1}
               :darkvision 60
               :modifiers [(mod5e/trait-cfg
                            {:name "Forceful Presence"
                             :page 168
                             :source :egw
                             :summary (str "When you make a Intimidation or Persuasion check, you can do so with advantage once per long rest.")})]}
              {:name "Ravenite"
               ;; :abilities {::char5e/str 2 ::char5e/con 1}
               :darkvision 60
               :modifiers [(mod5e/reaction
                            {:name "Vengeful Assault"
                             :page 168
                             :source :egw
                             :frequency units5e/rests-1
                             :summary (str "When you take damage from a creature in range of a weapon you are wielding, you can use your reaction to make an attack against that creature.")})]}]
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
   ;; :abilities {::char5e/int 2}
   :custom-ability-scores true
   :size :small
   :speed 25
   :darkvision 60
   :languages ["Gnomish" "Common"]
   :modifiers [(mod5e/saving-throw-advantage [:magic] [::char5e/int ::char5e/wis ::char5e/cha])]
   :traits [{:name "Gnome Cunning"
             :page 37
             :summary "You have advantage on all Intelligence, Wisdom, and Charisma saves against magic"}]
   :subraces
   [{:name "Rock Gnome"
     ;; :abilities {::char5e/con 1}
     :modifiers [(mod5e/tool-proficiency :tinkers-tools)]
     :traits [{:name "Artificer's Lore"
               :page 37
               :summary "Whenever you make an Intelligence (History) check related to magical, alchemical, or technological items, you can add twice your proficiency bonus instead of any other proficiency bonus that may apply"}
              {:name "Tinker"
               :page 37
               :summary (str "You have proficiency with artisan tools (tinker's tools). Using those tools, you can spend 1 hour and 10 gp worth of materials to construct a Tiny clockwork device (AC 5, 1 hp). The device ceases to function after 24 hours (unless you spend 1 hour repairing it to keep the device functioning), or when you use your action to dismantle it; at that time, you can reclaim the materials used to create it. You can have up to three such devices active at a time. When you create a device, choose one of the following options:"
                             "\n\u2022 Clockwork Toy. This toy is a clockwork animal, monster, or person, such as a frog, mouse, bird, dragon, or soldier. When placed on the ground, the toy moves 5 feet across the ground on each of your turns in a random direction. It makes noises as appropriate to the creature it represents."
                             "\n\u2022 Fire Starter. The device produces a miniature flame, which you can use to light a candle, torch, or campfire. Using the device requires your action."
                             "\n\u2022 Music Box. When opened, this music box plays a single song at a moderate volume. The box stops playing when it reaches the song's end or when it is closed."
                             "\n\u2022 At your DM's discretion, you may make other objects with effects similar in power to these. The Prestidigitation cantrip is a good baseline for such effects.")}]}
    {:name "Forest Gnome"
     ;; :abilities {::char5e/dex 1}
     :modifiers [(mod5e/spells-known 0 :minor-illusion ::char5e/int "Forest Gnome")]
     :traits [{:name "Speak with Small Beasts"
               :page 37
               :summary "Through sound and gestures, you may communicate simple ideas with Small or smaller beasts."}]}
    {:name "Svirfneblin (Deep Gnome)"
     ;; :abilities {::char5e/dex 1}
     :darkvision 120
     :languages ["Undercommon"]
     :traits [{:name "Stone Camouflage"
               :summary "You have advantage on Stealth checks to hide in rocky terrain."}]}]})

(defn deep-gnome-option-cfg [language-map]
  {:name "Deep Gnome"
   :key :deep-gnome
   :help "Gnomes are small, intelligent humanoids who live life with the utmost of enthusiasm."
   :custom-ability-scores true
   ;; :abilities {::char5e/int 2 ::char5e/dex 1}
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
                            " once per long rest without needing material components. You can also cast these using spell slots of the appropriate level. INT, WIS, or CHA is your spellcasting ability.")})
               (mod5e/action
                {:name "Svirfneblin Camouflage"
                 :page 11
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "When you make a Stealth check, you can make the check with advantage"})]
   :traits [{:name "Gnome"
             :page 11
             :summary "You are considered a gnome for any prerequisite or effect that requires you to be a gnome."}]})

(defn half-elf-option-cfg [language-map]
  {:name "Half-Elf"
   :key :half-elf
   :help "Half-elves are charismatic, and bear a resemblance to both their elvish and human parents and share many of the traits of each."
   :custom-ability-scores-2 true
  ;;  :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Elvish"]
   :selections [(assoc
                 (opt5e/skill-selection 2)
                 ::t/ref
                 [:race :half-elf :skill-proficiency])
                (opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/saving-throw-advantage [:charmed])]
   :traits [{:name "Fey Ancestry"
             :page 39
             :summary "You have advantage on saving throws against being charmed, and magic can't put you to sleep"}]})

;; (opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/cha) 2 true)

(defn half-elf-aoa-option-cfg [spell-lists spells-map]
  {:name "Half-Elf (AoA)"
   :key :half-elf-aoa
   :help "Half-elves are charismatic, and bear a resemblance to both their elvish and human parents and share many of the traits of each."
   :custom-ability-scores-2 true
  ;;  :abilities {::char5e/cha 2}
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Elvish"]
   :selections [(t/selection-cfg
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
             :summary "You have advantage on saving throws against being charmed, and magic can't put you to sleep"}]})

(def half-orc-option-cfg
  {:name "Half-Orc"
   :key :half-orc
   :help "Half-orcs are strong and bear an unmistakable resemblance to their orcish parent. They tend to make excellent warriors, especially Barbarians."
   ;; :abilities {::char5e/str 2 ::char5e/con 1}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Orc"]
   :modifiers [(mod5e/skill-proficiency :intimidation)]
   :traits [{:name "Relentless Endurance"
             :page 41
             :frequency units5e/long-rests-1
             :summary "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead"}
            {:name "Savage Attacks"
             :page 41
             :summary "When you score a critical hit with a melee weapon attack, you can roll one of the weapon's damage dice one additional time and add it to the extra damage of the critical hit"}]})

(def harpy-option-cfg
  {:name "Harpy"
   :key :harpy
   ;; :abilities {::char5e/dex 2}
   :custom-ability-scores true
   :speed 30
   :languages ["Common" "Harpian"]
   :profs {:skill {:perception true}}
   :subraces [{:name "Eagle Harpy"
               :size :medium
               ;; :abilities {::char5e/str 1}
               :modifiers [(mod5e/flying-speed-override 45)
                           (mod5e/attack
                            {:name "Talons"
                             :attack-type :melee
                             :damage-type :slashing
                             :damage-die 6
                             :damage-die-count 1
                             :damage-modifier (if (= (?class-level :monk) 0) (::char5e/str ?ability-bonuses) (max (::char5e/str ?ability-bonuses) (::char5e/dex ?ability-bonuses)))})]
               :traits [{:name "Flight"
                         :summary (str "You have a fly speed of 45 feet. If the walking speed is reduced to 0 in any way, your fly speed also becomes 0."
                                       "\n\nYou can’t fly if you are wearing medium or heavy armour and you cannot cast spells that use somatic components or have a weapon with the two-handed property equipped. For versatile weapons, you can only use the 1 handed option. Weapons with the Reload Property can’t be reloaded while flying")}
                        {:name "Harpy Flyby"
                         :summary "When you hit an enemy within 5 feet of you while flying, you don’t provoke opportunity attacks from that enemy for the rest of the turn"}]}
               {:name "Owl Harpy"
                :sizes [:small :medium]
                :darkvision 120
                ;; :abilities {::char5e/wis 1}
                :modifiers [(mod5e/flying-speed-override 30)
                            (mod5e/skill-proficiency :stealth)
                            (mod5e/attack
                              {:name "Talons"
                              :attack-type :melee
                              :damage-type :slashing
                              :damage-die 4
                              :damage-die-count 1
                              :damage-modifier (max (::char5e/str ?ability-bonuses) (::char5e/dex ?ability-bonuses))})]
                ;; :selections [(t/selection-cfg
                ;;               {:name "Size"
                ;;                 :tags #{:race}
                ;;                 :options [(t/option-cfg
                ;;                           {:name "Small"
                ;;                             :modifiers [(mod5e/size :small)]})
                ;;                           (t/option-cfg
                ;;                           {:name "Medium"
                ;;                             :modifiers [(mod5e/size :medium)]})]})]
                :traits [{:name "Flight"
                          :summary (str "You have a fly speed of 30 feet. If the walking speed is reduced to 0 in any way, your fly speed also becomes 0."
                                        "\n\nYou can’t fly if you are wearing medium or heavy armour and you cannot cast spells that use somatic components or have a weapon with the two-handed property equipped. For versatile weapons, you can only use the 1 handed option. Weapons with the Reload Property can’t be reloaded while flying")}
                         {:name "Dampening Feathers"
                          :summary "While you are gliding, your wings don’t make any noise, giving you advantage on Stealth checks that rely on you not making noise. While gliding, you descend 5 feet down for every 15 feet flown horizontally"}]}]})

(def hobgoblin-option-cfg
  {:name "Hobgoblin"
   :key :hobgoblin
   ;; :abilities {::char5e/con 2 ::char5e/str 1}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :modifiers [(mod5e/bonus-action
                {:name "Commander's Gift"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary (str "You can use this trait to take the Help action as a bonus action, and you can do so a number of times equal to your proficiency bonus. You regain all expended uses when you finish a long rest."
                           (common/bonus-str 
                            (condp <= ?total-levels 3 
                             (str " Choose one of the following options when using this trait:"
                                  "\n   Hospitality. You and the creature you help each gain a number of temporary hit points equal to 1d6 plus your proficiency bonus."
                                  "\n   Passage. You and the creature you help each get +10 walking speed until the start of your next turn."
                                  "\n   Spite. Until the start of your next turn, the first time the creature you help hits a target with an attack roll, that target has disadvantage on the next attack roll it makes within the next minute.") "")))})
               (mod5e/dependent-trait
                {:name "Army Advantage"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "If you miss with an attack roll or fail an ability check or a saving throw, you can draw on your bonds of reciprocity to gain a bonus to the roll equal to the number of allies you can see within 30 feet of you (maximum bonus of +3)"})]
   :subraces [{:name "Forest Tribe"
               :modifiers [(mod5e/trait-cfg
                            {:name "Forest Tribe Bonus"
                             :summary "You can cast animal friendship at will. Wisdom is your spell casting ability for this spell. If the target of the spell succeed's on the save or the spell otherwise ends on the target, then the target is immune to your animal friendship for 24 hours."})
                           (mod5e/spells-known 1 :animal-friendship ::char5e/wis "Hobgoblin")]}
              {:name "Rock Tribe"
               :modifiers [(mod/vec-mod ?unarmored-defense :hobgoblin)
                           (mod/cum-sum-mod ?unarmored-ac-bonus (- (+ 3 (?ability-bonuses ::char5e/con)) (?ability-bonuses ::char5e/dex))
                                           nil
                                           nil
                                           [(= :hobgoblin (first ?unarmored-defense))])
                           (mod/cum-sum-mod ?unarmored-with-shield-ac-bonus (- (+ 3 (?ability-bonuses ::char5e/con)) (?ability-bonuses ::char5e/dex))
                                           nil
                                           nil
                                           [(= :hobgoblin (first ?unarmored-defense))])]}
              {:name "Desert Tribe"
               :modifiers [(mod5e/armor-proficiency :shields)
                           (mod5e/damage-resistance :fire)]
               :traits [{:name "Desert Tribe Bonus"
                         :summary (str "You are proficient with light, medium and heavy shields and can ignore the bulky property on shields."
                                       "\n  Additionally, you have resistance to fire damage.")}]}
              {:name "Frost Tribe"
               :modifiers [(mod5e/weapon-proficiency :crossbow-hand)
                           (mod5e/weapon-proficiency :crossbow-light)
                           (mod5e/weapon-proficiency :crossbow-heavy)
                           (mod5e/damage-resistance :cold)]
               :selections [(t/selection-cfg
                             {:name "Tool Proficiency"
                              :tags #{:profs}
                              :options [(t/option-cfg
                                         {:name "Tinker's Tools"
                                          :modifiers [(mod5e/tool-proficiency :tinkers-tools)]})
                                        (t/option-cfg 
                                         {:name "Alchemist's Supplies"
                                          :modifiers [(mod5e/tool-proficiency :alchemists-supplies)]})]})]}
              ]})

(defn kobold-option-cfg [language-map]
  {:name "Kobold"
   :custom-ability-scores true
   :size :small
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :modifiers [(mod5e/bonus-action
                {:name "Draconic Cry"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "As a bonus action, you let out a cry at your enemies within 10 feet of you. Until the start of your next turn, you and your allies have advantage on attack rolls against any of those enemies who could hear you"})]
   :selections [(opt5e/language-selection-aux (vals language-map) 1)
                (t/selection-cfg
                 {:name "Kobold Legacy"
                  :tags #{:race}
                  :options [(t/option-cfg
                             {:name "Craftiness"
                              :selections [(opt5e/skill-selection [:arcana :investigation :medicine :sleight-of-hand :survival] 1)]})
                            (t/option-cfg
                             {:name "Defiance"
                              :modifiers [(mod5e/saving-throw-advantage [:frightened])
                                          (mod5e/trait-cfg
                                           {:name "Defiance"
                                            :summary "You have advantage on saving throws to avoid or end the frightened condition on yourself."})]})
                            (t/option-cfg
                             {:name "Draconic Sorcery"
                              :selections [(t/selection-cfg
                                            {:name "Kobold Spellcasting Ability"
                                             :tags #{:spells}
                                             :options [(t/option-cfg
                                                        {:name "Intelligence"
                                                         :selections [(opt5e/cantrip-selection :sorcerer "Kobold" ::char5e/int 1)]})
                                                       (t/option-cfg
                                                        {:name "Wisdom"
                                                         :selections [(opt5e/cantrip-selection :sorcerer "Kobold" ::char5e/wis 1)]})
                                                       (t/option-cfg
                                                        {:name "Charisma"
                                                         :selections [(opt5e/cantrip-selection :sorcerer "Kobold" ::char5e/cha 1)]})]})]})]})]})

(def lenuboon-option-cfg
  {:name "Lenuboon"
   :key :lenuboon
   :help "Monkey-like race that inhabit the jungles of Ngbutu, living in tribes in homes high in the trees."
   ;; :abilities {::char5e/dex 2 ::char5e/wis 1}
   :custom-ability-scores true
   :size :medium
   :speed 35
   :languages ["Common" "Lenuboon"]
   :modifiers [(mod5e/bonus-action
                {:name "Dextrous Feet"
                 :summary "As a bonus action, you can use your feet to manipulate an object, open or close a door or container, or pick up or set down a Tiny object"})
               (mod5e/reaction
                {:name "Lenuboon Dodge"
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "When you take damage, you can use your reaction to reduce the damage you take by 1d6 plus your proficiency bonus (minimum of 0 damage)"})
               (mod5e/climbing-speed-equal-to-walking)]
   :traits [{:name "Glide"
             :summary "You have stretchable membranes between your arms and legs, like a wingsuit, that you can use as wings to slow your fall or allow you to glide. When you fall and aren't incapacitated, you can subtract up to 100 feet from the fall when calculating falling damage, and you can move up to 3 feet horizontally for every 1 foot you descend"}
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
   ;; :abilities {::char5e/dex 2}
   :custom-ability-scores true
   :size :medium
   :speed 35
   :languages ["Common" "Lunar"]
   :selections (into [] (concat
                [
                ;; (opt5e/ability-increase-selection [::char5e/wis ::char5e/int] 1 true)
                (lumini-spellcasting-ability-selection spell-lists spells-map)]
                (opt5e/race-spell-selections spell-lists spells-map "Lumini" 0 9)
                (opt5e/race-cantrip-selections spell-lists spells-map "Lumini" 0 0)))
   :modifiers (into [] (concat
               [(mod5e/skill-proficiency :perception)
               (mod/cum-sum-mod ?initiative ?prof-bonus)
               (mod5e/dependent-trait
                {:name "Nimble Rabbit"
                 :summary (str "You have advantage skill checks and saves that would cause you to become grappled, prone or restrained."
                               "\n  Additionally, you add your proficiency to your initiative rolls")})]
               (opt5e/race-spells-known spell-lists spells-map "Lumini" 1 9)))
   :traits [{:name "Moon Jump"
             :summary "You can add +5 feet to any high jump and +10 feet to any long jump you perform"}
            {:name "Luminusborn"
             :summary (str "As a spellcaster, you gain access to the Graviturgy list of spells, and can learn a spell from this list in place of a spell from your regular spell list."
                           "\n  Additionally, you learn one 1st level spell from the graviturgy spell list, and can cast it for free once per long rest. Wisdom, Intelligence, or Charisma is your spellcasting ability for this spell (choose when creating the character).")}]})

(defn orc-option-cfg [language-map]
  {:name "Orc"
   :key :orc
   :help ""
   ;; :abilities {::char5e/str 2 ::char5e/con 1}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
   :modifiers [(mod5e/bonus-action
                {:name "Adrenaline Rush"
                 :page 28
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "You can take the Dash action as a bonus action. Whenever you use this trait, you gain a number of temporary hit points equal to your proficiency bonus"})]
   :traits [{:name "Relentless Endurance"
             :page 28
             :frequency units5e/long-rests-1
             :summary "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead"}
            (powerful-build 28)]})

(defn shifter-option-cfg [language-map]
  {:name "Shifter"
   :key :shifter
   :help ""
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :profs {:skill-options {:choose 1 :options {:acrobatics true :athletics true :intimidation true :survival true}}}
   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
   :subraces [{:name "Beasthide"
               :traits [{:name "Beasthide"
                         :page 32
                         :summary "You gain 1d6 additional temporary hit points. While shifted, you have a +1 bonus to your Armor Class"}]}
              {:name "Longtooth"
               :modifiers [(mod5e/bonus-action
                            {:name "Longtooth"
                             :page 32
                             :summary "When you shift and as a bonus action on your other turns while shifted, you can use your elongated fangs to make an unarmed strike. If you hit with your fangs, you can deal piercing damage equal to 1d6 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike"})
                           (mod5e/attack
                            {:name "Fangs"
                             :attack-type :melee
                             :damage-type :piercing
                             :damage-die 6
                             :damage-die-count 1
                             :damage-modifier (if (= (?class-level :monk) 0) (::char5e/str ?ability-bonuses) (max (::char5e/str ?ability-bonuses) (::char5e/dex ?ability-bonuses)))})]}
              {:name "Swiftstride"
               :modifiers [(mod5e/bonus-action
                            {:name "Longtooth"
                             :page 32
                             :summary "While shifted, your walking speed increases by 10 ft. When a creature ends its turn within 5 ft., move up to 10 ft. without provoking opportunity attacks"})]}
              {:name "Wildhunt"
               :traits [{:name "Wildhunt"
                         :page 32
                         :summary "While shifted, your walking speed increases by 10 feet. Additionally, you can move up to 10 feet as a reaction when a creature ends its turn within 5 feet of you. This reactive movement doesn’t provoke opportunity attacks"}]}]
   :modifiers [(mod5e/bonus-action
                {:name "Shifting"
                 :page 32
                 :frequency (units5e/long-rests ?prof-bonus)
                 :summary "As a bonus action, you can assume a more bestial appearance. This transformation lasts for 1 minute, until you die, or until you revert to your normal appearance as a bonus action. When you shift, you gain temporary hit points equal to 2 x your proficiency bonus"})]})

(def tiefling-option-cfg
  {:name "Tiefling"
   :key :tiefling
   :help "Tieflings bear the distinct marks of their infernal ancestry: horns, a tail, pointed teeth, and solid-colored eyes. They are smart and charismatic."
   ;; :abilities {::char5e/cha 2}
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common" "Infernal"]
   :modifiers [(mod5e/trait-cfg
                {:name "Hellish Resistance"
                 :page 43
                 :summary "You have resistance to fire damage"})
               (mod5e/damage-resistance :fire)]
   :subraces [{:name "Bloodline of Asmodeus"
               ;; :abilities {::char5e/int 1}
               :modifiers [(mod5e/dependent-trait
                            {:name "Infernal Legacy"
                            :page 43
                            :summary (str "You know Thaumaturgy and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Hellish Rebuke (2nd level)")
                                                (>= lvl 5) (conj "Darkness"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :thaumaturgy ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :hellish-rebuke ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :darkness ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Baalzebul"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Maladomini"
                            :summary (str "You know Thaumaturgy and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Ray of Sickness (2nd level)")
                                                (>= lvl 5) (conj "Crown of Madness"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :thaumaturgy ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :ray-of-sickness ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :crown-of-madness ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Dispater"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Dis"
                            :summary (str "You know Thaumaturgy and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Disguise Self")
                                                (>= lvl 5) (conj "Detect Thoughts"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :thaumaturgy ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :disguise-self ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :detect-thoughts ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Fierna"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Phlegethos"
                            :summary (str "You know Friends and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Charm Person (2nd level)")
                                                (>= lvl 5) (conj "Suggestion"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :friends ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :charm-person ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :suggestion ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Glasya"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Malbolge"
                            :summary (str "You know Minor Illusion and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Disguise Self")
                                                (>= lvl 5) (conj "Invisibility"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :minor-illusion ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :disguise-self ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :invisibility ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Levistus"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Stygia"
                            :summary (str "You know Ray of Frost and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Armor of Agathys (2nd level)")
                                                (>= lvl 5) (conj "Darkness"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :ray-of-frost ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :armor-of-agathys ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :darkness ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Mammon"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Minauros"
                            :summary (str "You know Mage Hand and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Tenser's Floating Disk")
                                                (>= lvl 5) (conj "Arcane Lock (without Material component)"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :mage-hand ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :tensers-floating-disk ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :arcane-lock ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Mephistopheles"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Cania"
                            :summary (str "You know Mage Hand and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Burning Hands (2nd level)")
                                                (>= lvl 5) (conj "Flame Blade"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :mage-hand ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :burning-hands ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :flame-blade ::char5e/cha "Tiefling" 5)]}
              {:name "Bloodline of Zariel"
               :modifiers [(mod5e/dependent-trait
                            {:name "Legacy of Avernus"
                            :summary (str "You know Thaumaturgy and can cast "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Searing Smite (2nd level)")
                                                (>= lvl 5) (conj "Branding Smite"))))
                                          " once per long rest. Charisma is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :thaumaturgy ::char5e/cha "Tiefling")
                           (mod5e/spells-known 1 :searing-smite ::char5e/cha "Tiefling" 3)
                           (mod5e/spells-known 2 :branding-smite ::char5e/cha "Tiefling" 5)]}
                ]})

(defn tiefling-aoa-option-cfg [language-map]
  {:name "Tiefling (AoA)"
   :key :tiefling-aoa
   :help "Tieflings bear the distinct marks of their infernal ancestry: horns, a tail, pointed teeth, and solid-colored eyes. They are smart and charismatic."
   :custom-ability-scores true
   :size :medium
   :speed 30
   :darkvision 60
   :languages ["Common"]
   :selections [(opt5e/language-selection-aux (vals language-map) 1)]
   :subraces [{:name "Third Plane"
               ;; :abilities {::char5e/str 2}
               :profs {:skill-options {:choose 1 :options {:athletics true :intimidation true}}}
              ;;  :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/str) 1)]
               :modifiers [(mod5e/trait-cfg
                            {:name "Devilish Resistance"
                            :summary "You have resistance to fire damage"})
                           (mod5e/damage-resistance :fire)
                           (mod5e/dependent-trait
                            {:name "Devil's Wrath"
                            :summary (str "You know Produce Flame and learn "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Searing Smite")
                                                (>= lvl 5) (conj "Enhance Ability"))))
                                          ", which you can cast once per long rest using this trait. Strength is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :produce-flame ::char5e/str "Tiefling")
                           (mod5e/spells-known 1 :searing-smite ::char5e/str "Tiefling" 3)
                           (mod5e/spells-known 2 :enhance-ability ::char5e/str "Tiefling" 5)]}
              {:name "Fourth Plane"
               ;; :abilities {::char5e/int 2}
               :profs {:skill-options {:choose 1 :options {:arcana true :history true}}}
              ;;  :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/int) 1)]
               :modifiers [(mod5e/trait-cfg
                            {:name "Devilish Resistance"
                            :summary "You have resistance to cold damage"})
                           (mod5e/damage-resistance :cold)
                           (mod5e/dependent-trait
                            {:name "Devil's Knowledge"
                            :summary (str "You know Frostbite and learn "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Detect Magic (can be casted as a ritual)")
                                                (>= lvl 5) (conj "Borrowed Knowledge"))))
                                          ", which you can cast once per long rest using this trait. Intelligence is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :frostbite ::char5e/int "Tiefling")
                           (mod5e/spells-known 1 :detect-magic ::char5e/int "Tiefling" 3)
                           (mod5e/spells-known 2 :borrowed-knowledge ::char5e/int "Tiefling" 5)]}
              {:name "Sixth Plane"
               ;; :abilities {::char5e/wis 2}
               :profs {:skill-options {:choose 1 :options {:insight true :perception true}}}
              ;;  :selections [(opt5e/ability-increase-selection (disj (set char5e/ability-keys) ::char5e/str) 1)]
               :modifiers [(mod5e/trait-cfg
                            {:name "Devilish Resistance"
                            :summary "You have resistance to acid damage and advantage on saving throws against spells."})
                           (mod5e/damage-resistance :acid)
                           (mod5e/saving-throw-advantage [:spells])
                           (mod5e/dependent-trait
                            {:name "Devil's Dissent"
                            :summary (str "You know Primal Savagery and learn "
                                          (common/list-print
                                            (let [lvl ?total-levels]
                                              (cond-> []
                                                (>= lvl 3) (conj "Cause Fear")
                                                (>= lvl 5) (conj "Hold Person"))))
                                          ", which you can cast once per long rest using this trait. Wisdom is your spellcasting ability for these spells.")})
                           (mod5e/spells-known 0 :primal-savagery ::char5e/wis "Tiefling")
                           (mod5e/spells-known 1 :cause-fear ::char5e/wis "Tiefling" 3)
                           (mod5e/spells-known 2 :hold-person ::char5e/wis "Tiefling" 5)]}]})

(defn tortle-option-cfg [language-map]
  {:name "Tortle"
   :key :tortle
   :custom-ability-scores true
   :sizes [:small :medium]
   :speed 30
   :languages ["Common"]
   :profs {:skill-options {:choose 1 :options {:animal-handling true :medicine true :nature true :perception true :stealth true :survival true}}}
   :modifiers [(mod5e/attack
                {:name "Claws"
                :attack-type :melee
                :damage-type :slashing
                :damage-die 6
                :damage-die-count 1
                :damage-modifier (if (= (?class-level :monk) 0) (::char5e/str ?ability-bonuses) (max (::char5e/str ?ability-bonuses) (::char5e/dex ?ability-bonuses)))})
               (mod/vec-mod ?unarmored-defense :tortle)
               (mod/cum-sum-mod ?unarmored-ac-bonus (- 7 (?ability-bonuses ::char5e/dex))
                                 nil
                                 nil
                                 [(= :tortle (first ?unarmored-defense))])
               (mod/cum-sum-mod ?unarmored-with-shield-ac-bonus (- 7 (?ability-bonuses ::char5e/dex))
                                 nil
                                 nil
                                 [(= :tortle (first ?unarmored-defense))])
               (mod5e/action
                {:name "Shell Defense"
                 :summary "You can withdraw into your shell as an action. Until you emerge, you gain a +4 bonus to your AC, and you have advantage on Strength and Constitution saving throws. While in your shell, you are prone, your speed is 0 and can’t increase, you have disadvantage on Dexterity saving throws, you can’t take reactions, and the only action you can take is a bonus action to emerge from your shell"})]
  ;;  :selections [(opt5e/language-selection-aux (vals language-map) 1)
  ;;               (t/selection-cfg
  ;;                {:name "Size"
  ;;                 :tags #{:race}
  ;;                 :options [(t/option-cfg
  ;;                            {:name "Small"
  ;;                             :modifiers [(mod5e/size :small)]})
  ;;                           (t/option-cfg
  ;;                            {:name "Medium"
  ;;                             :modifiers [(mod5e/size :medium)]})]})]
   :traits [{:name "Hold Breath"
             :summary "You can hold your breath for up to 1 hour."}
            {:name "Natural Armor"
             :summary "Your shell provides you a base AC of 17 (your Dexterity modifier doesn’t affect this number). You can’t wear light, medium, or heavy armor, but if you are using a shield, you can apply the shield’s bonus as normal"}]
   }
)

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
        duergar-option-cfg
        dwarf-option-cfg
        (elf-option-cfg spell-lists spells-map language-map)
        (elf-aoa-option-cfg spell-lists spells-map language-map)
        genasi-option-cfg
        (goblin-aoa-option-cfg spell-lists spells-map)
        (goliath-option-cfg language-map)
        (halfling-option-cfg spell-lists spells-map)
        harpy-option-cfg
        hobgoblin-option-cfg
        (human-option-cfg spell-lists spells-map language-map)
        kitsune-option-cfg
        dragonborn-standard-option-cfg
        dragonborn-option-cfg
        gnome-option-cfg
        (deep-gnome-option-cfg language-map)
        (half-elf-option-cfg language-map)
        (half-elf-aoa-option-cfg spell-lists spells-map)
        half-orc-option-cfg
        (kobold-option-cfg language-map)
        lenuboon-option-cfg
        (lumini-option-cfg spell-lists spells-map)
        (orc-option-cfg language-map)
        (shifter-option-cfg language-map)
        tiefling-option-cfg
        (tiefling-aoa-option-cfg language-map)
        (tortle-option-cfg language-map)]))))))

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
   (classes5e/warlock-cha-option spell-lists spells-map plugin-subclasses-map language-map weapons-map invocations boons)
   (classes5e/warlock-int-option spell-lists spells-map plugin-subclasses-map language-map weapons-map invocations boons)
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
