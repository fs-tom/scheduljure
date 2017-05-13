;;Purpose of the script is to generate the security check roster for FS.
;;Right now, input file dependencies are names.txt (just a vector
;;of strings in a text file) for a stack of names
;;for the roster and weeks.txt (just a vector of strings in a txt file)
;;for the weeks included in the roster.
;;roster-from will also prep the input files for the next roster
(ns scheduljure.core
  (:require [clj-time [core :as t] [format :as f] [periodic :as p]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;todo 1: function to make a new roster from scratch without depending
;;on inputs from last roster.  I think these inputs would be: names.txt, which week to start
;;(make sure it's a Monday)-generates weeks.txt first, call make-inputs for input files,
;;
;;todo 2: make uberjar file for non-Clojurfied user

;1) Vector of names ["Rick" "Tom" "Craig"]
;2) Map of unavailable days {"04/19/17" #{"Rick"} "04/26/17" #{"Rick" "Craig"}}
;3) Weeks ["04/12/17" "04/19/17" "04/26/17"]

;Outputs:
;1) New stack ["Craig" "Rick" "Tom"]
;2) New weeks ["05/03/17" "05/10/17" "05/17/17"]
;3) Roster [["04/12/17" "Rick"] ["04/19/17" "Tom"] ["04/26/17" "Tom"]]
                                   

(def names ["Rick" "Tom" "Craig"])
(def unavailables {"04-10-2017" #{"Rick"} "04-17-2017" #{"Rick" "Craig"}})
(def weeks ["04-03-2017" "04-10-2017" "04-17-2017" "04-24-2017"])

(defn spit-roster [vecs newdir]
  (spit (str newdir "\\roster.txt")
        (reduce (fn [curr [date name]]
                  (str curr date " " name "\r\n")) ""
                vecs)))

(defn get-first
  "Return the first item of xs where f(x) is True.  If xs is empty,
   return nil."
  [f xs]
  (first (filter f xs)))

(defn pop-out
  "Find the first itm in xs where f is true. Return a vector where
   first item is itm and second item is xs without itm."
  [f xs]
  (if-let [itm (get-first f xs)]
    [itm (remove (fn [i] (= i itm)) xs)]
    ["Whoever is here" xs]))

(defn available?
  "Returns true if the person is available for the week."
  [name unavailables week]
   (not (contains? (get unavailables week) name)))

(def myformat (f/formatter "MM-dd-yyyy"))

(defn mydate->jdate [mydate]
  (f/parse myformat mydate))

(defn jdate->mydate [jdate]
  (f/unparse myformat jdate))

(defn last-month
  "Returns the last integer month from jdate for numonths."
  [jdate numonths]
 (-> (t/month jdate)
      (+ numonths)
      (- 1)))

(defn include-week?
  "Returns true if Wednesday is in the same month as Monday."
  [monjdate]
  (let [wed (nth (p/periodic-seq monjdate (t/days 1)) 2)]
    (= (t/month monjdate) (t/month wed))))

(def ^:dynamic numonths 3)

(defn new-weeks
  "Given a start date, returns a sequence of dates every seven days
   for numonths."
  [jstartdate]
  (let [
        firstmonth  (t/plus jstartdate (t/weeks 1))
        lastmonth (last-month firstmonth numonths)
        pred (fn [jdate] (<= (t/month jdate) lastmonth))
        wks  (take-while pred (p/periodic-seq jstartdate (t/weeks 1)))]
    (if (include-week? (last wks))
       (map jdate->mydate wks)
      (pop (vec (map jdate->mydate wks))))
    ))

(defn new-weeks-from
  [mydate]
  (new-weeks  (t/plus  (mydate->jdate mydate) (t/weeks 1))))

(defn vec-conj
  "Conj onto the end of a sequence."
   [seq x]
  (conj (vec seq) x))

;return the stack, weeks, roster
(defn make-roster
  "Takes a sequence of names stacked in order of precedence for
  the schedule, a sequence of start days for each week, and a 
  map of start days to sets of unavailable names for each week. "
  [names weeks unavailables]
  (loop [pool names
         wks weeks
         roster []]
    (if (empty? wks)
      [pool  (new-weeks-from (last weeks)) (map #(-> [%1 %2]) weeks roster) roster]
      (let [wk (first wks)
            pout (fn [name] (available? name unavailables wk))
            [nxt newpool] (pop-out pout pool)
            poolcheck (if (= nxt "Whoever is here")
                        newpool
                        (vec-conj newpool nxt))]
        (recur poolcheck (rest wks) (conj roster nxt))))))

(defn input-intro
  "Given a first name string, generate a header and instructions for
   the input file"
  [firstname]
 (str/replace (str "Hello " firstname ",

Please indicate which weeks you will be unavailable for security checks by
putting an x inside the brackets below the example and then saving
this file.

In the example, this person is unavailable during the week of 05/01/2017 
but available for security checks during the week of 05/08/2017:

----------------Start Example

[x] 05/01/2017
[] 05/08/2017

----------------End Example

Unavailable means that you expect to have at least two days during the
week where you will either be out for the day (including CWS days but
excluding federal holidays) or go home for the day before 1400.

 
") #"\n" "\r\n")
  )

(defn fullname->first
  "fullname is first and last name separated by a space."
  [name]
   (first (str/split name #" ")))

(defn make-inputs
 "Given a vector of fullnames for the roster and a vector of string
  weeks in myformat, generate an input text file where each user can
  select their unavailability."
  [names weeks root]
  (doseq [n names]
    (spit (str root "\\" (str/replace n " " "") ".txt")
          (reduce (fn [curr wk] (str curr "\r\n[] " wk))
                  (input-intro  (fullname->first n)) weeks))))

(defn get-last-week [oldpath]
  (-> (slurp (str oldpath "\\roster.txt"))
      (str/split  #"\r\n")
      (last)
      (str/split #" ")
      (first)))

(defn lastweek-from-weeks [path]
  (last (read-string (slurp (str path "weeks.txt")))))

(defn make-new-inputs
  "use the previous roster data to generate the new input files"
  [newpath oldpath]
  (let [wks (new-weeks-from (get-last-week oldpath))
        _ (spit (str newpath "\\weeks.txt") (str wks))]
   (make-inputs (read-string (slurp  (str newpath "\\names.txt")))
                wks
                newpath)))

(defn camelcase->spaced
  "Takes a FirstnameLastname string and puts a string between
  first and last name"
  [camelstr]
  (let [[firstchar :as s] (vec camelstr)]
    (->> (rest s)
         (reduce (fn [curr c] (if (Character/isUpperCase c)
                                (str curr " " c)
                                (str curr c)))
                 (str firstchar)))))

(defn parse-availability [availstr]
  (let [date (last (str/split availstr #" "))
        ;;return the stuff between brackets. internet pull.
        v  (re-find (re-matcher #"(?<=\[)[^]]+(?=\])" availstr))
        ;;remove any spaces
        v (if v (str/trim v) v)] 
    [(case v "X" true "x" true false) date])) ;true if unavailable

(def ^:dynamic *last-text* #"1400.\r\n\r\n \r\n\r\n")

(defn available-booleans [path fullname]
  (when (.exists (io/file path))
    (map parse-availability (-> (slurp path)
                                ;(str/replace (input-intro (fullname->first fullname)) "")
                                (str/split *last-text*)
                                (second)
                                (str/split #"\r\n")
                                ;(rest)
                                ))))

(defn assoc-conj [m k v empty-coll]
  (if-let [valu (m k)]
    (assoc m k (conj valu v))
    (assoc m k (conj empty-coll v))))

(defn remove-extension [filename]
  (first (str/split filename #"\.")))

(defn add-unavailability
  "Used for a reduction over the unavailable files."
  [curr newpath]
  (let [fullname (camelcase->spaced (remove-extension (last (str/split newpath #"\\"))))
        wkly-status (filter identity (available-booleans newpath fullname))]
    (reduce (fn [curr [avail week]] (if avail (assoc-conj curr week fullname #{}) curr)) curr wkly-status)))

(defn trims [s]
  (str/replace s #" " ""))

(defn get-unavailability
  "Returns the unavailable map for all users."
  [rpath]
  (->> (str rpath "names.txt")
       (slurp)
       (read-string)
       (map (fn [nm]  (str rpath "\\" (trims nm) ".txt")))
       (reduce add-unavailability {})))

(defn spit-roster [rostervec dir]
  (spit (str dir "roster.txt")
        (->> (map (fn [[date name]] (str date " " name "\r\n")) rostervec)
             (reduce str))))
  

(defn but-last [coll]
  (take (- (count coll) 1) coll))

(defn up-one [path]
  (str (->> (str/split path #"\\")
        (but-last)
        (str/join "\\")) "\\"))

(defn read-file [path]
  (read-string (slurp path)))

(defn roster-email
"generate the text for the e-mail and put the results in email.txt"
  [path]

(str "FS,\r\n

Here is an updated security check roster.  I'm providing this electronic copy\r\n
and adding the roster to the FS calendar.  Make note of who has checks the week\r\n
after you as well.\r\n
\r\n
Please keep in mind that\r\n
\r\n
1.  Security check will be performed in accordance with Army Regulation 380-5.\r\n  
Department of the Army Information Security Program, section 6-11, requires all\r\n 
\"… Commands that access, process, or store classified information will establish\r\n 
a system of security checks at the close of each working day to ensure that all\r\n 
classified material is properly secured.  Standard Form 701 (Activity Security\r\n 
Checklist), will be used to record these checks...\".\r\n					
\r\n
2.  If you need to make adjustments due to your personal schedule, it is your responsibility.\r\n					
\r\n\r\n
The schedule follows below:\r\n\r\n"

(slurp (str path "roster.txt"))))

(defn input->roster [path]
  (make-roster (read-file (str path "names.txt"))
               (read-file (str path "weeks.txt"))
               (get-unavailability path)))

(defn roster-from
 "Make a new roster from the inputs (names.txt, weeks.txt, and
  unavailability files) in path.  Final roster goes in a file named
  roster.txt.  The names and weeks for the next roster are also
  generated."
  [path]
  (let [[pool newweeks roster] (make-roster (read-file (str path "names.txt"))
                                            (read-file (str path "weeks.txt"))
                                            (get-unavailability path))
        newpath (str (up-one path) (first newweeks))
        _ (.mkdir (io/file newpath))]
    (do (spit (str newpath "\\names.txt") (str pool))
        (spit (str newpath "\\weeks.txt") (str newweeks))
        (spit-roster roster path)
        (spit (str path "email.txt") (roster-email path))
        (make-new-inputs newpath path))))
