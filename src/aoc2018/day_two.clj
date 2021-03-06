(ns aoc2018.day-two)

(def box-codes
  [
    "naosmkcwtdbfivxuphzweraljq"
    "nvssmicltdbfiyxuphzgeraljq"
    "nvosmkcwwdbfiyxuphzeeraljx"
    "nvosmkcqtdbfiyxupkzgeraljw"
    "qvosmkcwtdbhiyxuphzgeraljh"
    "nvocqkcktdbfiyxuphzgeraljq"
    "nvosmhcwtdbfiyxmphzgekaljq"
    "nvosmkcwtdbfuyxwpszgeraljq"
    "nvosmocwtcbfiyxupfzgeraljq"
    "nvosmkcwtdbfiyxubczgeraljv"
    "nvosmkswtdbfiyxuphzgeruejq"
    "nlosmkcwtqbfiyxuphzgyraljq"
    "nvosmkcwtdbficxuphzgwraljk"
    "nvosmkkwtdbfiyxxphzgeralcq"
    "vvosmkcetdbfiyxumhzgeraljq"
    "evosmkcdtdbfiyxuphkgeraljq"
    "nvosmkvwtdbkiyxuphzgeraejq"
    "nvoszkcwtdbfimxuphzgeraljb"
    "nvozmkcwtdbfiyxuphzgrrcljq"
    "nvosvacwtdbfiyxuphzgeralzq"
    "nvosmkcwgdofiyxuthzgeraljq"
    "nvosmkcwasbfiyxuphzgeradjq"
    "nvosmkcatobfiyxtphzgeraljq"
    "nvosmkewtdsfiyxuphzgekaljq"
    "tvormkcwtdbfiyxuphzieraljq"
    "nvosgkcwtdbfiyxuuhzgeraqjq"
    "nvosmkcwtdbqiwxuphzgeralvq"
    "nvosmkcwtybfiydcphzgeraljq"
    "nvosnkcwtdbfiyxuphzulraljq"
    "nvosmkcwtdbbiyuupnzgeraljq"
    "nvosmwcwtdbfiyxuphzneraojq"
    "nvohlkcwtdbftyxuphzgeraljq"
    "nvasmkcwbdbfiyiuphzgeraljq"
    "nvosmkujtdbfiyxuphzgeraljz"
    "nvosmgcstdbfiyxuphzgeraljd"
    "nvoswkcwtsbziyxuphzgeraljq"
    "nvosmmcwtdbfiyxupbzzeraljq"
    "nvosmkcwtdbfifxulhzgeralji"
    "nvolmkcwtdbmiyxuphzgeraljv"
    "lvnsmkcwtdbfiyxuphzzeraljq"
    "nvqsmkcwtdbfiyxuphageralfq"
    "nvosmkcwtdmfiyluphzgeralzq"
    "nvommvcwtdbfiyxupjzgeraljq"
    "naosmkcwtdbfsyxuphzgsraljq"
    "avosmkcwtdbfiyxuphzgebafjq"
    "ndozmkcwtdbfiyxuhhzgeraljq"
    "nvosmkcwtubfiyxuphooeraljq"
    "nvosmkcwtdbliyxuphzgmraljx"
    "nvosmkcuddbfimxuphzgeraljq"
    "wvosmkzwrdbfiyxuphzgeraljq"
    "nvosmkcqtdbfiyxupjzgeraijq"
    "nvosbkcwtdbfiyduphzgeruljq"
    "yzosmkcntdbfiyxuphzgeraljq"
    "nvolmkcwtdbfiyxuphugeralfq"
    "nvrsmkcwtdbjiyxuphzgejaljq"
    "nvgsmkcwtdbfiyxuphoglraljq"
    "nvosmkcwtdbfioxuphzgezalhq"
    "nvosjkcwtdbfipxuphzgekaljq"
    "nvosmkcwtabfiyxlpazgeraljq"
    "nvosmkfwtpnfiyxuphzgeraljq"
    "nvokmbcwtdbeiyxuphzgeraljq"
    "nvosmkcwtdbfiyxupmzgmlaljq"
    "nvosmkcwtdhfiykurhzgeraljq"
    "nvosmkcwwdbfiyxumhzgiraljq"
    "cvosmscwtdbfikxuphzgeraljq"
    "nvosmkcwtdnzirxuphzgeraljq"
    "nvosmscwtdbfiyxuuhbgeraljq"
    "nvosmkcwtdbfidxpphzgeraajq"
    "nvosmkcwtdbfiyxuqhzgurcljq"
    "nvosmkcwtekfiyxrphzgeraljq"
    "ntosmkcwtpqfiyxuphzgeraljq"
    "nvosmkcdtdbfhyxsphzgrraljq"
    "nvolmkkwtdbfiyxuphzgeralgq"
    "nvosmrcwtdbfiyxuphzgefkljq"
    "nvoxmkcwtdbfiysuphzeeraljq"
    "nvjsmkswtdbfiyxuphzqeraljq"
    "nvosmkcetdbfiyfuphdgeraljq"
    "nvosmkkwtpbfsyxuphzgeraljq"
    "nvosdgcwtdbfiyxupyzgeraljq"
    "nvosmkcwudbfiyzvphzgeraljq"
    "nvosmkcwtlbfiyxupkzgerzljq"
    "nvosmkcwtdbfiywuphyzeraljq"
    "nvocmkcwtdufiyxukhzgeraljq"
    "nvosmkcwtdqfiyxuphzgevaxjq"
    "nvosvkcwtdbgiyxuphzgeralzq"
    "nqosmkcwtdbfiyxuphzeeraljr"
    "nvosmkcetdbfiyxuphzgeroljo"
    "nvosmkcwtdvfiyxuphzceraliq"
    "nvosmkcwtnxfiyxuphzgyraljq"
    "nvosmkfwtdefiyxupxzgeraljq"
    "nvosmacwtdbfiyxuphzseragjq"
    "nvpsmkcwtdbfzyxuvhzgeraljq"
    "nvormkcwtdbfiyxuphzairaljq"
    "rvysmkcwtdbfmyxuphzgeraljq"
    "nvosmscwzdbfiyxuphzgerbljq"
    "nvosmkcwtdufmyxuphzqeraljq"
    "nvosmkcwtxbfiyxxphzgeralxq"
    "nvosmkcwtdbsiyxupsfgeraljq"
    "nvosmccwtdbfiqxuthzgeraljq"
    "nvosmtcwtqbuiyxuphzgeraljq"
    "nvosmkcwtdbfiysurbzgeraljq"
    "nvowmkcwtdbfiyxuywzgeraljq"
    "xvosmkcktdbfiyxuhhzgeraljq"
    "nvosmkgwsdbfiyxmphzgeraljq"
    "jvofmkcwtdbfiyxupyzgeraljq"
    "nvozakcwtdbfiexuphzgeraljq"
    "nvosmkcptdbfiyxuphzgepaljn"
    "nvosmkcwtdbpiyxuphzgeraxjw"
    "nvoszkcwtdbfiyjuphzeeraljq"
    "nvosmkcwtdbfiyxuppzoeraejq"
    "nvosmkiytdbfiyhuphzgeraljq"
    "nvosmkcwtdvfiywupyzgeraljq"
    "nvosmecwtdofiyxuphzgeralja"
    "nvosmkqwtdbfixxuphzgeraojq"
    "nvosmkwwtdbfiyxfpdzgeraljq"
    "nvosmkgwtdbfiyzupwzgeraljq"
    "nmosmucwtdvfiyxuphzgeraljq"
    "nvosmdcwtdbmiyxuphzveraljq"
    "wvosmkcwtpbfiyxuphzgetaljq"
    "nvosmmcwtdlfbyxuphzgeraljq"
    "nvosmkcwtabmiexuphzgeraljq"
    "nvosqpcwtdbfiyxuphzgqraljq"
    "nvosmecwjdbfiyxuphzgeraljk"
    "nyohmkcwtdbfiyxuphzgzraljq"
    "nlosmkcwtkbfiyxuphzgeraejq"
    "nvosmkcwrdbliyxuphzgerpljq"
    "nvusmkzwtdbfxyxuphzgeraljq"
    "nvosmkcwtdbfiyxuhizgerazjq"
    "nvosmkhptdbfbyxuphzgeraljq"
    "nvosmfcwtdbgiyxupdzgeraljq"
    "nvosmkmwtdbfiyxuphzgevalpq"
    "nvosmkcwtdwfiyxuphzherjljq"
    "nvosmkcwjwbfiyxuphzgeualjq"
    "nvosmkcwxdbflymuphzgeraljq"
    "nvosmkcwpdbriyxuphzoeraljq"
    "nvoshkcwcdbfiyxuphzgeravjq"
    "nvosmkcetcbfiyxgphzgeraljq"
    "nvosmkcwtdyfiyxuphzgerwqjq"
    "nuosmkcwedbfiyxurhzgeraljq"
    "nvosmkcwtdbfiixuphzctraljq"
    "nvoszkcwtdbfwyxuphzgerpljq"
    "nvormkcwtdbfiyxuphzgeralzn"
    "nvosmkyttdbfiywuphzgeraljq"
    "nvosmkcwtdbhiyxupazgeralhq"
    "nvotmkcwtdbfiyxuphzgevalbq"
    "nvosmkcwedbfiyxuphzguraljr"
    "nvssmkcwtdbfiyxushzgeralbq"
    "nvosmkcwtdeziyxuphzgeralhq"
    "nvogmkcwtdbfiyxuphzgerrxjq"
    "ncormkcwtdbfiyxuphzgeraloq"
    "nvosmkcwbdbfiyeuphzgerqljq"
    "nvosxkcwtdbfsyxupfzgeraljq"
    "nvohmkcwtdbfiyxuphzseraajq"
    "nvoscdcwtdbfiyxuphzgeralqq"
    "neosmkcwtdbfiyxuchzgeralgq"
    "njosmvcwpdbfiyxuphzgeraljq"
    "nvosmkcwtwbfiyxuphzgehamjq"
    "nvosmkcwtdbfiyxushzgejaljv"
    "nvosmkcwodbfiyxuphzgeryqjq"
    "nvoymqcwtdbfiyxuphzgeralbq"
    "nvosmkcwtdjfiyxuphzgesaljb"
    "nvjsmdcwedbfiyxuphzgeraljq"
    "nvosmkcwydbfiyxuihzmeraljq"
    "nvrsmkcwtdifiyxuphzgqraljq"
    "nposmkcwtdbfiyxiohzgeraljq"
    "dvosmkcwtdbfiyxuphzrvraljq"
    "pvosmkcwudbfsyxuphzgeraljq"
    "noosmkcwtdbfiyxuphtgexaljq"
    "nvosmkcwtdbfiaxuphyferaljq"
    "nvhsmlcwtdbfiyxuphzgeualjq"
    "nvosekcwtdbbiyxuphzgerabjq"
    "nvosvkcitdbfiyxuphzgerarjq"
    "nvotmkkwtdbfiyxuphzgeraljj"
    "nvosmecwtdbfiyxuphzgyralwq"
    "hvosmkcwtdbfiyxuphzysraljq"
    "nvosmkcvtdbfiyxlphzgeraljb"
    "nvosmkcwttbfiyxuphngtraljq"
    "nvoslkcwtdbfiyxuphzqeraljr"
    "nxosmkcwtdbfibxuphzgrraljq"
    "nvokmkhwtdbfiyxuphzgwraljq"
    "nvosmkfwtdbfiyxuphzgdraljo"
    "nvcsmkcwtdbfibxuphzgeraljl"
    "nvosmkcwtdcfiaxuphzeeraljq"
    "wvosmkcwtdbyiyxjphzgeraljq"
    "nyosmbcwtjbfiyxuphzgeraljq"
    "nvosmkcwtdbiiyxuahzieraljq"
    "nqosmkcwtdbfiyxuyhzgerapjq"
    "nvosmkcwtdbfiyxuwhzzetaljq"
    "nvosmkcwfgbfiyxuphzgerrljq"
    "nvosmbcwtdbfipxuphzderaljq"
    "nvosmkcwtdgfiyxupdzgerjljq"
    "noosmkcwtdcfiyxuphlgeraljq"
    "nvonmkcutdbfiyxuphzieraljq"
    "nvocmkcwtdbfiyyuphageraljq"
    "nvosmkcwtdbfoyxuphzneraqjq"
    "nvoskkcwtdbtiyxuphzgevaljq"
    "ocosmkswtdbfiyxuphzgeraljq"
    "nvosmkcqtdbfiyxfvhzgeraljq"
    "noosmkcwtdbfiyquphzberaljq"
    "nvosmkcwttbfijxuchzgeraljq"
    "nvogmkcwtdbfiyxupazgeralaq"
    "nvqsmkcwtdbfikxuphzgeraliq"
    "nvosmkuwtdbfiyxuphzjwraljq"
    "nyosmhcwtdbfiyxuphzgereljq"
    "nvosmncwtdbfietuphzgeraljq"
    "gvpsmkcwtdbfiyxuyhzgeraljq"
    "nvozmkewtlbfiyxuphzgeraljq"
    "nvostkcltpbfiyxuphzgeraljq"
    "nvosmkcwtdbdiyxuphzgehaljz"
    "nvosmkcwtjbziyxuphzgexaljq"
    "nvosmkcwtdbfiyptphzggraljq"
    "nvosmkcwtdbliyxupjzgebaljq"
    "nvosmkawtdbfiyxupxzgtraljq"
    "vvosmkcwtdbfiyxfphzperaljq"
    "nvosmkawtdbfiyxutczgeraljq"
    "nvosmkcbtdbuiyxrphzgeraljq"
    "nvbsmkcwtdbfiyxdphzgerasjq"
    "nvosnkcwqdsfiyxuphzgeraljq"
    "nvosmkcwtdbfiyxwphzgzzaljq"
    "nvosmkcwtdbffyquphzgeralcq"
    "nvosmkcwtzbfiyxdphzgzraljq"
    "nvysmkcwtdbfiycvphzgeraljq"
    "nvowmkcwtdbfiycuyhzgeraljq"
    "nvosbkcwtdbfiyiuphzgeraqjq"
    "nvosmecwtdbfiyxupqzmeraljq"
    "nvosmkcdtdbfhyxsphzgeraljq"
    "nmosmkcwtdbziyxuphzgercljq"
    "nvosmkcwtdbfiyxupfmgersljq"
    "nvosmkcvtdbpyyxuphzgeraljq"
    "nvosmkcwtkbfiyaupxzgeraljq"
    "nvosmkcwtzbiiyxuphzgerazjq"
    "nvoxmkcwtdbfiyxuphztegaljq"
    "nvonmkcwtdafiyxuphzgerkljq"
    "rvommkcwtdbfiyxzphzgeraljq"
    "nvosmkcwthbfiysuphzgeraxjq"
    "nvosmkcwtdbfnyxuphzgerccjq"
    "nrosmzcwtdbfiyxuphkgeraljq"
    "nvolmkcdtdbfiyxuphtgeraljq"
    "nvosfkcwtdbfiyeuphcgeraljq"
    "nvowmkcwtdbfhyxuphzgerafjq"
    "gvosmkcwtdbfiyxupbpgeraljq"
    "nvosmkcwtdbkiyxuphegebaljq"
    "nvommufwtdbfiyxuphzgeraljq"
    "uvksmkcwtdbfiysuphzgeraljq"
    "nvosmkcwevbfiyxuphtgeraljq"
    "nvosmkcmtdbfiycuphzgeraxjq"
    "nvcsxkcwtdbfiyxuphzgeraljn"
    "nvosmkcwtdbtiymuphzgeraltq"
    "nvosmfcwtdlfjyxuphzgeraljq"
    "svosmkcitdbfiyxuphzgsraljq"
  ]
)

(defn filter-occurrences [n s]
  (->>
    s
    seq
    (group-by identity)
    (filter (fn [[k v]] (= n (count v))))
    (map first)
  )
)

(defn count-occurrences [n s]
  (->>
    s
    (filter-occurrences n)
    count
  )
)

(defn checksum [box-codes]
  (reduce *
    (map
      (fn [c]
        (->>
          box-codes
          (map
            #(count-occurrences c %)
          )
          (filter pos?)
          count
        )
      )
      [2 3]
    )
  )
)

(checksum box-codes)

;; part 2

(defn drop-at [n s]
  (let [[f b] (split-at n s)]
    (clojure.string/join (concat f (rest b)))
  )
)

(defn common [codes]
  (let
    [
      lr (range (count (first codes)))
    ]
    (->>
      (for [l lr]
        (filter-occurrences 2
            (for [code codes]
              (drop-at l code)
            )
        )
      )
      (filter (complement empty?))
      flatten
      first
    )
  )
)

(common box-codes)
