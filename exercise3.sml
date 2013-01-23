functor F(M: ORD_MAP where type Key.ord_key = string)
(S:ORD_SET where type Key.ord_key = string) :>
sig
val proc: string list -> S.set M.map
end
=
struct
  fun proc l = 
      let
       val myMap = M.empty
       fun analyzeWords((s,fi),ma) = if (isSome(M.find(ma,s))) then M.insert(ma, s, S.add(valOf(M.find(ma,s)),fi)) else M.insert(ma,s,S.add(S.empty,fi))
       fun getFile strm = TextIO.inputAll strm
       fun analyzeFiles(fi,ma) = foldl analyzeWords ma (map (fn s => (s,fi)) (String.tokens Char.isSpace (getFile (TextIO.openIn fi))))
      in
	foldl analyzeFiles myMap l
      end
end
