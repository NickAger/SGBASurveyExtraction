# SGBASurveyExtraction

* People who are interested in junior sailing
* People who are senior/assistant instructor.
* People who have PB2, Safety boat or who are interested in gain it.

*Empowerment*, *enablement* - that what we should be aiming to do

Qualifications people have
Qualifications people want

Group free Text
 - priority
 - urgency
 - cost

* Demand for facilities eg is there more demand dinghy storage.

* Page 1 - is it representative of the membership.
* Page 1 might be necessary to weight response say for families who ask for something might be useful
* Page 1 - relate locker, boat park to demand

* How representative it is terms of length of membership

* Page 2 - Extract who are most experienced are.

## How do you use the club:
* demand for activies
* are we matching facilities to demand.
* Club facilities cross-tab to relate answers to demographics


## Technical

See:

* https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/
* http://hackage.haskell.org/package/cassava-0.4.5.1/docs/Data-Csv.html
* [Maintaining laziness](https://wiki.haskell.org/Maintaining_laziness)

It seems that the [cassava](http://hackage.haskell.org/package/cassava-0.4.5.1/docs/Data-Csv.html) Csv decode library isn't lazy as (from [Maintaining laziness](https://wiki.haskell.org/Maintaining_laziness)):

> Some laziness breakers are visible in type signatures:
>
> `decodeUTF8 :: [Word8] -> Either Message String`
> The Either type signals that the function marks decoding-failure by using the Left constructor of Either. This function cannot be lazy, because when you access the first character of the result, it must already be computed, whether the result is Left or Right.
For this decision, the complete input must be decoded. A better type signature is
>
> `decodeUTF8 :: [Word8] -> (Maybe Message, String)`
> where the String contains as much characters as could be decoded and Maybe Message gives the reason for the stop of the decoding. Nothing means the input was completely read, Just msg means the decoding was aborted for the reason described in msg.
> If you touch the first element of the pair, the complete decodings is triggered, thus laziness is broken.

I asked on IRC #haskell:

> Hi, I'm parsing some CSV using http://hackage.haskell.org/package/cassava-0.4.5.1/docs/Data-Csv.html#v:decode It seems that only way to know that the `decode` API expects a lazy ByteString is by looking at the source (the Hackage documentation just shows `ByteString`). However IIUC https://wiki.haskell.org/Maintaining_laziness, indicates that as `decode` returns `Either String (Vector a)` then "This function cannot be lazy, because when you access the first character of the result, it must already be computed, whether the result is Left or Right”. So ... is `decode` gaining anything by expecting a lazy ByteString rather than the non-lazy ByteString?

the result:

> lyxia: you can see that the ByteString type in the signature links to ByteString.Lazy
>
> lyxia: a lazy bytestring doesn't need to be in memory all at once to be consumed.
>
> nickager: so `decode` producing a non-lazy result, but does so efficiently using a lazy bytestring?
>
> lyxia: efficiently meaning it doesn't hold the whole bytestring in memory at once

```haskell
test n m = runPar $ do
     fn <- spawnP (fib n) 
     fm <- spawnP (fib m)
     a  <- get fn        
     b  <- get fm        
     return (a,b)
```
