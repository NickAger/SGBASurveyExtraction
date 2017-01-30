# SGBASurveyExtraction

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
