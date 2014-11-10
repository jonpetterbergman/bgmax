bgmax
=====

"bgmax" is a low-level parser for the BgMax format used by 
[BankGirot](http://www.bgc.se)(Sweden's only clearing house). 
It can parse all the 
[example files](http://www.bgc.se/templates/Iframe____3125.aspx) 
for "Bankgiro Inbetalningar" from the BGC website.

It is "low level" in that it only parses the "posts" (lines), 
it doesn't put them together into a coherent data 
structure(some posts may refer back to others and so on). 
The plan is to include this information in the future.

The parser tries to be forgiving with the input, as the BGC 
technical documentation states that "implementations should ignore 
any posts they don't understand". Unknown posts will be returned 
untouched, and junk at the end of a line will be ignored. 
Where applicable, room has been kept for "reserved for future use"-values.

You can find [bgmax on Hackage](http://hackage.haskell.org/package/bgmax).
