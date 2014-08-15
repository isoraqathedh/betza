betza
=====

A small library for transforming betza notation into movement diagrams.

The library will transform it first through parsed lumps of betza notation,
then into a list of locations
and finally into a diagram.

This modular approach is used in hopes that one of the components might be useful
(because the combination of the three of them in this way certainly won't be!)

About betza notation
--------------------

Betza notation, also known as Betza Funny notation or Funny notation,
is a way of describing chess pieces and what they do.
There are many explanations of how the notation works, for example
in [here](http://www.chessvariants.org/piececlopedia.dir/betzanot.html) and 
[here [in Japanese]](http://www.geocities.jp/tohokuchess/co/betza/betza.html).
The [original Betza's notation](www.chessvariants.org/d.betza/chessvar/pieces/notation.html)
has been extended by himself in
[other pages of his](http://www.chessvariants.org/d.betza/chessvar/16x16.html),
and even so the notation is clearly imperfect;
however as a description of most pieces used in chess variants today it is more than sufficient.

Parts
-----

### Parsing betza notation

Betza notation is introduced into the code via #?.
For instance, a piece with funny notation A would be written #?"A",
and a piece with funny notation frlRrlbK would be written #?"frlRrlbK".

To parse Betza notation into a list of powers, use the function `(powers *funny-notation*)`.

### Transforming into target squares

### Transforming into diagrams

Functions exposed
-----------------

Examples
--------

    (powers #?"rAlR")
    » (rA lR)

Take note that the items produced are not symbols