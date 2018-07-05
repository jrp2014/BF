This is an implementation of [[Pure BF]] in Haskell. (This wiki article is a valid Haskell code. If you [http://esolangs.org/wiki/Pure_BF/Implementation?action=raw&ctype=text/css download the raw article code], and give the file a ".lhs" extension then you can load it in a Haskell compiler/interpreter.)

<pre>

> module PureBF where {
> import Control.Applicative;
> import Control.Comonad;
> import Control.Monad;
> import Data.Monoid;
> import Data.Word;

</pre>

== Utility Function ==

<pre>

> bool :: x -> x -> Bool -> x;
> bool x _ False = x;
> bool _ x True = x;

> transEnum :: (Enum x, Enum y) => x -> y;
> transEnum = toEnum . fromEnum;

</pre>

== Value Wrapping ==

This implements wrapping of values.

<pre>

> succ8 :: Word8 -> Word8;
> succ8 255 = 0;
> succ8 x = succ x;

> pred8 :: Word8 -> Word8;
> pred8 0 = 255;
> pred8 x = pred x;

</pre>

== Program Types ==

This is the same type as described in the [[Pure BF]] article.

<pre>

> type Program = (Tape, World) -> (Tape, World);

</pre>

But now, <tt>Tape</tt> and <tt>World</tt> still have to be defined. The tape is infinite. The world simply consists of an input stream and an output stream (not properly interactive).

<pre>

> type Tape = Sum Integer -> Word8;
> type World = ([Word8], [Word8]);

</pre>

The tape is a function from the position number to the value. The position number is <tt>Sum 0</tt> for the head position, negative for to the left, and positive for to the right. So it is shifted left/right by composing the input.

The reason for <tt>Sum Integer</tt> is to make a monoid, so that it can be used as a comonad (see below).

== Operations ==

Increment:<pre>

> increment :: Program;
> increment (t, w) = (t >>= flip (bool id succ8 . (== Sum 0)), w);

</pre>

Decrement:<pre>

> decrement :: Program;
> decrement (t, w) = (t >>= flip (bool id pred8 . (== Sum 0)), w);

</pre>

Next cell:<pre>

> next :: Program;
> next (t, w) = (t =>> ($ Sum 1), w);

</pre>

Previous cell:<pre>

> prev :: Program;
> prev (t, w) = (t =>> ($ Sum (-1)), w);

</pre>

Input (using convention where EOF sets cell to 0):<pre>

> input :: Program;
> input (t, (ih : it, o)) = (\x -> bool ih (t x) (x == Sum 0), (it, o));
> input (t, ([], o)) = (\x -> bool 0 (t x) (x == Sum 0), ([], o));

</pre>

Output:<pre>

> output :: Program;
> output (t, (i, o)) = (t, (i, o ++ [extract t]));

</pre>

Loop:<pre>

> loop :: Program -> Program;
> loop f (t, w) = bool (loop f . f) id (extract t == 0) (t, w);

</pre>

== Compiler ==

<pre>

> compile :: String -> Program;
> compile [] = id;
> compile ('+' : x) = compile x . increment;
> compile ('-' : x) = compile x . decrement;
> compile ('<' : x) = compile x . prev;
> compile ('>' : x) = compile x . next;
> compile (',' : x) = compile x . input;
> compile ('.' : x) = compile x . output;
> compile ('[' : x) = case (parseLoop 1 ([], x)) of {
>   (inner, outer) -> compile outer . loop (compile inner);
> };
> compile (']' : x) = error "End loop with no begin loop";
> compile (_ : x) = compile x;

</pre>

Now we have to parse the matching pair of brackets to make loops.

<pre>

> parseLoop :: Int -> (String, String) -> (String, String);
> parseLoop 0 x = x;
> parseLoop n (x, '[' : t) = parseLoop (succ n) (x ++ "[", t);
> parseLoop n (x, ']' : t) = parseLoop (pred n) (x ++ (guard (n /= 1) >> "]"), t);
> parseLoop n (x, h : t) = parseLoop n (x ++ [h], t);
> parseLoop n (x, []) = error "Unterminated loop block";

</pre>

== Interpreter ==

This program uses Lazy I/O to attempt to make a brainfuck program working.

<pre>

> interpret :: Program -> IO ();
> interpret p = interact $ \i -> transEnum <$> snd (snd (p (const 0, (transEnum <$> i, []))));

</pre>

== Download ==

* [http://esolangs.org/w/index.php?title=Pure_BF/Implementation&action=raw PureBF.lhs] (You have to give this file a .lhs extension to load it into Haskell)

<!--

> }

-->
