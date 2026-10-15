# Terminal Cistercian numerals

Each number is a single Cistercian glyph, three Braille characters wide and
exactly two lines high. Each quadrant uses a 3×3-dot matrix, leaving one extra
stem dot toward the centre in each half to separate the upper and lower strokes. Use a UTF-8 terminal with a font that supports Braille.
Both implementations use only their standard runtime (Bash or Python 3.9+).

```sh
./cistercian.sh 1234 6789
./cistercian.py number 1234 6789
./cistercian.py time
./cistercian.py watch
./cistercian.py chart
```

Numbers must be decimal integers from 0 to 9999; leading zeros are accepted.
Zero is represented by the bare vertical stem. Units occupy the upper right,
tens the upper left, hundreds the lower right, and thousands the lower left.
The digit 5 uses a single outer-middle dot; 6 uses two outer dots with a gap
between them. These dotted variants are mirrored into each decimal place.
Digits 7–9 retain their bar-based forms.

`time` displays four glyphs: YYYY, MMDD, HHMM (24-hour local time), and seconds.
`watch` displays the same date/clock below the invoking prompt, redrawing only
its two output lines when the displayed time changes. It preserves the rest of
the terminal and exits on Ctrl-C, leaving the final display in place. It requires
an interactive terminal wide enough for the four glyphs (15 columns).
`chart` shows a row of glyphs labelled 0000–0009 underneath, followed by a
separate row labelled 1, 10, 100, 1000, and 1111 to illustrate decimal places.

Run tests with:

```sh
python3 -m unittest -v test_cistercian.py
```
