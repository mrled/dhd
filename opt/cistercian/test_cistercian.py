import argparse
import io
from datetime import datetime
from pathlib import Path
import subprocess
import unittest
from unittest.mock import patch

import cistercian


SHELL = Path(__file__).with_name("cistercian.sh")


class CistercianTests(unittest.TestCase):
    def test_all_glyphs_are_distinct_and_two_lines(self):
        glyphs = [cistercian.glyph(n) for n in range(10000)]
        self.assertEqual(len(set(glyphs)), 10000)
        for rows in glyphs:
            self.assertEqual(tuple(map(len, rows)), (3, 3))
            self.assertTrue(all(0x2800 <= ord(c) <= 0x28FF for c in "".join(rows)))

    def test_strokes(self):
        expected = {
            0: set(),
            1: {(x, 0) for x in range(3)},
            2: {(x, 2) for x in range(3)},
            3: {(x, x) for x in range(3)},
            4: {(x, 2 - x) for x in range(3)},
            5: {(2, 1)},
            6: {(2, 0), (2, 2)},
        }
        bar = {(2, y) for y in range(3)}
        expected[7] = expected[1] | bar
        expected[8] = expected[2] | bar
        expected[9] = expected[1] | expected[2] | bar
        for digit, points in expected.items():
            self.assertEqual({(x, y) for x in range(3) for y in range(3)
                              if cistercian.stroke(digit, x, y)}, points)

    def test_stem_extensions(self):
        for number in range(10000):
            rows = cistercian.glyph(number)
            for row, dot_y in ((0, 3), (1, 0)):
                for cell, character in enumerate(rows[row]):
                    for dot_x in range(2):
                        lit = bool((ord(character) - 0x2800) &
                                   (1 << cistercian.BITS[dot_y][dot_x]))
                        self.assertEqual(lit, cell * 2 + dot_x == 2)

    def test_shell_python_parity(self):
        numbers = [0, 1234, 6789, 9999]
        numbers += [digit * 10 ** place for place in range(4) for digit in range(10)]
        numbers += list(range(0, 10000, 137))
        result = subprocess.run(["bash", str(SHELL), *map(str, numbers)],
                                capture_output=True, text=True, check=True)
        self.assertEqual(result.stdout, cistercian.render(numbers) + "\n")

    def test_validation(self):
        for value in ("-1", "10000", "1.0", "abc", "", "١"):
            with self.assertRaises(argparse.ArgumentTypeError):
                cistercian.parse_number(value)
            result = subprocess.run(["bash", str(SHELL), "1", value], capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, b"")
        self.assertEqual(cistercian.parse_number("00001234"), 1234)
        result = subprocess.run(["bash", str(SHELL), "00001234", "000"],
                                capture_output=True, text=True, check=True)
        self.assertEqual(result.stdout, cistercian.render([1234, 0]) + "\n")

    def test_clock(self):
        self.assertEqual(cistercian.clock_text(datetime(2026, 1, 1, 23, 59, 58)),
                         cistercian.render([2026, 101, 2359, 58]))

    def test_chart(self):
        lines = cistercian.chart().splitlines()
        self.assertEqual(len(lines), 7)
        self.assertEqual(lines[2].split(), [f"{n:04d}" for n in range(10)])
        self.assertEqual(lines[6].split(), ["1", "10", "100", "1000", "1111"])
        for row in range(2):
            self.assertEqual(lines[row].split(),
                             [cistercian.glyph(n)[row] for n in range(10)])

    def test_watch_redraws_only_its_rows(self):
        output = io.StringIO()
        moments = [datetime(2026, 12, 31, 23, 59, 59),
                   datetime(2027, 1, 1, 0, 0, 0)]
        with patch.object(output, "isatty", return_value=True), \
                patch.object(cistercian.sys, "stdout", output), \
                patch.object(cistercian, "datetime") as clock, \
                patch.object(cistercian.time, "sleep", side_effect=[None, KeyboardInterrupt]):
            clock.now.side_effect = moments
            cistercian.watch()
        text = output.getvalue()
        self.assertNotIn("\033[2J", text)
        self.assertNotIn("\033[H", text)
        self.assertEqual(text.count("\033[2A"), 1)
        self.assertEqual(text.count("\033[2K"), 4)
        for moment in moments:
            for line in cistercian.clock_text(moment).splitlines():
                self.assertIn(line, text)
        self.assertTrue(text.endswith("\033[?25h"))


if __name__ == "__main__":
    unittest.main()
