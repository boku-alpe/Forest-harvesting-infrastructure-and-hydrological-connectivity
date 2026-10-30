# -*- coding: utf-8 -*-
"""
fix_glyphs.py  -  Repairs text/*.txt files where pypdf emitted glyph codes like
"/C84/C104/C101" instead of characters (a font ToUnicode-mapping failure).
Each "/Cnnn" decodes to chr(nnn). Backs up the original to <file>.orig.

Run:  python fix_glyphs.py
Then re-run Skill B for the affected key(s) via rerun_b.py.
"""
import glob, os, re

GLYPH = re.compile(r"/C(\d+)")
THRESHOLD = 50   # files with more than this many glyph tokens are treated as garbled


def decode(text):
    def _sub(m):
        n = int(m.group(1))
        return chr(n) if 0 <= n < 0x110000 else ""
    return GLYPH.sub(_sub, text)


def main():
    fixed = []
    for path in glob.glob(os.path.join("text", "*.txt")):
        t = open(path, encoding="utf-8").read()
        n = len(GLYPH.findall(t))
        if n > THRESHOLD:
            with open(path + ".orig", "w", encoding="utf-8") as b:
                b.write(t)
            decoded = decode(t)
            with open(path, "w", encoding="utf-8") as f:
                f.write(decoded)
            fixed.append((os.path.basename(path), n))
            print(f"decoded {path}  ({n} glyph tokens)  preview: {decoded[:80]!r}")
    if not fixed:
        print("No glyph-encoded files found. Nothing to fix.")
    else:
        keys = [os.path.splitext(f)[0] for f, _ in fixed]
        print(f"\nFixed {len(fixed)} file(s). Now set KEYS = {keys} in rerun_b.py and run it.")


if __name__ == "__main__":
    main()
