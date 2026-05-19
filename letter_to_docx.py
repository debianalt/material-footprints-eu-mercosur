"""
letter_to_docx.py — convierte un .md (carta de presentación / respuesta a
revisores) a .docx con formato de carta: Times New Roman 12 pt, interlineado
1.15, justificado, encabezados en negrita, SIN numeración de líneas continua.

Uso:  python tools/letter_to_docx.py <src.md> <dst.docx>
"""

import re
import sys
import os
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_LINE_SPACING, WD_ALIGN_PARAGRAPH, WD_BREAK

BLACK = RGBColor(0, 0, 0)

INLINE = re.compile(r"\*\*(.+?)\*\*|\*(.+?)\*|`(.+?)`")


def base_run(r, size=12, bold=False, italic=False, mono=False):
    r.font.name = "Courier New" if mono else "Times New Roman"
    r.font.size = Pt(size - 1 if mono else size)
    r.font.bold = bold
    r.font.italic = italic
    r.font.color.rgb = BLACK


def add_inline(p, text, size=12):
    last = 0
    for m in INLINE.finditer(text):
        if m.start() > last:
            base_run(p.add_run(text[last:m.start()]), size=size)
        b, i, c = m.group(1), m.group(2), m.group(3)
        if b is not None:
            base_run(p.add_run(b), size=size, bold=True)
        elif i is not None:
            base_run(p.add_run(i), size=size, italic=True)
        else:
            base_run(p.add_run(c), size=size, mono=True)
        last = m.end()
    if last < len(text):
        base_run(p.add_run(text[last:]), size=size)


def norm(p, align=WD_ALIGN_PARAGRAPH.JUSTIFY, after=6):
    p.alignment = align
    f = p.paragraph_format
    f.line_spacing_rule = WD_LINE_SPACING.ONE_POINT_FIVE
    f.space_after = Pt(after)
    f.space_before = Pt(0)


def is_tbl(l):
    return l.strip().startswith("|")


def is_sep(l):
    return bool(re.match(r"^\|[\s\-\|:]+\|$", l.strip()))


def add_table(doc, rows):
    rows = [r for r in rows if not is_sep(r)]
    rows = [[c.strip() for c in r.strip().strip("|").split("|")] for r in rows]
    if not rows:
        return
    n = max(len(r) for r in rows)
    rows = [r + [""] * (n - len(r)) for r in rows]
    t = doc.add_table(rows=len(rows), cols=n)
    t.style = "Table Grid"
    for i, row in enumerate(rows):
        for j, txt in enumerate(row):
            cell = t.rows[i].cells[j].paragraphs[0]
            cell.paragraph_format.line_spacing_rule = WD_LINE_SPACING.SINGLE
            cell.paragraph_format.space_after = Pt(2)
            cell.paragraph_format.space_before = Pt(2)
            add_inline(cell, txt, size=10)
            for rr in cell.runs:
                rr.font.size = Pt(10)
                if i == 0:
                    rr.font.bold = True
    doc.add_paragraph().paragraph_format.space_after = Pt(6)


def convert(src, dst):
    with open(src, encoding="utf-8") as f:
        lines = f.read().splitlines()

    doc = Document()
    s = doc.sections[0]
    for a in ("left_margin", "right_margin", "top_margin", "bottom_margin"):
        setattr(s, a, Cm(2.5))
    nm = doc.styles["Normal"]
    nm.font.name = "Times New Roman"
    nm.font.size = Pt(12)
    nm.font.color.rgb = BLACK

    i, buf = 0, []
    while i < len(lines):
        ln = lines[i]
        if is_tbl(ln):
            buf.append(ln)
            i += 1
            continue
        if buf:
            add_table(doc, buf)
            buf = []
        st = ln.strip()
        if st == "":
            i += 1
            continue
        if st in (r"\pagebreak", "<!-- pagebreak -->", r"\newpage"):
            doc.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
            i += 1
            continue
        if re.match(r"^-{3,}$", st):
            i += 1
            continue
        # blockquote (superseded banner etc.) — skip leading '>'
        if st.startswith(">"):
            st = st.lstrip(">").strip()
        m = re.match(r"^(#{1,4})\s+(.+)$", st)
        if m:
            lvl = len(m.group(1))
            p = doc.add_paragraph()
            norm(p, align=WD_ALIGN_PARAGRAPH.LEFT, after=6)
            base_run(p.add_run(m.group(2)), size=15 - lvl, bold=True)
            i += 1
            continue
        m = re.match(r"^[-*]\s+(.+)$", st)
        if m:
            p = doc.add_paragraph()
            norm(p)
            p.paragraph_format.left_indent = Cm(0.75)
            p.paragraph_format.first_line_indent = Cm(-0.75)
            base_run(p.add_run("•\t"), size=12)
            add_inline(p, m.group(1), size=12)
            i += 1
            continue
        p = doc.add_paragraph()
        norm(p)
        add_inline(p, st, size=12)
        i += 1
    if buf:
        add_table(doc, buf)

    os.makedirs(os.path.dirname(dst) or ".", exist_ok=True)
    doc.save(dst)
    print(f"Guardado: {dst}")


if __name__ == "__main__":
    convert(sys.argv[1], sys.argv[2])
