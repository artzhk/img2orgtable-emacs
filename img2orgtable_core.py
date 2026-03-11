import pandas as pd
from img2table.ocr import TesseractOCR
from img2table.document import Image as Img
import shutil
import sys

def to_org(df: pd.DataFrame) -> str:
    rows = [list(df.columns)] + df.fillna("").astype(str).values.tolist()
    widths = [max(len(c) for c in col) for col in zip(*rows[1:])] if rows else []

    def fmt(r: list[str]):
        return (
            "| "
            + " | ".join(c.replace("\n", " ").ljust(w) for c, w in zip(r, widths))
            + " |"
        )

    sep = "|" + "+".join("-" * (w + 2) for w in widths) + "|"
    return "\n".join([fmt(rows[1]), sep] + [fmt(r) for r in rows[2:]])


class Error: 
    code: int
    mes: str

    def __init__(self): 
        self.code = 0
        self.mes = ""

def extract_table(img_path: str, err: Error) -> pd.DataFrame | None:
    if not shutil.which("tesseract"):
        err.code = 69
        err.mes = "tesseract not found in PATH"
        return None

    try:
        ocr = TesseractOCR(n_threads=2, lang="eng")
        img = Img(img_path)
        tables = img.extract_tables(
            ocr=ocr,
            implicit_rows=True,
            borderless_tables=True,
            min_confidence=50,
        )
    except Exception as exc:
        err.code = 70  # EX_SOFTWARE
        err.mes = f"extraction failed: {exc}"
        return None

    if len(tables) == 0:
        err.code = 2
        err.mes = "no table detected"
        return None

    return tables[0].df
