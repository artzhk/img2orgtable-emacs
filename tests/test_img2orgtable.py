import importlib.util as iu
from pathlib import Path

import img2orgtable_core
import pandas as pd
import pytest


spec = iu.spec_from_file_location(
        "img2orgtable", Path(__file__).parent.parent / "img2orgtable.py"
        )
img2orgtable = iu.module_from_spec(spec)
spec.loader.exec_module(img2orgtable)  # type: ignore

def test_to_org_basic():
    df = pd.DataFrame({0: ["A", 1, 2], 1: ["B", "x", "yy"]})
    out = img2orgtable_core.to_org(df)
    assert out.splitlines()[0].startswith("| A") and "+-" in out


def test_cli_success(tmp_path: Path, monkeypatch, capsys):
    # Write a tiny PNG + mock extract_tables to avoid OCR/model
    from types import SimpleNamespace
    called = {}

    def fake_extract_tables(**kwargs):
        fake_df = pd.DataFrame({0: ["col1", "a", "b"], 1: ["col2", "1", "2"]})
        called["x"] = True
        return fake_df

    p = "assets/test.jpg"
    monkeypatch.setattr("img2orgtable_core.extract_table", lambda p, _err: fake_extract_tables())
    img2orgtable.main([str(Path(p))])
    out, err = capsys.readouterr()
    assert called.get("x") is True
    assert err is None or err == ""
    assert "| col1" in out and "|----" in out


def test_cli_no_table(tmp_path: Path, monkeypatch, capsys):
    from types import SimpleNamespace

    p = "assets/test.jpg"
    monkeypatch.setattr("img2orgtable_core.extract_table", lambda p, _err: None)

    with pytest.raises(SystemExit):
        img2orgtable.main([str(Path(p))])
    out, err = capsys.readouterr()
    assert "no table" in err.lower()
