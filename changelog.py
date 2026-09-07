#!/usr/bin/env python3

"""Build CHANGELOG.md from this repository's GitHub releases.

The release bodies are the canonical text -- they are written when the release is cut, while the
reasoning is still to hand, and they are what somebody arriving from the releases page reads. This
script gathers them into one file so that the same history is readable from a clone, offline, and in
a diff. Nothing here writes prose: a claim that is wrong in CHANGELOG.md is wrong on the release it
came from, and that is the copy to correct.

Run it as the last step of a release, once the new release exists:

    ./changelog.py

It needs `gh` authenticated against this repository and makes one API call per release.
"""

import json
import re
import subprocess
import sys
from pathlib import Path

REPO = "sysl-lang/sysl-bootstrap"

HEADER = """# Changelog

Every release of sysl, newest first.

This file is **generated** by `changelog.py` from the GitHub release bodies, which are the canonical
copy -- correct a mistake there and regenerate, rather than editing this file. Versions are
`MAJOR.MINOR.PATCH`; while the leading zero stands the language is still moving, and a release may
change what an existing program means. Where it does, the release says so.
"""

FENCE = re.compile(r"^\s*(```|~~~)")
HEADING = re.compile(r"^(#+)(\s)")


def releases():
    """Every release, newest first, as (tag, name, date, body)."""
    tags = subprocess.run(
        ["gh", "release", "list", "--repo", REPO, "--limit", "500", "--json", "tagName",
         "--jq", ".[].tagName"],
        capture_output=True, text=True, check=True).stdout.split()

    for tag in tags:
        out = subprocess.run(
            ["gh", "release", "view", tag, "--repo", REPO,
             "--json", "tagName,name,publishedAt,body"],
            capture_output=True, text=True, check=True).stdout
        r = json.loads(out)
        yield tag, (r.get("name") or "").strip(), (r.get("publishedAt") or "")[:10], \
            (r.get("body") or "").strip()


def demoted(body):
    """The body with its headings pushed under the version's own `##`.

    A fenced block is left exactly as it is -- a `#` at the start of a line inside one is a comment
    in whatever language the block is written in, and a changelog carries plenty of shell.

    The floor at `###` is not cosmetic: two release bodies open at `#`, and demoting those by one
    would put them at the same level as a version, so a reader scanning for versions would find two
    section titles among them.
    """
    out = []
    fence = None

    for line in body.splitlines():
        m = FENCE.match(line)

        if fence is None and m:
            fence = m.group(1)
        elif fence is not None and m and m.group(1) == fence:
            fence = None
        elif fence is None:
            h = HEADING.match(line)

            if h:
                line = "#" * min(max(len(h.group(1)) + 1, 3), 5) + h.group(2) + line[h.end():]

        out.append(line)

    return "\n".join(out)


def first_heading(body):
    """The body's own opening heading, which is usually what the release is about."""
    for line in body.splitlines():
        m = re.match(r"^#+\s+(.*)", line)

        if m:
            return m.group(1).strip()

    return ""


def flat(text):
    """A heading reduced to its letters and digits, for comparing two spellings of one claim."""
    return re.sub(r"[^a-z0-9]", "", text.lower())


def summary(name, body):
    """The one-line summary a version heading carries, where it carries one.

    A release's name leads with its own version and often continues with a summary -- but the body
    then opens with a heading saying the same thing in better words, and printing both puts a claim
    on the screen twice. So the summary survives only where the body does not already make it: the
    ten releases whose two agree exactly, and the several whose name is the heading with a clause
    added, all lose it.
    """
    stripped = re.sub(r"^\s*(sysl\s+)?v?\d+\.\d+\.\d+\s*[-–—:]*\s*", "", name).strip()

    if not stripped:
        return ""

    a, b = flat(stripped), flat(first_heading(body))

    return "" if a and b and (a.startswith(b) or b.startswith(a)) else stripped


def main():
    parts = [HEADER]

    for tag, name, date, body in releases():
        version = tag.lstrip("v")
        said = summary(name, body)
        heading = f"## {version} — {date}"

        if said:
            heading += f"\n\n**{said}**"

        parts.append(f"{heading}\n\n{demoted(body)}\n")

    path = Path(__file__).parent / "CHANGELOG.md"
    path.write_text("\n".join(parts).rstrip() + "\n")
    print(f"wrote {path} ({len(parts) - 1} releases)")


if __name__ == "__main__":
    sys.exit(main())
