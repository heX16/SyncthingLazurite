#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Simple comparator for .po/.pot files.
Reads a source POT and a target PO/POT, then prints:
- Missing keys: present in source, absent in target
- Obsolete keys: present in target, absent in source

Notes:
- Uniqueness key is msgid only (msgctxt is ignored for comparison).
- Header entry (msgid == '') is ignored.
"""
from __future__ import annotations

import argparse
import os
import sys
from typing import Dict, Iterable, List, Optional, Set, Tuple


def _unquote(s: str) -> str:
    """
    Extracts content inside the first and last double quotes and unescapes typical PO escapes.
    Example:
    input:  msgid "Hello \"World\"\\n"
    stored: Hello "World"\n
    """
    s = s.strip()
    if not s:
        return ''
    # Expecting a quoted string like: "...."
    # If no quotes found, return as-is (defensive)
    if s[0] == '"' and s[-1] == '"':
        s = s[1:-1]
    # Unescape common sequences used in .po files
    s = s.encode('utf-8').decode('unicode_escape')
    return s


class PoEntry:
    def __init__(self) -> None:
        self.msgctxt: Optional[str] = None
        self.msgid: Optional[str] = None
        self.msgstr: Optional[str] = None
        self._current_field: Optional[str] = None

    def start_field(self, field: str, value: str) -> None:
        self._current_field = field
        if field == 'msgctxt':
            self.msgctxt = value
        elif field == 'msgid':
            self.msgid = value
        elif field == 'msgstr':
            self.msgstr = value

    def append_to_current(self, value: str) -> None:
        if self._current_field is None:
            return
        if self._current_field == 'msgctxt':
            self.msgctxt = (self.msgctxt or '') + value
        elif self._current_field == 'msgid':
            self.msgid = (self.msgid or '') + value
        elif self._current_field == 'msgstr':
            self.msgstr = (self.msgstr or '') + value

    def is_valid(self) -> bool:
        return self.msgid is not None

    def is_header(self) -> bool:
        return self.msgid == ''


def parse_po_like(path: str) -> List[PoEntry]:
    """
    Minimal .po/.pot parser sufficient for msgctxt/msgid/msgstr with multi-line values.
    """
    entries: List[PoEntry] = []
    current = PoEntry()

    def _value_from_directive(directive_line: str, directive: str) -> str:
        """
        Extract the quoted value that follows the directive keyword.
        Example: directive='msgid', line='msgid "Hello"' -> 'Hello'
        """
        rest = directive_line[len(directive):].lstrip()
        if rest.startswith('"'):
            return _unquote(rest)
        # Defensive fallback: try to find the first quote
        qpos = rest.find('"')
        if qpos != -1:
            return _unquote(rest[qpos:])
        return ''

    try:
        with open(path, 'r', encoding='utf-8') as f:
            for raw_line in f:
                line = raw_line.rstrip('\n')
                stripped = line.strip()

                # Entry separator
                if stripped == '':
                    if current.is_valid():
                        entries.append(current)
                    current = PoEntry()
                    continue

                # Ignore comments entirely
                if stripped.startswith('#'):
                    continue

                if stripped.startswith('msgctxt'):
                    # msgctxt "..."
                    value = _value_from_directive(stripped, 'msgctxt')
                    current.start_field('msgctxt', value)
                    continue

                if stripped.startswith('msgid'):
                    # msgid "..."
                    value = _value_from_directive(stripped, 'msgid')
                    current.start_field('msgid', value)
                    continue

                if stripped.startswith('msgstr'):
                    # msgstr "..."
                    value = _value_from_directive(stripped, 'msgstr')
                    current.start_field('msgstr', value)
                    continue

                # Continuation of previous field: "..."
                if stripped.startswith('"') and stripped.endswith('"'):
                    value = _unquote(stripped)
                    current.append_to_current(value)
                    continue

                # Unknown line type — end current entry defensively
                # but do not drop the line content (we do not need it for comparison).
                # We simply break the entry on unexpected content.
                if current.is_valid():
                    entries.append(current)
                current = PoEntry()
    except FileNotFoundError:
        print(f'Error: file not found: {path}', file=sys.stderr)
        return []

    # Flush last entry if any content
    if current.is_valid():
        entries.append(current)

    return entries


def collect_msgids(entries: Iterable[PoEntry]) -> Set[str]:
    """
    Collect msgid values, excluding the header (msgid == '').
    """
    ids: Set[str] = set()
    for e in entries:
        if e.msgid is None:
            continue
        if e.is_header():
            continue
        ids.add(e.msgid)
    return ids


def compare(source_path: str, target_path: str) -> Tuple[Set[str], Set[str]]:
    source_entries = parse_po_like(source_path)
    target_entries = parse_po_like(target_path)

    source_ids = collect_msgids(source_entries)
    target_ids = collect_msgids(target_entries)

    missing = source_ids - target_ids
    obsolete = target_ids - source_ids
    return missing, obsolete


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(description='Compare .po/.pot files by msgid (ignoring msgctxt).')
    parser.add_argument('--source', required=True, help='Path to source POT file')
    parser.add_argument('--target', required=True, help='Path to target PO/POT file')
    args = parser.parse_args(argv)

    missing, obsolete = compare(args.source, args.target)

    print(f'Source: {args.source}')
    print(f'Target: {args.target}')
    print('')
    print(f'Missing ({len(missing)}):')
    for k in sorted(missing):
        print(k)
    print('')
    print(f'Obsolete ({len(obsolete)}):')
    for k in sorted(obsolete):
        print(k)

    return 0


if __name__ == '__main__':
    raise SystemExit(main())


