#!/usr/bin/env python3
#
# Loads a case-folding rule list in the format of RFC3454 section B.2 (referenced by RFC4518) and
# converts it into equivalent Rust code.
#
import argparse
from collections import defaultdict
import io
import sys
from typing import DefaultDict, Iterable, Mapping


class CharMapping:
    @property
    def sort_key(self) -> int:
        raise NotImplementedError()

    def to_rust(self) -> str:
        raise NotImplementedError()


class BlockMapping(CharMapping):
    def __init__(self, first_point: int, last_point: int, code_offset: int) -> None:
        self.first_point: int = first_point
        self.last_point: int = last_point
        self.code_offset: int = code_offset

    @property
    def sort_key(self) -> int:
        return self.first_point

    def to_rust(self) -> str:
        ob, cb = "{", "}"
        if self.first_point == self.last_point:
            range_str = f"c == '\\u{ob}{self.first_point:02X}{cb}'"
        else:
            range_str = f"c >= '\\u{ob}{self.first_point:02X}{cb}' && c <= '\\u{ob}{self.last_point:02X}{cb}'"
        sign, abs_offset = ("+", self.code_offset) if self.code_offset >= 0 else ("-", -self.code_offset)
        return "".join((
            f"if {range_str} {ob}\n",
            f"        write!(output, \"{ob}{cb}\", char::from_u32((c as u32) {sign} {abs_offset}).unwrap()).unwrap();\n"
            f"    {cb}"
        ))

    def __repr__(self) -> str:
        return f"BlockMapping(first_point=0x{self.first_point:02X}, last_point=0x{self.last_point:02X}, code_offset={self.code_offset})"


class PairMapping(CharMapping):
    def __init__(self, first_point: int, last_point: int, remainder_by_2: int, code_offset: int) -> None:
        self.first_point: int = first_point
        self.last_point: int = last_point
        self.remainder_by_2: int = remainder_by_2
        self.code_offset: int = code_offset

    @property
    def sort_key(self) -> int:
        return self.first_point

    def to_rust(self) -> str:
        ob, cb = "{", "}"
        range_str = f"c >= '\\u{ob}{self.first_point:02X}{cb}' && c <= '\\u{ob}{self.last_point:02X}{cb}' && c % 2 == {self.remainder_by_2}"
        sign, abs_offset = ("+", self.code_offset) if self.code_offset >= 0 else ("-", -self.code_offset)
        return "".join((
            f"if {range_str} {ob}\n",
            f"        write!(output, \"{ob}{cb}\", char::from_u32((c as u32) {sign} {abs_offset}).unwrap()).unwrap();\n"
            f"    {cb}"
        ))

    def __repr__(self) -> str:
        return f"PairMapping(first_point=0x{self.first_point:02X}, last_point=0x{self.last_point:02X}, remainder_by_2={self.remainder_by_2}, code_offset={self.code_offset})"


class SingletonMapping(CharMapping):
    def __init__(self, from_point: int, to_points: list[int]) -> None:
        self.from_point: int = from_point
        self.to_points: list[int] = to_points

    @property
    def sort_key(self) -> int:
        return self.from_point

    def to_rust(self) -> str:
        ob, cb = "{", "}"
        brace_sequence = len(self.to_points) * "{}"
        pieces = [f"if c == '\\u{ob}{self.from_point:02X}{cb}' {ob}\n"]
        if len(self.to_points) == 1:
            # short output style
            to_point = self.to_points[0]
            pieces.append(f"        write!(output, \"{brace_sequence}\", char::from_u32(0x{to_point:02X}).unwrap()).unwrap();\n")
        else:
            # long output style
            pieces.append(f"        write!(\n")
            pieces.append(f"            output,\n")
            pieces.append(f"            \"{brace_sequence}\",\n")
            for to_point in self.to_points:
                pieces.append(f"            char::from_u32(0x{to_point:02X}).unwrap(),\n")
            pieces.append("        ).unwrap();\n")
        pieces.append(f"    {cb}")
        return "".join(pieces)

    def __repr__(self) -> str:
        return f"SingletonMapping(from_point=0x{self.from_point:02X}, to_point=0x{self.to_point:02X})"


def table_to_raw_mappings(table: io.TextIOBase) -> dict[str, str]:
    raw_mappings: dict[str, str] = {}
    for raw_line in table:
        line = raw_line.removesuffix("\n").strip()
        (original_str, replacement_str, reason) = line.split("; ")
        original = chr(int(original_str, 16))
        replacements = "".join(chr(int(piece, 16)) for piece in replacement_str.split(" "))
        raw_mappings[original] = replacements

    return raw_mappings


def collect_block_mappings(mappings: list[CharMapping], raw_mappings: dict[str, str]) -> None:
    # handles the common pattern: A B C D E F ... a b c d e f ...

    if len(raw_mappings) < 2:
        return
    assert all(len(source) == 1 for (source, _target) in raw_mappings.items())

    # only consider mappings to one character
    sorted_raw_mappings = [(ord(k), ord(v)) for (k, v) in raw_mappings.items() if len(v) == 1]
    sorted_raw_mappings.sort()

    prev_source, prev_target = sorted_raw_mappings[0]
    start_source, start_delta = prev_source, (prev_target - prev_source)
    for next_source, next_target in sorted_raw_mappings[1:]:
        if next_source - prev_source != 1 or next_target - prev_target != 1:
            # that's it for this block

            if prev_source - start_source > 1:
                # perfect, we have more than one such entry; store the block
                mappings.append(BlockMapping(start_source, prev_source, start_delta))

                # delete these mappings from raw_mappings
                for code in range(start_source, prev_source+1):
                    del raw_mappings[chr(code)]

            # start a new block
            start_source, start_delta = next_source, (next_target - next_source)

        # remember for next time
        prev_source, prev_target = next_source, next_target

    # do we have a final block?
    if prev_source - start_source > 1:
        # perfect, we have more than one such entry; store the block
        mappings.append(BlockMapping(start_source, prev_source, start_delta))

        # delete these mappings from raw_mappings
        for code in range(start_source, prev_source+1):
            del raw_mappings[chr(code)]


def collect_pair_mappings(mappings: list[CharMapping], raw_mappings: dict[str, str]) -> None:
    # handles the common pattern: A a B b C c D d E e F f ...

    if len(raw_mappings) < 2:
        return
    assert all(len(source) == 1 for (source, _target) in raw_mappings.items())

    # only consider mappings to one character that are 1 character away
    sorted_raw_mappings = [
        (ord(k), ord(v))
        for (k, v) in raw_mappings.items()
        if len(v) == 1
        and ord(k) + 1 == ord(v)
    ]
    sorted_raw_mappings.sort()

    # this time, make steps by 2 and see if the difference is 1
    for remainder_by_2 in range(0, 2):
        relevant_mappings = [(k, v) for (k, v) in sorted_raw_mappings if k % 2 == remainder_by_2]
        if len(relevant_mappings) < 2:
            continue

        prev_source, prev_target = relevant_mappings[0]
        start_source = prev_source
        for next_source, next_target in relevant_mappings[1:]:
            assert next_source + 1 == next_target
            if prev_source + 2 != next_source:
                # block of pair mappings ended
                if prev_source - start_source > 1:
                    mappings.append(PairMapping(start_source, prev_source, remainder_by_2, 1))
                    for code in range(start_source, prev_source+1, 2):
                        del raw_mappings[chr(code)]

                start_source = next_source

            prev_source, prev_target = next_source, next_target

    # final block
    if prev_source + 2 != next_source:
        # block of pair mappings ended
        if prev_source - start_source > 1:
            mappings.append(PairMapping(start_source, prev_source, remainder_by_2, 1))
            for code in range(start_source, prev_source+1, 2):
                del raw_mappings[chr(code)]


def collect_singleton_mappings(mappings: list[CharMapping], raw_mappings: dict[str, str]) -> None:
    # handles everything else

    for k, vs in raw_mappings.items():
        mappings.append(SingletonMapping(ord(k), [ord(v) for v in vs]))
    raw_mappings.clear()


def wr(f: io.TextIOBase, fmt: str, *args) -> None:
    print(fmt, *args, file=f, end="")

def wrln(f: io.TextIOBase, fmt: str, *args) -> None:
    print(fmt, *args, file=f, end="\n")


def write_replacements_rust(output_file: io.TextIOBase, mappings: Iterable[CharMapping]) -> None:
    ob, cb = "{", "}"

    wrln(output_file, "//! An implementation of the folding table from RFC3454 § B.2.")
    wrln(output_file, "//!")
    wrln(output_file, "//! Automatically generated by contrib/gen_case_fold.py.")
    wrln(output_file, "")
    wrln(output_file, "// Any manual changes will be lost.")
    wrln(output_file, "")
    wrln(output_file, "")
    wrln(output_file, "/// Maps the characters in the source string according to RFC3454 § B.2.")
    wrln(output_file, "pub(crate) fn map_rfc3454_b2(c: char, output: &mut String) {")

    first_mapping = True
    for mapping in mappings:
        if first_mapping:
            first_mapping = False
            wr(output_file, "        ")
        else:
            wr(output_file, " else ")

        wr(output_file, mapping.to_rust())

    wrln(output_file, " else {")
    wrln(output_file, f"        write!(output, \"{ob}{cb}\", c).unwrap();")
    wrln(output_file, "    }")
    wrln(output_file, "}")


def main():
    parser = argparse.ArgumentParser(
        description=(
            "Loads a case-folding rule list in the format of RFC3454 section B.2 (referenced by"
            " RFC4518) and converts it into equivalent Rust code."
        ),
    )
    parser.add_argument(
        dest="input_file", type=argparse.FileType("r", encoding="utf-8"), nargs="?",
        default=sys.stdin,
        help=(
            "Input file (RFC3454 section B.2 format). Reads from stdin by default or when passed"
            " \"-\"."
        ),
    )
    parser.add_argument(
        dest="output_file", type=argparse.FileType("w", encoding="utf-8"), nargs="?",
        default=sys.stdout,
        help="Output file (Rust source code). Writes to stdout by default or when passed \"-\".",
    )
    args = parser.parse_args()

    with args.output_file:
        mappings = []

        with args.input_file:
            raw_mappings = table_to_raw_mappings(args.input_file)
            collect_block_mappings(mappings, raw_mappings)
            collect_pair_mappings(mappings, raw_mappings)
            collect_singleton_mappings(mappings, raw_mappings)

        mappings.sort(key=lambda mapping: mapping.sort_key)

        write_replacements_rust(args.output_file, mappings)


if __name__ == "__main__":
    main()
