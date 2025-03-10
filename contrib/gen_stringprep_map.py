#!/usr/bin/env python3
#
# Loads a case-folding rule list in the format of RFC3454 section B.2 (referenced by RFC4518) and
# converts it into equivalent Rust code.
#
import argparse
from collections import defaultdict
import io
import sys
from typing import Collection, DefaultDict, Mapping, Tuple


class CharMapping:
    @property
    def covered_range(self) -> Tuple[int, int]:
        raise NotImplementedError()

    def to_rust_mapping_target(self) -> str:
        raise NotImplementedError()

    def to_rust(self) -> str:
        ob, cb = "{", "}"
        return f"    Mapping::new('\\u{ob}{self.covered_range[0]:02X}{cb}', '\\u{ob}{self.covered_range[1]:02X}{cb}', {self.to_rust_mapping_target()}),"


class AllToOneMapping(CharMapping):
    def __init__(self, first_point: int, last_point: int, to_points: list[int]) -> None:
        self.first_point: int = first_point
        self.last_point: int = last_point
        self.to_points: list[int] = to_points

    @property
    def covered_range(self) -> Tuple[int, int]:
        return (self.first_point, self.last_point)

    def to_rust_mapping_target(self) -> str:
        ob, cb = "{", "}"
        target_count = len(self.to_points)
        if target_count == 0:
            return f"MappingTarget::ThrowAway"
        else:
            try:
                variant = {
                    1: "One",
                    2: "Two",
                    3: "Three",
                    4: "Four",
                }[target_count]
            except KeyError as e:
                raise ValueError(f"don't know which enumeration variant to take for {len(result_count)} target characters") from e
            char_list_str = ", ".join(f"'\\u{ob}{to_point:02X}{cb}'" for to_point in self.to_points)
            return f"MappingTarget::{variant}({char_list_str})"
        return "".join(pieces)

    def __repr__(self) -> str:
        pts = ", ".join(f"0x{to_point:02X}" for to_point in self.to_points)
        return f"AllToOneMapping(first_point=0x{self.first_point:02X}, last_point=0x{self.last_point:02X}, to_points=[{pts}])"


class BlockMapping(CharMapping):
    def __init__(self, first_point: int, last_point: int, code_offset: int, trailing_constants: list[int]) -> None:
        self.first_point: int = first_point
        self.last_point: int = last_point
        self.code_offset: int = code_offset
        self.trailing_constants: list[int] = trailing_constants

    @property
    def covered_range(self) -> Tuple[int, int]:
        return (self.first_point, self.last_point)

    def to_rust_mapping_target(self) -> str:
        if not self.trailing_constants:
            return f"MappingTarget::BlockShift({self.code_offset})"
        elif len(self.trailing_constants) == 1:
            ob, cb = "{", "}"
            return f"MappingTarget::BlockShiftAndConst({self.code_offset}, '\\u{ob}{self.trailing_constants[0]:02X}{cb}')"
        else:
            raise ValueError(f"don't know MappingTarget variant for block shift with {len(self.trailing_constants)} trailing constants")

    def __repr__(self) -> str:
        return f"BlockMapping(first_point=0x{self.first_point:02X}, last_point=0x{self.last_point:02X}, code_offset={self.code_offset}, trailing_constants={self.trailing_constants!r})"


class PairMapping(CharMapping):
    def __init__(self, first_point: int, last_point: int, remainder_by_2: int, code_offset: int) -> None:
        self.first_point: int = first_point
        self.last_point: int = last_point
        self.remainder_by_2: int = remainder_by_2

    @property
    def covered_range(self) -> Tuple[int, int]:
        return (self.first_point, self.last_point)

    def to_rust_mapping_target(self) -> str:
        return f"MappingTarget::PairShift({self.remainder_by_2})"

    def __repr__(self) -> str:
        return f"PairMapping(first_point=0x{self.first_point:02X}, last_point=0x{self.last_point:02X}, remainder_by_2={self.remainder_by_2})"


def table_to_raw_mappings(table: io.TextIOBase) -> dict[str, str]:
    raw_mappings: dict[str, str] = {}
    for raw_line in table:
        line = raw_line.removesuffix("\n").strip()
        (original_str, replacement_str, reason) = line.split("; ")
        if "-" in original_str:
            first_str, last_str = original_str.split("-")
            first, last = int(first_str, 16), int(last_str, 16)
        else:
            original = int(original_str, 16)
            first = last = original
        replacements = "".join(chr(int(piece, 16)) for piece in replacement_str.split(" ") if piece.strip())
        for code in range(first, last+1):
            raw_mappings[chr(code)] = replacements

    return raw_mappings


def collect_all_to_one_mappings(mappings: list[CharMapping], raw_mappings: dict[str, str]) -> None:
    # handles the common pattern where a range is mapped to one character/sequence
    if len(raw_mappings) < 2:
        return
    assert all(len(source) == 1 for (source, _target) in raw_mappings.items())

    sorted_raw_mappings = [(ord(k), v) for (k, v) in raw_mappings.items()]
    sorted_raw_mappings.sort()

    prev_source, prev_targets = sorted_raw_mappings[0]
    start_source = prev_source
    for next_source, next_targets in sorted_raw_mappings[1:]:
        if next_source != prev_source + 1 or next_targets != prev_targets:
            # that's it for that block

            # is it even a block?
            if prev_source > start_source:
                mappings.append(AllToOneMapping(start_source, prev_source, [ord(c) for c in prev_targets]))
                for code in range(start_source, prev_source+1):
                    del raw_mappings[chr(code)]

            # start a new block
            start_source = next_source

        prev_source, prev_targets = next_source, next_targets

    # do we have a final block?
    if prev_source > start_source:
        mappings.append(AllToOneMapping(start_source, prev_source, [ord(c) for c in prev_targets]))
        for code in range(start_source, prev_source+1):
            del raw_mappings[chr(code)]


def collect_block_mappings(mappings: list[CharMapping], raw_mappings: dict[str, str]) -> None:
    # handles the common pattern: A B C D E F ... a b c d e f ...

    if len(raw_mappings) < 2:
        return
    assert all(len(source) == 1 for (source, _target) in raw_mappings.items())

    # only look at entries that map to at least one character
    # the first character is the most important to us
    sorted_raw_mappings = [(ord(k), ord(v[0]), [ord(vc) for vc in v[1:]]) for (k, v) in raw_mappings.items() if len(v) > 0]
    if len(sorted_raw_mappings) < 2:
        return
    sorted_raw_mappings.sort()

    prev_source, prev_target, prev_tail = sorted_raw_mappings[0]
    start_source, start_delta = prev_source, (prev_target - prev_source)
    for next_source, next_target, next_tail in sorted_raw_mappings[1:]:
        if next_source - prev_source != 1 or next_target - prev_target != 1 or prev_tail != next_tail:
            # that's it for this block

            if prev_source - start_source > 0:
                # perfect, we have more than one such entry; store the block
                mappings.append(BlockMapping(start_source, prev_source, start_delta, prev_tail))

                # delete these mappings from raw_mappings
                for code in range(start_source, prev_source+1):
                    del raw_mappings[chr(code)]

            # start a new block
            start_source, start_delta = next_source, (next_target - next_source)

        # remember for next time
        prev_source, prev_target, prev_tail = next_source, next_target, next_tail

    # do we have a final block?
    if prev_source - start_source > 0:
        # perfect, we have more than one such entry; store the block
        mappings.append(BlockMapping(start_source, prev_source, start_delta, prev_tail))

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
    if len(sorted_raw_mappings) < 2:
        return
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
        mappings.append(AllToOneMapping(ord(k), ord(k), [ord(v) for v in vs]))
    raw_mappings.clear()


def wr(f: io.TextIOBase, fmt: str, *args) -> None:
    print(fmt, *args, file=f, end="")

def wrln(f: io.TextIOBase, fmt: str, *args) -> None:
    print(fmt, *args, file=f, end="\n")


def write_replacements_rust(output_file: io.TextIOBase, mappings: Collection[CharMapping]) -> None:
    ob, cb = "{", "}"

    wrln(output_file, "//! A sorted list of character mappings.")
    wrln(output_file, "//!")
    wrln(output_file, "//! Automatically generated by contrib/gen_case_fold.py.")
    wrln(output_file, "")
    wrln(output_file, "// Any manual changes will be lost.")
    wrln(output_file, "")
    wrln(output_file, "")
    wrln(output_file, "use crate::stringprep::mapping::{Mapping, MappingTarget};")
    wrln(output_file, "")
    wrln(output_file, "")
    wrln(output_file, "/// The mapping.")
    wrln(output_file, f"pub(crate) const MAPPING: [Mapping; {len(mappings)}] = [")

    for mapping in mappings:
        wrln(output_file, mapping.to_rust())

    wrln(output_file, "];")


def main():
    parser = argparse.ArgumentParser(
        description=(
            "Loads a case-folding rule list in the format of RFC3454 section B.2 (referenced by"
            " RFC4518) and converts it into equivalent Rust code."
        ),
    )
    parser.add_argument(
        "-o", "--output",
        dest="output_file", type=argparse.FileType("w", encoding="utf-8"),
        default=sys.stdout,
        help="Output file (Rust source code). Writes to stdout by default or when passed \"-\".",
    )
    parser.add_argument(
        dest="input_files", type=argparse.FileType("r", encoding="utf-8"), nargs="*",
        default=None,
        help=(
            "Input file (RFC3454 section B.2 format). Reads from stdin by default or when passed"
            " \"-\"."
        ),
    )
    args = parser.parse_args()
    if args.input_files is None:
        args.input_files = [sys.stdin]

    with args.output_file:
        mappings = []

        raw_mappings = {}
        for input_file in args.input_files:
            with input_file:
                raw_mappings.update(table_to_raw_mappings(input_file))

        collect_all_to_one_mappings(mappings, raw_mappings)
        collect_block_mappings(mappings, raw_mappings)
        collect_pair_mappings(mappings, raw_mappings)
        collect_singleton_mappings(mappings, raw_mappings)

        mappings.sort(key=lambda mapping: mapping.covered_range)
        for i in range(len(mappings)-1):
            prev, succ = mappings[i], mappings[i+1]
            if prev.covered_range[1] >= succ.covered_range[0]:
                raise ValueError(f"overlap between {prev} and {succ}")

        write_replacements_rust(args.output_file, mappings)


if __name__ == "__main__":
    main()
