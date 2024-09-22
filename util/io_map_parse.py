import re

addrpart = r"(?P<address>(\d|[A-F])+)h"
sizepart = r"(?P<size>\d|-|2x((\d|[A-F])+h))?"
rwpart = r"(?P<mut>(R/W|R|W|-))"
namepart = r"(?P<name>(\S|\(|\)|\?|-)+)"
descpart = r"(?P<desc>.+)"
r = re.compile(rf"{addrpart}\s\s{sizepart}\s+{rwpart}\s+{namepart}\s+{descpart}")

with open("io_map.txt") as f:
    lines = tuple(f)

for line in lines:
    m = r.match(line)
    if not m:
        continue

    # print(m.group("desc"))
    # print(m.group("name"))

    readonly = False

    match m.group("mut"):
        case "R":
            readonly = True
        case "W":
            readonly = False
        case "R/W":
            readonly = False

    dt = "u32"
    # match m.group("size"):
    #     case "4":
    #         dt = ""
    #     case "2":
    #         dt = "gshort_t"
    #     case "2x10h":
    #         dt = "gshort_t"

    ty = f"const {dt}"
    # if readonly:
    #     ty = f"const {dt} * const"
    # else:
    #     ty = f"{dt} * const"

    match m.group("name"):
        case "?":
            name = "UNKNOWN_" + m.group("address")
        case "-":
            name = ""
        case "(3DS)":
            name = ""
        case _:
            name = m.group("name")

    if not name:
        continue

    print(f"// {m.group('desc')}")
    print(f"{ty} {m.group('name')} = 0x{m.group('address')};\n")
