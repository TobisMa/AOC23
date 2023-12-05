# -*- coding: utf-8 -*-

import builtins
import re
import threading
from decimal import Decimal
from typing import List


PAT = re.compile(r"\d+ \d+ \d+")


class DecimalRange:
    __slots__ = ("start", "end")
    
    def __init__(self, start, end):
        self.start = Decimal(start)
        self.end = Decimal(end)
    
    def __contains__(self, value):
        return value >= self.start and value < self.end
    
    def __repr__(self) -> str:
        return "<DecimalRange [%s;%s)>" % (self.start, self.end)

range = DecimalRange

def solve(inp: str) -> str:
    seeds, other, *_ = inp.split("\n", 1)
    seeds = list(map(Decimal, seeds[7:].split()))
    s = []
    for i in builtins.range(0, len(seeds), 2):  # shadows above
        s.append(seeds[i:i+2])
    
    seeds = []
    for (a, b) in s:
        seeds.append(DecimalRange(a, a+b))

    maps = []
    
    for line in other.splitlines():
        if not line:
            maps.append([])
        
        elif PAT.fullmatch(line):
            # a, b, c = map(int, line.split())
            a, b, c = map(Decimal, line.split())
            maps[-1].append((range(a, a+c), range(b, b+c)))
        
        else: print("Ignored line %r" % line)
    
    threads: List[threading.Thread] = []
    result = []
    calc_map = {}
    result_seed = {}
    for seedrange in seeds:
        x = Decimal(seedrange.start)
        while x < seedrange.end:
            jmp = convert(x, maps, result, calc_map, result_seed)
            x += jmp
       

    # result = list(map(lambda x: x[-1], result))
    return min(result)


def convert(s, map, r, c, sr):
    if sr.get(s) is not None:
        return sr[s]
    old = Decimal(s)
    new = Decimal()
    jmp = 0
    for group in map:
        new = old
        gt = tuple(y for x in group for y in x), new
        if c.get(gt) is not None:
            new = c[gt]
            continue
        
        for ran in group:
            if old in ran[1]:
                new = ran[0].start + old - ran[1].start
                c[gt] = new
                if jmp == 0:
                    jmp = ran[1].end - old
                break
        else:
            jmp = s
        
        if old > new and new < 0:
            print(f"Got smaller from {old} to {new}")
        old = new

    sr[s] = new
    # print(s, "is done")
    if not r or min(r) > new:
        r.append(new)
    
    return Decimal(jmp)
    

def main():
    with open("day05\\data_day05.txt") as f:
    # with open("day05\\example.txt") as f:
    # with open("day05\\flo5.txt") as f:
        res = solve(f.read())
    
    print(res)

if __name__ == "__main__":
    main()
