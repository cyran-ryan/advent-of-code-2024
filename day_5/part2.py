
from collections import defaultdict

def is_row_valid(row: list[str], scan_dict: dict[str, set[str]]):
    found_pages = set()
    for i in range(len(row)):
        if row[i] in scan_dict:
            if found_pages.intersection(scan_dict[row[i]]):
                return False
        found_pages.add(row[i])
    return True

def fix_row(row: list[str], scan_dict: dict[str, set[str]]):
    print(row)
    cycle = {}
    row_set = set(row)
    for d in row:
        if d in scan_dict:
            cycle[d] = row_set.intersection(scan_dict[d])
    return list(reversed([d[0] for d in sorted(cycle.items(), key=lambda x: len(x[1]))]))

def main():
    scan_dict = defaultdict(set)
    updates = []
    with open('input') as f:
        for line in f:
            if '|' in line:
                split_line = [x.strip() for x in line.split('|')]
                assert len(split_line) == 2
                scan_dict[split_line[0]].add(split_line[1])
            else:
                if parsed_line := [x.strip() for x in line.split(',') if x.strip()]:
                    updates.append(parsed_line)
    good_updates = []
    for row in updates:
        if is_row_valid(row, scan_dict):
            continue
        good_updates.append(fix_row(row, scan_dict))        
    print(good_updates)
    nums_to_sum = []
    for row in good_updates:
        nums_to_sum.append(int(row[int(len(row)/2)]))
    print(nums_to_sum)
    print(sum(nums_to_sum))

if __name__ == '__main__':
    main()