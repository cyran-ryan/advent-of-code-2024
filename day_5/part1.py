
from collections import defaultdict


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
    print(scan_dict)
    print(updates)
    good_updates = []
    for row in updates:
        found_pages = set()
        for i in range(len(row)):
            if row[i] in scan_dict:
                if found_pages.intersection(scan_dict[row[i]]):
                    print('Found a cycle')
                    break
            found_pages.add(row[i])
        else:
            good_updates.append(row)
    print(good_updates)
    nums_to_sum = []
    for row in good_updates:
        nums_to_sum.append(int(row[int(len(row)/2)]))
    print(nums_to_sum)
    print(sum(nums_to_sum))

if __name__ == '__main__':
    main()