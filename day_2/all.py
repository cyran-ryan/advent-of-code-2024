import math
import numpy as np


def load_data():
    with open("data") as f:
        result = [[int(y) for y in x.split(" ")] for x in f]
        max_cols = max([len(x) for x in result])
        second_result = []
        
        return result

def diff_is_good(result):
    if abs(result) > 3 or result == 0:
        return False
    return True
    
def sign_is_good(sign, current_sign):
    return sign is None or sign == current_sign
    
def is_good(prev, curr, sign):
    result = prev - curr
    current_sign = 1 if result > 0 else -1
    if sign is None:
        sign = current_sign
        #return True, sign
    if not sign_is_good(sign, current_sign):
        print(f"sign: {sign}, current_sign: {current_sign} result: {result}")
        return False, sign
    if not diff_is_good(result):
        print(f"result: {result}")
        return False, sign
    return True, sign

def report_is_safe(data):
    prev = None
    sign_ = None
    i = 0
    print(f"data: {data}")
    for i, d in enumerate(data[1:], start=1):
        good, sign_ = is_good(data[i-1], d, sign_)
        print(f"i: {i}, d: {d}, good: {good}, sign: {sign_}")
        if not good:
            return False, [i-1, i, i+1]
    return True, []

def is_safe_with_removals(data, possible_vals):
    for val in possible_vals:
        if val < 0 or val >= len(data):
            continue
        new_data = data.copy()
        new_data.pop(val)
        good, _ = report_is_safe(new_data)
        if good:
            return True
    return False
def main():
    safe = 0
    all_data = []
    with open("report.txt", 'w') as f:
        for data in load_data():
            f.write(f"{data}: ")
            good, possible_vals = report_is_safe(data)
            all_data.append((data, 1 if good else 0))
            if good:
                safe += 1
                print("Safe")
                f.write("safe")
            """else:
                if is_safe_with_removals(data, possible_vals):
                    safe += 1
                    print("Safe")
                    f.write("safe")
                else:
                    print("Not safe")
                    f.write("unsafe")
            f.write("\n")"""
    print(safe)
    print(all_data)
if __name__ == '__main__':
    main()