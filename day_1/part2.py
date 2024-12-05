
from collections import Counter


def load_data():
    with open('data') as f:
        data = [
            [int(i) for i in line.split('   ')]
            for line in f
        ]
    return zip(*data)

def main():
    left, right = load_data()
    left_c = Counter(left)
    right_c = Counter(right)
    sim_score = 0
    for num in left_c:
        if num in right_c:
            sim_score += num*right_c[num]*left_c[num]
    print(sim_score)

if __name__ == '__main__':
    for i in range(100000):
        main()