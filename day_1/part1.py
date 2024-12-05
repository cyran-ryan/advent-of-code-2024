
def load_data():
    with open('data') as f:
        data = [
            [int(i) for i in line.split('   ')]
            for line in f
        ]
    return zip(*data)

def main():
    left, right = load_data()
    left = sorted(left)
    right = sorted(right)
    total_diff = 0
    for i in range(len(left)):
        total_diff += abs(left[i]-right[i])
    print(total_diff)

if __name__ == '__main__':
    main()