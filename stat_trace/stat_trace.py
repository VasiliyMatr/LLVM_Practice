from heapq import nlargest
import matplotlib.pyplot as plt
import sys
import re
import itertools

def sliding_window(iterable, n):
    iterables = itertools.tee(iterable, n)

    for iterable, num_skipped in zip(iterables, itertools.count()):
        for _ in range(num_skipped):
            next(iterable, None)

    return zip(*iterables)

# Collect instructions combinations statistics
# Return statistics for most frequent instructions combinations: {combination: rate}
def window_stat(window_size: int, instr_it, stat_size: int) -> dict[str, float]:
    assert(window_size > 0)
    assert(stat_size > 0)

    stat: dict[str, int] = {}

    iterations_num = 0

    for window in sliding_window(instr_it, window_size):
        sequence = '\n'.join([match.group() for match in window])

        count = stat.setdefault(sequence, 0)
        stat[sequence] = count + 1

        iterations_num += 1

    most_freq = nlargest(stat_size, stat.items(), key = lambda x: x[1])

    return {k: float(v) / iterations_num for (k, v) in most_freq}

def plot_stat(window_size: int, stat: dict[str, float]):
    fig = plt.figure(figsize = (10, 5))

    plt.bar(stat.keys(), stat.values())

    plt.ylabel("Rate")
    plt.title("Statistics for window_size = %d" % window_size)
    plt.savefig("stat_%d.png" % window_size, bbox_inches="tight")

def main():
    IN_NAME = sys.argv[1]

    MIN_WINDOW_SIZE = 1
    MAX_WINDOW_SIZE = 5

    STAT_SIZE = 7

    with open(IN_NAME, 'r') as in_file:
        raw_instrs = in_file.read()

    for window_size in range(MIN_WINDOW_SIZE, MAX_WINDOW_SIZE):
        print("Stat for window_size = %d ... " % window_size)
        it = re.finditer(r'[^,]+', raw_instrs)
        stat = window_stat(window_size, it, STAT_SIZE)
        plot_stat(window_size, stat)
        print("Done")

if __name__ == "__main__":
    main()
