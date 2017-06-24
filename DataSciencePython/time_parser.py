import csv
import random


def process(input_file, output_file):
    names = load_data(input_file)
    data = list(map(generate_time, names))
    dump_data(output_file, data)


def generate_time(name):
    return name, ('START', random.randint(0, 24)), ('STOP', random.randint(0, 24))


def load_data(input_file):
    data = []
    with open(input_file, mode='r', encoding='utf-8') as file:
        reader = csv.reader(file, delimiter=',')
        for line in reader:
            data.append(line[0])
    return set(data)


def dump_data(output_file, data):
    with open(output_file, mode='w', encoding='utf-8') as file:
        writer = csv.writer(file, delimiter=',')
        for item in data:
            row = []
            row.append(item[0])
            row.append(item[1][0])
            row.append(item[1][1])
            writer.writerow(row)
            row = []
            row.append(item[0])
            row.append(item[2][0])
            row.append(item[2][1])
            writer.writerow(row)

if __name__ == '__main__':
    process('./data/time.txt', './output/time.txt')
