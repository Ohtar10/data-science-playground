import csv
import datetime
from functools import reduce
from collections import Counter

import numpy
# This module requires tkinter installed but is a standalone package not pip so:
# sudo dnf install python3-tkinter.x86_64
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

EQUALS = lambda a, b: a == b
CONTAINS = lambda a, b: a in b
GREATER = lambda a, b: a > b
LESS = lambda a, b: a < b
BETWEEN = lambda a, b, c: b <= a <= c

# this is needed in order to avoid writing strings in bytes strings i.e. b'string' but 'string' instead
fn = lambda row: [j.decode() if isinstance(j, bytes) else j for j in row]


class Group:
    name = None
    predicate = None
    attributes = None
    matched = False

    def __init__(self, name, predicate, *args):
        self.name = name
        self.predicate = predicate
        self.attributes = args

    def test(self, value):
        result = self.predicate(value, *self.attributes)
        if not self.matched and result:
            self.matched = True
        return result


class RawDataCollector:
    """This data collector will work
    with the raw data represented as a python list
    loaded directly from the file contents, it will not
    add any extra functionality and the purpose is to
    compare against the collector using numpy
    """
    filename = None
    delimiter = None
    encoding = None
    data = None
    skip_header = False

    def __init__(self, filename='data.csv', delimiter='\t', encoding='utf-8'):
        self.filename = filename
        self.delimiter = delimiter
        self.encoding = encoding

    def load_file(self):
        """ Will load all the records for the given file
         into the data attribute of the class for later
         processing.
        """
        data = []
        with open(self.filename, mode='r', encoding=self.encoding) as file:
            reader = csv.reader(file, delimiter=self.delimiter)
            for row in reader:
                data.append(row)

        self.skip_header = True
        self.data = data

    def dump_file(self, output):
        if not output:
            output = self.filename+'-'+str(datetime.datetime.now())+'.csv'

        with open(output, mode='w', encoding=self.encoding) as file:
            dump = csv.writer(file, delimiter=self.delimiter)
            dump.writerows(self.data)

    def dump_line_chart(self, output, g_title, x_label, y_label, index=0):
        figure = plt.figure()
        ax = figure.add_subplot(1, 1, 1)
        # Use only the given column to graph
        sample = [x[index] for x in self.data[1 if self.skip_header else 0:]]

        # Sort the values as a float value
        field = sorted(map(float, sample))

        # establish the x axis values
        x_axis_ticks = list(range(len(sample)))
        ax.plot(x_axis_ticks, field, linewidth=2)
        ax.set_title(g_title)
        # establish the maximum x value
        ax.set_xlim([0, len(sample)])
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)

        figure.savefig(output)

    def dump_full_bar_chart(self, output, index=0):
        sample = [x[index] for x in self.data[1 if self.skip_header else 0:]]

        field = list(map(int, map(float, sample)))
        X = numpy.arange(len(field))
        width = 0.25
        plt.bar(X+width, field, width)
        plt.xlim([0, len(field)])
        plt.savefig(output)

    def dump_bar_chart(self, output, g_title, x_label, y_label, groups, index=0):
        figure = plt.figure()
        ax = figure.add_subplot(1, 1, 1)
        plt.style.use('ggplot')
        colors = plt.rcParams['axes.color_cycle']

        sample = [x[index] for x in self.data[1 if self.skip_header else 0:]]
        field = list(map(int, map(float, sample)))

        # generate the groups that are equivalent to the bars
        tally = Counter()
        for value in field:
            bucket = 0
            for group in groups:
                if group.test(value):
                    bucket = groups.index(group) + 1

            if bucket == 0:
                bucket = 7

            tally[bucket] += 1

        for bar in tally:
            ax.bar(bar, tally[bar], color=colors[bar % len(tally)])

        labels = [g.name for g in groups if g.matched]
        ax.legend(labels)

        ax.set_title(g_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        ax.set_xticklabels(labels, ha='left')
        ax.set_xticks(range(1, len(tally)+1))

        plt.grid(True)

        figure.savefig(output)

        pp = PdfPages(output+'.pdf')
        pp.savefig(figure, bbox_inches='tight')
        pp.close()

    def get_total_records(self):
        """ Will return the total number of
        records loaded from the file, omitting
        the header record.
        """
        if self.skip_header:
            return len(self.data) - 1
        else:
            return len(self.data)

    def sum_by_field_index(self, index=0):
        # Sum skipping the header
        values = [float(x[index]) for x in self.data[1 if self.skip_header else 0:]]
        # another way with lambda
        # values = list(map(lambda x: float(x[index]), self.data[1:]))
        return sum(values)

    def average_by_field_index(self, index=0):
        total_records = self.get_total_records()
        total_value = self.sum_by_field_index(index)
        return total_value / total_records

    def max_value_by_field_index(self, index=0):
        # Here we use a map and a lambda function to get the float values
        values = self.get_float_values_for_field_index(index)
        # Then use a reduce and lambda function to get the max value
        return reduce(lambda x, y: x if x > y else y, values)

    def min_value_by_field_index(self, index=0):
        values = self.get_float_values_for_field_index(index)
        return reduce(lambda x, y: x if x < y else y, values)

    def get_float_values_for_field_index(self, index=0):
        return list(map(lambda x: float(x[index]), self.data[1 if self.skip_header else 0:]))

    def count_by_criteria_index_field(self, criteria, predicate, index=0):
        count = 0
        for row in self.data[1 if self.skip_header else 0:]:
            if predicate(criteria, row[index]):
                count += 1
        return count

    def filter_by_criteria_index_field(self, criteria, predicate, index=0):
        return list(filter(lambda x: predicate(criteria, type(criteria)(x[index])), self.data[1 if self.skip_header else 0:]))

    def filter_by_criteria_collector(self, criteria, predicate, index=0):
        data = self.filter_by_criteria_index_field(criteria, predicate, index)
        collector = RawDataCollector()
        collector.data = data
        return collector


class DataCollector:
    """This data collector demonstrates
    the use of numpy for processing the data
    loaded from the file
    """
    filename = 'data.csv'
    delimiter = '\t'
    encoding = 'utf-8'
    data = None
    fieldnames = None
    datatypes = None

    def __init__(self, **kwargs):
        for k, v in kwargs.items():
            setattr(self, k, v)

    def load_file(self):
        self.data = numpy.genfromtxt(self.filename,
                                     delimiter=self.delimiter,
                                     skip_header=1,
                                     invalid_raise=False,
                                     names=self.fieldnames,
                                     dtype=self.datatypes)

    def dump_file(self, output):
        if not output:
            output = self.filename+'-'+str(datetime.datetime.now())+'.csv'

        # with open(output, mode='w', encoding=self.encoding) as file:
        #    dump = csv.writer(file, delimiter=self.delimiter)
        #    dump.writerows(self.data)
        #    numpy.savetxt(output, self.datatypes, delimiter=self.delimiter, fmt='%s')

        with open(output, mode='w', encoding=self.encoding) as file:
            dump = csv.writer(file, delimiter=self.delimiter)
            for r in self.data:
                dump.writerow(fn(r))

    def get_total_records(self):
        return self.data.size

    def sum_by_field(self, fieldname):
        values = self.data[fieldname]
        float_values = [float(line) for line in values]
        return numpy.sum(float_values)

    def average_by_field(self, fieldname):
        values = self.data[fieldname]
        float_values = [float(line) for line in values]
        return numpy.average(float_values)

    def max_min_by_field(self, fieldname):
        values = self.data[fieldname]
        return numpy.amax(values), numpy.amin(values)

    def filter_by_field(self, fieldname, predicate, criteria):
        return list(filter(lambda x: predicate(criteria, x[fieldname]), self.data))

    def filter_by_field_collector(self, fieldname, predicate, criteria):
        data = numpy.array(self.filter_by_field(fieldname, predicate, criteria))
        collector = DataCollector()
        collector.data = data
        return collector


if __name__ == '__main__':
    raw_data_collector = RawDataCollector('./data/data.csv')
    raw_data_collector.load_file()
    title = "With raw data collector"
    print("=" * len(title))
    print(title)
    print("=" * len(title))
    print("Total records for file {} is {}".format(raw_data_collector.filename, raw_data_collector.get_total_records()))
    print("Sum of all prices i.e. field index 2 is {}".format(raw_data_collector.sum_by_field_index(2)))
    print("Average of all prices i.e. field index 2 is {}".format(raw_data_collector.average_by_field_index(2)))
    print("Maximum price i.e. field index 2 is {}".format(raw_data_collector.max_value_by_field_index(2)))
    print("Minimum price i.e. field index 2 is {}".format(raw_data_collector.min_value_by_field_index(2)))
    print("Amount of cashmere ties is: {}".format(
        raw_data_collector.count_by_criteria_index_field('cashmere', CONTAINS, 7)))
    print("Ties made of _silk is: {}".format(
        len(raw_data_collector.filter_by_criteria_index_field('_silk', CONTAINS, 10))))
    print("Ties made of _wool is: {}".format(
        len(raw_data_collector.filter_by_criteria_index_field('_wool', CONTAINS, 10))))
    print("Ties made of _cotton is: {}".format(
        len(raw_data_collector.filter_by_criteria_index_field('_cotton', CONTAINS, 10))))
    print("Ties that cost less than $20: {}".format(
        len(raw_data_collector.filter_by_criteria_index_field(20.0, LESS, 2))
    ))

    secondary_collector = raw_data_collector.filter_by_criteria_collector('cashmere', CONTAINS, 7)
    print("Max - Min Prices of cashmere ties: Max: ${:03.2f} Min: ${:03.2f}".format(
        secondary_collector.max_value_by_field_index(2),
        secondary_collector.min_value_by_field_index(2)
    ))
    print("Average Price of cashmere ties: ${:03.2f}".format(
        secondary_collector.average_by_field_index(2)
    ))
    print("Total Price of all cashmere ties: ${:03.2f}".format(
        secondary_collector.sum_by_field_index(2)
    ))
    print("Total records of all cashmere ties: {}".format(
        secondary_collector.get_total_records()
    ))
    # let's dump the secondary collector for future reference
    secondary_collector.dump_file('./output/cashmere-ties.csv')

    # let's dump the secondary collector as a chart
    secondary_collector.dump_line_chart('./output/cashmere-ties.png',
                                        'Cashmere Ties Prices',
                                        'Prices in $',
                                        'Number of ties',
                                        2)

    # let's dump the same data set but using bar chart
    secondary_collector.dump_full_bar_chart('./output/cashmere-ties-bar.png', 2)

    # now let's generate a more readable chart by using bar groups instead of every single value
    # define the groups
    groups = [Group('$0-50', BETWEEN, 0, 50),
              Group('$50-100', BETWEEN, 50, 100),
              Group('$100-150', BETWEEN, 100, 150),
              Group('$150-200', BETWEEN, 150, 200),
              Group('$200-250', BETWEEN, 200, 250),
              Group('$250+', GREATER, 250)]
    secondary_collector.dump_bar_chart('./output/cashmere-ties-enhanced-bar.png',
                                       'Cashmere Ties Prices',
                                       'Prices in $',
                                       'Number of ties',
                                       groups,
                                       2)

    # and now the bars for all ties possible
    raw_data_collector.dump_bar_chart('./output/ties-enhanced-bar.png',
                                       'Ties Prices',
                                       'Prices in $',
                                       'Number of ties',
                                       groups,
                                       2)

    fieldNames = ['', 'id', 'priceLabel', 'name', 'brandId', 'brandName', 'imageLink',
                  'desc', 'vendor', 'patterned', 'material']
    # https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html
    dataTypes = [('myint', 'i'), ('myid', 'i'), ('price', 'f8'), ('name', 'a200'),
                 ('brandId', '<i8'), ('brandName', 'a200'), ('imageUrl', '|S500'),
                 ('description', '|S900'), ('vendor', '|S100'), ('pattern', '|S50'), ('material', '|S50'), ]

    data_collector = DataCollector(filename='./data/data.csv', fieldnames=fieldNames, datatypes=dataTypes)
    data_collector.load_file()
    title = "With enhanced data collector"
    print("=" * len(title))
    print(title)
    print("=" * len(title))
    print("Total records for file {} is {}".format(data_collector.filename, data_collector.get_total_records()))
    print("Sum of all prices i.e. field 'priceLabel' is {}".format(data_collector.sum_by_field('priceLabel')))
    print("Average of all prices i.e. field 'priceLabel' is {}".format(data_collector.average_by_field('priceLabel')))
    vmax, vmin = data_collector.max_min_by_field('priceLabel')
    print("Maximum price i.e. field 'priceLabel' is {}".format(vmax))
    print("Minimum price i.e. field 'priceLabel' is {}".format(vmin))
    # Here with the usage of numpy data set we need to use bytes strings instead of unicode strings
    print("Amount of cashmere ties is: {}".format(
        len(data_collector.filter_by_field('desc', CONTAINS, b'cashmere'))))
    print("Ties made of _silk is: {}".format(
        len(data_collector.filter_by_field('material', EQUALS, b'_silk'))))
    print("Ties made of _wool is: {}".format(
        len(data_collector.filter_by_field('material', EQUALS, b'_wool'))))
    print("Ties made of _cotton is: {}".format(
        len(data_collector.filter_by_field('material', EQUALS, b'_cotton'))))
    print("Ties that cost less than $20: {}".format(
        len(data_collector.filter_by_field('priceLabel', LESS, 20.0))
    ))

    secondary_collector = data_collector.filter_by_field_collector('desc', CONTAINS, b'cashmere')
    max_, min_ = secondary_collector.max_min_by_field('priceLabel')
    print("Max - Min Prices of cashmere ties: Max: ${:03.2f} Min: ${:03.2f}".format(
        max_,
        min_
    ))
    print("Average Price of cashmere ties: ${:03.2f}".format(
        secondary_collector.average_by_field('priceLabel')
    ))
    print("Total Price of all cashmere ties: ${:03.2f}".format(
        secondary_collector.sum_by_field('priceLabel')
    ))
    print("Total records of all cashmere ties: {}".format(
        secondary_collector.get_total_records()
    ))

    # let's dump the secondary collector for future reference
    secondary_collector.dump_file('./output/cashmere-ties-enhanced.csv')
