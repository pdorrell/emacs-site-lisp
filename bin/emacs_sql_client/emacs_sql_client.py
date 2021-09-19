import sqlalchemy
from sqlalchemy import create_engine
import traceback
import sys

THICK_LINE = "=" * 80
THIN_LINE = "-" * 60

class PagedRowWriter:

    def __init__(self, page_size = 2000):
        self.page_size = page_size

    def format_value(self, value):
        return "" if value is None else str(value)

    def width_padded_row(self, values, widths):
        padded_values = [value + " " *(width-len(value)) for value, width in zip(values, widths)]
        return "  ".join(padded_values)

    def write_out_page(self, rows, columns):
        formatted_rows = [[self.format_value(value) for value in row] for row in rows]
        column_widths = [len(column) for column in columns]
        for formatted_row in formatted_rows:
            for i, formatted_value in enumerate(formatted_row):
                column_widths[i] = max(column_widths[i], len(formatted_value))

        yield self.width_padded_row(columns, column_widths)
        yield ""
        for formatted_row in formatted_rows:
            yield self.width_padded_row(formatted_row, column_widths)

    def write_rows(self, rows, columns):
        any_rows_written = False
        page_of_rows = []
        num_rows = 0
        for row in rows:
            page_of_rows.append(row)
            num_rows += 1
            if len(page_of_rows) >= self.page_size:
                yield from self.write_out_page(page_of_rows, columns)
                page_of_rows = []
                any_rows_written = True
        if page_of_rows or not any_rows_written:
            yield from self.write_out_page(page_of_rows, columns)
            any_rows_written = True
        yield ""
        yield "(%d rows found)" % num_rows


class LineScriptable:

    def __init__(self, output_writer):
        self.output_writer = output_writer
        self.params = None
        self.params_count = None

    def read_param_count(self, line):
        self.params_count = int(line)
        self.params = []

    def read_param(self, line):
        self.params.append(line)
        if len(self.params) == self.params_count:
            self.process_command(self.params[0], self.params[1:])
            self.params = None
            self.params_count = None

    def process_line(self, line):
        if self.params_count is None:
            self.read_param_count(line)
        else:
            self.read_param(line)

    def process_input_lines(self, lines):
        for line in lines:
            self.output_writer("IN: %s" % line)
            self.process_line(line)

    def process_command(self, command, params):
        getattr(self, command)(*params)
        self.output_writer(THICK_LINE)

class SqlClient(LineScriptable):

    def rewrite_jdbc_url(self, jdbc_url):
        unprefixed_url = jdbc_url[5:]
        prefix, colon, url_path = unprefixed_url.partition(':')
        if colon != ':':
            raise Exception("Missing ':' in %r from %r" % (unprefixed_url, jdbc_url))
        if prefix == 'sqlite':
            return 'sqlite:///%s' % url_path
        else:
            return url_path

    def setConnection(self, db_url, driver_class):
        if db_url.startswith("jdbc:"):
            actual_db_url = self.rewrite_jdbc_url(db_url)
        else:
            actual_db_url = db_url
        self.engine = create_engine(actual_db_url)
        self.output_writer("Connection set to URL: %s, driver = %s" % (db_url, driver_class))

    def add_user_and_password(self, db_url, user, password):
        """Like mysql://scott:tiger@hostname/dbname"""
        db_type, separator, rest_of_url = db_url.partition("://")
        if separator == "://":
            return "%s://%s:%s@%s" % (db_type, user, password, rest_of_url)
        else:
            raise Exception("Cannot add username & password to %r - no '://' found" %
                            db_url)

    def setConnectionWithUserPassword(self, db_url, driver_class, user, password):
        if db_url.startswith("jdbc:"):
            actual_db_url = self.rewrite_jdbc_url(db_url)
        else:
            actual_db_url = db_url
        actual_db_url = self.add_user_and_password(actual_db_url, user, password)
        print("actual_db_url = %r" % actual_db_url)
        self.engine = create_engine(actual_db_url)
        self.output_writer("Connection set to URL: %s, driver = %s, user = %s" % (db_url, driver_class, user))

    def executeQuery(self, sql):
        self.output_writer('QUERY: %s' % sql)
        with self.engine.connect() as connection:
            try:
                result = connection.execute(sqlalchemy.text(sql))
                if result.returns_rows:
                    self.output_writer(THIN_LINE)
                    for line in PagedRowWriter().write_rows(result, columns = list(result.keys())):
                        self.output_writer(line)
                else:
                    updated_rows = 0 if result.rowcount == -1 else result.rowcount
                    self.output_writer("Updated rows: %d" % updated_rows)
            except sqlalchemy.exc.OperationalError as oe:
                self.output_writer('SQL ERROR: %s' % oe.orig)
            except Exception as e:
                #self.output_writer(traceback.format_exc())
                raise

def write_output(text):
    print(text)

def input_lines():
    for line in sys.stdin:
        if line.endswith("\n"):
            line = line[:-1]
        yield line

def main():
    sql_client = SqlClient(output_writer = write_output)
    sql_client.process_input_lines(input_lines())
    write_output("END OF INPUT")

if __name__ == "__main__":
    main()
