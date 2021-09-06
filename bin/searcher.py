import os
from pathlib import Path
from collections import defaultdict
import sys
import json
import fnmatch
import re
from argparse import ArgumentParser

class CensusResults:

    def __init__(self):
        self.expected_results = defaultdict(list)
        self.unexpected_results = defaultdict(list)

    def add_result(self, is_expected, result_type, result):
        results = self.expected_results if is_expected else self.unexpected_results
        results[result_type].append(str(result))

    def display(self):
        def show_results(label, results):
            print("  %s:" % label)
            if not results:
                print("    (none)")
            for key in sorted(results.keys()):
                paths = results[key]
                print("     %s, %d items: %r" % (key, len(paths), paths))

        print("="*100)
        print("Census results")
        show_results("EXPECTED", self.expected_results)
        show_results("UNEXPECTED", self.unexpected_results)

class PathMatcher:
    def __init__(self, specs):
        def raise_unsupported_glob_error(glob):
            raise Exception('* only supported for **/ prefix, or wildcards in name part: %r' % glob)

        self.names_set = set()
        self.paths_set = set()
        self.located_globs = defaultdict(list)
        self.name_globs = []
        self.specs = specs or []
        for spec in specs:
            if '*' in spec:
                if spec.startswith('**/'):
                    self.names_set.add(spec[3:])
                elif '*' in spec:
                    path_spec, slash, file_spec = spec.rpartition('/')
                    if slash == '/':
                        if '*' in path_spec:
                            raise_unsupported_glob_error(spec)
                        self.located_globs[path_spec].append(file_spec)
                    else:
                        self.name_globs.append(spec)
                else:
                    raise_unsupported_glob_error(spec)
            else:
                self.paths_set.add(Path(spec))

    def description(self):
        return "%r" % self.specs

    def match(self, path):
        if path in self.paths_set:
            return True, 'path'
        elif path.name in self.names_set:
            return True, 'name'
        elif self.matches_glob(path):
            return True, 'glob'
        else:
            return False, None

    def matches_glob(self, path):
        for glob in self.name_globs:
            if fnmatch.fnmatchcase(path.name, glob):
                return True
        parent_path = str(path.parent)
        if parent_path in self.located_globs:
            for glob in self.located_globs[parent_path]:
                if fnmatch.fnmatchcase(path.name, glob):
                    return True
        return False

    def matches(self, path):
        return (path in self.paths_set) or (path.name in self.names_set) or self.matches_glob(path)

class AbstractTextSearch:

    def matches_file_path(self, file_path):
        return self.matches_on_text(file_path.name)

    def search_in_file(self, file_path):
        """Search for value in lines of file"""
        file_text = file_path.read_text()
        if self.matches_on_text(file_text):
            lines = file_text.split("\n")
            for line_number, line in enumerate(lines):
                if self.matches_on_text(line):
                    yield line_number+1, line


class ExactStringSearch(AbstractTextSearch):
    def __init__(self, value):
        self.value = value
        self.description = "%r" % self.value

    def matches_on_text(self, text):
        return self.value in text


class RegexSerch(AbstractTextSearch):
    def __init__(self, regex_string):
        self.regex_string = regex_string
        self.regex = re.compile(self.regex_string)

    def matches_on_text(self, file_path):
        return self.regex.search(file_path)


class SearchResultsReporter:

    def report_search_start(self, search, description):
        print("SEARCH FOR %s, %s" % (search.description, description))
        print("")

    def report_search_end(self):
        print("="*100)

    def report_match_on_file_path(self, file_path):
        print("%s:0:<file>" % file_path)


    def report_match_on_line(self, file_path, line_number, line):
        print("%s:%d:%s" % (file_path, line_number, line))

    def report_unexpected_files(self, unexpected_files):
        if unexpected_files:
            print("")
            print(" UNEXPECTED FILES: %r" % [str(file_path) for file_path in unexpected_files])

class SourceCodeSearcher:

    search_spec_params = ['included_extensions', 'excluded_extensions',
                          'included_files', 'excluded_files', 'excluded_dirs']

    @classmethod
    def get_searcher(cls, source_dir, project_type, spec_path, verbose=False):
        source_dir_spec_dir = source_dir / '_project' / 'search'
        project_search_spec_file_name = '%s.search-spec.json' % project_type
        search_specs = {param: [] for param in cls.search_spec_params}
        for spec_dir in spec_path + [source_dir_spec_dir]:
            spec_file = spec_dir / project_search_spec_file_name
            if spec_file.is_file():
                if verbose:
                    print(" reading spec file %s ..." % spec_file)
                spec_json = json.loads(spec_file.read_text())
                for param in cls.search_spec_params:
                    if param in spec_json:
                        search_specs[param].extend(spec_json[param])
        return SourceCodeSearcher(source_dir, **search_specs)

    def __init__(self, base_dir, included_extensions=None, excluded_extensions=None,
                 included_files=None, excluded_files=None, excluded_dirs=None):
        def set_from_list_or_none(list_or_none):
            return set(list_or_none or [])

        self.base_dir = Path(base_dir)
        os.chdir(self.base_dir)
        self.included_extensions = set_from_list_or_none(included_extensions)
        self.excluded_extensions = set_from_list_or_none(excluded_extensions)
        self.included_files = PathMatcher(included_files)
        self.excluded_files = PathMatcher(excluded_files)
        self.excluded_dirs = PathMatcher(excluded_dirs)
        self.search_target_description = self.get_search_target_description()

    def get_search_target_description(self):
        return (" extensions %r (exclude %r), files %r (exclude %r), exclude directories %r" %
                (sorted(self.included_extensions),
                 sorted(self.excluded_extensions),
                 self.included_files.description(),
                 self.excluded_files.description(),
                 self.excluded_dirs.description()))

    def census(self):
        census_results = CensusResults()
        print("Census:")
        for item, is_dir, is_expected, is_excluded, reason in self.iterate_for_search(yield_excluded=True):
            action = 'unexpected' if (not is_expected) else ('excluded' if is_excluded else 'included')
            if is_dir:
                census_results.add_result(is_expected, ('%s_dir' % (action,),), item)
            else:
                label = ('%s_file_%s' % (action, reason))
                extension = item.suffix
                if extension != '' and reason != 'not_file_or_dir':
                    result_key = (label, extension)
                else:
                    result_key = (label,)
                census_results.add_result(is_expected, result_key, item)
        census_results.display()

    def is_backup_file(self, file_path):
        return file_path.name.endswith("~")

    def search_on_files(self, search_pattern, results_reporter):
        results_reporter.report_search_start(search_pattern, self.search_target_description)
        unexpected_files = []
        for file_path, is_dir, is_expected, _, _ in self.iterate_for_search():
            if not is_dir:
                if is_expected:
                    if search_pattern.matches_file_path(file_path):
                        results_reporter.report_match_on_file_path(file_path)
                    for line_number, line in search_pattern.search_in_file(file_path):
                        results_reporter.report_match_on_line(file_path, line_number, line)
                else:
                    unexpected_files.append(file_path)
        results_reporter.report_unexpected_files(unexpected_files)
        results_reporter.report_search_end()

    def iterate_for_search(self, yield_excluded=False):
        return self.iterate_for_search_on_subdir(Path("."), yield_excluded=yield_excluded)

    def iterate_for_search_on_subdir(self, subdir, yield_excluded):
        """Yield file path, is_dir,  is_expected, is_excluded, reason (for inclusion/exclusion)"""
        items = sorted(subdir.iterdir())
        for item in items:
            is_excluded = False
            is_expected = True
            reason = None
            is_dir = item.is_dir()
            if is_dir:
                if self.excluded_dirs.matches(item):
                    is_excluded = True
                    reason = 'dir'
            elif item.is_file():
                if self.is_backup_file(item):
                    is_excluded = True
                    reason = 'backup'
                elif self.included_files.matches(item):
                    reason = 'file'
                elif self.excluded_files.matches(item):
                    is_excluded = True
                    reason = 'file'
                else:
                    stem = item.stem
                    extension = item.suffix
                    if extension == '':
                        is_expected = False
                        reason = 'extensionless'
                    else:
                        if extension in self.included_extensions:
                            reason = 'extension'
                        elif extension in self.excluded_extensions:
                            is_excluded = True
                            reason = 'extension'
                        else:
                            is_expected = False
                            reason = 'extension'
            else:
                is_excluded = True
                reason = 'not_file_or_dir'
            if (not is_excluded) or yield_excluded:
                yield item, is_dir, is_expected, is_excluded, reason
            if is_excluded and not is_expected:
                raise Exception('%r is excluded, but also not expected, reason %r' % (item, reason))
            if is_dir and not is_excluded:
                yield from self.iterate_for_search_on_subdir(item, yield_excluded=yield_excluded)

    def test_iterator_on_search(self, yield_excluded=False):
        print("SEARCH ITERATION:")
        unexpected_files = []
        for item, is_dir, is_expected, is_excluded, reason in self.iterate_for_search(yield_excluded=yield_excluded):
            if is_expected:
                label = "DIR" if is_dir else "FILE"
                if is_excluded:
                    print("EXCLUDED (%s): %s: %s" % (reason, label, item))
                else:
                    print("%s: %s" % (label, item))
            else:
                unexpected_files.append((str(item), reason))
        print("")
        print("UNEXPECTED: %r" % unexpected_files)

def get_search_spec_path():
    spec_path_string = os.getenv('SOURCE_CODE_SEARCHER_SPEC_PATH')
    spec_path_list = spec_path_string.split(':') if spec_path_string else []
    return [Path(dir_path) for dir_path in spec_path_list]

def get_argument_parser():
    parser = ArgumentParser(description='Search source code')
    parser.add_argument('base_dir', type=Path)
    parser.add_argument('--project-type', default = 'default')
    #parser.add_argument('--spec-path', default = None, type=get_search_spec_path)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--census', action='store_true')
    group.add_argument('--regex')
    group.add_argument('--value')
    return parser

def run_search_command(command_line_args):
    arg_parser = get_argument_parser()
    args = arg_parser.parse_args(command_line_args)

    searcher = SourceCodeSearcher.get_searcher(args.base_dir,
                                               project_type = args.project_type,
                                               spec_path = get_search_spec_path(),
                                               verbose = args.census)
    if args.census:
        searcher.census()
    else:
        results_reporter = SearchResultsReporter()
        if args.regex:
            search = RegexSearch(args.regex)
        elif args.value:
            search = ExactStringSearch(args.value)
        else:
            raise Exception("Unexpected args %r, no census, regex or value" % command_line_args)
        searcher.search_on_files(search, results_reporter)

def run_test():
    example_source_dir = os.getenv("SOURCE_CODE_SEARCHER_TEST_EXAMPLE_DIR")
    spec_path = get_search_spec_path()
    results_reporter = SearchResultsReporter()
    searcher = SourceCodeSearcher.get_searcher(Path(example_source_dir),
                                               project_type = 'python',
                                               spec_path = spec_path)
    search = ExactStringSearch("def")

    print('-'*100)
    searcher.census()

    print('-'*100)
    searcher.test_iterator_on_search(yield_excluded=True)

    print('-'*100)
    searcher.search_on_files(search, results_reporter)

if __name__ == "__main__":
    args = sys.argv[1:]
    if args:
        run_search_command(args)
    else:
        run_test()
