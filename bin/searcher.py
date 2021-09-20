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
        show_results("UNEXPECTED", self.unexpected_results)
        show_results("EXPECTED", self.expected_results)

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
        file_text = file_path.read_text(encoding='utf-8', errors='replace')
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

def stringified_path_list(paths):
    return [str(path) for path in paths]

class SearchResultsReporter:

    def __init__(self, max_line_length_to_show):
        self.max_line_length_to_show = max_line_length_to_show

    def report_search_start(self, search, base_dir, description):
        print("  SEARCH FOR %s in %s, %s" % (search.description, base_dir,
                                           description))
        print("")

    def report_search_end(self):
        print("")
        print("="*100)

    def report_match_on_file_path(self, file_path, file_type):
        print("%s:0:<%s>" % (file_path, file_type))

    def report_match_on_line(self, file_path, line_number, line):
        if len(line) > self.max_line_length_to_show:
            line_value = "[LINE LENGTH = %d > %d]%s ..." % (len(line),
                                                            self.max_line_length_to_show,
                                                            line[0:self.max_line_length_to_show])
        else:
            line_value = line
        print("%s:%d:%s" % (file_path, line_number, line_value))

    def report_unexpected_files_summary(self, unexpected_files):
        def extension_summary(suffix, files):
            return "%r(%d)" % (suffix, len(files))
        unexpected_files_by_suffix = defaultdict(list)
        unexpected_extensionless_files = []
        for unexpected_file in unexpected_files:
            suffix = unexpected_file.suffix
            if suffix == "":
                unexpected_extensionless_files.append(unexpected_file)
            else:
                unexpected_files_by_suffix[suffix].append(unexpected_file)
        print("")
        print(" UNEXPECTED FILES:")
        unexpected_files_by_suffix_items = list(unexpected_files_by_suffix.items())
        if unexpected_files_by_suffix:
            print("     extensioned: %s" % (", ".join([extension_summary(suffix, files)
                                                       for suffix, files in list(unexpected_files_by_suffix.items())])))
        if unexpected_extensionless_files:
            print("     extensionless: %r" % stringified_path_list(unexpected_extensionless_files))

    def report_unexpected_files(self, unexpected_files):
        if unexpected_files:
            print("")
            print(" UNEXPECTED FILES: %r" % stringified_path_list(unexpected_files))

    def report_unsearched_items(self, label, items, display_function):
        if items:
            print("")
            print(" UNSEARCHED (%s): %s" % (label, ", ".join([display_function(item) for item in items])))

class PathItem:

    is_dir: bool = False
    is_file: bool = False
    is_included: bool = False
    is_excluded: bool = False
    is_expected: bool = True
    is_too_large: bool = False
    is_symlink: bool = False
    reason: str
    too_large_file_size: int

    def __init__(self, path):
        self.path = path

class SourceCodeSearcher:

    search_spec_params = ['included_extensions', 'excluded_extensions',
                          'included_files', 'excluded_files', 'excluded_dirs']

    @classmethod
    def get_searcher(cls, source_dir, project_type, spec_path, verbose=False, include_unexpected=False,
                     max_line_length_to_show = 1000, max_file_size = 2000000):
        source_dir_spec_dir = source_dir / '_project' / 'search'
        project_search_spec_file_name = '%s.search-spec.json' % project_type
        search_specs = {param: [] for param in cls.search_spec_params}
        spec_files = []
        for spec_dir in spec_path + [source_dir_spec_dir]:
            spec_file = spec_dir / project_search_spec_file_name
            if spec_file.is_file():
                if verbose:
                    print(" reading spec file %s ..." % spec_file)
                spec_files.append(spec_file)
                spec_json = json.loads(spec_file.read_text())
                for param in cls.search_spec_params:
                    if param in spec_json:
                        search_specs[param].extend(spec_json[param])
        project_spec_file = source_dir_spec_dir / project_search_spec_file_name
        missing_project_spec_file = project_spec_file if not project_spec_file.is_file() else None
        search_specs['include_unexpected'] = include_unexpected
        search_specs['max_line_length_to_show'] = max_line_length_to_show
        search_specs['max_file_size'] = max_file_size
        return SourceCodeSearcher(source_dir, project_type, spec_files, missing_project_spec_file,
                                  **search_specs)

    def __init__(self, base_dir, project_type, spec_files, missing_project_spec_file,
                 included_extensions=None, excluded_extensions=None,
                 included_files=None, excluded_files=None, excluded_dirs=None,
                 include_unexpected=False,
                 max_line_length_to_show=None,
                 max_file_size=None):
        def set_from_list_or_none(list_or_none):
            return set(list_or_none or [])

        self.base_dir = Path(base_dir)
        self.project_type = project_type
        self.spec_files = spec_files
        self.missing_project_spec_file = missing_project_spec_file
        os.chdir(self.base_dir)
        self.full_base_dir = self.base_dir.absolute()
        self.included_extensions = set_from_list_or_none(included_extensions)
        self.excluded_extensions = set_from_list_or_none(excluded_extensions)
        self.included_files = PathMatcher(included_files)
        self.excluded_files = PathMatcher(excluded_files)
        self.excluded_dirs = PathMatcher(excluded_dirs)
        self.include_unexpected = include_unexpected
        self.max_line_length_to_show = max_line_length_to_show
        self.max_file_size = max_file_size
        self.search_target_description = self.get_search_target_description()

    def get_search_target_description(self):
        return ("extensions %r (exclude %r), files %r (exclude %r), exclude directories %r" %
                (sorted(self.included_extensions),
                 sorted(self.excluded_extensions),
                 self.included_files.description(),
                 self.excluded_files.description(),
                 self.excluded_dirs.description()))

    def show_missing_project_spec_file(self):
        print("")
        print("Project spec file %s not found (execute command below to create it)"
              % self.missing_project_spec_file)
        print("")
        print('(project-create-project-search-spec-file "%s")' % self.project_type)
        print("")

    def census(self):
        census_results = CensusResults()
        if self.missing_project_spec_file:
            self.show_missing_project_spec_file()
        for item in self.iterate_for_search(yield_excluded=True):
            action = 'unexpected' if (not item.is_expected) else ('excluded' if item.is_excluded else 'included')
            if item.is_dir:
                census_results.add_result(item.is_expected, ('%s_dir' % (action,),), item.path)
            else:
                label = ('%s_file_%s' % (action, item.reason))
                extension = item.path.suffix
                if extension != '' and item.reason != 'not_file_or_dir':
                    result_key = (label, extension)
                else:
                    result_key = (label,)
                census_results.add_result(item.is_expected, result_key, item.path)
        census_results.display()

    def is_backup_file(self, file_path):
        return file_path.name.endswith("~")

    def search_on_files(self, search_pattern, results_reporter):
        def symlink_display(item):
            return str(item.path)
        def too_large_display(item):
            return "%s(%.1fM)" % (str(item.path), item.too_large_size/1048576.0)
        results_reporter.report_search_start(search_pattern, self.full_base_dir, self.search_target_description)
        unexpected_files = []
        symlinks = []
        too_large_files = []
        for item in self.iterate_for_search():
            if item.is_dir:
                if search_pattern.matches_file_path(item.path):
                    results_reporter.report_match_on_file_path(item.path,
                                                               'dir_symlink' if item.is_symlink else 'dir')
            elif item.is_file:
                if (item.is_expected or self.include_unexpected):
                    if search_pattern.matches_file_path(item.path):
                        results_reporter.report_match_on_file_path(item.path,
                                                                   'symlink' if item.is_symlink else 'file')
                    if item.is_symlink:
                        symlinks.append(item)
                    elif item.is_too_large:
                        too_large_files.append(item)
                    else:
                        for line_number, line in search_pattern.search_in_file(item.path):
                            results_reporter.report_match_on_line(item.path, line_number, line)
                if not item.is_expected:
                    unexpected_files.append(item.path)
        results_reporter.report_unsearched_items('symlinks', symlinks, symlink_display)
        results_reporter.report_unsearched_items('too large', too_large_files, too_large_display)
        if self.project_type == 'general':
            if unexpected_files:
                results_reporter.report_unexpected_files_summary(unexpected_files)
                print("")
                print("  ( SEARCH SPECS: %s )" % (", ".join(stringified_path_list(self.spec_files))))
        else:
            results_reporter.report_unexpected_files(unexpected_files)
        results_reporter.report_search_end()

    def iterate_for_search(self, yield_excluded=False):
        return self.iterate_for_search_on_subdir(Path("."), yield_excluded=yield_excluded)

    def iterate_for_search_on_subdir(self, subdir, yield_excluded):
        """Yield file path, is_dir, is_expected, is_excluded, reason (for inclusion/exclusion)"""
        try:
            children = sorted(subdir.iterdir())
        except PermissionError as pe:
            print("WARNING: permisssion error %s" % pe)
            return
        for child in children:
            path_item = PathItem(child)
            path_item.is_dir = child.is_dir()
            if child.is_symlink():
                path_item.is_symlink = True
            if path_item.is_dir:
                if self.excluded_dirs.matches(child):
                    path_item.is_excluded = True
                    path_item.reason = 'dir'
            elif child.is_file():
                path_item.is_file = True
                if self.is_backup_file(child):
                    path_item.is_excluded = True
                    path_item.reason = 'backup'
                elif self.excluded_files.matches(child):
                    path_item.is_excluded = True
                    path_item.reason = 'file'
                elif self.included_files.matches(child):
                    path_item.reason = 'file'
                else:
                    stem = child.stem
                    extension = child.suffix
                    if extension == '':
                        path_item.is_expected = False
                        path_item.reason = 'extensionless'
                    else:
                        if extension in self.excluded_extensions:
                            path_item.is_excluded = True
                            path_item.reason = 'extension'
                        elif extension in self.included_extensions:
                            path_item.reason = 'extension'
                        else:
                            path_item.is_expected = False
                            path_item.reason = 'extension'
                    if not (path_item.is_excluded or path_item.is_symlink):
                        file_size = child.stat().st_size
                        path_item.is_too_large = file_size > self.max_file_size
                        if path_item.is_too_large:
                            path_item.reason = 'too_large'
                            path_item.too_large_size = file_size
            else:
                path_item.is_excluded = True
                path_item.reason = 'not_file_or_dir'
            if (not path_item.is_excluded) or yield_excluded:
                yield path_item
            if path_item.is_excluded and not path_item.is_expected:
                raise Exception('%r is excluded, but also not expected, reason %r' % (item, reason))
            if path_item.is_dir and not path_item.is_excluded and not path_item.is_symlink:
                yield from self.iterate_for_search_on_subdir(path_item.path, yield_excluded=yield_excluded)

    def test_iterator_on_search(self, yield_excluded=False):
        print("SEARCH ITERATION:")
        unexpected_files = []
        for item in self.iterate_for_search(yield_excluded=yield_excluded):
            if item.is_expected:
                label = "DIR" if item.is_dir else "FILE"
                if item.is_excluded:
                    print("EXCLUDED (%s): %s: %s" % (item.reason, label, item.path))
                else:
                    print("%s: %s" % (label, item.path))
            else:
                unexpected_files.append((str(item.path), item.reason))
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
    parser.add_argument('--include-unexpected', action='store_true')
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
                                               verbose = args.census,
                                               include_unexpected = args.include_unexpected)
    if args.census:
        searcher.census()
    else:
        results_reporter = SearchResultsReporter(max_line_length_to_show=searcher.max_line_length_to_show)
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
    searcher = SourceCodeSearcher.get_searcher(Path(example_source_dir),
                                               project_type = 'python',
                                               spec_path = spec_path)
    results_reporter = SearchResultsReporter(searcher.max_file_size)
    search = ExactStringSearch("def ")

    print('-'*100)
    searcher.census()

    print('-'*100)
    searcher.test_iterator_on_search(yield_excluded=True)

    print('-'*100)
    searcher.search_on_files(search, results_reporter)

def show_if_dev():
    emacs_site_lisp_dir = os.getenv("EMACS_SITE_LISP_DIR")
    if not emacs_site_lisp_dir:
        print("Running in environment where $EMACS_SITE_LISP_DIR is not defined")
    else:
        emacs_site_lisp_dir_path = Path(emacs_site_lisp_dir)
        this_dir = Path(__file__).parent
        if not emacs_site_lisp_dir_path in this_dir.parents:
            banner_line = "##### DEVELOPMENT VERSION running from %s #" % this_dir
            banner_length = len(banner_line)
            print("#" * banner_length)
            print(banner_line)
            print("#" * banner_length)

if __name__ == "__main__":
    show_if_dev()
    args = sys.argv[1:]
    if args:
        run_search_command(args)
    else:
        run_test()
    show_if_dev()
