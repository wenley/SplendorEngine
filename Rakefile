
require 'rake/clean'

LIBRARIES = ['str.cmxa']

DEPENDENCIES = {
  globals: [],
  shuffle: [],
  data: [:globals],

  board: [:data],
  player: [:data],
  parse: [:data],

  read: [:parse],
  cleanup: [:player, :board],
  game: [:player, :board],
  display: [:board],

  start: [:board, :read, :shuffle, :game],
  engine: [:board, :player, :start, :game],
}

SOURCE_FILES = Rake::FileList.new('*.ml') do |fl|
  # exclude things here
end
INTERFACE_FILES = Rake::FileList.new('*.mli')
TEST_FILES = Rake::FileList.new('test/*.ml')

CLEAN.include(SOURCE_FILES.ext(''))
CLEAN.include(SOURCE_FILES.ext('.o'))
CLEAN.include(TEST_FILES.ext(''))
CLEAN.include(TEST_FILES.ext('.o'))

def execute(command)
  puts command
  `#{command}`
end

def expand_dependencies(target)
  DEPENDENCIES[target].flat_map { |dep| expand_dependencies(dep) }.uniq + [target]
end

task :default => ['engine']

# Define tasks for executables
DEPENDENCIES.keys.each do |key|
  all_dependencies = expand_dependencies(key).map do |dependency|
    "#{dependency}.cmx"
  end

  task key => all_dependencies do |task|
    execute "ocamlopt -o #{key} #{LIBRARIES.join(' ')} #{all_dependencies.join(' ')}"
  end
  CLOBBER << key
end

# Test executable = test_<>
#   Defined below
# Test build target = test/test_<>.cmx
#   Covered by generic *.cmx rule
# Task to execute = test:<>
#   TODO

# Define tasks for tests
# Creates a naming convention of test_<filename>.ml
DEPENDENCIES.keys.each do |key|
  test_file = "test_#{key}"

  all_dependencies = expand_dependencies(key).map do |dependency|
    "#{dependency}.cmx"
  end + ["test/#{test_file}.cmx"]

  all_files = LIBRARIES + all_dependencies

  task test_file => all_dependencies do |task|
    execute "ocamlopt -o #{test_file} #{all_files.join(' ')}"
  end
end

task :all => DEPENDENCIES.keys

rule '.cmx' => ['.ml', '.cmi'] do |task|
  execute "ocamlopt -c #{task.source}"
end
CLEAN.include(SOURCE_FILES.ext('.cmx'))
CLEAN.include(TEST_FILES.ext('.cmx'))

def source_for_interface(filename)
  root = filename.ext('')
  interface = INTERFACE_FILES.detect { |f| f.ext('') == root }
  source = SOURCE_FILES.detect { |f| f.ext('') == root }

  # This should maybe strip leading directory
  test = TEST_FILES.detect { |f| f.ext('') == root }

  interface || source || test
end

rule '.cmi' => ->(f){ source_for_interface(f) } do |task|
  execute "ocamlopt -c #{task.source}"
end

CLEAN.include(INTERFACE_FILES.ext('.cmi'))
CLEAN.include(SOURCE_FILES.ext('.cmi'))
CLEAN.include(TEST_FILES.ext('.cmi'))

