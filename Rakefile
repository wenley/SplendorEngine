
require 'rake/clean'

LIBRARIES = ['str.cmxa']

DEPENDENCIES = {
  # No dependencies
  globals: [],
  shuffle: [],

  # Data types
  data: [:globals],
  board: [:data],
  player: [:data],
  parse: [:data],
  action: [:data, :board],
  game: [:player, :board],

  # IO modules
  display: [:board],
  read: [:parse],
  prompt: [:data, :action],
  cleanup: [:player, :board, :display],

  # Update game state
  validation: [:action, :player, :board],
  process: [:action, :player, :board],

  # High-level modules
  start: [:board, :read, :shuffle, :game],
  engine: [:start, :prompt, :validation, :process, :cleanup],
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
# Test source = test/<>_test.ml
# Test build target = test/<>_test.cmx
#   Covered by generic *.cmx rule
# Task to execute = test:<>
#   TODO
#
def test_executable(root)
  "test_#{root}"
end

# Creates a naming convention of <filename>_test.ml
def test_source(root)
  "test/#{root}_test.ml"
end

def test_build(root)
  "test/#{root}_test.cmx"
end

# Define tasks for tests
DEPENDENCIES.keys.each do |key|
  executable = test_executable(key)
  all_dependencies = expand_dependencies(key).map do |dependency|
    "#{dependency}.cmx"
  end + ['test/shared.cmx', test_build(key)]

  all_files = LIBRARIES + all_dependencies

  task executable => all_dependencies do |task|
    execute "ocamlopt -o #{executable} #{all_files.join(' ')}"
  end

  # TODO do this better
  task "test:#{key}" => [executable] do |task|
    puts execute "./#{executable}"
  end
  CLOBBER << executable
end

task :all => DEPENDENCIES.keys

rule '.cmx' => ['.ml', '.cmi'] do |task|
  execute "ocamlopt -c #{task.source} -I test"
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
  execute "ocamlopt -c #{task.source} -I test"
end

CLEAN.include(INTERFACE_FILES.ext('.cmi'))
CLEAN.include(SOURCE_FILES.ext('.cmi'))
CLEAN.include(TEST_FILES.ext('.cmi'))

