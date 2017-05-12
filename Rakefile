
require 'rake/clean'

LIBRARIES = ['str.cmxa']

DEPENDENCIES = {
  globals: [],
  data: [:globals],
  player: [:data],
  parse: [:data],
  read: [:parse],
  shuffle: [],
  board: [:globals, :data],
  start: [:board, :read, :shuffle],
  engine: [:player, :start],
}

SOURCE_FILES = Rake::FileList.new('*.ml') do |fl|
  # exclude things here
end
INTERFACE_FILES = Rake::FileList.new('*.mli')

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

task :all => DEPENDENCIES.keys

rule '.cmx' => '.ml' do |task|
  execute "ocamlopt -c #{task.source}"
end

rule '.cmi' => '.mli' do |task|
  execute "ocamlopt -c #{task.source}"
end

CLEAN.include(SOURCE_FILES.ext(''))
CLEAN.include(SOURCE_FILES.ext('.cmx'))
CLEAN.include(SOURCE_FILES.ext('.o'))
CLEAN.include(SOURCE_FILES.ext('.cmi'))