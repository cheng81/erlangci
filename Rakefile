task :default => [:build, :test]
task :build => [:make, :ebin, :clean]
task :run => [:build, :runerl]
def output_file(input_file)
  'ebin/' + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

def module_name(input_file)
  File.basename(input_file)[0..-6]
end

#remember to add chapters directories here
ERL_SRC_DIRS = ['common','chap2']
#create the list of all sources
ERL_SRC = FileList.new( (ERL_SRC_DIRS).map { |dir| "#{dir}/src/**/*.erl" } )

ERL_SRC.each do |input|
  file output_file(input) => input do
    sh "erlc -o artifacts/beam #{input}"
  end
end

task :make => (ERL_SRC).map { |input_file| output_file(input_file) }

task :ebin do
  FileList["artifacts/beam/*.beam"].each { |file| cp file, "ebin" }
end

#start the unit tests runner with the source directories
task :test do
  sh "escript unittest #{ERL_SRC_DIRS.join(" ")}"
end

task :clean do
  FileList['artifacts/**/*.{erl,beam}'].each { |f| rm_f f }
end

task :runerl do
  sh "erl -pa ebin -start sasl"
end