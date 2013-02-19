# ex: ft=ruby ts=2 sw=2 et
# automatically run the eunit tests

# From: http://stackoverflow.com/a/5471032
# Cross-platform way of finding an executable in the $PATH.
#   which('ruby') #=> /usr/bin/ruby
def which(cmd)
  exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
  ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
    exts.each { |ext|
      exe = "#{path}/#{cmd}#{ext}"
      return exe if File.executable? exe
    }
  end
  return nil
end

rebar = which 'rebar'
rebar = File.join '.', 'rebar' unless rebar
if File.executable? rebar
  puts "Using rebar script at #{rebar}"
else
  throw "Cannot find rebar!"
end

guard :shell do
  app = File.basename(Dir.pwd)
  watch(%r{^(.*/)?(?:src|test)/([^.].*?)(?:_test(s)?)?.erl$}) do |matches|
    filepath, subdir, modname, plural = matches
    suite = "#{modname}_test#{plural}"

    test_module = File.join 'test', "#{suite}.erl"
    test_module = File.join subdir, test_module if subdir

    if Dir[test_module].empty?
      # n "no tests for #{modname}", app, :warning
    else
      `tmux set-window-option status-left #{modname}`
      puts `#{rebar} eunit skip_deps=true suites=#{suite}`
      if $? == 0
        # n "eunit passed for #{suite}", app, :success
        `tmux set-window-option status-left-bg green`
      else
        # n "eunit failed for #{suite}", app, :failed
        `tmux set-window-option status-left-bg red`
      end
      nil
    end

  end
end
