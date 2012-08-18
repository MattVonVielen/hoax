# ex: ft=ruby ts=2 sw=2 et
# automatically run the eunit tests

guard 'shell' do
  app = File.basename(Dir.pwd)
  watch(%r{^(.*/)?(?:src|test)/([^.].*?)(?:_tests?)?.erl$}) do |matches|
    suite = matches[2]

    test_module = File.join 'test', "#{suite}_test*.erl"
    test_module = File.join matches[1], test_module if matches[1]

    cmd = "./rebar eunit skip_deps=true suite=#{suite}"
    puts `#{cmd}`
    if $? == 0
      Growl.notify_ok "#{app}: eunit passed for #{suite}."
    else
      Growl.notify_error "#{app}: eunit failed for #{suite}"
    end
  end
end

