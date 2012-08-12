# ex: ft=ruby ts=2 sw=2 et
# automatically run the eunit tests

guard 'shell' do
  app = File.basename(Dir.pwd)
  watch(%r{^(?:src|include|test)/(?:[^.].*?).[eh]rl$}) do
    cmd = "./rebar eunit skip_deps=true"
    puts `#{cmd}`
    if $? == 0
      Growl.notify_ok "#{app}: eunit passed."
    else
      Growl.notify_error "#{app}: eunit failed"
    end
  end
end

