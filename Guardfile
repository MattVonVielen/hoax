# ex: ft=ruby ts=2 sw=2 et
# automatically run the eunit tests

guard 'shell' do
  watch(%r{^(?:src|test)/([^.].*?)(?:_test)?.erl$}) do |m|
    suite = m[1]
    looking_for = File.join(File.dirname(__FILE__), 'test', "#{suite}_test.erl")
    if File.exist?(looking_for)
      cmd = "./rebar eunit skip_deps=true suite=#{suite}"
      puts `#{cmd}`
      if $? == 0
        Growl.notify_ok "#{suite}: eunit passed."
      else
        Growl.notify_error "#{suite}: eunit failed"
      end
    else
      Growl.notify_warning "No tests for #{suite}!"
    end
  end
end

