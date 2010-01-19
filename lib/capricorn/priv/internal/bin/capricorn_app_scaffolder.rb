#!/usr/bin/env ruby

require File.dirname(__FILE__)+'/../lib/driver'
require File.dirname(__FILE__)+'/../lib/system_context'

# -record(application, {
#   node=undefined,
#   name=undefined,
#   domains=[],
#   environment=development,
#   www_user=undefined,
#   www_group=undefined,
#   root_path=undefined,
#   gems=[] % #gem_ref
# }).
def convert_app(app)
  application = {}
  application[:id]          = app[1]
  application[:node]        = app[2]
  application[:name]        = app[3]
  application[:domain]      = app[4].shift
  application[:aliases]     = app[4]
  application[:environment] = app[5]
  application
end

Erlang do |cmd|
  case cmd.shift
  when :create
    recipe = cmd.shift
    app = convert_app(cmd.first)
    
    ctx = SystemContext.run(File.dirname(__FILE__)+"/../recipes/#{recipe}.rb",
      :application => app)
    
    send t[true, t[ctx.www_user, ctx.www_group, ctx.root_path]]
  end
end
