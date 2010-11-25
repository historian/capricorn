class Capr::CLI::Capr
  include Opts::DSL
  include Capr::Helpers::Shared

  class_use Opts::Shell
  class_use Opts::ErrorHandler
  class_use Opts::Environment, 'CAPRD_'
  class_use Opts::ManHelp,
    :path    => File.expand_path('../../../man', __FILE__),
    :default => 'capr.1'

  option   'node', :type => :string, :required => true
  argument 'REPO', :type => :string, :required => true
  def ping(env, branches)
    node  = env['NODE']
    repo  = env['REPO']
    shell = env['opts.shell']

    EM.run do
      action = Capr::Httpc::Forward.new(node, repo, branches)
      
      action.on_message do |m|
        if m['success']
          shell.status('Info', m['message'], :green)
        else
          shell.status('Error', m['message'], :red)
        end
      end
      
      action.callback { EM.stop_event_loop }
      action.errback  {
        shell.status('Error', "failed to ping #{node}", :red)
        EM.stop_event_loop
      }
      action.call
    end
  end

end