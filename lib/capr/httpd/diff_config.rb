class Capr::Httpd::DiffConfig < Capr::Do::Action(:start)

  def initialize(options={})
    @since = options[:since]
  end

  def start
    repo_url  = config.config_repo

    action = Capr::Git::FindChanges.new(repo_url, :branch => 'master',
                                                  :since  => @since)
    action.on_message &method(:fire_message)
    action.errback    &method(:fail)
    action.callback do |changes|
      all = []
      changes[:created].each { |f| all.push([:C, f]) }
      changes[:updated].each { |f| all.push([:U, f]) }
      changes[:deleted].each { |f| all.push([:D, f]) }
      succeed(all)
    end
    action.call
  end

end