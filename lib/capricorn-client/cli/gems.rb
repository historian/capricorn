class Capricorn::CLI::Gems < Thor
  include Capricorn::Helpers
  
  desc "push PATHS...", "push gems to capricorn"
  def push(*paths)
    paths = paths.flatten.collect do |path|
      Dir.glob(path)
    end.flatten
    paths.each do |path|
      begin
        client.call(:stream => path).gems.push
        puts ui.color("Pushed: #{File.basename(path)}", :green)
      rescue BERTRPC::UserError => e
        puts ui.color(e.message, :red)
      end
    end
  end
  
  desc "missing", "list missing gems"
  def missing
    ui.table %w( Gem Requirements ), clean_missing(client.call.gems.missing), :columns_across, 2
  end
  
  desc "all", "list all gems"
  def all
    ui.table %w( Gem Versions ), clean_all(client.call.gems.all), :columns_across, 2
  end
  
private
  
  def clean_all(gems)
    gems = gems.last
    gems.inject(Hash.new { |h,k| h[k] = [] }) do |m, g|
      header, name, version = *(g.to_ary)
      version = version.last.join('.')
      m[name.to_s] << version
      m
    end.collect do |name, versions|
      [name, versions.join(', ')]
    end.flatten
  end
  
  def clean_missing(deps)
    deps = deps.last if deps and deps.first == :ok
    return [] if deps.nil? or deps.empty?
    deps.sort! { |a,b| a.first <=> b.first }
    deps.collect do |dep|
      reqs = dep.last.collect do |req|
        op = req.shift
        version = req.shift.to_a.last.join('.')
        "#{op} #{version}"
      end.join(', ')
      [dep.first, reqs]
    end.flatten
  end
  
end