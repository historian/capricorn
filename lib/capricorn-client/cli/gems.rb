class Capricorn::CLI::Gems < Capricorn::CLI
  include Capricorn::Helpers

  namespace :gems

  desc "push PATHS...", "push gems to capricorn"
  def push(*paths)
    paths = paths.flatten.collect do |path|
      Dir.glob(path)
    end.flatten

    paths.each do |path|
      begin
        client.call(:stream => path).gems.push
        info "Pushed: #{File.basename(path)}"
      rescue BERTRPC::UserError => e
        halt(e.message, $capr_gems_weak_push)
      end
    end
  end

  desc "missing", "list missing gems"
  def missing
    all = clean_missing(client.call.gems.missing)
    all.sort! { |a, b| a[0] <=> b[0] }
    width = all.inject(0) { |m, (name, _)| (m > name.size ? m : name.size) }
    all.each do |(name, versions)|
      padding = ' ' * (width - name.size)
      puts "#{padding}#{name}: #{versions}"
    end
  end

  desc "all", "list all gems"
  def all
    all = clean_all(client.call.gems.all)
    all.sort! { |a, b| a[0] <=> b[0] }
    width = all.inject(0) { |m, (name, _)| (m > name.size ? m : name.size) }
    all.each do |(name, versions)|
      padding = ' ' * (width - name.size)
      puts "#{padding}#{name}: #{versions}"
    end
  end

private

  def clean_all(gems)
    gems = gems.last
    gems.inject(Hash.new { |h,k| h[k] = [] }) do |m, g|
      header, name, version = *(g.to_ary)
      next(m) unless version.last
      version = version.last.join('.')
      m[name.to_s] << version
      m
    end.collect do |name, versions|
      versions = versions.reverse
      if versions.size > 5
        versions = versions[0, 5].join(', ') + ", (and #{versions.size - 5} more)"
      else
        versions = versions.join(', ')
      end
      [name, versions]
    end
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
      end.reverse.join(', ')
      [dep.first, reqs]
    end
  end

end